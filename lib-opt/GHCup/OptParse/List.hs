{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.List where




import           GHCup
import           GHCup.Prelude
import           GHCup.Types
import           GHCup.Utils.Parsers (dayParser, toolParser, criteriaParser)
import           GHCup.OptParse.Common
import           GHCup.Prelude.String.QQ
import           GHCup.Utils.Output
import           GHCup.Utils.Pager

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Char
import           Data.List                      ( intercalate, sort )
import           Data.Functor
import           Data.Maybe
import           Data.Time.Calendar             ( Day )
import           Data.Versions
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Exit
import           System.Console.Pretty   hiding ( color )

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified System.Console.Pretty         as Pretty
import Control.Exception.Safe (MonadMask)
import GHCup.Types.Optics
import GHCup.Prelude.Logger (logDebug)






    ---------------
    --[ Options ]--
    ---------------


data ListOptions = ListOptions
  { loTool     :: Maybe Tool
  , lCriteria  :: Maybe ListCriteria
  , lFrom      :: Maybe Day
  , lTo        :: Maybe Day
  , lHideOld   :: Bool
  , lShowNightly :: Bool
  , lRawFormat :: Bool
  } deriving (Eq, Show)




    ---------------
    --[ Parsers ]--
    ---------------

listOpts :: Parser ListOptions
listOpts =
  ListOptions
    <$> optional
          (option
            (eitherReader toolParser)
            (short 't' <> long "tool" <> metavar "<ghc|cabal|hls|stack>" <> help
              "Tool to list versions for. Default is all"
              <> completer toolCompleter
            )
          )
    <*> optional
          (option
            (eitherReader criteriaParser)
            (  short 'c'
            <> long "show-criteria"
            <> metavar "<installed|set|available>"
            <> help "Apply filtering criteria, prefix with + or -"
              <> completer (listCompleter
                [ "+installed", "+set", "+available", "-installed", "-set", "-available"])
            )
          )
    <*> optional
          (option
            (eitherReader dayParser)
            (short 's' <> long "since" <> metavar "YYYY-MM-DD" <> help
              "List only tools with release date starting at YYYY-MM-DD or later"
              <> completer toolCompleter
            )
          )
    <*> optional
          (option
            (eitherReader dayParser)
            (short 'u' <> long "until" <> metavar "YYYY-MM-DD" <> help
              "List only tools with release date earlier than YYYY-MM-DD"
              <> completer toolCompleter
            )
          )
    <*> switch
          (short 'o' <> long "hide-old" <> help "Hide 'old' tool versions (installed ones are always shown)"
          )
    <*> switch
          (short 'n' <> long "show-nightly" <> help "Show nightlies (installed ones are always shown)"
          )
    <*> switch
          (short 'r' <> long "raw-format" <> help "More machine-parsable format"
          )


    --------------
    --[ Footer ]--
    --------------


listToolFooter :: String
listToolFooter = [s|Discussion:
  Lists tool versions with optional criteria.
  Nightlies are by default hidden.

Examples:
  # query nightlies in a specific range
  ghcup list --show-nightly --since 2022-12-07 --until 2022-12-31
  # show all installed GHC versions
  ghcup list -t ghc -c installed|]


    -----------------
    --[ Utilities ]--
    -----------------


printListResult :: (HasLog env , MonadReader env m, MonadIO m)
                => Bool -> PagerConfig -> Bool -> [ListResult] -> m ()
printListResult no_color (PagerConfig pList pCmd) raw lr = do

  let
    color | raw || no_color = (\_ x -> x)
          | otherwise       = Pretty.color

  let
    printTag Recommended        = color Green "recommended"
    printTag Latest             = color Yellow "latest"
    printTag Prerelease         = color Red "prerelease"
    printTag Nightly            = color Red "nightly"
    printTag (Base       pvp'') = "base-" ++ T.unpack (prettyPVP pvp'')
    printTag (UnknownTag t    ) = t
    printTag LatestPrerelease   = color Red "latest-prerelease"
    printTag LatestNightly      = color Red "latest-nightly"
    printTag Old                = ""

  let
    rows =
      (\x -> if raw
          then x
          else [color Green "", "Tool", "Version", "Tags", "Notes"] : x
        )
        . fmap
            (\ListResult {..} ->
              let marks = if
                    | lSet       -> (color Green (if isWindows then "IS" else "✔✔"))
                    | lInstalled -> (color Green (if isWindows then "I " else "✓ "))
                    | otherwise  -> (color Red   (if isWindows then "X " else "✗ "))
              in
                (if raw then [] else [marks])
                  ++ [ fmap toLower . show $ lTool
                     , case lCross of
                       Nothing -> T.unpack . prettyVer $ lVer
                       Just c  -> T.unpack (c <> "-" <> prettyVer lVer)
                     , intercalate "," (filter (/= "") . fmap printTag $ sort lTag)
                     , intercalate ","
                     $  (if hlsPowered
                          then [color Green "hls-powered"]
                          else mempty
                        )
                     ++ (if lStray then [color Yellow "stray"] else mempty)
                     ++ (case lReleaseDay of
                           Nothing -> mempty
                           Just d  -> [color Blue (show d)])
                     ++ (if lNoBindist
                          then [color Red "no-bindist"]
                          else mempty
                        )
                     ]
            )
        $ lr
  let cols =
        foldr (\xs ys -> zipWith (:) xs ys) (repeat []) rows
      lengths = fmap (maximum . fmap strWidth) cols
      padded  = fmap (\xs -> zipWith padTo xs lengths) rows

  let text = fmap (T.pack . unwords) (if raw then rows else padded)
  fits <- liftIO $ fitsInTerminal text
  if | pList
     , not raw
     , Just False <- fits
     , Just cmd <- pCmd -> do
         r <- liftIO $ sendToPager cmd text
         case r of
           Left e -> do
             logDebug $ "Failed to send to pager '" <> T.pack cmd <> "': " <> T.pack (show e)
             liftIO $ forM_ text T.putStrLn
           Right _ -> pure ()
     | otherwise -> liftIO $ forM_ text T.putStrLn





    ------------------
    --[ Entrypoint ]--
    ------------------



list :: ( Monad m
         , MonadMask m
         , MonadUnliftIO m
         , MonadFail m
         )
      => ListOptions
      -> Bool
      -> PagerConfig
      -> (ReaderT AppState m ExitCode -> m ExitCode)
      -> m ExitCode
list ListOptions{..} no_color pgc runAppState =
  runAppState (do
      l <- listVersions loTool (maybeToList lCriteria) lHideOld lShowNightly (lFrom, lTo)
      printListResult no_color pgc lRawFormat l
      pure ExitSuccess
    )
