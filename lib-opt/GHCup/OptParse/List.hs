{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module GHCup.OptParse.List where




import           GHCup.Command.List
import           GHCup.Errors
import           GHCup.Prelude
import           GHCup.Types
import           GHCup.Input.Parsers (dayParser, toolParserWithGHCup, criteriaParser, revisionShowParser)
import           GHCup.OptParse.Common
import           GHCup.Prelude.String.QQ
import           GHCup.Compat.Terminal
import           GHCup.Compat.Pager

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad (forM_)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Char
import           Data.List                      ( intercalate, sort )
import           Data.Functor
import           Data.Maybe
import           Data.Time.Calendar             ( Day )
import           Data.Variant.Excepts
import           Data.Versions
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Exit
import           System.Console.Pretty   hiding ( color )
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified System.Console.Pretty         as Pretty
import Control.Exception.Safe (MonadMask)
import GHCup.Types.Optics
import qualified Data.Map.Strict as M






    ---------------
    --[ Options ]--
    ---------------



data ListOptions = ListOptions
  { loTool         :: Maybe [Tool]
  , lCriteria      :: [ListCriteria]
  , lFrom          :: Maybe Day
  , lTo            :: Maybe Day
  , lHideOld       :: Bool
  , lShowNightly   :: Bool
  , lRawFormat     :: Bool
  , lShowRevisions :: ShowRevisions
  } deriving (Eq, Show)




    ---------------
    --[ Parsers ]--
    ---------------

listOpts :: Parser ListOptions
listOpts =
  ListOptions
    <$> optional
         (some
          (option
            (eitherReader toolParserWithGHCup)
            (short 't' <> long "tool" <> metavar "<ghc|cabal|hls|stack>" <> help
              "Tool to list versions for. Default is all"
              <> completer toolCompleter
            )
          )
        )
    <*> many
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
    <*> option
          (eitherReader revisionShowParser)
          (long "show-revisions" <> help "How to show revisions"
              <> metavar "<updates|all|none>"
              <> completer revisionCompleter
              <> value ShowUpdates
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


printListResult ::
  ( HasLog env
  , MonadReader env m
  , MonadIO m
  )
  => Bool
  -> ShowRevisions
  -> PagerConfig
  -> Bool
  -> ToolListResult
  -> m ()
printListResult no_color show_revisions (PagerConfig pList pCmd) raw lr = do

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
    printTag Experimental       = color Red "experimental"
    printTag Old                = ""

  let
    rows =
      (\x -> if raw
          then x
          else [color Green "", "Tool", "Version", "Tags", "Notes"] : x
        )
        . mconcat . fmap
            (\(lTool, (_, ls)) -> ls <&> \ListResult{..} ->
              let marks = if
                   | lSet       -> (color Green (if isWindows then "IS" else "✔✔"))
                   | lInstalled -> (color Green (if isWindows then "I " else "✓ "))
                   | otherwise  -> (color Red   (if isWindows then "X " else "✗ "))
              in
                (if raw then [] else [marks])
                  ++ [ fmap toLower . prettyShow $ lTool
                     , let rev = case lRev of
                                   (rev', RevUpdate)
                                     | show_revisions == ShowNone -> ""
                                     | otherwise -> "-r" <> show rev'
                                   (rev', RevOutdated)
                                     | show_revisions == ShowNone -> ""
                                     | otherwise -> "-r" <> show rev'
                                   (rev', RevNormal)
                                     | show_revisions == ShowAll -> "-r" <> show rev'
                                     | otherwise -> ""
                       in case lCross of
                            Nothing -> T.unpack (prettyVer lVer) <> rev
                            Just c  -> T.unpack (c <> "-" <> prettyVer lVer) <> rev
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
        $ M.toList lr
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



list ::
  ( Monad m
  , MonadMask m
  , MonadUnliftIO m
  , MonadFail m
  )
  => ListOptions
  -> Bool
  -> PagerConfig
  -> (IO (AppState, IO ()), LeanAppState)
  -> m ExitCode
list ListOptions{..} no_color pgc (getAppState', leanAppstate) = do
  r <- run $ do
      l <- listVersions loTool lCriteria lShowRevisions lHideOld lShowNightly (lFrom, lTo)
      lift $ printListResult no_color lShowRevisions pgc lRawFormat l
  case r of
    (VRight _, up) -> do
      liftIO up
      pure ExitSuccess
    (VLeft e, _) -> do
      runLogger $ logError $ T.pack $ prettyHFError e
      pure $ ExitFailure 44
 where
  runLogger = flip runReaderT leanAppstate
  run action' = do
    (appstate', up) <- liftIO getAppState'
    r <- flip runReaderT appstate'
                  . runResourceT
                  . runE
                    @'[GHCup.Errors.ParseError]
                  $ action'
    pure (r, up)
