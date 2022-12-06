{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.List where




import           GHCup
import           GHCup.Prelude
import           GHCup.Prelude.Ansi
import           GHCup.Types
import           GHCup.OptParse.Common

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Char
import           Data.List                      ( intercalate, sort )
import           Data.Functor
import           Data.Maybe
import           Data.Versions           hiding ( str )
import           Data.Void
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Exit
import           System.Console.Pretty   hiding ( color )

import qualified Data.Text                     as T
import qualified System.Console.Pretty         as Pretty
import Control.Exception.Safe (MonadMask)
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MPC






    ---------------
    --[ Options ]--
    ---------------


data ListOptions = ListOptions
  { loTool     :: Maybe Tool
  , lCriteria  :: Maybe ListCriteria
  , lRawFormat :: Bool
  }




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
              <> completer (toolCompleter)
            )
          )
    <*> optional
          (option
            (eitherReader criteriaParser)
            (  short 'c'
            <> long "show-criteria"
            <> metavar "<installed|set|available>"
            <> help "Show only installed/set/available tool versions"
              <> completer (listCompleter ["installed", "set", "available"])
            )
          )
    <*> switch
          (short 'r' <> long "raw-format" <> help "More machine-parsable format"
          )




    -----------------
    --[ Utilities ]--
    -----------------


printListResult :: Bool -> Bool -> [ListResult] -> IO ()
printListResult no_color raw lr = do

  let
    color | raw || no_color = (\_ x -> x)
          | otherwise       = Pretty.color

  let
    printTag Recommended        = color Green "recommended"
    printTag Latest             = color Yellow "latest"
    printTag Prerelease         = color Red "prerelease"
    printTag (Base       pvp'') = "base-" ++ T.unpack (prettyPVP pvp'')
    printTag (UnknownTag t    ) = t
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
                     ++ (if fromSrc then [color Blue "compiled"] else mempty)
                     ++ (if lStray then [color Yellow "stray"] else mempty)
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

  forM_ (if raw then rows else padded) $ \row -> putStrLn $ unwords row
 where

  padTo str' x =
    let lstr = strWidth str'
        add' = x - lstr
    in  if add' < 0 then str' else str' ++ replicate add' ' '





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
      -> (ReaderT AppState m ExitCode -> m ExitCode)
      -> m ExitCode
list ListOptions{..} no_color runAppState =
  runAppState (do
      l <- listVersions loTool lCriteria
      liftIO $ printListResult no_color lRawFormat l
      pure ExitSuccess
    )
