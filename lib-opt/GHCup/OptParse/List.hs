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
          (short 'o' <> long "hide-old" <> help "Hide 'old' GHC versions (installed ones are always shown)"
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


printListResult :: Bool -> Bool -> [ListResult] -> IO ()
printListResult no_color raw lr = do

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

  forM_ (if raw then rows else padded) $ \row -> putStrLn $ unwords row
 where

  padTo str' x =
    let lstr = strWidth str'
        add' = x - lstr
    in  if add' < 0 then str' else str' ++ replicate add' ' '

  -- | Calculate the render width of a string, considering
  -- wide characters (counted as double width), ANSI escape codes
  -- (not counted), and line breaks (in a multi-line string, the longest
  -- line determines the width).
  strWidth :: String -> Int
  strWidth =
    maximum
      . (0 :)
      . map (foldr (\a b -> charWidth a + b) 0)
      . lines
      . stripAnsi

  -- | Strip ANSI escape sequences from a string.
  --
  -- >>> stripAnsi "\ESC[31m-1\ESC[m"
  -- "-1"
  stripAnsi :: String -> String
  stripAnsi s' =
    case
        MP.parseMaybe (many $ "" <$ MP.try ansi <|> pure <$> MP.anySingle) s'
      of
        Nothing -> error "Bad ansi escape"  -- PARTIAL: should not happen
        Just xs -> concat xs
   where
      -- This parses lots of invalid ANSI escape codes, but that should be fine
    ansi =
      MPC.string "\ESC[" *> digitSemicolons *> suffix MP.<?> "ansi" :: MP.Parsec
          Void
          String
          Char
    digitSemicolons = MP.takeWhileP Nothing (\c -> isDigit c || c == ';')
    suffix = MP.oneOf ['A', 'B', 'C', 'D', 'H', 'J', 'K', 'f', 'm', 's', 'u']

  -- | Get the designated render width of a character: 0 for a combining
  -- character, 1 for a regular character, 2 for a wide character.
  -- (Wide characters are rendered as exactly double width in apps and
  -- fonts that support it.) (From Pandoc.)
  charWidth :: Char -> Int
  charWidth c = case c of
    _ | c < '\x0300'                     -> 1
      | c >= '\x0300' && c <= '\x036F'   -> 0
      |  -- combining
        c >= '\x0370' && c <= '\x10FC'   -> 1
      | c >= '\x1100' && c <= '\x115F'   -> 2
      | c >= '\x1160' && c <= '\x11A2'   -> 1
      | c >= '\x11A3' && c <= '\x11A7'   -> 2
      | c >= '\x11A8' && c <= '\x11F9'   -> 1
      | c >= '\x11FA' && c <= '\x11FF'   -> 2
      | c >= '\x1200' && c <= '\x2328'   -> 1
      | c >= '\x2329' && c <= '\x232A'   -> 2
      | c >= '\x232B' && c <= '\x2E31'   -> 1
      | c >= '\x2E80' && c <= '\x303E'   -> 2
      | c == '\x303F'                    -> 1
      | c >= '\x3041' && c <= '\x3247'   -> 2
      | c >= '\x3248' && c <= '\x324F'   -> 1
      | -- ambiguous
        c >= '\x3250' && c <= '\x4DBF'   -> 2
      | c >= '\x4DC0' && c <= '\x4DFF'   -> 1
      | c >= '\x4E00' && c <= '\xA4C6'   -> 2
      | c >= '\xA4D0' && c <= '\xA95F'   -> 1
      | c >= '\xA960' && c <= '\xA97C'   -> 2
      | c >= '\xA980' && c <= '\xABF9'   -> 1
      | c >= '\xAC00' && c <= '\xD7FB'   -> 2
      | c >= '\xD800' && c <= '\xDFFF'   -> 1
      | c >= '\xE000' && c <= '\xF8FF'   -> 1
      | -- ambiguous
        c >= '\xF900' && c <= '\xFAFF'   -> 2
      | c >= '\xFB00' && c <= '\xFDFD'   -> 1
      | c >= '\xFE00' && c <= '\xFE0F'   -> 1
      | -- ambiguous
        c >= '\xFE10' && c <= '\xFE19'   -> 2
      | c >= '\xFE20' && c <= '\xFE26'   -> 1
      | c >= '\xFE30' && c <= '\xFE6B'   -> 2
      | c >= '\xFE70' && c <= '\xFEFF'   -> 1
      | c >= '\xFF01' && c <= '\xFF60'   -> 2
      | c >= '\xFF61' && c <= '\x16A38'  -> 1
      | c >= '\x1B000' && c <= '\x1B001' -> 2
      | c >= '\x1D000' && c <= '\x1F1FF' -> 1
      | c >= '\x1F200' && c <= '\x1F251' -> 2
      | c >= '\x1F300' && c <= '\x1F773' -> 1
      | c >= '\x20000' && c <= '\x3FFFD' -> 2
      | otherwise                        -> 1





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
      l <- listVersions loTool (maybeToList lCriteria) lHideOld lShowNightly (lFrom, lTo)
      liftIO $ printListResult no_color lRawFormat l
      pure ExitSuccess
    )
