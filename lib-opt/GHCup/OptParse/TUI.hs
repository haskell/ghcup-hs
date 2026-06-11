{-# LANGUAGE CPP               #-}

module GHCup.OptParse.TUI where

import           GHCup.Command.List
import           GHCup.Types
import           GHCup.Input.Parsers (dayParser, toolParserWithGHCup, criteriaParser, revisionShowParser, nightlyShowParser)
import           GHCup.OptParse.Common

import           Data.Functor
import           Data.Maybe
import           Data.Time.Calendar             ( Day )
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )



    ---------------
    --[ Options ]--
    ---------------


data InteractiveOptions = InteractiveOptions
  { ioTool          :: Maybe [Tool]
  , ioCriteria      :: [ListCriteria]
  , ioFrom          :: Maybe Day
  , ioTo            :: Maybe Day
  , ioHideOld       :: Bool
  , ioShowNightly   :: ShowNightly
  , ioShowRevisions :: ShowRevisions
  } deriving (Eq, Show)



    ---------------
    --[ Parsers ]--
    ---------------

interactiveParser :: Parser InteractiveOptions
interactiveParser =
  InteractiveOptions
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
    <*> (option
          (eitherReader nightlyShowParser)
          (long "show-nightlies" <> help "How to show nightlies (default: latest)"
              <> metavar "<latest|all|none>"
              <> completer nightlyCompleter
              <> value NShowLatest
          )
          <|> fmap (\_ -> NShowAll) (switch
                (short 'n' <> long "show-nightly" <> help "Show nightlies (installed ones are always shown)"
                )
                )
        )
    <*> option
          (eitherReader revisionShowParser)
          (long "show-revisions" <> help "How to show revisions (default: updates)"
              <> metavar "<updates|all|none>"
              <> completer revisionCompleter
              <> value ShowUpdates
          )










