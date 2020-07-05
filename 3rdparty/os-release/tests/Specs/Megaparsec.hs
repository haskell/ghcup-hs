module Specs.Megaparsec where

import           System.OsRelease

import           Data.Either
import           Data.Void
import           Test.Hspec
import           Test.Hspec.Megaparsec

import qualified Text.Megaparsec               as MP


megaparsecSpec :: Spec
megaparsecSpec = do
  describe "parseAssignment" $ do
    it "parses simple value" $ shouldParse' "foo=bar" ("foo", "bar")
    it "parses single quoted value" $ shouldParse' "foo='bar'" ("foo", "bar")
    it "parses double quoted value" $ shouldParse' "foo=\"bar\"" ("foo", "bar")

    it "parses _ var" $ shouldParse' "f_x=''" ("f_x", "")

    -- this is not valid per spec, but many files do this
    it "parses ._- in unquoted assignment"
      $ shouldParse' "VERSION_ID=bar-1.9_rc2" ("VERSION_ID", "bar-1.9_rc2")

    it "parses quoted space" $ shouldParse' "f='a b'" ("f", "a b")
    it "parses quoted -" $ shouldParse' "f='a-b'" ("f", "a-b")

    it "parses special \\" $ shouldParse' "foo='ba\\\\r'" ("foo", "ba\\r")
    it "parses special `" $ shouldParse' "foo='ba\\`r'" ("foo", "ba`r")
    it "parses special '" $ shouldParse' "foo='ba\\'r'" ("foo", "ba'r")
    it "parses special $" $ shouldParse' "foo='ba\\$r'" ("foo", "ba$r")
    it "parses special \"" $ shouldParse' "foo=\"ba\\\"r\"" ("foo", "ba\"r")

    it "parses empty val noquotes" $ shouldParse' "foo=" ("foo", "")
    it "parses empty val quote \"" $ shouldParse' "foo=\"\"" ("foo", "")
    it "parses empty val quote '" $ shouldParse' "foo=''" ("foo", "")

    it "breaks on comments" $ shouldFail' "# foo=\"bar'"
    it "breaks on misquoting 1" $ shouldFail' "foo=\"bar'"
    it "breaks on misquoting 2" $ shouldFail' "foo='bar\""
    it "breaks on unquoted $" $ shouldFail' "foo='ba$r'"
    it "breaks on unquoted `" $ shouldFail' "foo='ba`r'"
    it "breaks on unquoted \"" $ shouldFail' "foo=\"ba\"r\""
    it "breaks on unquoted '" $ shouldFail' "foo='ba'r'"
    it "breaks on unquoted \\" $ shouldFail' "foo='ba\\r'"
    it "breaks on unquoted val with space" $ shouldFail' "foo=ba r"
    it "breaks on unquoted val with ;" $ shouldFail' "foo=ba;r"
    it "breaks on unquoted val with \\" $ shouldFail' "foo=ba\\r"
    it "breaks on trailing NL" $ shouldFail' "foo=bar\n"


  describe "parseAssignments" $ do
    it "parses simple values" $ shouldParseMany "foo=bar" [("foo", "bar")]

    it "parses multiple values"
      $ shouldParseMany "foo=bar\nbar=baz" [("foo", "bar"), ("bar", "baz")]
    it "parses multiple values with comments" $ shouldParseMany
      "foo=bar\n# comment\nbar=baz"
      [("foo", "bar"), ("bar", "baz")]

    it "parses gracefully" $ shouldParseMany "foo=bar\nbar=baz\"" [("foo", "bar")]

    it "parses empty files" $ shouldParseMany "" []
    it "parses empty files with newlines" $ shouldParseMany "\n\n" []
    it "parses empty files with newlines and comments"
      $ shouldParseMany "\n\n#\n" []

    it "parses comments with leading spaces" $ shouldParseMany "  #" []

parse :: String -> Either (MP.ParseErrorBundle String Void) (String, String)
parse = MP.parse (parseAssignment <* MP.eof) ""

shouldParse' :: String -> (String, String) -> Expectation
shouldParse' s s' = parse s `shouldParse` s'

shouldFail' :: String -> Expectation
shouldFail' s = parse `shouldFailOn` s

parseMany :: String
          -> Either (MP.ParseErrorBundle String Void) [(String, String)]
parseMany = fmap rights . MP.parse parseAssignments ""

shouldParseMany :: String -> [(String, String)] -> Expectation
shouldParseMany s s' = parseMany s `shouldParse` s'

shouldFailMany :: String -> Expectation
shouldFailMany s = parseMany `shouldFailOn` s
