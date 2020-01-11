module GHCup.Utils.Bash
  ( findAssignment
  , equalsAssignmentWith
  , getRValue
  , getAssignmentValueFor
  )
where

import           Control.Monad
import           Data.ByteString.UTF8           ( toString )
import           Data.List
import           Data.Maybe
import           HPath
import           HPath.IO
import           Language.Bash.Parse
import           Language.Bash.Syntax
import           Language.Bash.Word
import           Prelude                 hiding ( readFile )

import qualified Data.ByteString.Lazy.UTF8     as UTF8


extractAssignments :: List -> [Assign]
extractAssignments (List stms) = join $ fmap getAssign $ getCommands stms
 where
  getCommands :: [Statement] -> [Command]
  getCommands = join . fmap commands . catMaybes . fmap findPipes
   where
    findPipes (Statement (Last p@(Pipeline{})) Sequential) = Just p
    findPipes _ = Nothing

  getAssign :: Command -> [Assign]
  getAssign (Command (SimpleCommand ass _) _) = ass
  getAssign _ = []


-- | Find an assignment matching the predicate in the given file.
findAssignment :: Path b -> (Assign -> Bool) -> IO (Maybe Assign)
findAssignment p predicate = do
  fileContents <- readFile p
  -- TODO: this should accept bytestring:
  --       https://github.com/knrafto/language-bash/issues/37
  case parse (toString . toFilePath $ p) (UTF8.toString fileContents) of
    Left  e -> fail $ show e
    Right l -> pure $ find predicate (extractAssignments $ l)


-- | Check that the assignment is of the form Foo= ignoring the
-- right hand-side.
equalsAssignmentWith :: String -> Assign -> Bool
equalsAssignmentWith n ass = case ass of
  (Assign (Parameter name' Nothing) Equals _) -> n == name'
  _ -> False


-- | This pretty-prints the right hand of an Equals assignment, removing
-- quotations. No evaluation is performed.
getRValue :: Assign -> Maybe String
getRValue ass = case ass of
  (Assign (Parameter _ _) Equals (RValue w)) -> Just $ unquote w
  _ -> Nothing


-- | Given a bash assignment such as Foo="Bar" in the given file,
-- will return "Bar" (without quotations).
getAssignmentValueFor :: Path b -> String -> IO (Maybe String)
getAssignmentValueFor p n = do
  mass <- findAssignment p (equalsAssignmentWith n)
  pure (mass >>= getRValue)
