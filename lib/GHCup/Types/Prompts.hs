module GHCup.Types.Prompts where

import Data.Text (Text)

type PromptQuestion = Text

data PromptResponse = PromptYes | PromptNo
  deriving (Show, Eq)
