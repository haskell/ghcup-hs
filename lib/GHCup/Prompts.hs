{-# LANGUAGE OverloadedStrings #-}
module GHCup.Prompts
  (module GHCup.Types.Prompts,
   getUserPromptResponse)
where

import GHCup.Types.Prompts
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (MonadIO, liftIO)

putPrompt :: MonadIO m => PromptQuestion -> m ()
putPrompt prompt = liftIO $ TIO.putStrLn prompt

getUserPromptResponse :: (MonadIO m) => PromptQuestion -> m PromptResponse
getUserPromptResponse prompt = do
  putPrompt prompt
  resp <- liftIO TIO.getLine
  if resp `elem` ["YES", "yes", "y", "Y"]
    then pure PromptYes
    else pure PromptNo
