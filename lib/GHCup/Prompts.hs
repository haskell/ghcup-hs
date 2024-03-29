{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GHCup.Prompts
  ( PromptQuestion,
    PromptResponse (..),
    getUserPromptResponse,
  )
where

import Control.Monad.Reader
import qualified Data.Text.IO as TIO
import GHCup.Prelude.Logger
import GHCup.Types.Optics
import GHCup.Types (PromptQuestion, PromptResponse(..))

getUserPromptResponse :: ( HasLog env
                         , MonadReader env m
                         , MonadIO m)
                      => PromptQuestion
                      -> m PromptResponse

getUserPromptResponse prompt = do
  logInfo prompt
  resp <- liftIO TIO.getLine
  if resp `elem` ["YES", "yes", "y", "Y"]
    then pure PromptYes
    else pure PromptNo
