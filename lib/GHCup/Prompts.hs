{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GHCup.Prompts
  ( PromptQuestion,
    PromptResponse (..),
    getUserPromptResponse,
  )
where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHCup.Prelude.Logger
import GHCup.Types.Optics
import GHCup.Types (PromptQuestion, PromptResponse(..))

getUserPromptResponse :: ( HasLog env
                         , MonadReader env m
                         , MonadIO m)
                      => PromptQuestion
                      -> PromptResponse -- ^ Default response to use if user doesn't type explicit response (e.g. just presses Return)
                      -> m PromptResponse
getUserPromptResponse prompt defaultResponse = do
  logInfo prompt
  resp <- liftIO TIO.getLine
  pure $
    if T.null resp
      then defaultResponse
      else if resp `elem` ["YES", "yes", "y", "Y"]
        then PromptYes
        else PromptNo
