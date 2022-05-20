{-# LANGUAGE FlexibleContexts  #-}

module GHCup.Utils.File (
    recycleFile
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Reader (MonadReader)
import GHCup.Types.Optics (HasDirs)


recycleFile :: (MonadIO m, MonadMask m, MonadReader env m, HasDirs env) => FilePath -> m ()

