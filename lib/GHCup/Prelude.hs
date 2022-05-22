{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-|
Module      : GHCup.Prelude
Description : MegaParsec utilities
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

GHCup specific prelude. Lots of Excepts functionality.
-}
module GHCup.Prelude
  (module GHCup.Prelude,
   module GHCup.Prelude.Internal,
#if defined(IS_WINDOWS)
   module GHCup.Prelude.Windows
#else
   module GHCup.Prelude.Posix
#endif
  )
where

import           GHCup.Prelude.Internal
import           GHCup.Types.Optics   (HasLog)
import           GHCup.Prelude.Logger (logWarn)
#if defined(IS_WINDOWS)
import GHCup.Prelude.Windows
#else
import GHCup.Prelude.Posix
#endif

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Haskus.Utils.Variant.Excepts
import           Text.PrettyPrint.HughesPJClass ( prettyShow, Pretty )
import qualified Data.Text                     as T



-- for some obscure reason... this won't type-check if we move it to a different module
catchWarn :: forall es m env . ( Pretty (V es)
                             , MonadReader env m
                             , HasLog env
                             , MonadIO m
                             , Monad m) => Excepts es m () -> Excepts '[] m ()
catchWarn = catchAllE @_ @es (\v -> lift $ logWarn (T.pack . prettyShow $ v))

