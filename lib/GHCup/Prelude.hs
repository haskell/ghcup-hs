{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

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

import           GHCup.Errors
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
import           Text.PrettyPrint.HughesPJClass ( Pretty )
import qualified Data.Text                     as T



-- for some obscure reason... this won't type-check if we move it to a different module
catchWarn :: forall es m env . ( Pretty (V es)
                             , HFErrorProject (V es)
                             , MonadReader env m
                             , HasLog env
                             , MonadIO m
                             , Monad m) => Excepts es m () -> Excepts '[] m ()
catchWarn = catchAllE @_ @es (\v -> lift $ logWarn (T.pack . prettyHFError $ v))


runBothE' :: forall e m a b .
             ( Monad m
             , Show (V e)
             , Pretty (V e)
             , HFErrorProject (V e)
             , PopVariant InstallSetError e
             , LiftVariant' e (InstallSetError ': e)
             , e :<< (InstallSetError ': e)
             )
          => Excepts e m a
          -> Excepts e m b
          -> Excepts (InstallSetError ': e) m ()
runBothE' a1 a2 = do
   r1 <- lift $ runE @e a1
   r2 <- lift $ runE @e a2
   case (r1, r2) of
      (VLeft e1, VLeft e2) -> throwE (InstallSetError e1 e2)
      (VLeft e , _       ) -> throwSomeE e
      (_       , VLeft e ) -> throwSomeE e
      (VRight _, VRight _) -> pure ()


-- | Throw some exception
throwSomeE :: forall es' es a m. (Monad m, LiftVariant es' es) => V es' -> Excepts es m a
{-# INLINABLE throwSomeE #-}
throwSomeE = Excepts . pure . VLeft . liftVariant
