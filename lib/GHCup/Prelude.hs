{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
   module GHCup.Prelude.JSON,
   module GHCup.Prelude.File,
   module GHCup.Prelude.Logger,
   module GHCup.Prelude.Version,
#if defined(IS_WINDOWS)
   module GHCup.Prelude.Windows,
#else
   module GHCup.Prelude.Posix,
#endif
  )
where

import GHCup.Errors
import GHCup.Prelude.File
import GHCup.Prelude.Internal
import GHCup.Prelude.JSON
import GHCup.Prelude.Logger
import GHCup.Prelude.Version
import GHCup.Types            ( EnvUnion (..) )
import GHCup.Types.Optics     ( HasLog )
#if defined(IS_WINDOWS)
import GHCup.Prelude.Windows
#else
import GHCup.Prelude.Posix
#endif

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.List                      ( intercalate )
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T
import           Data.Variant.Excepts
import           System.Environment             ( getEnvironment )
import           System.FilePath
import           Text.PrettyPrint.HughesPJClass ( Pretty, prettyShow )



-- for some obscure reason... this won't type-check if we move it to a different module
catchWarn :: forall es m env . ( Pretty (V es)
                             , HFErrorProject (V es)
                             , MonadReader env m
                             , HasLog env
                             , MonadIO m
                             , Monad m) => Excepts es m () -> Excepts '[] m ()
catchWarn = catchAllE @_ @es (\v -> lift $ logWarn (T.pack . prettyHFError $ v))

prettyText :: Pretty a => a -> T.Text
prettyText = T.pack . prettyShow

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

augmentEnvironment :: [(String, String)] -> EnvUnion -> IO [(String, String)]
augmentEnvironment (Map.fromList -> addEnv) PreferSpec = do
 cEnv <- Map.fromList <$> getEnvironment
 pure (Map.toList $ Map.union addEnv cEnv)
augmentEnvironment (Map.fromList -> addEnv) PreferSystem = do
 cEnv <- Map.fromList <$> getEnvironment
 pure (Map.toList $ Map.union cEnv addEnv)
augmentEnvironment addEnv OnlySpec = pure addEnv

addToPath :: [FilePath]
          -> Bool         -- ^ if False will prepend
          -> IO [(String, String)]
addToPath paths append = do
 cEnv <- getEnvironment
 return $ addToPath' cEnv paths append

addToPath' :: [(String, String)]
          -> [FilePath]
          -> Bool         -- ^ if False will prepend
          -> [(String, String)]
addToPath' cEnv' newPaths append =
  let cEnv           = Map.fromList cEnv'
      paths          = ["PATH", "Path"]
      curPaths       = (\x -> maybe [] splitSearchPath (Map.lookup x cEnv)) =<< paths
      {- HLINT ignore "Redundant bracket" -}
      newPath        = intercalate [searchPathSeparator] (if append then (curPaths ++ newPaths) else (newPaths ++ curPaths))
      envWithoutPath = foldr (\x y -> Map.delete x y) cEnv paths
      pathVar        = if isWindows then "Path" else "PATH"
      envWithNewPath = Map.toList $ Map.insert pathVar newPath envWithoutPath
  in envWithNewPath
