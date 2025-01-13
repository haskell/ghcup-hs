{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.DInfo where




import           GHCup
import           GHCup.Errors
import           GHCup.Version
import           GHCup.Types
import           GHCup.Utils.Dirs
import           GHCup.Prelude
import           GHCup.Prelude.Logger
import           GHCup.Prelude.Process

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Maybe
import           Data.List                      ( intercalate )
import           Data.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Exit
import           System.FilePath
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           URI.ByteString (serializeURIRef')

import qualified Data.Text                     as T
import Control.Exception.Safe (MonadMask)
import Language.Haskell.TH



    -----------------
    --[ Utilities ]--
    -----------------


describe_result :: String
describe_result = $( LitE . StringL <$>
                     runIO (do
                             CapturedProcess{..} <-  do
                              dirs <- liftIO getAllDirs
                              let settings = AppState (defaultSettings { noNetwork = True })
                                               dirs
                                               defaultKeyBindings
                              flip runReaderT settings $ executeOut "git" ["describe"] Nothing
                             case _exitCode of
                               ExitSuccess   -> pure . T.unpack . decUTF8Safe' $ _stdOut
                               ExitFailure _ -> pure numericVer
                     )
                   )


prettyDebugInfo :: DebugInfo -> String
prettyDebugInfo DebugInfo { diDirs = Dirs { .. }, ..} =
  "===== Main ======"                              <> "\n"  <>
  "Architecture:  "   <> prettyShow diArch         <> "\n"  <>
  "Platform:      "   <> prettyShow diPlatform     <> "\n"  <>
  "GHCup Version: "   <> describe_result           <> "\n"  <>
  "===== Directories ======"                       <> "\n"  <>
  "base:    " <> fromGHCupPath baseDir             <> "\n"  <>
  "bin:     " <> binDir                            <> "\n"  <>
  "GHCs:    " <> (fromGHCupPath baseDir </> "ghc") <> "\n"  <>
  "cache:   " <> fromGHCupPath cacheDir            <> "\n"  <>
  "logs:    " <> fromGHCupPath logsDir             <> "\n"  <>
  "config:  " <> fromGHCupPath confDir             <> "\n"  <>
  "db:      " <> fromGHCupPath dbDir               <> "\n"  <>
  whenWin ("recycle: " <> fromGHCupPath recycleDir <> "\n") <>
  "temp:    " <> fromGHCupPath tmpDir              <> "\n"  <>
  whenWin ("msys2:   " <> msys2Dir                 <> "\n") <>
  "\n===== Metadata ======\n" <>
  intercalate "\n" ((\(c, u) -> pad (c <> ":") <> " " <> u) <$> channels)
 where
  pad xs
    | xl < maxLength = xs <> replicate (maxLength - xl) ' '
    | otherwise = xs
   where
    xl = length xs
  channels = (\(c, u) -> (T.unpack . channelAliasText $ c, T.unpack . decUTF8Safe . serializeURIRef'$ u)) <$> diChannels
  maxLength = (+1) . maximum . fmap (length . fst) $ channels
  whenWin x = if isWindows then x else mempty




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type DInfoEffects = '[ NoCompatiblePlatform , NoCompatibleArch , DistroNotFound ]

runDebugInfo :: (ReaderT env m (VEither DInfoEffects a) -> m (VEither DInfoEffects a))
             -> Excepts DInfoEffects (ReaderT env m) a
             -> m (VEither DInfoEffects a)
runDebugInfo runAppState =
        runAppState
        . runE
          @DInfoEffects



    ------------------
    --[ Entrypoint ]--
    ------------------



dinfo :: ( Monad m
         , MonadMask m
         , MonadUnliftIO m
         , MonadFail m
         , Alternative m
         )
      => (ReaderT AppState m (VEither DInfoEffects DebugInfo)
          -> m (VEither DInfoEffects DebugInfo))
      -> (ReaderT LeanAppState m () -> m ())
      -> m ExitCode
dinfo runAppState runLogger = do
  runDebugInfo runAppState (liftE getDebugInfo)
    >>= \case
          VRight di -> do
            liftIO $ putStrLn $ prettyDebugInfo di
            pure ExitSuccess
          VLeft e -> do
            runLogger $ logError $ T.pack $ prettyHFError e
            pure $ ExitFailure 8
