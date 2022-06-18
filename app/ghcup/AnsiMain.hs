{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RankNTypes        #-}

module AnsiMain where

import           GHCup
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Prelude                  ( decUTF8Safe )
import           GHCup.Prelude.File
import           GHCup.Prelude.Logger
import           GHCup.Prelude.Process
import           GHCup.Types             hiding ( LeanAppState(..) )
import           GHCup.Types.Optics             ( getDirs )
import           GHCup.Utils

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
-- import           System.Console.ANSI
import           System.Console.ANSI
import           System.Console.ANSI.Codes
import           System.Console.ANSI.Types
import           Terminal.Game
import           Text.PrettyPrint.HughesPJClass ( prettyShow )

import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Data.IORef
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as Tx
import qualified Data.Tuple                    as T
import qualified Data.Vector                   as V
import           GHC.IO                         ( unsafePerformIO )
import           Haskus.Utils.Variant.Excepts
import           System.Exit
import Data.Versions (prettyVer)


data BrickData = BrickData
  { lr :: [ListResult]
  }
  deriving Show

data BrickSettings = BrickSettings
  { showAllVersions :: Bool
  , showAllTools    :: Bool
  }
  deriving Show

data BrickInternalState = BrickInternalState
  { clr :: V.Vector ListResult
  , ix  :: Int
  }
  deriving Show

data BrickState = BrickState
  { appData     :: BrickData
  , appSettings :: BrickSettings
  , appState    :: BrickInternalState
  , appKeys     :: KeyBindings
  , appQuit     :: Bool
  }
  deriving Show

ansiMain :: AppState -> IO ()
ansiMain s = do
  writeIORef settings' s

  eAppData <- getAppData (Just $ ghcupInfo s)
  case eAppData of
    Right ad -> do
      let g = BrickState ad
                         defaultAppSettings
                         (constructList ad defaultAppSettings Nothing)
                         (keyBindings (s :: AppState))
                         False


      sizeCheck
      errorPress $ playGame (ghcupGame g)
    Left e -> do
      flip runReaderT s $ logError $ "Error building app state: " <> Tx.pack
        (show e)
      exitWith $ ExitFailure 2


 where
  sizeCheck :: IO ()
  sizeCheck = let (w, h) = T.swap . snd $ boundaries in assertTermDims w h


ghcupGame :: BrickState -> Game BrickState
ghcupGame bs = Game 13
                    bs                                  -- ticks per second
                    (\_ s e -> logicFun s e)            -- logic function
                    (\r s -> centerFull r $ drawFun s r)  -- draw function
                    appQuit                             -- quit function


drawFun :: BrickState -> GEnv -> Plane
drawFun (BrickState {..}) GEnv{..} =
  blankPlane mw mh
    & (1, 1) % box mw mh '.'
    & (2, 2) % box (mw-2) (mh-2) ' '
    & (2, 2) % renderItems (renderItem <$> lr appData)
 where
  mh :: Height
  mw :: Width
  (mh, mw) = snd boundaries
  renderItems = foldr1 (===)
  renderItem ListResult{..} =
    let marks = if
          | lSet       -> (stringPlane "✔✔")
          | lInstalled -> (stringPlane "✓ ")
          | otherwise  -> (stringPlane "✗ ")
        ver = case lCross of
          Nothing -> stringPlane . Tx.unpack . prettyVer $ lVer
          Just c  -> stringPlane . Tx.unpack $ (c <> "-" <> prettyVer lVer)
        tool = printTool lTool
    in marks ||| space ||| space ||| tool ||| space ||| ver

  printTool Cabal = stringPlane "cabal"
  printTool GHC   = stringPlane "GHC"
  printTool GHCup = stringPlane "GHCup"
  printTool HLS   = stringPlane "HLS"
  printTool Stack = stringPlane "Stack"

  space = stringPlane " "

logicFun :: BrickState -> Event -> BrickState
logicFun gs (KeyPress 'q') = gs { appQuit = True }
logicFun gs Tick           = gs
logicFun gs (KeyPress c)   = gs


defaultAppSettings :: BrickSettings
defaultAppSettings =
  BrickSettings { showAllVersions = False, showAllTools = False }


constructList
  :: BrickData
  -> BrickSettings
  -> Maybe BrickInternalState
  -> BrickInternalState
constructList appD appSettings = replaceLR
  (filterVisible (showAllVersions appSettings) (showAllTools appSettings))
  (lr appD)


-- | Replace the @appState@ or construct it based on a filter function
-- and a new @[ListResult]@ evidence.
-- When passed an existing @appState@, tries to keep the selected element.
replaceLR
  :: (ListResult -> Bool)
  -> [ListResult]
  -> Maybe BrickInternalState
  -> BrickInternalState
replaceLR filterF lr s =
  let oldElem = s >>= listSelectedElement'
      newVec  = V.fromList . filter filterF $ lr
      newSelected =
        case oldElem >>= \(_, oldE) -> V.findIndex (toolEqual oldE) newVec of
          Just ix -> ix
          Nothing -> selectLatest newVec
  in  BrickInternalState newVec newSelected
 where
  toolEqual e1 e2 =
    lTool e1 == lTool e2 && lVer e1 == lVer e2 && lCross e1 == lCross e2


filterVisible :: Bool -> Bool -> ListResult -> Bool
filterVisible v t e
  | lInstalled e                   = True
  | v, not t, lTool e `notElem` hiddenTools = True
  | not v, t, Old `notElem` lTag e = True
  | v, t                           = True
  | otherwise = (Old `notElem` lTag e) && (lTool e `notElem` hiddenTools)


hiddenTools :: [Tool]
hiddenTools = []


selectLatest :: V.Vector ListResult -> Int
selectLatest = fromMaybe 0
  . V.findIndex (\ListResult {..} -> lTool == GHC && Latest `elem` lTag)


listSelectedElement' :: BrickInternalState -> Maybe (Int, ListResult)
listSelectedElement' BrickInternalState {..} = fmap (ix, ) $ clr V.!? ix


boundaries :: (Coords, Coords)
boundaries = ((1, 1), (24, 80))


settings' :: IORef AppState
{-# NOINLINE settings' #-}
settings' = unsafePerformIO $ do
  dirs <- getAllDirs
  let loggerConfig = LoggerConfig { lcPrintDebug  = False
                                  , consoleOutter = \_ -> pure ()
                                  , fileOutter    = \_ -> pure ()
                                  , fancyColors   = True
                                  }
  newIORef $ AppState defaultSettings
                      dirs
                      defaultKeyBindings
                      (GHCupInfo mempty mempty mempty)
                      (PlatformRequest A_64 Darwin Nothing)
                      loggerConfig


getAppData :: Maybe GHCupInfo -> IO (Either String BrickData)
getAppData mgi = runExceptT $ do
  r <- ExceptT $ maybe getGHCupInfo (pure . Right) mgi
  liftIO $ modifyIORef settings' (\s -> s { ghcupInfo = r })
  settings <- liftIO $ readIORef settings'

  flip runReaderT settings $ do
    lV <- listVersions Nothing Nothing
    pure $ BrickData (reverse lV)


getGHCupInfo :: IO (Either String GHCupInfo)
getGHCupInfo = do
  settings <- readIORef settings'

  r        <-
    flip runReaderT settings
    . runE
      @'[ DigestError
        , GPGError
        , JSONError
        , DownloadFailed
        , FileDoesNotExistError
        ]
    $ liftE getDownloadsF

  case r of
    VRight a -> pure $ Right a
    VLeft  e -> pure $ Left (prettyShow e)


