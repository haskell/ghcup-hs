{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE MultiWayIf        #-}

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

import           Control.Monad                  ( join, when )
import           Control.Monad.ST
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Data.Bifunctor
import           Data.STRef
import           Data.IORef
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as Tx
import qualified Data.Tuple                    as T
import qualified Data.Vector                   as V
import           GHC.IO                         ( unsafePerformIO )
import           Haskus.Utils.Variant.Excepts
import           System.Exit
import Data.Versions (prettyVer)

data Direction = Up
               | Down
  deriving (Show, Eq)

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
  { appData      :: BrickData
  , appSettings  :: BrickSettings
  , appState     :: BrickInternalState
  , appKeys      :: KeyBindings
  , appQuit      :: Bool
  , appMoreInput :: Maybe String
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
                         Nothing


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
                    (\ge s e -> logicFun ge s e)            -- logic function
                    (\r s -> centerFull r $ drawFun s r)  -- draw function
                    appQuit                             -- quit function


drawFun :: BrickState -> GEnv -> Plane
drawFun (BrickState {..}) GEnv{..} =
  blankPlane mw mh
    & (1, 1)   % box 1        1        '┌'
    & (2, 1)   % box 1        (mh - 2) '│'
    & (1, 2)   % box (mw - 2) 1        '─'
    & (2, mw)  % box 1        (mh - 2) '│'
    & (1, mw)  % box 1        1        '┐'
    & (mh, 2)  % box (mw - 2) 1        '─'
    & (mh, 1)  % box 1        1        '└'
    & (mh, mw) % box 1        1        '┘'
    & (2, 2)   % box (mw - 2) (mh - 2) ' '
    & (2, 2) % renderItems
 where
  mh :: Height
  mw :: Width
  (mh, mw) = T.swap eTermDims
  renderItems = drawListElements renderItem True appState
  renderItem _ b ListResult{..} =
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

  -- | Draws the list elements.
  --
  -- Evaluates the underlying container up to, and a bit beyond, the
  -- selected element. The exact amount depends on available height
  -- for drawing and 'listItemHeight'. At most, it will evaluate up to
  -- element @(i + h + 1)@ where @i@ is the selected index and @h@ is the
  -- available height.
  drawListElements :: (Int -> Bool -> ListResult -> Plane)
                   -> Bool
                   -> BrickInternalState
                   -> Plane
  drawListElements drawElem foc is@(BrickInternalState clr _) =
      let es = clr
          listSelected        = fmap fst $ listSelectedElement' is

          (drawnElements, selIx) = runST $ do
            ref <- newSTRef (Nothing :: Maybe Int)
            elem' <- newSTRef 0
            arr <- fmap join $ flip V.imapM es $ \i' e -> do
              let isSelected  = Just i' == listSelected
                  elemWidget  = drawElem i' isSelected e
                  selItemAttr = if foc
                    then listSelectedFocusedAttr
                    else listSelectedAttr
                  markSelected = if isSelected then selItemAttr else id
              case es V.!? (i' - 1) of
                    Just e' | lTool e' /= lTool e -> do
                      modifySTRef elem' (+2)
                      i <- readSTRef elem'
                      when isSelected $ writeSTRef ref (Just i)
                      pure $ V.fromList [hBorder, markSelected elemWidget] -- add separator
                    _ -> do
                      modifySTRef elem' (+1)
                      i <- readSTRef elem'
                      when isSelected $ writeSTRef ref (Just i)
                      pure $ V.fromList [markSelected elemWidget]
            i <- readSTRef ref
            pure (arr, i)
        in vcat $ V.toList (makeVisible drawnElements (mh - 2) selIx)
   where
    makeVisible :: V.Vector Plane -> Height -> Maybe Int -> V.Vector Plane
    makeVisible listElements drawableHeight (Just ix) =
      let listHeight = V.length listElements
      in if | listHeight <= 0 -> listElements
            | listHeight > drawableHeight ->
              if | ix <= drawableHeight -> makeVisible (V.init listElements) drawableHeight (Just ix)
                 | otherwise            -> makeVisible (V.tail listElements) drawableHeight (Just (ix - 1))
            | otherwise -> listElements
    makeVisible listElements _ Nothing = listElements

    listSelectedFocusedAttr = invert

    listSelectedAttr = invert

    hBorder = box (mw - 2) 1 '─'


logicFun :: GEnv -> BrickState -> Event -> BrickState
logicFun _ gs (KeyPress 'q') = gs { appQuit = True }
logicFun _ gs Tick           = gs
logicFun _ gs@BrickState{appMoreInput = Nothing} (KeyPress '\ESC') = gs { appMoreInput = Just "\ESC" }
logicFun _ gs@BrickState{appMoreInput = Just "\ESC"} (KeyPress '[') = gs { appMoreInput = Just "\ESC[" }
logicFun _ gs@BrickState{appMoreInput = Just "\ESC[", appState = s'} (KeyPress 'A')
  = gs { appMoreInput = Nothing, appState = moveCursor 1 s' Up }
logicFun _ gs@BrickState{appMoreInput = Just "\ESC[", appState = s'} (KeyPress 'B')
  = gs { appMoreInput = Nothing, appState = moveCursor 1 s' Down }
logicFun _ gs@BrickState{appMoreInput = Just _} _ = gs { appMoreInput = Nothing }
logicFun _ gs (KeyPress c)   = gs

moveCursor :: Int -> BrickInternalState -> Direction -> BrickInternalState
moveCursor steps ais@BrickInternalState{..} direction =
  let newIx = if direction == Down then ix + steps else ix - steps
  in  case clr V.!? newIx of
        Just _  -> BrickInternalState { ix = newIx, .. }
        Nothing -> ais

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


