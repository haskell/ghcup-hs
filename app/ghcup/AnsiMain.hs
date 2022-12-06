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
import           GHCup.Prompts
import           GHCup.Types             hiding ( LeanAppState(..) )
import           GHCup.Types.Optics             ( getDirs )
import           GHCup.Utils

import           Data.List (sort, intersperse)
import           Data.Versions (prettyPVP)
import           Data.Maybe (catMaybes)
import           Codec.Archive
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
-- import           System.Console.ANSI
import           System.Console.ANSI
import           System.Console.ANSI.Codes
import           System.Console.ANSI.Types
import           Terminal.Game
import           Text.PrettyPrint.HughesPJClass ( prettyShow )

import           Control.Exception.Safe
import           Control.Monad                  ( join, when, void, forM_ )
import           Control.Monad.ST
import           Control.Monad.Reader           ( ReaderT(runReaderT), MonadReader, ask, lift )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource
import           Data.Functor
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
import System.Environment (getExecutablePath)
import Data.Versions (prettyVer)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy.Builder        as B
import qualified Data.Text.Lazy                as L
import System.FilePath
import URI.ByteString (serializeURIRef')



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
  , appRestart   :: Bool
  , appMoreInput :: Maybe String
  }
  deriving Show


startGame :: BrickState -> IO BrickState
startGame g = do
  g'@BrickState { appRestart } <- errorPress $ playGameT liftIO (ghcupGame g)
  if appRestart
  then do
    putStrLn "Press enter to continue"
    _ <- getLine
    startGame $ g' { appRestart = False }
  else pure g'

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
                         False
                         Nothing


      void $ startGame g
      cleanAndExit
    Left e -> do
      flip runReaderT s $ logError $ "Error building app state: " <> Tx.pack
        (show e)
      exitWith $ ExitFailure 2


 where
  sizeCheck :: IO ()
  sizeCheck = let (w, h) = T.swap . snd $ boundaries in assertTermDims w h


ghcupGame :: BrickState -> Game BrickState
ghcupGame bs = Game 13
                    bs                                    -- ticks per second
                    (\ge s e -> logicFun ge s e)          -- logic function
                    (\r s -> centerFull r $ drawFun s r)  -- draw function
                    (\bs -> appQuit bs || appRestart bs)  -- quit function


drawFun :: BrickState -> GEnv -> Plane
drawFun (BrickState {..}) GEnv{..} =
  blankPlane mw mh
    & (1, 1)   % box 1        1        '┌'
    & (2, 1)   % box 1        (mh - 3) '│'
    & (1, 2)   % box (mw - 2) 1        '─'
    & (2, mw)  % box 1        (mh - 3) '│'
    & (1, mw)  % box 1        1        '┐'
    & (mh-1, 2)  % box (mw - 2) 1        '─'
    & (mh-1, 1)  % box 1        1        '└'
    & (mh-1, mw) % box 1        1        '┘'
    & (2, 2)   % box (mw - 2) (mh - 3) ' '
    & (2, 2)   % (header === box (mw - 2) 1 '─' === renderItems)
    & (mh, 1)  % footer
    & (1, mw `div` 2 - 2) % stringPlane "GHCup"
 where
  mh :: Height
  mw :: Width
  (mh, mw) = T.swap eTermDims
  footer = hcat
         . intersperse (stringPlane "  ")
         . fmap stringPlane
         $ ["q:Quit", "i:Install", "u:Uninstall", "s:Set", "c:Changelog", "a:all versions", "↑:Up", "↓:Down"]
  header = hcat
         . intersperse space
         . fmap stringPlane
         $ ["Tool", "Version", "Tags", "Notes"]
  renderItems = drawListElements renderItem True appState
  renderItem _ b listResult@ListResult{..} =
    let marks = if
          | lSet       -> color Green Vivid (stringPlane "✔✔")
          | lInstalled -> color Green Dull (stringPlane "✓ ")
          | otherwise  -> color Red Vivid (stringPlane "✗ ")
        ver = case lCross of
          Nothing -> stringPlane . Tx.unpack . prettyVer $ lVer
          Just c  -> stringPlane . Tx.unpack $ (c <> "-" <> prettyVer lVer)
        tool = printTool lTool
        tag = let l = catMaybes . fmap printTag $ sort lTag
              in  if null l then blankPlane 1 1 else foldr1 (\x y -> x ||| stringPlane "," ||| y) l
        notes = let n = printNotes listResult
                in  if null n
                      then blankPlane 1 1
                      else foldr1 (\x y -> x ||| stringPlane "," ||| y) n

    in hcat [marks, space, space, tool, space, ver, space, tag, space, notes]

  printTag Recommended    = Just $ color Green Dull $ stringPlane "recommended"
  printTag Latest         = Just $ color Yellow Dull $ stringPlane "latest"
  printTag Prerelease     = Just $ color Red Dull $ stringPlane "prerelease"
  printTag (Base pvp'')   = Just $ stringPlane ("base-" ++ T.unpack (prettyPVP pvp''))
  printTag Old            = Nothing
  printTag (UnknownTag t) = Just $ stringPlane t

  printTool Cabal = stringPlane "cabal"
  printTool GHC   = stringPlane "GHC"
  printTool GHCup = stringPlane "GHCup"
  printTool HLS   = stringPlane "HLS"
  printTool Stack = stringPlane "Stack"

  printNotes ListResult {..} =
    (if hlsPowered then [color Green Dull $ stringPlane "hls-powered"] else mempty
      )
      ++ (if fromSrc then [color Blue Dull $ stringPlane "compiled"] else mempty)
      ++ (if lStray then [color Blue Dull $ stringPlane "stray"] else mempty)

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
        in vcat $ V.toList (makeVisible drawnElements (mh - 5) selIx)
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


logicFun :: GEnv -> BrickState -> Event -> IO BrickState
logicFun _ gs (KeyPress 'q') = pure gs { appQuit = True }
logicFun _ gs Tick           = pure gs
logicFun _ gs@BrickState{appMoreInput = Nothing} (KeyPress '\ESC') = pure gs { appMoreInput = Just "\ESC" }
logicFun _ gs@BrickState{appMoreInput = Just "\ESC"} (KeyPress '[') = pure gs { appMoreInput = Just "\ESC[" }
logicFun _ gs@BrickState{appMoreInput = Just "\ESC[", appState = s'} (KeyPress 'A')
  = pure gs { appMoreInput = Nothing, appState = moveCursor 1 s' Up }
logicFun _ gs@BrickState{appMoreInput = Just "\ESC[", appState = s'} (KeyPress 'B')
  = pure gs { appMoreInput = Nothing, appState = moveCursor 1 s' Down }
logicFun _ gs@BrickState{appMoreInput = Just _} _ = pure gs { appMoreInput = Nothing }
logicFun _ gs (KeyPress 'i') = do
  bs <- withIOAction install' gs
  pure bs { appRestart = True }
logicFun _ gs (KeyPress 'u') = do
  bs <- withIOAction del' gs
  pure bs { appRestart = True }
logicFun _ gs (KeyPress 's') = do
  bs <- withIOAction set' gs
  pure bs { appRestart = True }
logicFun _ gs (KeyPress 'c') = do
  bs <- withIOAction changelog' gs
  pure bs { appRestart = True }
logicFun _ gs (KeyPress 'a') = pure $ hideShowHandler (not . showAllVersions) showAllTools gs
 where
  hideShowHandler :: (BrickSettings -> Bool) -> (BrickSettings -> Bool) -> BrickState -> BrickState
  hideShowHandler f p BrickState{..} =
    let newAppSettings   = appSettings { showAllVersions = f appSettings , showAllTools = p appSettings }
        newInternalState = constructList appData newAppSettings (Just appState)
    in  BrickState appData newAppSettings newInternalState appKeys appQuit appRestart appMoreInput
logicFun _ gs (KeyPress c) = pure gs

withIOAction :: (BrickState
                 -> (Int, ListResult)
                 -> ReaderT AppState IO (Either String a))
             -> BrickState
             -> IO BrickState
withIOAction action as = case listSelectedElement' (appState as) of
  Nothing      -> pure as
  Just (ix, e) -> do
    clearScreen

    settings <- readIORef settings'
    flip runReaderT settings $ action as (ix, e) >>= \case
      Left  err -> liftIO $ putStrLn ("Error: " <> err)
      Right _   -> liftIO $ putStrLn "Success"
    getAppData Nothing >>= \case
      Right data' -> do
        pure (updateList data' as)
      Left err -> throwIO $ userError err

moveCursor :: Int -> BrickInternalState -> Direction -> BrickInternalState
moveCursor steps ais@BrickInternalState{..} direction =
  let newIx = if direction == Down then ix + steps else ix - steps
  in  case clr V.!? newIx of
        Just _  -> BrickInternalState { ix = newIx, .. }
        Nothing -> ais

defaultAppSettings :: BrickSettings
defaultAppSettings =
  BrickSettings { showAllVersions = False, showAllTools = False }

-- | Update app data and list internal state based on new evidence.
-- This synchronises @BrickInternalState@ with @BrickData@
-- and @BrickSettings@.
updateList :: BrickData -> BrickState -> BrickState
updateList appD BrickState{..} =
  let newInternalState = constructList appD appSettings (Just appState)
  in  BrickState { appState    = newInternalState
                 , appData     = appD
                 , appSettings = appSettings
                 , appKeys     = appKeys
                 , appQuit     = appQuit
                 , appRestart  = appRestart
                 , appMoreInput = appMoreInput
                 }

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

install' :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m)
         => BrickState
         -> (Int, ListResult)
         -> m (Either String ())
install' _ (_, ListResult {..}) = do
  AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask

  let run =
        runResourceT
          . runE
            @'[ AlreadyInstalled
              , ArchiveResult
              , UnknownArchive
              , FileDoesNotExistError
              , CopyError
              , NoDownload
              , NotInstalled
              , BuildFailed
              , TagNotFound
              , DigestError
              , GPGError
              , DownloadFailed
              , DirNotEmpty
              , NoUpdate
              , TarDirDoesNotExist
              , FileAlreadyExistsError
              , ProcessError
              , ToolShadowed
              , UninstallFailed
              , MergeFileTreeError
              ]

  run (do
      ce <- liftIO $ fmap (either (const Nothing) Just) $
        try @_ @SomeException $ getExecutablePath >>= canonicalizePath
      dirs <- lift getDirs
      case lTool of
        GHC   -> do
          let vi = getVersionInfo lVer GHC dls
          liftE $ installGHCBin lVer GHCupInternal False [] $> (vi, dirs, ce)
        Cabal -> do
          let vi = getVersionInfo lVer Cabal dls
          liftE $ installCabalBin lVer GHCupInternal False $> (vi, dirs, ce)
        GHCup -> do
          let vi = snd <$> getLatest dls GHCup
          liftE $ upgradeGHCup Nothing False False $> (vi, dirs, ce)
        HLS   -> do
          let vi = getVersionInfo lVer HLS dls
          liftE $ installHLSBin lVer GHCupInternal False $> (vi, dirs, ce)
        Stack -> do
          let vi = getVersionInfo lVer Stack dls
          liftE $ installStackBin lVer GHCupInternal False $> (vi, dirs, ce)
    )
    >>= \case
          VRight (vi, Dirs{..}, Just ce) -> do
            forM_ (_viPostInstall =<< vi) $ \msg -> logInfo msg
            case lTool of
              GHCup -> do
                up <- liftIO $ fmap (either (const Nothing) Just)
                  $ try @_ @SomeException $ canonicalizePath (binDir </> "ghcup" <.> exeExt)
                when ((normalise <$> up) == Just (normalise ce)) $
                  -- TODO: track cli arguments of previous invocation
                  void $ liftIO $ exec ce ["tui"] Nothing Nothing
                logInfo "Please restart 'ghcup' for the changes to take effect"
              _ -> pure ()
            pure $ Right ()
          VRight (vi, _, _) -> do
            forM_ (_viPostInstall =<< vi) $ \msg -> logInfo msg
            logInfo "Please restart 'ghcup' for the changes to take effect"
            pure $ Right ()
          VLeft  (V (AlreadyInstalled _ _)) -> pure $ Right ()
          VLeft (V NoUpdate) -> pure $ Right ()
          VLeft e -> pure $ Left $ prettyShow e <> "\n"
            <> "Also check the logs in ~/.ghcup/logs"


set' :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m)
     => BrickState
     -> (Int, ListResult)
     -> m (Either String ())
set' bs input@(_, ListResult {..}) = do
  settings <- liftIO $ readIORef settings'

  let run =
        flip runReaderT settings
          . runE @'[FileDoesNotExistError , NotInstalled , TagNotFound]

  run (do
      case lTool of
        GHC   -> liftE $ setGHC (GHCTargetVersion lCross lVer) SetGHCOnly Nothing $> ()
        Cabal -> liftE $ setCabal lVer $> ()
        HLS   -> liftE $ setHLS lVer SetHLSOnly Nothing $> ()
        Stack -> liftE $ setStack lVer $> ()
        GHCup -> pure ()
    )
    >>= \case
          VRight _ -> pure $ Right ()
          VLeft  e -> case e of
            (V (NotInstalled tool _)) -> do
              promptAnswer <- getUserPromptResponse userPrompt
              case promptAnswer of
                PromptYes -> do
                  res <- install' bs input
                  case res of
                    (Left err) -> pure $ Left err
                    (Right _) -> do
                      logInfo "Setting now..."
                      set' bs input

                PromptNo -> pure $ Left (prettyShow e)
              where
                userPrompt = L.toStrict . B.toLazyText . B.fromString $
                  "This Version of "
                  <> show tool
                  <> " you are trying to set is not installed.\n"
                  <> "Would you like to install it first? [Y/N]: "

            _ -> pure $ Left (prettyShow e)



del' :: (MonadReader AppState m, MonadIO m, MonadFail m, MonadMask m, MonadUnliftIO m)
     => BrickState
     -> (Int, ListResult)
     -> m (Either String ())
del' _ (_, ListResult {..}) = do
  AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask

  let run = runE @'[NotInstalled, UninstallFailed]

  run (do
      let vi = getVersionInfo lVer lTool dls
      case lTool of
        GHC   -> liftE $ rmGHCVer (GHCTargetVersion lCross lVer) $> vi
        Cabal -> liftE $ rmCabalVer lVer $> vi
        HLS   -> liftE $ rmHLSVer lVer $> vi
        Stack -> liftE $ rmStackVer lVer $> vi
        GHCup -> pure Nothing
    )
    >>= \case
          VRight vi -> do
            forM_ (_viPostRemove =<< vi) $ \msg ->
              logInfo msg
            pure $ Right ()
          VLeft  e -> pure $ Left (prettyShow e)


changelog' :: (MonadReader AppState m, MonadIO m)
           => BrickState
           -> (Int, ListResult)
           -> m (Either String ())
changelog' _ (_, ListResult {..}) = do
  AppState { pfreq, ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask
  case getChangeLog dls lTool (Left lVer) of
    Nothing -> pure $ Left $
      "Could not find ChangeLog for " <> prettyShow lTool <> ", version " <> T.unpack (prettyVer lVer)
    Just uri -> do
      let cmd = case _rPlatform pfreq of
            Darwin  -> "open"
            Linux _ -> "xdg-open"
            FreeBSD -> "xdg-open"
            Windows -> "start"
      exec cmd [T.unpack $ decUTF8Safe $ serializeURIRef' uri] Nothing Nothing >>= \case
        Right _ -> pure $ Right ()
        Left  e -> pure $ Left $ prettyShow e


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


