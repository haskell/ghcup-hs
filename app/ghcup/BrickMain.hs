{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module BrickMain where

import           GHCup
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Utils
import           GHCup.Utils.File
import           GHCup.Utils.Logger

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.List
#if !defined(TAR)
import           Codec.Archive
#endif
import           Control.Exception.Safe
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Bool
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Char
import           Data.IORef
import           Data.String.Interpolate
import           Data.Vector                    ( Vector )
import           Data.Versions           hiding ( str )
import           Haskus.Utils.Variant.Excepts
import           Prelude                 hiding ( appendFile )
import           System.Exit
import           System.IO.Unsafe
import           URI.ByteString

import qualified Data.Text                     as T
import qualified Graphics.Vty                  as Vty
import qualified Data.Vector                   as V


data AppState = AppState {
    lr :: LR
  , dls :: GHCupDownloads
  , pfreq :: PlatformRequest
}

type LR = GenericList String Vector ListResult


keyHandlers :: [(Char, String, AppState -> EventM n (Next AppState))]
keyHandlers =
  [ ('q', "Quit"     , halt)
  , ('i', "Install"  , withIOAction install')
  , ('u', "Uninstall", withIOAction del')
  , ('s', "Set"      , withIOAction set')
  , ('c', "ChangeLog", withIOAction changelog')
  ]


ui :: AppState -> Widget String
ui AppState {..} =
  ( padBottom Max
    $ ( withBorderStyle unicode
      $ borderWithLabel (str "GHCup")
      $ (center $ renderList renderItem True lr)
      )
    )
    <=> ( withAttr "help"
        . txtWrap
        . T.pack
        . foldr1 (\x y -> x <> "  " <> y)
        . (++ ["↑↓:Navigation"])
        $ (fmap (\(c, s, _) -> (c : ':' : s)) keyHandlers)
        )

 where
  renderItem b ListResult {..} =
    let marks = if
          | lSet       -> (withAttr "set" $ str "✔✔")
          | lInstalled -> (withAttr "installed" $ str "✓ ")
          | otherwise  -> (withAttr "not-installed" $ str "✗ ")
        ver = case lCross of
          Nothing -> T.unpack . prettyVer $ lVer
          Just c  -> T.unpack (c <> "-" <> prettyVer lVer)
        dim = if lNoBindist
          then updateAttrMap (const dimAttributes) . withAttr "no-bindist"
          else id
    in  dim
          (   marks
          <+> ( padLeft (Pad 2)
              $ minHSize 20
              $ ((if b then withAttr "active" else id)
                  (str $ (fmap toLower . show $ lTool) <> " " <> ver)
                )
              )
          <+> (padLeft (Pad 1) $ if null lTag
                then emptyWidget
                else
                  foldr1 (\x y -> x <+> str "," <+> y)
                    $ (fmap printTag $ sort lTag)
              )
          )

  printTag Recommended        = withAttr "recommended" $ str "recommended"
  printTag Latest             = withAttr "latest" $ str "latest"
  printTag (Base       pvp'') = str ("base-" ++ T.unpack (prettyPVP pvp''))
  printTag (UnknownTag t    ) = str t


minHSize :: Int -> Widget n -> Widget n
minHSize s' = hLimit s' . vLimit 1 . (<+> fill ' ')


app :: App AppState e String
app = App { appDraw         = \st -> [ui st]
          , appHandleEvent  = eventHandler
          , appStartEvent   = return
          , appAttrMap      = const defaultAttributes
          , appChooseCursor = neverShowCursor
          }

defaultAttributes :: AttrMap
defaultAttributes = attrMap
  Vty.defAttr
  [ ("active"       , Vty.defAttr `Vty.withBackColor` Vty.blue)
  , ("not-installed", Vty.defAttr `Vty.withForeColor` Vty.red)
  , ("set"          , Vty.defAttr `Vty.withForeColor` Vty.green)
  , ("installed"    , Vty.defAttr `Vty.withForeColor` Vty.green)
  , ("recommended"  , Vty.defAttr `Vty.withForeColor` Vty.green)
  , ("latest"       , Vty.defAttr `Vty.withForeColor` Vty.yellow)
  , ("help"         , Vty.defAttr `Vty.withStyle` Vty.italic)
  ]


dimAttributes :: AttrMap
dimAttributes = attrMap
  (Vty.defAttr `Vty.withStyle` Vty.dim)
  [ ("active"    , Vty.defAttr `Vty.withBackColor` Vty.blue)
  , ("no-bindist", Vty.defAttr `Vty.withStyle` Vty.dim)
  ]



eventHandler :: AppState -> BrickEvent n e -> EventM n (Next AppState)
eventHandler st (VtyEvent (Vty.EvResize _               _)) = continue st
eventHandler st (VtyEvent (Vty.EvKey    (Vty.KChar 'q') _)) = halt st
eventHandler st (VtyEvent (Vty.EvKey    Vty.KEsc        _)) = halt st
eventHandler AppState {..} (VtyEvent (Vty.EvKey (Vty.KUp) _)) =
  continue (AppState (listMoveUp lr) dls pfreq)
eventHandler AppState {..} (VtyEvent (Vty.EvKey (Vty.KDown) _)) =
  continue (AppState (listMoveDown lr) dls pfreq)
eventHandler as (VtyEvent (Vty.EvKey (Vty.KChar c) _)) =
  case find (\(c', _, _) -> c' == c) keyHandlers of
    Nothing              -> continue as
    Just (_, _, handler) -> handler as
eventHandler st _ = continue st


-- | Suspend the current UI and run an IO action in terminal. If the
-- IO action returns a Left value, then it's thrown as userError.
withIOAction :: (AppState -> (Int, ListResult) -> IO (Either String a))
             -> AppState
             -> EventM n (Next AppState)
withIOAction action as = case listSelectedElement (lr as) of
  Nothing      -> continue as
  Just (ix, e) -> suspendAndResume $ do
    r <- action as (ix, e)
    case r of
      Left  err -> throwIO $ userError err
      Right _   -> do
        apps <- (fmap . fmap)
          (\AppState {..} -> AppState { lr = listMoveTo ix lr, .. })
          $ getAppState Nothing (pfreq as)
        case apps of
          Right nas -> do
            putStrLn "Press enter to continue"
            _ <- getLine
            pure nas
          Left err -> throwIO $ userError err


install' :: AppState -> (Int, ListResult) -> IO (Either String ())
install' AppState {..} (_, ListResult {..}) = do
  settings <- readIORef settings'
  l        <- readIORef logger'
  let runLogger = myLoggerT l

  let
    run =
      runLogger
        . flip runReaderT settings
        . runResourceT
        . runE
          @'[AlreadyInstalled
            , UnknownArchive
#if !defined(TAR)
            , ArchiveResult
#endif
            , DistroNotFound
            , FileDoesNotExistError
            , CopyError
            , NoCompatibleArch
            , NoDownload
            , NotInstalled
            , NoCompatiblePlatform
            , BuildFailed
            , TagNotFound
            , DigestError
            , DownloadFailed
            , NoUpdate]

  (run $ do
      case lTool of
        GHC   -> liftE $ installGHCBin dls lVer pfreq
        Cabal -> liftE $ installCabalBin dls lVer pfreq
        GHCup -> liftE $ upgradeGHCup dls Nothing False pfreq $> ()
    )
    >>= \case
          VRight _                          -> pure $ Right ()
          VLeft  (V (AlreadyInstalled _ _)) -> pure $ Right ()
          VLeft (V (BuildFailed _ e)) ->
            pure $ Left [i|Build failed with #{e}|]
          VLeft (V NoDownload) ->
            pure $ Left [i|No available version for #{prettyVer lVer}|]
          VLeft (V NoUpdate) -> pure $ Right ()
          VLeft e -> pure $ Left [i|#{e}
Also check the logs in ~/.ghcup/logs|]


set' :: AppState -> (Int, ListResult) -> IO (Either String ())
set' _ (_, ListResult {..}) = do
  settings <- readIORef settings'
  l        <- readIORef logger'
  let runLogger = myLoggerT l

  let run =
        runLogger
          . flip runReaderT settings
          . runE @'[FileDoesNotExistError, NotInstalled, TagNotFound]

  (run $ do
      case lTool of
        GHC   -> liftE $ setGHC (GHCTargetVersion lCross lVer) SetGHCOnly $> ()
        Cabal -> liftE $ setCabal lVer $> ()
        GHCup -> pure ()
    )
    >>= \case
          VRight _ -> pure $ Right ()
          VLeft  e -> pure $ Left [i|#{e}|]


del' :: AppState -> (Int, ListResult) -> IO (Either String ())
del' _ (_, ListResult {..}) = do
  settings <- readIORef settings'
  l        <- readIORef logger'
  let runLogger = myLoggerT l

  let run = runLogger . flip runReaderT settings . runE @'[NotInstalled]

  (run $ do
      case lTool of
        GHC   -> liftE $ rmGHCVer (GHCTargetVersion lCross lVer) $> ()
        Cabal -> liftE $ rmCabalVer lVer $> ()
        GHCup -> pure ()
    )
    >>= \case
          VRight _ -> pure $ Right ()
          VLeft  e -> pure $ Left [i|#{e}|]


changelog' :: AppState -> (Int, ListResult) -> IO (Either String ())
changelog' AppState {..} (_, ListResult {..}) = do
  case getChangeLog dls lTool (Left lVer) of
    Nothing -> pure $ Left
      [i|Could not find ChangeLog for #{lTool}, version #{prettyVer lVer}|]
    Just uri -> do
      exec "xdg-open" True [serializeURIRef' uri] Nothing Nothing >>= \case
        Right _ -> pure $ Right ()
        Left  e -> pure $ Left [i|#{e}|]


uri' :: IORef (Maybe URI)
{-# NOINLINE uri' #-}
uri' = unsafePerformIO (newIORef Nothing)


settings' :: IORef Settings
{-# NOINLINE settings' #-}
settings' = unsafePerformIO
  (newIORef Settings { cache      = True
                     , noVerify   = False
                     , keepDirs   = Never
                     , downloader = Curl
                     , verbose    = False
                     }
  )


logger' :: IORef LoggerConfig
{-# NOINLINE logger' #-}
logger' = unsafePerformIO
  (newIORef $ LoggerConfig { lcPrintDebug = False
                           , colorOutter  = \_ -> pure ()
                           , rawOutter    = \_ -> pure ()
                           }
  )


brickMain :: Settings -> Maybe URI -> LoggerConfig -> GHCupDownloads -> PlatformRequest -> IO ()
brickMain s muri l av pfreq' = do
  writeIORef uri'      muri
  writeIORef settings' s
  -- logger interpreter
  writeIORef logger'   l
  let runLogger = myLoggerT l

  eApps <- getAppState (Just av) pfreq'
  case eApps of
    Right as -> defaultMain app (selectLatest as) $> ()
    Left  e  -> do
      runLogger ($(logError) [i|Error building app state: #{show e}|])
      exitWith $ ExitFailure 2
 where
  selectLatest :: AppState -> AppState
  selectLatest AppState {..} =
    (\ix -> AppState { lr = listMoveTo ix lr, .. })
      . fromJust
      . V.findIndex (\ListResult {..} -> lTool == GHC && Latest `elem` lTag)
      $ (listElements lr)


getAppState :: Maybe GHCupDownloads -> PlatformRequest -> IO (Either String AppState)
getAppState mg pfreq' = do
  muri     <- readIORef uri'
  settings <- readIORef settings'
  l        <- readIORef logger'
  let runLogger = myLoggerT l

  r <-
    runLogger
    . flip runReaderT settings
    . runE
      @'[JSONError, DownloadFailed, FileDoesNotExistError]
    $ do
        dls <- maybe (fmap _ghcupDownloads $ liftE $ getDownloadsF (maybe GHCupURL OwnSource muri)) pure mg

        lV <- lift $ listVersions dls Nothing Nothing pfreq'
        pure $ (AppState (list "Tool versions" (V.fromList lV) 1) dls pfreq')

  case r of
    VRight a -> pure $ Right a
    VLeft  e -> pure $ Left [i|#{e}|]
