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
import           Codec.Archive
import           Control.Exception.Safe
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Bool
import           Data.Functor
import           Data.List
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
}

type LR = GenericList String Vector ListResult


ui :: AppState -> Widget String
ui AppState {..} =
  ( padBottom Max
    $ ( withBorderStyle unicode
      $ borderWithLabel (str "GHCup")
      $ (center $ renderList renderItem True lr)
      )
    )
    <=> foldr1
          (\x y -> x <+> str "  " <+> y)
          [ (str "q:Quit")
          , (str "i:Install")
          , (str "s:Set")
          , (str "u:Uninstall")
          , (str "c:ChangeLog")
          , (str "↑↓:Navigation")
          ]

 where
  renderItem b ListResult {..} =
    let marks = if
          | lSet       -> (withAttr "set" $ str "✔✔")
          | lInstalled -> (withAttr "installed" $ str "✓ ")
          | otherwise  -> (withAttr "not-installed" $ str "✗ ")
        ver = case lCross of
          Nothing -> T.unpack . prettyVer $ lVer
          Just c  -> T.unpack (c <> "-" <> prettyVer lVer)
    in  (   marks
        <+> ( padLeft (Pad 2)
            $ minHSize 20
            $ (withAttr
                (bool "inactive" "active" b)
                (str (fmap toLower . show $ lTool) <+> str " " <+> str ver)
              )
            )
        <+> (padLeft (Pad 1) $ if null lTag
              then str ""
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
minHSize s' = hLimit s' . vLimit 1 . (<+> str (replicate s' ' '))


app :: App AppState e String
app = App { appDraw         = \st -> [ui st]
          , appHandleEvent  = eventHandler
          , appStartEvent   = return
          , appAttrMap      = const theMap
          , appChooseCursor = neverShowCursor
          }
 where
  theMap = attrMap
    Vty.defAttr
    [ ("active"       , bg Vty.blue)
    , ("not-installed", fg Vty.red)
    , ("set"          , fg Vty.green)
    , ("installed"    , fg Vty.green)
    , ("recommended"  , fg Vty.green)
    , ("latest"       , fg Vty.yellow)
    ]


eventHandler :: AppState -> BrickEvent n e -> EventM n (Next AppState)
eventHandler st (VtyEvent (Vty.EvResize _               _)) = continue st
eventHandler st (VtyEvent (Vty.EvKey    (Vty.KChar 'q') _)) = halt st
eventHandler st (VtyEvent (Vty.EvKey    Vty.KEsc        _)) = halt st
eventHandler AppState {..} (VtyEvent (Vty.EvKey (Vty.KUp) _)) =
  continue (AppState (listMoveUp lr) dls)
eventHandler AppState {..} (VtyEvent (Vty.EvKey (Vty.KDown) _)) =
  continue (AppState (listMoveDown lr) dls)
eventHandler AppState { dls = dls', lr = lr' } (VtyEvent (Vty.EvKey (Vty.KChar c) _))
  | (Just (ix, e)) <- listSelectedElement lr'
  , c `elem` ['i', 's', 'u', 'c']
  = suspendAndResume $ do
    r <- case c of
      'i' -> install' e dls'
      's' -> set' e
      'u' -> del' e
      'c' -> changelog' e dls'
      _   -> error ""
    case r of
      Left  err -> throwIO $ userError err
      Right _   -> do
        apps <- (fmap . fmap)
          (\AppState {..} -> AppState { lr = listMoveTo ix lr, .. })
          getAppState
        case apps of
          Right as  -> do
            putStrLn "Press enter to continue"
            _ <- getLine
            pure as
          Left  err -> throwIO $ userError err
eventHandler st _ = continue st


install' :: ListResult -> GHCupDownloads -> IO (Either String ())
install' ListResult {..} dls = do
  l <- readIORef logger'
  let runLogger = myLoggerT l

  let
    run =
      runLogger
        . flip runReaderT settings
        . runResourceT
        . runE
          @'[AlreadyInstalled, UnknownArchive, ArchiveResult, DistroNotFound, FileDoesNotExistError, CopyError, NoCompatibleArch, NoDownload, NotInstalled, NoCompatiblePlatform, BuildFailed, TagNotFound, DigestError, DownloadFailed, NoUpdate]

  (run $ do
      case lTool of
        GHC   -> liftE $ installGHCBin dls lVer Nothing
        Cabal -> liftE $ installCabalBin dls lVer Nothing
        GHCup -> liftE $ upgradeGHCup dls Nothing False $> ()
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


set' :: ListResult -> IO (Either String ())
set' ListResult {..} = do
  l <- readIORef logger'
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


del' :: ListResult -> IO (Either String ())
del' ListResult {..} = do
  l <- readIORef logger'
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


changelog' :: ListResult -> GHCupDownloads -> IO (Either String ())
changelog' ListResult {..} dls = do
  case getChangeLog dls lTool (Left lVer) of
    Nothing -> pure $ Left
      [i|Could not find ChangeLog for #{lTool}, version #{prettyVer lVer}|]
    Just uri -> do
      exec "xdg-open" True [serializeURIRef' uri] Nothing Nothing >>= \case
        Right _ -> pure $ Right ()
        Left  e -> pure $ Left [i|#{e}|]


settings :: Settings
settings = Settings { cache      = True
                    , noVerify   = False
                    , keepDirs   = Never
                    , downloader = Curl
                    }


logger' :: IORef LoggerConfig
{-# NOINLINE logger' #-}
logger' = unsafePerformIO
  (newIORef $ LoggerConfig { lcPrintDebug = False
                           , colorOutter  = \_ -> pure ()
                           , rawOutter    = \_ -> pure ()
                           }
  )


brickMain :: LoggerConfig -> IO ()
brickMain l = do
  -- logger interpreter
  writeIORef logger' l
  let runLogger = myLoggerT l

  eApps <- getAppState
  case eApps of
    Right as -> defaultMain app as $> ()
    Left  _  -> do
      runLogger ($(logError) [i|Error building app state|])
      exitWith $ ExitFailure 2


getAppState :: IO (Either String AppState)
getAppState = do
  l <- readIORef logger'
  let runLogger = myLoggerT l

  r <-
    runLogger
    . flip runReaderT settings
    . runE
      @'[JSONError, DownloadFailed, FileDoesNotExistError, NoCompatiblePlatform, NoCompatibleArch, DistroNotFound]
    $ do
        (GHCupInfo _ dls) <- liftE $ getDownloadsF GHCupURL

        lV                 <- liftE $ listVersions dls Nothing Nothing
        pure $ (AppState (list "Tool versions" (V.fromList lV) 1) dls)

  case r of
    VRight a -> pure $ Right a
    VLeft  e -> pure $ Left [i|#{e}|]

