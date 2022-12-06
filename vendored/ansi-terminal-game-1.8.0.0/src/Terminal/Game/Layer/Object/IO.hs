-------------------------------------------------------------------------------
-- Layer 2 (mockable IO), as per
-- https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html
-- 2019 Francesco Ariis GPLv3
-------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Terminal.Game.Layer.Object.IO where

import Terminal.Game.Utils

import Terminal.Game.Layer.Object.Interface
import Terminal.Game.Layer.Object.Primitive
import Terminal.Game.Plane

import qualified Control.Concurrent           as CC
import qualified Control.Monad                as CM
import qualified Control.Monad.Catch          as MC
import qualified Control.Monad.Trans          as T
import qualified Data.List.Split              as LS
import qualified System.Clock                 as SC
import qualified System.Console.ANSI          as CA
import qualified System.Console.Terminal.Size as TS
import qualified System.IO                    as SI

-- Most General MonadIO operations.

----------------
-- Game input --
----------------

instance {-# OVERLAPS #-} (Monad m, T.MonadIO m) => MonadInput m where
    startEvents tps = T.liftIO $ startIOInput tps
    pollEvents ve = T.liftIO $ CC.swapMVar ve []
    stopEvents ts = T.liftIO $ stopEventsIO ts

-- filepath = logging
startIOInput :: TPS -> IO InputHandle
startIOInput tps =
            SI.hSetBuffering SI.stdin SI.NoBuffering  >>
            SI.hSetBuffering SI.stdout SI.NoBuffering >>
            SI.hSetEcho SI.stdin False                >>
                -- all the buffering settings has to happen
                -- at the top of startIOInput. If i move
                -- them to display, you need to press enter
                -- before playing the game on some machines.

            -- event and log variables
            CC.newMVar [] >>= \ve ->

            getTimeTick tps               >>= \it ->
            CC.forkIO (addTick ve tps it) >>= \te ->
            CC.forkIO (addKeypress ve)    >>= \tk ->
            return (InputHandle ve [te, tk])

-- a precise timer, not based on `threadDelay`
type Elapsed = Integer -- in `Ticks`

-- elapsed from Epoch in ticks
getTimeTick :: TPS -> IO Elapsed
getTimeTick tps =
        getTime >>= \tm ->
        let ns = 10 ^ (9 :: Integer)
            t1 = quot ns tps in
        return (quot tm t1)

-- mr: maybe recording
addTick :: CC.MVar [Event] -> TPS -> Elapsed -> IO ()
addTick ve tps el =
                -- precise timing. With `treadDelay`, on finer TPS,
                -- ticks take too much (check threadDelay doc).
                getTimeTick tps                   >>= \t ->
                CM.replicateM_ (fromIntegral $ t-el)
                               (addEvent ve Tick) >>

                -- sleep some
                sleepABit tps >>
                addTick ve tps t

-- get action char
-- mr: maybe recording
addKeypress :: CC.MVar [Event] -> IO ()
addKeypress ve = -- vedi platform-dep/
                 inputCharTerminal        >>= \c ->
                 addEvent ve (KeyPress c) >>
                 addKeypress ve

-- mr: maybe recording
addEvent :: CC.MVar [Event] -> Event -> IO ()
addEvent ve e = vf ve
    where
          vf d = CC.modifyMVar_ d (return . (++[e]))

stopEventsIO :: [CC.ThreadId] -> IO ()
stopEventsIO ts = mapM_ CC.killThread ts

-----------------
-- Game timing --
-----------------

instance {-# OVERLAPS #-} (Monad m, T.MonadIO m) => MonadTimer m where
    getTime = T.liftIO $ SC.toNanoSecs <$> SC.getTime SC.Monotonic
    sleepABit tps = T.liftIO $
        CC.threadDelay (fromIntegral $ quot oneTickSec (tps*10))

--------------------
-- Error handling --
--------------------

instance {-# OVERLAPS #-}
        (Monad m, T.MonadIO m, MC.MonadMask m, MC.MonadThrow m) =>
          MonadException m where
    cleanUpErr m c = MC.finally m c
    throwExc t = MC.throwM t

-----------
-- Logic --
-----------

instance {-# OVERLAPS #-} (Monad m, T.MonadIO m) =>
          MonadLogic m where
    checkQuit fb s = return (fb s)

-------------
-- Display --
-------------

instance {-# OVERLAPS #-} (Monad m, T.MonadIO m) => MonadDisplay m where
    setupDisplay = T.liftIO initPart
    clearDisplay = T.liftIO clearScreen
    displaySize = T.liftIO displaySizeIO
    blitPlane mp p = T.liftIO (blitPlaneIO mp p)
    shutdownDisplay = T.liftIO cleanAndExit

displaySizeIO :: IO (Maybe Dimensions)
displaySizeIO =
        TS.size        >>= \ts ->
            -- cannot use ansi-terminal, on Windows you get
            -- "ConsoleException 87" (too much scrolling)
            -- and it does not work for mintty and it is
            -- inefficient as it gets (attempts to scroll past
            -- bottom right)
        isWin32Console >>= \bw ->

        return (fmap (f bw) ts)
    where
          f :: Bool -> TS.Window Int -> Dimensions
          f wbw (TS.Window h w) =
                let h' | wbw       = h - 1
                       | otherwise = h
                in (w, h')

-- pn: new plane, po: old plane
-- wo, ho: dimensions of the terminal. If they change, reinit double buffering
blitPlaneIO :: Maybe Plane -> Plane -> IO ()
blitPlaneIO mpo pn =

        -- remember that Nothing will be passed:
        -- - at the beginning of the game (first blit)
        -- - when resolution changes (see gameLoop)
        -- so do not duplicate hasResChanged checks here!

        -- old plane
        let
            (pw, ph) = planeSize pn
            bp  = blankPlane pw ph
            po  = pastePlane (maybe bp id mpo) bp (1, 1)
        in

        -- new plane
        let pn'  = pastePlane pn bp (1, 1)
        in

            -- trimming is foundamental, as blitMap could otherwise print
            -- outside terminal boundaries and scroll to its death
            -- (error 87 on Win32 console).

        CA.setSGR [CA.Reset] >>
        blitMap po pn'


-----------------
-- ANCILLARIES --
-----------------

initPart :: IO ()
initPart = -- check thread support
           CM.unless CC.rtsSupportsBoundThreads
                     (error errMes)             >>

           -- initial setup/checks
           CA.hideCursor >>

           -- text encoding
           SI.mkTextEncoding "UTF-8//TRANSLIT" >>= \te ->
           SI.hSetEncoding SI.stdout te        >>

           clearScreen
    where
          errMes = unlines
            ["\nError: you *must* compile this program with -threaded!",
             "Just add",
             "",
             "    ghc-options:      -threaded",
             "",
             "in your .cabal file (executable section) and you will be fine!"]

-- clears screen
clearScreen :: IO ()
clearScreen = CA.setCursorPosition 0 0 >>
              CA.setSGR [CA.Reset]     >>
              displaySizeErr           >>= \(w, h) ->
              CM.replicateM_ (fromIntegral $ w*h) (putChar ' ')

cleanAndExit :: IO ()
cleanAndExit = CA.setSGR [CA.Reset]     >>
               CA.clearScreen           >>
               CA.setCursorPosition 0 0 >>
               CA.showCursor

-- plane
blitMap :: Plane -> Plane -> IO ()
blitMap po pn =
            CM.when (planeSize po /= planeSize pn)
                    (error "blitMap: different plane sizes")      >>
            CA.setCursorPosition 0 0                              >>
                -- setCursorPosition is *zero* based!
            blitToTerminal (0, 0) (orderedCells po) (orderedCells pn)

orderedCells :: Plane -> [[Cell]]
orderedCells p = LS.chunksOf (fromIntegral w) cells
    where
          cells  = map snd $ assocsPlane p
          (w, _) = planeSize p


-- ordered sequence of cells, both old and new, like they were a String to
-- print to screen.
-- Coords: initial blitting position
-- Remember that this Column is *zero* based
blitToTerminal :: Coords -> [[Cell]] -> [[Cell]] -> IO ()
blitToTerminal (rr, rc) ocs ncs = CM.foldM_ blitLine rr oldNew
    where
          oldNew :: [[(Cell, Cell)]]
          oldNew = zipWith zip ocs ncs

          -- row = previous row
          blitLine :: Row -> [(Cell, Cell)] -> IO Row
          blitLine pr ccs =
                CM.foldM_ blitCell 0 ccs               >>
                -- have to use setCursorPosition (instead of nextrow) b/c
                -- on win there is an auto "go-to-next-line" when reaching
                -- column end and on win it does not do so
                let wr = pr + 1 in
                CA.setCursorPosition (fromIntegral wr)
                                     (fromIntegral rc) >>
                return wr

          -- k is "spaces to skip"
          blitCell :: Int -> (Cell, Cell) -> IO Int
          blitCell k (clo, cln)
                | cln == clo = return (k+1)
                | otherwise  = moveIf k         >>= \k' ->
                               putCellStyle cln >>
                               return k'

          moveIf :: Int -> IO Int
          moveIf k | k == 0    = return k
                   | otherwise = CA.cursorForward k >>
                                 return 0

putCellStyle :: Cell -> IO ()
putCellStyle c = CA.setSGR ([CA.Reset] ++ sgrb ++ sgrr ++ sgrc) >>
                 putChar (cellChar c)
    where
          sgrb | isBold c  = [CA.SetConsoleIntensity CA.BoldIntensity]
               | otherwise = []

          sgrr | isReversed c = [CA.SetSwapForegroundBackground True]
               | otherwise    = []

          sgrc | Just (k, i) <- cellColor c = [CA.SetColor CA.Foreground i k]
               | otherwise                  = []

oneTickSec :: Integer
oneTickSec = 10 ^ (6 :: Integer)
