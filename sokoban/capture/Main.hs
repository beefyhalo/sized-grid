-- | Take a picture of the game's own window, from inside the game.
--
-- The demos in this repo cannot be checked by looking at them: screen capture
-- needs a permission this machine does not grant, and every attempt to reason
-- about a gloss layout from the source has been wrong (sized-grid-23y3). What
-- works is making the program photograph itself --- @playIO@, and a draw action
-- that calls @glReadPixels@ on the front buffer once the picture is on screen.
--
-- Reads the viewport rather than the window size, because those are not the
-- same number on a HiDPI display: the window is in points and the framebuffer
-- is in pixels. The viewport is what the GLFW backend sets from the real
-- framebuffer size every frame, so it is the one that is in the units
-- @glReadPixels@ wants.
module Main (main) where

import           Sokoban.Level
import           Sokoban.Render

import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Char8              as BC
import           Data.IORef
import           Foreign.Marshal.Alloc              (allocaBytes)
import           Foreign.Ptr                        (castPtr)
import qualified Graphics.Gloss.Interface.IO.Game   as G
import qualified Graphics.Rendering.OpenGL          as GL
import           Graphics.Rendering.OpenGL          (($=))
import           System.Environment                 (getArgs)
import           System.Exit                        (exitFailure, exitSuccess)
import           System.IO                          (hPutStrLn, stderr)

data Options = Options
    { optOut   :: FilePath
    , optLevel :: Int
    , optView  :: View
    , optFrame :: Bool
    -- ^ Whether to switch to the player frame.
    , optKeys  :: String
    }

defaults :: Options
defaults =
    Options {optOut = "shot.ppm", optLevel = 1, optView = Flat, optFrame = False, optKeys = ""}

parse :: [String] -> Options -> Either String Options
parse [] o = Right o
parse ("--out":v:as) o = parse as o {optOut = v}
parse ("--level":v:as) o =
    case reads v of
        [(n, "")] -> parse as o {optLevel = n}
        _         -> Left ("--level wants a number, got " ++ v)
parse ("--view":"flat":as) o = parse as o {optView = Flat}
parse ("--view":"centred":as) o = parse as o {optView = Centred}
parse ("--frame":"player":as) o = parse as o {optFrame = True}
parse ("--frame":"chart":as) o = parse as o {optFrame = False}
parse ("--keys":v:as) o = parse as o {optKeys = v}
parse (a:_) _ = Left ("unknown argument " ++ a)

main :: IO ()
main = do
    args <- getArgs
    case parse args defaults of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right o -> do
            let app0 = newApp defaultWindow builtinLevels
                app1 = feed (levelKeys (optLevel o) ++ viewKeys o ++ optKeys o) app0
            frames <- newIORef (0 :: Int)
            G.playIO
                (G.InWindow "sokoban shot" defaultWindow (40, 40))
                (G.greyN 0.12)
                30
                app1
                (shoot (optOut o) frames)
                (\_ w -> pure w)
                (\_ w -> pure w)

-- | Level @n@ is @n-1@ presses of the next-level key, since the app starts on
-- the first one. Going through the real key handler rather than reaching into
-- the state means the picture is of a state the game can actually be in.
levelKeys :: Int -> String
levelKeys n = replicate (max 0 (n - 1)) 'n'

viewKeys :: Options -> String
viewKeys o =
    (case optView o of
         Flat    -> ""
         Centred -> "v") ++
    (if optFrame o
         then "f"
         else "")

feed :: String -> App -> App
feed keys app = foldl one app keys
  where
    one a c = onEvent (G.EventKey (G.Char c) G.Down noMods (0, 0)) a
    noMods = G.Modifiers G.Up G.Up G.Up

-- | Draw, and on the fourth frame photograph the third and stop. The delay is
-- because the front buffer holds the /previous/ frame: reading it before
-- anything has been swapped into it gives whatever the window manager left
-- there.
shoot :: FilePath -> IORef Int -> App -> IO G.Picture
shoot path frames app = do
    n <- readIORef frames
    writeIORef frames (n + 1)
    if n < 4
        then pure (drawApp app)
        else do
            writePPM path
            exitSuccess

writePPM :: FilePath -> IO ()
writePPM path = do
    GL.readBuffer $= GL.FrontBuffers
    GL.rowAlignment GL.Pack $= 1
    (pos@(GL.Position _ _), size@(GL.Size vw vh)) <- GL.get GL.viewport
    let w = fromIntegral vw
        h = fromIntegral vh
        bytes = w * h * 3
    allocaBytes bytes $ \ptr -> do
        GL.readPixels pos size (GL.PixelData GL.RGB GL.UnsignedByte ptr)
        raw <- BS.packCStringLen (castPtr ptr, bytes)
        -- OpenGL's origin is bottom-left and a PPM's is top-left.
        let stride = w * 3
            rows = [BS.take stride (BS.drop (r * stride) raw) | r <- [h - 1,h - 2 .. 0]]
            header = BC.pack ("P6\n" ++ show w ++ " " ++ show h ++ "\n255\n")
        BS.writeFile path (BS.concat (header : rows))
    hPutStrLn stderr ("wrote " ++ path)
