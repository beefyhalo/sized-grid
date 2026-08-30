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
--
-- Writes a PPM, which is a header and some bytes. @ppm2png.py@ beside this
-- file turns one into something a person can look at, with nothing but the
-- Python standard library, and pools by maximum when it downsamples --- see
-- its header for why that is load bearing rather than a detail.
module Main (main) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.IORef
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)
import Graphics.Gloss.Interface.IO.Game qualified as G
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL qualified as GL
-- Not @Sokoban.Band (..)@: that module's own constructor is called Band too,
-- and so is the view that draws one.
import Sokoban.Band (Band, bandDistance, bandGirth, bandSpin, bandTilt, defaultBand)
import Sokoban.Level
import Sokoban.Render
import Sokoban.Shell
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

data Options = Options
  { optOut :: FilePath,
    optLevel :: Int,
    optView :: View,
    -- | Whether to switch to the player frame.
    optFrame :: Bool,
    -- | The band, for the view that draws one. Its proportions are here and
    -- not only in 'defaultBand' because choosing them is what this harness is
    -- for: a band is a picture, and picking a picture's proportions by
    -- rebuilding between guesses is how sized-grid-23y3 went wrong.
    optBand :: Band,
    -- | Which screen to photograph. The keys are always fed with the game
    -- playing --- that is what they mean --- and the stage is set afterwards,
    -- so @--level 3 --stage levels@ is the level screen with the cursor on the
    -- third one.
    optStage :: Stage,
    optKeys :: String
  }

defaults :: Options
defaults =
  Options
    { optOut = "shot.ppm",
      optLevel = 1,
      optView = Flat,
      optFrame = False,
      optBand = defaultBand,
      optStage = Playing,
      optKeys = ""
    }

parse :: [String] -> Options -> Either String Options
parse [] o = Right o
parse ("--out" : v : as) o = parse as o {optOut = v}
parse ("--level" : v : as) o =
  case reads v of
    [(n, "")] -> parse as o {optLevel = n}
    _ -> Left ("--level wants a number, got " ++ v)
parse ("--view" : "flat" : as) o = parse as o {optView = Flat}
parse ("--view" : "centred" : as) o = parse as o {optView = Centred}
parse ("--view" : "band" : as) o = parse as o {optView = Band}
parse ("--spin" : v : as) o = number "--spin" v as o (\x b -> b {bandSpin = x})
parse ("--girth" : v : as) o = number "--girth" v as o (\x b -> b {bandGirth = x})
parse ("--tilt" : v : as) o = number "--tilt" v as o (\x b -> b {bandTilt = x})
parse ("--dist" : v : as) o = number "--dist" v as o (\x b -> b {bandDistance = x})
parse ("--stage" : "title" : as) o = parse as o {optStage = Title}
parse ("--stage" : "levels" : as) o = parse as o {optStage = Levels}
parse ("--stage" : "play" : as) o = parse as o {optStage = Playing}
parse ("--cleared" : v : as) o =
  case reads v of
    [(t, "")] -> parse as o {optStage = Cleared t}
    _ -> Left ("--cleared wants a number of seconds, got " ++ v)
parse ("--frame" : "player" : as) o = parse as o {optFrame = True}
parse ("--frame" : "chart" : as) o = parse as o {optFrame = False}
parse ("--keys" : v : as) o = parse as o {optKeys = v}
parse (a : _) _ = Left ("unknown argument " ++ a)

-- | One of the band's numbers, off the command line and into it.
number :: String -> String -> [String] -> Options -> (Float -> Band -> Band) -> Either String Options
number flag v as o set =
  case reads v of
    [(x, "")] -> parse as o {optBand = set x (optBand o)}
    _ -> Left (flag ++ " wants a number, got " ++ v)

main :: IO ()
main = do
  args <- getArgs
  case parse args defaults of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right o -> do
      let app0 =
            (newApp defaultWindow builtinLevels)
              { appBand = optBand o,
                appStage = Playing
              }
          app1 =
            (feed (levelKeys (optLevel o) ++ viewKeys o ++ optKeys o) app0)
              { appStage = optStage o
              }
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

-- | The view key cycles, and the app starts on the first view, so asking for
-- the @n@th one is @n@ presses. Written off the 'Enum' rather than case by
-- case so that a fourth view does not need a line here.
viewKeys :: Options -> String
viewKeys o =
  replicate (fromEnum (optView o)) 'v'
    ++ ( if optFrame o
           then "f"
           else ""
       )

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
        rows = [BS.take stride (BS.drop (r * stride) raw) | r <- [h - 1, h - 2 .. 0]]
        header = BC.pack ("P6\n" ++ show w ++ " " ++ show h ++ "\n255\n")
    BS.writeFile path (BS.concat (header : rows))
  hPutStrLn stderr ("wrote " ++ path)
