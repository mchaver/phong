module Main where

import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.GL
import Graphics.GLU ( gluPerspective )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )

import Data.IORef

initGL :: GLFW.Window -> IO ()
initGL win = do
  glShadeModel GL_SMOOTH -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  glClearDepth 1 -- enables clearing of the depth buffer
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LEQUAL  -- type of depth test
  glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h

resizeScene :: GLFW.FramebufferSizeCallback
resizeScene win w     0      = resizeScene win w 1 -- prevent divide by zero
resizeScene _   width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode GL_PROJECTION
  glLoadIdentity
  glOrtho 0 (fromIntegral width) 0 (fromIntegral height) 0 1
  -- gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
  glFlush



drawScene :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> GLFW.Window -> IO ()
drawScene racketLeftYRef racketRightYRef ballPosXRef ballPosYRef _ = do
  -- clear the screen and the depth bufer
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT
  glLoadIdentity -- reset view
{-
  glTranslatef (-1.5) 0 (-6.0) --Move left 1.5 Units and into the screen 6.0
  
  -- draw a triangle
  glBegin GL_TRIANGLES
  glVertex3f 0      1  0 -- top
  glVertex3f 1    (-1) 0 -- bottom right
  glVertex3f (-1) (-1) 0 -- bottom left
  glEnd
-}
  -- glTranslatef 3 0 0  -- move right three units
  racketLeftY  <- readIORef racketLeftYRef
  racketRightY <- readIORef racketRightYRef
  drawRect racketLeftX  racketLeftY  racketWidth racketHeight
  drawRect racketRightX racketRightY racketWidth racketHeight
  
  modifyIORef ballPosXRef (+ ballDirX * ballSpeed)
  modifyIORef ballPosYRef (+ ballDirY * ballSpeed)
  ballPosX  <- readIORef ballPosXRef
  ballPosY <- readIORef ballPosYRef
  drawRect ballPosX ballPosY ballSize ballSize

{-
  glBegin GL_QUADS
  glVertex3f (-1)   1  0 -- top left
  glVertex3f   1    1  0 -- top right
  glVertex3f   1  (-1) 0 -- bottom right
  glVertex3f (-1) (-1) 0 -- bottom left
  glEnd
-}  
  -- drawRect 0 0 1 1
  
  glFlush

width :: Int
width = 500

height :: Int
height = 200
-- interval = 1000 / 60;

racketWidth :: GLfloat
racketWidth = 10

racketHeight :: GLfloat
racketHeight = 80

racketSpeed :: GLfloat
racketSpeed = 3

racketLeftX :: GLfloat
racketLeftX = 10.0

-- racketLeftY :: GLfloat
-- racketLeftY = 50.0

racketRightX :: GLfloat
racketRightX = (fromIntegral width) - racketWidth - 10

-- racketRightY :: GLfloat
-- racketRightY = 50

-- drawRect :: IO ()

-- ball


ballDirX :: GLfloat
ballDirX = -1.0

ballDirY :: GLfloat
ballDirY = 0.0

ballSize :: GLfloat
ballSize = 8
-- int ball_size = 8
ballSpeed :: GLfloat
ballSpeed = 2

drawRect :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawRect x y width height = do 
  glBegin GL_QUADS
  glVertex2f x y -- top left
  glVertex2f (x + width) y -- top right
  glVertex2f (x + width) (y + height) -- bottom right
  glVertex2f x (y + height) -- bottom left
  glEnd

  {-

  void drawRect(float x, float y, float width, float height) {
      glBegin(GL_QUADS);
          glVertex2f(x, y);
          glVertex2f(x + width, y);
          glVertex2f(x + width, y + height);
          glVertex2f(x, y + height);
      glEnd();
  }

  -}

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: IORef GLfloat -> IORef GLfloat -> GLFW.KeyCallback
keyPressed _              _               win GLFW.Key'Escape _ GLFW.KeyState'Pressed   _ = shutdown win
keyPressed racketLeftYRef _               _   GLFW.Key'W      _ GLFW.KeyState'Pressed   _ = modifyIORef racketLeftYRef (+15)
keyPressed racketLeftYRef _               _   GLFW.Key'W      _ GLFW.KeyState'Repeating _ = modifyIORef racketLeftYRef (+15)
keyPressed racketLeftYRef _               _   GLFW.Key'S      _ GLFW.KeyState'Pressed   _ = modifyIORef racketLeftYRef (subtract 15)
keyPressed racketLeftYRef _               _   GLFW.Key'S      _ GLFW.KeyState'Repeating _ = modifyIORef racketLeftYRef (subtract 15)
keyPressed _              racketRightYRef _   GLFW.Key'I      _ GLFW.KeyState'Pressed   _ = modifyIORef racketRightYRef (+15)
keyPressed _              racketRightYRef _   GLFW.Key'I      _ GLFW.KeyState'Repeating _ = modifyIORef racketRightYRef (+15)
keyPressed _              racketRightYRef _   GLFW.Key'K      _ GLFW.KeyState'Pressed   _ = modifyIORef racketRightYRef (subtract 15)
keyPressed _              racketRightYRef _   GLFW.Key'K      _ GLFW.KeyState'Repeating _ = modifyIORef racketRightYRef (subtract 15)
keyPressed _    _    _   _               _ _                     _ = return ()


main :: IO ()
main = do
     True <- GLFW.init
     
     racketLeftYRef  <- newIORef 50
     racketRightYRef <- newIORef 50
     ballPosXRef     <- newIORef ((fromIntegral width) / 2)
     ballPosYRef     <- newIORef ((fromIntegral width) / 2)
     
     -- select type of display mode:
     -- Double buffer
     -- RGBA color
     -- Alpha components supported
     -- Depth buffer
     GLFW.defaultWindowHints
     -- open a window
     Just win <- GLFW.createWindow width height "phong" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     -- register the function to do all our OpenGL drawing
     GLFW.setWindowRefreshCallback win (Just (drawScene racketLeftYRef racketRightYRef ballPosXRef ballPosYRef) )
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win (Just (keyPressed racketLeftYRef racketRightYRef))
     GLFW.setWindowCloseCallback win (Just shutdown)
     -- initialize our window.
     initGL win
     
     
     -- start event processing engine
     forever $ do
       GLFW.pollEvents
       drawScene racketLeftYRef racketRightYRef ballPosXRef ballPosYRef win
       GLFW.swapBuffers win
