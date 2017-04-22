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



drawScene :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> GLFW.Window -> IO ()
drawScene racketLeftYRef racketRightYRef ballPosXRef ballPosYRef ballDirXRef ballDirYRef _ = do
  -- clear the screen and the depth bufer
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT
  glLoadIdentity -- reset view
  
  -- draw rackets
  racketLeftY  <- readIORef racketLeftYRef
  racketRightY <- readIORef racketRightYRef
  
  drawRect racketLeftX  racketLeftY  racketWidth racketHeight
  drawRect racketRightX racketRightY racketWidth racketHeight
  
  -- make the ball fly a bit
  ballDirX <- readIORef ballDirXRef
  ballDirY <- readIORef ballDirYRef
  
  modifyIORef ballPosXRef (+ ballDirX * ballSpeed)
  modifyIORef ballPosYRef (+ ballDirY * ballSpeed)
  
  ballPosX <- readIORef ballPosXRef
  ballPosY <- readIORef ballPosYRef
  
  -- collision detection
  
  -- hit by left racket
  if (ballPosX < racketLeftX + racketWidth &&
      ballPosX > racketLeftX &&
      ballPosY < racketLeftY + racketHeight &&
      ballPosY > racketLeftY)
      then do
        writeIORef ballDirXRef (abs ballDirX)
        writeIORef ballDirYRef (((ballPosY - racketLeftY) / racketHeight) - 0.5)        
      else return ()
  
  -- hit by right racket
  if (ballPosX > racketRightX &&
      ballPosX < racketRightX + racketWidth &&
      ballPosY < racketRightY + racketHeight &&
      ballPosY > racketRightY)
      then do
        writeIORef ballDirXRef (negate (abs ballDirX))
        writeIORef ballDirYRef (((ballPosY - racketRightY) / racketHeight) - 0.5)        
      else return ()
  
  -- hit left wall
  if ballPosX < 0 
    then do
      -- add score_right
      writeIORef ballPosXRef ((fromIntegral width) / 2)
      writeIORef ballPosYRef ((fromIntegral height) / 2)
      writeIORef ballDirXRef (abs ballDirX)
      writeIORef ballDirYRef 0 
    else return ()
  
  -- hit right wall
  if ballPosX > (fromIntegral width)
    then do 
      -- add score_left
      writeIORef ballPosXRef ((fromIntegral width) / 2)
      writeIORef ballPosYRef ((fromIntegral height) / 2)
      writeIORef ballDirXRef (negate . abs $ ballDirX)
      writeIORef ballDirYRef 0
    else return ()
  
  -- hit the top wall
  if ballPosY > (fromIntegral height)
    then writeIORef ballDirYRef (negate . abs $ ballDirY)
    else return ()
    
  -- hit the bottom wall
  if ballPosY < 0
    then writeIORef ballDirYRef (abs ballDirY)
    else return ()
  
  -- vec2Norm ballPosXRef ballPosYRef
  
  drawRect ballPosX ballPosY ballSize ballSize
    
  glFlush


-- set a vector's lenght to 1 (x + y == 1)
vec2Norm :: IORef GLfloat -> IORef GLfloat -> IO ()
vec2Norm xRef yRef = do
  x <- readIORef xRef
  y <- readIORef yRef
  let len = sqrt $ ((x ** 2) + (y ** 2))
  case len /= 0 of
    True -> do 
      let nLen = 1 / len
      print nLen
      modifyIORef xRef (* nLen)
      modifyIORef yRef (* nLen)
    _    -> return ()


width :: Int
width = 500

height :: Int
height = 200

racketWidth :: GLfloat
racketWidth = 10

racketHeight :: GLfloat
racketHeight = 80

racketSpeed :: GLfloat
racketSpeed = 3

racketLeftX :: GLfloat
racketLeftX = 10.0

racketRightX :: GLfloat
racketRightX = (fromIntegral width) - racketWidth - 10

-- ball

ballSize :: GLfloat
ballSize = 8

ballSpeed :: GLfloat
ballSpeed = 2

drawRect :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawRect x y width height = do 
  glBegin GL_QUADS
  glVertex2f x y            -- top left
  glVertex2f (x + width) y  -- top right
  glVertex2f (x + width) (y + height) -- bottom right
  glVertex2f x (y + height) -- bottom left
  glEnd


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
     ballDirXRef     <- newIORef (-1)
     ballDirYRef     <- newIORef (0)
     
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
     GLFW.setWindowRefreshCallback win (Just (drawScene racketLeftYRef racketRightYRef ballPosXRef ballPosYRef ballDirXRef ballDirYRef) )
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win (Just (keyPressed racketLeftYRef racketRightYRef))
     GLFW.setWindowCloseCallback win (Just shutdown)
     GLFW.setStickyKeysInputMode win GLFW.StickyKeysInputMode'Enabled
     -- initialize our window.
     initGL win
     
     -- start event processing engine
     forever $ do
       GLFW.pollEvents
       drawScene racketLeftYRef racketRightYRef ballPosXRef ballPosYRef ballDirXRef ballDirYRef win
       GLFW.swapBuffers win
