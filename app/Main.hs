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
resizeScene _   width' height' = do
  let width''  = if width' < 600 then 600 else width'
      height'' = if height' < 200 then 200 else height'
  glViewport 0 0 (fromIntegral width'') (fromIntegral height'')
  glMatrixMode GL_PROJECTION
  glLoadIdentity
  glOrtho 0 (fromIntegral width'') 0 (fromIntegral height'') 0 1
  -- gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
  glFlush



drawScene :: IORef GLfloat -> GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> Int -> Int -> GLFW.Window ->  IO ()
drawScene racketLeftYRef racketRightX racketRightYRef ballPosXRef ballPosYRef ballDirXRef ballDirYRef width height _ = do
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
      modifyIORef xRef (* nLen)
      modifyIORef yRef (* nLen)
    _    -> return ()


-- width :: Int
-- width = 1200

-- height :: Int
-- height = 500

racketWidth :: GLfloat
racketWidth = 10

racketHeight :: GLfloat
racketHeight = 80

racketSpeed :: GLfloat
racketSpeed = 3

racketLeftX :: GLfloat
racketLeftX = 10.0


-- ball

ballSize :: GLfloat
ballSize = 8

ballSpeed :: GLfloat
ballSpeed = 8

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


keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed   _ = shutdown win
keyPressed _   _               _ _                       _ = return ()

isPressed :: GLFW.KeyState -> Bool
isPressed GLFW.KeyState'Pressed   = True
isPressed GLFW.KeyState'Repeating = True
isPressed _ = False

readMultipleKeys :: GLfloat -> IORef GLfloat -> IORef GLfloat -> GLFW.Window -> IO ()
readMultipleKeys height racketLeftYRef racketRightYRef win = do
  -- left
  w <- GLFW.getKey win GLFW.Key'W
  if isPressed w then moveRacketUp height racketLeftYRef else return ()
  
  s <- GLFW.getKey win GLFW.Key'S
  if isPressed s then moveRacketDown racketLeftYRef else return ()
    
  -- right
  i <- GLFW.getKey win GLFW.Key'I
  if isPressed i then moveRacketUp height racketRightYRef else return ()
  
  k <- GLFW.getKey win GLFW.Key'K
  if isPressed k then moveRacketDown racketRightYRef else return ()

moveRacketUp :: GLfloat -> IORef GLfloat -> IO ()
moveRacketUp height racketYRef = do
  racketY <- readIORef racketYRef
  let newRacketY = racketY + 10
      newRacketY' = 
        if newRacketY < (height - racketHeight)
          then newRacketY
          else height - racketHeight
  writeIORef racketYRef newRacketY'

moveRacketDown :: IORef GLfloat -> IO ()
moveRacketDown racketYRef = do
  racketY <- readIORef racketYRef
  let newRacketY = racketY - 10
      newRacketY' = 
        if newRacketY > 0
          then newRacketY
          else 0
  writeIORef racketYRef newRacketY'  

main :: IO ()
main = do
     True <- GLFW.init
          
     -- select type of display mode:
     -- Double buffer
     -- RGBA color
     -- Alpha components supported
     -- Depth buffer
     -- GLFW.defaultWindowHints
     -- open a window
     Just win <- GLFW.createWindow 500 200 "phong" Nothing Nothing
     (width,height) <- GLFW.getFramebufferSize win
     
     let racketRightX = (fromIntegral width) - racketWidth - 10

     
     racketLeftYRef  <- newIORef 50
     racketRightYRef <- newIORef 50
     ballPosXRef     <- newIORef ((fromIntegral width) / 2)
     ballPosYRef     <- newIORef ((fromIntegral height) / 2)
     ballDirXRef     <- newIORef (-1)
     ballDirYRef     <- newIORef (0)
     
     GLFW.makeContextCurrent (Just win)
     -- register the function to do all our OpenGL drawing
     GLFW.setWindowRefreshCallback win (Just (drawScene racketLeftYRef racketRightX racketRightYRef ballPosXRef ballPosYRef ballDirXRef ballDirYRef width height) )
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win (Just keyPressed)
     GLFW.setWindowCloseCallback win (Just shutdown)
     -- initialize our window.
     initGL win
     
     -- start event processing engine
     forever $ do
       GLFW.pollEvents
       readMultipleKeys (fromIntegral height) racketLeftYRef racketRightYRef win
       drawScene racketLeftYRef racketRightX racketRightYRef ballPosXRef ballPosYRef ballDirXRef ballDirYRef width height win
       GLFW.swapBuffers win
