{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Control.Concurrent (threadDelay)

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.FTGL as FTGL
-- import qualified Graphics.UI.GLUT.Fonts as GLUT
-- everything from here starts with gl or GL
import Graphics.GL
import Graphics.GLU ( gluPerspective )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )

import Data.IORef

import qualified Data.Text.Encoding as T

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

-- drawScene :: Scene -> GameState -> IO ()

drawMainMenu :: FTGL.Font -> Int -> Int -> GLFW.Window ->  IO ()
drawMainMenu font width height _ = 
  drawText font "Hpong" ((((fromIntegral width) / 2) - 20), ((fromIntegral height) - 50)) (1,1,1)

  

-- drawGameScene :: IORef GLfloat -> GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef Int -> IORef Int -> Int -> Int -> GLFW.Window ->  IO ()
-- drawGameScene racketLeftYRef racketRightX racketRightYRef ballPosXRef ballPosYRef ballDirXRef ballDirYRef leftScoreRef rightScoreRef width height _ = do
drawGameScene :: GameState -> GLFW.Window -> IO ()
drawGameScene GameState{..} _ = do
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
      modifyIORef rightScoreRef (+ 1)
      writeIORef ballPosXRef ((fromIntegral width) / 2)
      writeIORef ballPosYRef ((fromIntegral height) / 2)
      writeIORef ballDirXRef (abs ballDirX)
      writeIORef ballDirYRef 0 
    else return ()
  
  -- hit right wall
  if ballPosX > (fromIntegral width)
    then do 
      -- add score_left
      modifyIORef leftScoreRef (+ 1)
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

mainMenuReadKeys :: IORef Scene -> GLFW.Window -> IO ()
mainMenuReadKeys sceneRef win = do
  enter <- GLFW.getKey win GLFW.Key'Enter
  if isPressed enter then writeIORef sceneRef Game else return ()
  return ()
  

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


data GameState = GameState
  { racketLeftYRef  :: IORef GLfloat
  , racketRightYRef :: IORef GLfloat
  , ballPosXRef     :: IORef GLfloat
  , ballPosYRef     :: IORef GLfloat
  , ballDirXRef     :: IORef GLfloat
  , ballDirYRef     :: IORef GLfloat
  , leftScoreRef    :: IORef Int
  , rightScoreRef   :: IORef Int
  , racketRightX    :: GLfloat
  , width           :: Int
  , height          :: Int
  }

data Scene = 
  MainMenu |
  Game

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
     
     currentSceneRef <- newIORef MainMenu
     
     racketLeftYRef  <- newIORef 50
     racketRightYRef <- newIORef 50
     ballPosXRef     <- newIORef ((fromIntegral width) / 2)
     ballPosYRef     <- newIORef ((fromIntegral height) / 2)
     ballDirXRef     <- newIORef (-1)
     ballDirYRef     <- newIORef (0)
     leftScoreRef    <- newIORef (0) :: IO (IORef Int)
     rightScoreRef   <- newIORef (0) :: IO (IORef Int)
     let racketRightX = (fromIntegral width) - racketWidth - 10
     let gameState = 
           GameState
             racketLeftYRef
             racketRightYRef
             ballPosXRef
             ballPosYRef
             ballDirXRef
             ballDirYRef
             leftScoreRef
             rightScoreRef
             racketRightX
             width
             height

     font <- FTGL.createExtrudeFont "FreeSans.ttf"
     FTGL.fsetFontCharMap font (FTGL.marshalCharMap FTGL.EncodingUnicode)
     _ <- FTGL.setFontFaceSize font 50 50
     
     GLFW.makeContextCurrent (Just win)
     -- register the function to do all our OpenGL drawing
     GLFW.setWindowRefreshCallback win (Just (drawMainMenu font width height) )
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
       
       scene <- readIORef currentSceneRef
       case scene of
         MainMenu -> do
           mainMenuReadKeys currentSceneRef win
           drawMainMenu font width height win           
         Game -> do
           readMultipleKeys (fromIntegral height) racketLeftYRef racketRightYRef win           
           drawGameScene gameState win
           leftScore  <- readIORef $ leftScoreRef
           rightScore <- readIORef $ rightScoreRef
           drawText font ((show leftScore) ++ ":" ++ (show rightScore)) ((((fromIntegral width) / 2) - 20), ((fromIntegral height) - 50)) (1,1,1)
           glColor4f 1 1 1 1
           

       -- threadDelay 100000
       GLFW.swapBuffers win


drawText :: FTGL.Font -> String -> (GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat) -> IO()
drawText fnt s (x, y) (r, g, b) = do
  glColor3f r g b
  glTranslatef x y 0
  glRasterPos2f 0 0
  FTGL.renderFont fnt s FTGL.All
