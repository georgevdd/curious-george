module Main where
import Control.Monad (when)
import Data.Maybe
import Graphics.UI.GLUT hiding (Inside, Outside)
import Data.IORef

import Data.Vect.Double
import Data.Vect.Double.OpenGL

import KiteDart
import Clip

type ClearColor = Color4 GLclampf

data FrameState = FrameState {
  bgColor :: ClearColor,
  cameraPos :: Vec3,
  fsAngle :: Scalar,
  deflations :: Integer
} deriving Show

initialState :: FrameState
initialState = FrameState {
  bgColor = Color4 0.4 0.4 1.0 1.0,
  cameraPos = Vec3 0 0 1,
  fsAngle = 0,
  deflations = 1
}

strokeColor = Color4 0.5 0.5 1 1
kiteColor   = Color4 0.8 0.8 1 1
dartColor   = Color4 0.7 0.7 1 1

unitVector :: Int -> Vec3
unitVector 0 = vec3X
unitVector 1 = vec3Y
unitVector 2 = vec3Z

drawAxis :: Int -> IO ()
drawAxis i = do
  let axis = unitVector i
  color (zero :: Vec3)
  vertex (zero :: Vec3)
  color axis
  vertex axis

drawAxes :: IO ()
drawAxes = renderPrimitive Lines $ do
  mapM_ drawAxis [0..2]

drawTri :: Half -> Color4 GLfloat -> IO ()
drawTri tile color = do
  let vertices = mapM_ vertex $ (snd . realise) tile
  currentColor $= color
  renderPrimitive Triangles vertices
  currentColor $= strokeColor
  renderPrimitive LineStrip vertices

drawTile :: Half -> IO ()
drawTile t@(Half Kite m) = drawTri t kiteColor
drawTile t@(Half Dart m) = drawTri t dartColor

glvt3 :: Vec3 -> Vertex3 GLdouble
glvt3 (Vec3 x y z) = fmap glflt (Vertex3 x y z)

glvc3 :: Vec3 -> Vector3 GLdouble
glvc3 (Vec3 x y z) = fmap glflt (Vector3 x y z)

drawFrame :: IORef FrameState -> IO ()
drawFrame stateRef = do
  state <- readIORef stateRef
  drawScene state
  swapBuffers

drawScene :: FrameState -> IO ()
drawScene state = do
  clearColor $= bgColor state
  clear [ColorBuffer, DepthBuffer]

  currentColor $= Color4 1 1 0 1

  matrixMode $= Modelview 0
  loadIdentity
  lookAt (glvt3 $ cameraPos state) (glvt3 zero) (glvc3 vec3Y)
  cullFace $= Just Back
  let tiles = clipDeflate b sun' (deflations state)
      sun' = [Half Kite (m .*. linear (rotMatrix2 (fsAngle state))) | Half Kite m <- sun]
      b = [(Vec3 (sin a') (cos a') 0, (-0.8)) | a' <- [0, pi/2 .. 2*pi]]
  mapM_ drawTile (fst tiles ++ snd tiles)

onKeypress :: IORef FrameState -> KeyboardMouseCallback
onKeypress stateRef key keyState modifiers position = do
  oldState <- readIORef stateRef

  let newState =
        case (key, keyState) of
          (Char 'k', Down) -> Just oldState { deflations = deflations oldState + 1 }
          (Char 'j', Down) -> Just oldState { deflations = max (deflations oldState - 1) 0 }
          otherwise -> Nothing
  when (isJust newState) $ do
       writeIORef stateRef $ fromJust newState
       postRedisplay Nothing


think :: FrameState -> FrameState
think oldState = oldState

onReshape :: Size -> IO ()
onReshape (Size x y) = do
  matrixMode $= Projection
  loadIdentity
  viewport $= (Position 0 0, Size x y)
  let x' = (fromIntegral x :: GLdouble)
  let y' = (fromIntegral y :: GLdouble)
  let aspect = x' / y'
  perspective 90.0 aspect 1.0 1000.0
  matrixMode $= Modelview 0


onDisplay :: IORef FrameState -> IO ()
onDisplay = drawFrame

idleAnimation = False

onIdle :: IORef FrameState -> IO ()
onIdle stateRef = do
  oldState <- readIORef stateRef
  let newState = think oldState
  writeIORef stateRef newState
  when idleAnimation $ postRedisplay Nothing


onClose :: IO ()
onClose = do
  return ()

main :: IO ()
main = do
  initialWindowSize $= Size 800 800
  (progName, args) <- getArgsAndInitialize
  window <- createWindow progName
  stateRef <- newIORef initialState
  reshapeCallback $= Just onReshape
  displayCallback $= onDisplay stateRef
  idleCallback $= Just (onIdle stateRef)
  keyboardMouseCallback $= Just (onKeypress stateRef)
  mainLoop
