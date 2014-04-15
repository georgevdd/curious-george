module Main where
import Data.Bits (testBit)
import Codec.Binary.Gray (gray)
import Control.Monad (when)
import Data.Maybe
import Data.Word (Word32, Word8)
import Graphics.UI.GLUT
import Data.IORef

import Data.Vect.Double
import Data.Vect.Double.OpenGL

unitVector :: Int -> Vec3
unitVector 0 = Vec3 1 0 0
unitVector 1 = Vec3 0 1 0
unitVector 2 = Vec3 0 0 1

type ClearColor = Color4 GLclampf
type Scalar = Double


color3f :: Float -> Float -> Float -> Color3 Float
color3f = Color3

data FrameState = FrameState {
  bgColor :: ClearColor,
  cameraPos :: Vec3,
  cubeAngle :: Scalar,
  deflations :: Integer
} deriving Show


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

cubeCorners :: Scalar -> [Vec3]
cubeCorners halfSize = [f n | n <- map gray [0..7 :: Int]]
  where f n = Vec3 (a n 0) (a n 1) (a n 2)
        a n b = if testBit n b then halfSize else -halfSize

phi :: Floating a => a
phi = (1 + sqrt 5) / 2

faceCorners :: Int -> [Vec3]
faceCorners i = [f n | n <- map gray [0..3 :: Int]]
  where f n = Vec3 (a n 0) (a n 1) (a n 2)
        a n x = if ((i + x) `mod` 3) == 2
                 then if ((i + x) `div` 3) `mod` 2 == 0 then 1.2 else -1.2
                 else if testBit n ((i + x) `mod` 3) then 1 else -1


data Type = Kite | Dart deriving Show

data Half = Half Type Proj3

deflate :: Half -> [Half]
deflate (Half Kite m) = [Half t (foldr1 (.*.) l .*. m) | (t, l) <- [
  (Dart, [translation $ Vec2 (-1) 0,
          linear $ rotMatrix2 (-4/5 * pi),
          scaling $ Vec2 (2 - phi) (2 - phi)]),
  (Kite, [linear $ rotMatrix2 (-3/5 * pi),
          scaling $ Vec2 (phi - 1) (phi - 1),
          translation $ rotate2 (1/5 * pi) (Vec2 1 0)]),
  (Kite, [scaling $ Vec2 (1 - phi) (phi - 1),
          linear $ rotMatrix2 (2/5 * pi),
          translation $ rotate2 (1/5 * pi) (Vec2 1 0)])
  ]]
deflate (Half Dart m) = [Half t (foldr1 (.*.) l .*. m) | (t, l) <- [
  (Dart, [translation $ rotate2 (-2/5 * pi) (Vec2 1 0),
          linear $ rotMatrix2 (4/5 * pi),
          scaling $ Vec2 (phi - 1) (phi - 1)]),
  (Kite, [scaling $ Vec2 (-1) 1,
          translation $ Vec2 1 0])
  ]]

realise :: Half -> (Type, [Vec2])
realise (Half t m) = (t, [
 (trim $ (extendWith 1 x :: Vec3) .* (fromProjective m) :: Vec2)  | x <- [
  Vec2 0 0,
  rotate2 a (Vec2 1 0),
  Vec2 1 0
 ]])
 where a = case t of Kite -> (pi/5)
                     Dart -> (3 * pi/5)

strokeColor = Color4 0.5 0.5 1 1
kiteColor   = Color4 0.8 0.8 1 1
dartColor   = Color4 0.7 0.7 1 1

drawTile :: Half -> IO ()
drawTile t@(Half Kite m) = drawTri t kiteColor
drawTile t@(Half Dart m) = drawTri t dartColor

drawTri :: Half -> Color4 GLfloat -> IO ()
drawTri tile color = do
  let vertices = mapM_ vertex $ (snd . realise) tile
  currentColor $= color
  renderPrimitive Triangles vertices
  currentColor $= strokeColor
  renderPrimitive LineStrip vertices

drawPts3 n buf = drawRangeElements Lines (0, n) 3 Float buf

up = Vec3 0 1 0

glvt3 :: Vec3 -> Vertex3 GLdouble
glvt3 (Vec3 x y z) = fmap glflt (Vertex3 x y z)

glvc3 :: Vec3 -> Vector3 GLdouble
glvc3 (Vec3 x y z) = fmap glflt (Vector3 x y z)


kite = [Half Kite $ scaling $ Vec2 1 (-1),
              Half Kite one]
sun = [Half Kite (m .*. linear (rotMatrix2 $ n * 2/5 * pi)) |
             Half Kite m <- kite,
             n <- [0..4]]


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
  lookAt (glvt3 $ cameraPos state) (glvt3 zero) (glvc3 up)
  let tiles = foldl (\l n -> concatMap deflate l) sun [1 .. deflations state]
  mapM_ drawTile tiles


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
  when idleAnimation $ drawFrame stateRef

onKeypress :: IORef FrameState -> KeyboardMouseCallback
onKeypress stateRef key keyState modifiers position = do
  oldState <- readIORef stateRef

  let newState =
        case (key, keyState) of
          (Char 'k', Down) -> Just oldState { deflations = deflations oldState + 1 }
          (Char 'j', Down) -> Just oldState { deflations = deflations oldState - 1 }
          otherwise -> Nothing
  when (isJust newState) $
       writeIORef stateRef $ fromJust newState
  when (not idleAnimation) $ drawFrame stateRef

onClose :: IO ()
onClose = do
  return ()

initialState :: FrameState
initialState = FrameState {
  bgColor = Color4 0.4 0.4 1.0 1.0,
  cameraPos = Vec3 0 0 1,
  cubeAngle = 0,
  deflations = 3
}

think :: FrameState -> FrameState
think oldState = oldState { cubeAngle = cubeAngle oldState + 0.1 }

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
