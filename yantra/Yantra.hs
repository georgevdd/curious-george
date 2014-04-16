module Main where
import Data.Bits (testBit)
import Codec.Binary.Gray (gray)
import Control.Monad (when)
import Data.Maybe
import Data.Word (Word32, Word8)
import Graphics.UI.GLUT hiding (Line)
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
  yA :: Double
} deriving Show

initialState :: FrameState
initialState = FrameState {
  bgColor = Color4 0.8 0.8 1.0 1.0,
  cameraPos = Vec3 0 0 1,
  yA = 0.25
}

data Line = Line Vec2 Vec2 deriving Show

-- Find the intersections (if any) of a line with the unit circle.
--
-- P(s) = (s*x' + (1-s)*x), (s*y' + (1-s)*y)
-- D(s)^2 = (s*x' + (1-s)*x)^2 + (s*y' + (1-s)*y)^2
-- D(s)^2 = x'^2*s^2 + 2s(1-s)(x'*x) + x^2*(1-s)^2 + y'^2*s^2 + 2s(1-s)(y'*y) + y^2*(1-s)^2
-- D(s)^2 = s^2(x'^2 -2*x'*x + x^2 + y'^2 -2*y'*y + y^2) + s(2*x'*x -2*x^2 + 2*y'*y -2*y^2) + (x^2 + y^2)
-- D(s)^2 = s^2((x'-x)^2 + (y'-y)^2) + 2s(x'*x - x^2 + y'*y -y^2) + (x^2 + y^2)
-- D(s)^2 == 1
-- D(s)^2 - 1 == 0
intersectCircle :: Line -> Maybe Line
intersectCircle (Line (Vec2 x y) (Vec2 x' y')) =
  let a = (x'-x)^2 + (y'-y)^2
      b = 2*(x'*x - x^2 + y'*y -y^2)
      c = (x^2 + y^2) - 1
      r = sqrt(b*b - 4*a*c)
  in if isNaN r
     then Nothing
     else let q r = (-b + r)/(2*a)
              s   = q (-r)
              s'  = q (r)
              pointAt s = Vec2 (s*x' + (1-s)*x) (s*y' + (1-s)*y)
          in Just (Line (pointAt s) (pointAt s'))


baseLine :: Double -> Line
baseLine y = fromJust $ intersectCircle $ Line (Vec2 (-1) y) (Vec2 1 y)



data Type = Kite | Dart deriving Show

data Half = Half Type Proj3

phi = 0
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

drawCircle :: Double -> IO ()
drawCircle radius = do
  let vertices = mapM_ vertex
                       [Vec2 (radius * cos(2*pi*a)) (radius * sin(2*pi*a)) |
                        a <- [0, 1/32 .. 1]]
  renderPrimitive LineLoop vertices
  renderPrimitive Lines $ mapM_ vertex $ concat [[s, s'] | (Line s s') <- map baseLine [0, 0.1 .. 1]]

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

  matrixMode $= Modelview 0
  loadIdentity
  lookAt (glvt3 $ cameraPos state) (glvt3 zero) (glvc3 up)

  currentColor $= Color4 0 0 0.8 1
  drawCircle 1


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
          (Char 'k', Down) -> Just oldState
          (Char 'j', Down) -> Just oldState
          otherwise -> Nothing
  when (isJust newState) $
       writeIORef stateRef $ fromJust newState
  when (not idleAnimation) $ drawFrame stateRef

onClose :: IO ()
onClose = do
  return ()

think :: FrameState -> FrameState
think oldState = oldState

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
