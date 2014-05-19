module Main where
import Control.Monad (when)
import Data.Maybe
import Data.Either
import Data.Word (Word32, Word8)
import Graphics.UI.GLUT hiding (Inside, Outside)
import Data.IORef
import Data.Hash

import Data.Vect.Double
import Data.Vect.Double.OpenGL

unitVector :: Int -> Vec3
unitVector 0 = vec3X
unitVector 1 = vec3Y
unitVector 2 = vec3Z

type ClearColor = Color4 GLclampf
type Scalar = Double


color3f :: Float -> Float -> Float -> Color3 Float
color3f = Color3

data FrameState = FrameState {
  bgColor :: ClearColor,
  cameraPos :: Vec3,
  fsAngle :: Scalar,
  deflations :: Integer
} deriving Show

initialState :: FrameState
initialState = FrameState {
  bgColor = Color4 1.0 0.4 0.4 1.0,
  cameraPos = Vec3 0 0 1,
  fsAngle = 0,
  deflations = 1
}


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

phi :: Floating a => a
phi = (1 + sqrt 5) / 2

data Type = Kite | Dart deriving Show

data Half = Half Type Proj3 deriving Show

deflate1 :: Half -> [Half]
deflate1 (Half Kite m) = [Half t (foldr1 (.*.) l .*. m) | (t, l) <- [
  (Dart, [translation $ Vec2 (-1) 0,
          linear $ rotMatrix2 (-4/5 * pi),
          scaling $ Vec2 (2 - phi) (2 - phi)]),
  (Kite, [linear $ rotMatrix2 (-3/5 * pi),
          scaling $ Vec2 (phi - 1) (phi - 1),
          translation $ rotate2 (1/5 * pi) (Vec2 1 0)]),
  (Kite, [scaling $ Vec2 (phi - 1) (phi - 1),
          linear $ rotMatrix2 (3/5 * pi),
          translation $ rotate2 ((-1)/5 * pi) (Vec2 1 0)])
  ]]
deflate1 (Half Dart m) = [Half t (foldr1 (.*.) l .*. m) | (t, l) <- [
  (Dart, [translation $ rotate2 (-2/5 * pi) (Vec2 1 0),
          linear $ rotMatrix2 (4/5 * pi),
          scaling $ Vec2 (phi - 1) (phi - 1)]),
  (Kite, [linear $ rotMatrix2 pi,
          translation $ Vec2 1 0])
  ]]

deflate = concatMap deflate1

type HalfSpace = (Vec3, Scalar)
type Bounds = [HalfSpace]

data ClipResult = Outside | Across | Inside deriving Eq
clipHalf :: Bounds -> Half -> ClipResult
clipHalf b h =
  let corners = [clipPoint b p | p <- snd $ realise h]
  in if all (/= Outside) corners
     then Inside
     else if all (/= Inside) corners
          then Outside
          else Across

clipPoint :: Bounds -> Vec3 -> ClipResult
clipPoint b x =
  let ds = [(n &. x) - d | (n, d) <- b]
  in case ds of
     _ | any (<0) ds -> Outside
     _ | all (>0) ds -> Inside
     otherwise -> Across

realise :: Half -> (Type, [Vec3])
realise (Half t m) = (t, [
 extendZero (project x m) | x <- [
  Vec2 1 0,
  rotate2 a (Vec2 1 0),
  Vec2 0 0,
  Vec2 0 0,
  rotate2 (-a) (Vec2 1 0),
  Vec2 1 0
 ]])
 where a = case t of Kite -> (pi/5)
                     Dart -> (3 * pi/5)
       project :: Vec2 -> Proj3 -> Vec2
       project x m = trim $ (extendWith 1 x :: Vec3) .* fromProjective m

-- strokeColor = Color4 0.5 0.5 1 1
-- kiteColor   = Color4 0.8 0.8 1 1
-- dartColor   = Color4 0.7 0.7 1 1
strokeColor = Color4 1 0.5 0.5 1
kiteColor   = Color4 1 0.8 0.8 1
dartColor   = Color4 1 0.7 0.7 1

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


kite = [Half Kite one]
sun = [Half Kite (m .*. linear (rotMatrix2 $ n * 2/5 * pi)) |
             Half Kite m <- kite,
             n <- [0..4]]


drawFrame :: IORef FrameState -> IO ()
drawFrame stateRef = do
  state <- readIORef stateRef
  drawScene state
  swapBuffers


clipHalf' :: Bounds -> Half -> Maybe (Either Half Half)
clipHalf' b h =
  case clipHalf b h of
  Inside -> Just $ Left h
  Outside -> Nothing
  Across -> Just $ Right h

-- | Give bounds and some halves, deflate the halves n times.
--   Return the resulting halves that are entirely within the bounds,
--   and the resulting halves that are on the bounds.
clipDeflate b hs 0 = ([], hs)
clipDeflate b hs n = let (ins', ons') = partitionEithers $ mapMaybe (clipHalf' b) hs
                         (x, y) = clipDeflate b (deflate ons') (n-1)
                     in (foldl (\h _ -> deflate h) ins' [1..n] ++ x, y)

drawScene :: FrameState -> IO ()
drawScene state = do
  clearColor $= bgColor state
  clear [ColorBuffer, DepthBuffer]

  currentColor $= Color4 1 1 0 1

  matrixMode $= Modelview 0
  loadIdentity
  lookAt (glvt3 $ cameraPos state) (glvt3 zero) (glvc3 up)
  cullFace $= Just Back
  --glRotate (fsAngle state) vec3Z
  let tiles = clipDeflate b sun' (deflations state)
      sun' = [Half Kite (m .*. linear (rotMatrix2 (fsAngle state))) | Half Kite m <- sun]
      b = [(Vec3 (sin $ a + a') (cos $ a + a') 0, (-0.8)) | a' <- [0, pi/2 .. 2*pi]]
      a = 0 -- pi/6 -- fsAngle state
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
think oldState = oldState -- { fsAngle = fsAngle oldState + 0.01 }


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
