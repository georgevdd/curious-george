module Main where
import Control.Monad (when)
import Data.List (iterate, partition)
import Data.Maybe
import Graphics.UI.GLUT hiding (Inside, Outside)
import Data.IORef
import Data.Array

import Debug.Trace

import Data.Vect.Double
import Data.Vect.Double.OpenGL

bezier :: (Vector v) => Double -> v -> v -> v -> v -> v
bezier s p0 p1 p2 p3 =
       (z*z*z) *& p0 &+
     (3*z*z*s) *& p1 &+
     (3*z*s*s) *& p2 &+
       (s*s*s) *& p3
  where z = 1-s

curve = [bezier s p0 p1 p2 p3 | s <- [0, 0.05 .. 1.0]]
  where p0 = Vec2 0 0
        p1 = Vec2 0 0.5
        p2 = Vec2 1 0.5
        p3 = Vec2 1 1

curveX = listArray (0, length curve - 1) [_1 p | p <- curve]
curveY = listArray (0, length curve - 1) [_2 p | p <- curve]

-- | Binary search in a sorted array
lowerBound :: (Ix i, Integral i, Ord e) => Array i e -> e -> i
lowerBound arr e = lowerBound' arr e begin (end'+1)
  where (begin, end') = bounds arr
lowerBound' arr e begin end =
  if begin == end then begin
  else let middle = (end + begin) `div` 2
           probe = arr!middle
       in if e <= probe
          then lowerBound' arr e begin middle
          else lowerBound' arr e (middle+1) end

curveAt y =
  let i = lowerBound curveY y
      (min, _) = bounds curveY
  in if i == min
     then Vec2 (curveX!i) (curveY!i)
     else let (x0, y0) = (curveX!(i-1), curveY!(i-1))
              (x1, y1) = (curveX!i    , curveY!i)
          in interpolate ((y-y0)/(y1-y0)) (Vec2 x0 y0) (Vec2 x1 y1)

type Year = Int

data Fish = Fish {
  fishBirth :: Year,
  fishDeath :: Year,
  fishSpans :: [(Vec2, Vec2)]
} deriving (Read, Show)

initialFish :: [Fish]
initialFish = replicate 25 $ Fish 0 40 []

type Shoal = (Year, [Fish], [Fish])

data FrameState = FrameState {
  fsShoal :: Shoal
} deriving Show

initialState :: FrameState
initialState = FrameState {
  fsShoal = (iterate evolve (0, initialFish, []))!!800
}

fishLifespan fish = fishDeath fish - fishBirth fish

fishWidth :: Year -> Fish -> Double
fishWidth year fish =
  let age = year - fishBirth fish
      lifespan = fishLifespan fish
      lifeDone = fromIntegral age / fromIntegral lifespan
  in _1 (if lifeDone <= 0.5
         then curveAt (lifeDone * 2)
         else curveAt ((1 - lifeDone) * 2))

evolve :: Shoal -> Shoal
evolve (year, liveFishes, deadFishes) = (year+1, liveFishes', deadFishes')
 where
  newAndOldFishes = arrangeInYear year $ (concatMap maybeSpawn . concatMap maybeSpawn) liveFishes
  maybeSpawn fish = if inBounds fish && year == (fishBirth fish + fishDeath fish) `div` 2
                    then if (year `div` (fishLifespan fish `div` 2) `mod` 2 == 0)
                         then [fish, Fish year (year + fishLifespan fish) []]
                         else [Fish year (year + fishLifespan fish) [], fish]
                    else [fish]
  (stillAliveFishes, newlyDeadFishes) = partition stillAlive newAndOldFishes
  stillAlive fish = fishDeath fish > year
  inBounds fish = null (fishSpans fish) || let (l, r) = head $ fishSpans fish in (_1 l < 20 && _1 r > -20)
  liveFishes' = stillAliveFishes
  deadFishes' = newlyDeadFishes ++ deadFishes

rowFor year = -1.2 + (fromIntegral year) / 200

arrangeInYear year fishes =
  let y = rowFor year
      widths = map (fishWidth year) fishes
      totalWidth = sum widths
      pts = [Vec2 ((x - totalWidth / 2 + adjustment) / 20) y |
                    x <- scanl (+) 0 widths]
      adjustment = (fishWidth ((year `div` 1) `mod` 40) (Fish 0 40 []))/2
      spans = zip pts (tail pts)
      addSpan fish span = fish { fishSpans = span : fishSpans fish }
  in [addSpan fish span | (fish, span) <- zip fishes spans]

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
  clearColor $= Color4 0.4 0.4 1.0 1.0
  --clear [ColorBuffer, DepthBuffer]

  matrixMode $= Modelview 0
  loadIdentity
  lookAt (glvt3 $ Vec3 0 0 1) (glvt3 zero) (glvc3 vec3Y)
  -- cullFace $= Just Back

  let (year, liveFishes, deadFishes) = fsShoal state
      fishes = liveFishes ++ deadFishes
      -- vs = [mapM_ vertex $ concat [[l, r] | (l, r) <- fishSpans fish] | fish <- fishes]
      vs = [mapM_ vertex line | line <- concat [let (a, b) = unzip $ fishSpans fish in [a, b] | fish <- fishes]]

  currentColor $= Color4 0 0 0.3 1
  lineWidth $= 2
  pointSize $= 4
  --renderPrimitive LineStrip vs
  mapM_ (renderPrimitive LineStrip) vs
  --mapM_ (renderPrimitive LineStrip) vs2
  --currentColor $= Color4 0 0 0 1
  --lineWidth $= 1
  --renderPrimitive LineStrip vs3

onKeypress :: IORef FrameState -> KeyboardMouseCallback
onKeypress stateRef key keyState modifiers position = return ()


think :: Size -> FrameState -> FrameState
think (Size ix iy) (FrameState shoal) = FrameState (evolve shoal)
  -- where
  --   (sx, sy) = (fromIntegral ix, fromIntegral iy)
  --   (pts', vels') = unzip $ map bounce $ zip pts vels
  --   bounce (Vec2 x y, Vec2 dx dy) = let (x', dx') = bounce' sx x dx
  --                                       (y', dy') = bounce' sy y dy
  --                                   in (Vec2 x' y', Vec2 dx' dy')
  --   bounce' sv v dv =
  --     let v' = v + dv
  --     in case v' of
  --        _ | v' < 0 -> (-v', (-dv))
  --        _ | v' >= sv -> (sv - (v' - sv), (-dv))
  --        otherwise -> (v', dv)


--  fsPts = [Vec2 0 (-1), Vec2 0 (-0.5), Vec2 0.5 (-0.5), Vec2 0.5 0],
-- y s = s*s*s*(-1) + 3*s*s*(1-s)*(-0.5) + 3*s*(1-s)*(1-s)*(-0.5)
y s -- = (-s*s*s) - 1.5*(s*s*s*(-1) + s*s + s - s*s - s*s + s*s*s)
    -- = (-s*s*s) - 1.5*(                   s - s*s              )
    = -(z*z*z) + 1.5*(z*z) - 1.5*z
  where z = 1-s

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
  (_, size) <- get viewport
  let newState = oldState -- think size oldState
  writeIORef stateRef newState
  when idleAnimation $ postRedisplay Nothing


onClose :: IO ()
onClose = do
  return ()

main :: IO ()
main = do
  initialWindowSize $= Size 315 851
  (progName, args) <- getArgsAndInitialize
  window <- createWindow progName
  stateRef <- newIORef initialState
  reshapeCallback $= Just onReshape
  displayCallback $= onDisplay stateRef
  idleCallback $= Just (onIdle stateRef)
  keyboardMouseCallback $= Just (onKeypress stateRef)
  clearColor $= Color4 0.4 0.4 1.0 1.0
  clear [ColorBuffer, DepthBuffer]
  mainLoop
