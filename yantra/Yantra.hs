module Main where
import Data.Bits (testBit)
import Codec.Binary.Gray (gray)
import Control.Monad (when)
import Data.List
import Data.Maybe
import Data.Word (Word32, Word8)
import Graphics.UI.GLUT hiding (Line, normalize)
import Data.IORef

import Data.Vect.Double
import Data.Vect.Double.OpenGL

import Debug.Trace

unitVector :: Int -> Vec3
unitVector 0 = Vec3 1 0 0
unitVector 1 = Vec3 0 1 0
unitVector 2 = Vec3 0 0 1

type ClearColor = Color4 GLclampf
type Scalar = Double

color3f :: Float -> Float -> Float -> Color3 Float
color3f = Color3

type Line = (Vec2, Vec2)

lineLen :: Line -> Double
lineLen (a, b) = len (b &- a)

perp :: Vec2 -> Vec2
perp (Vec2 x y) = Vec2 (-y) x

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
intersectCircle ((Vec2 x y), (Vec2 x' y')) =
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
          in Just ((pointAt s), (pointAt s'))

-- Find the intersection of two lines
--
-- P(s) = (s*a'x + (1-s)*ax), (s*ay' + (1-s)*ay)
-- D(s) = (P(s) . perp(b' - b)) - (b . perp(b' - b))
-- D(s) = (s*a'x + (1-s)*ax)*nbx + (s*a'y + (1-s)*ay)*nby - (bx * nbx + by * nby)
-- D(s) = s((a'x - ax)*nbx + (a'y - ay)*nby) + ax*nbx + ay*nby - (bx * nbx + by * nby)
-- D(s) == 0
-- s = ( (bx * nbx + by * nby) - ax*nbx + ay*nby ) / ((a'x - ax)*nbx + (a'y - ay)*nby)
intersectLine :: Line -> Line -> Vec2
intersectLine (a, a') (b, b') =
  let da = a' &- a
      db = b' &- b
      nb = perp db
      s = ((b &- a) &. nb) / (da &. nb)
  in (s *& a') &+ ((1-s) *& a)


baseLine :: Double -> Line
baseLine y = let (_, p) = fromJust $ intersectCircle $ ((Vec2 (-1) y), (Vec2 1 y))
             in (Vec2 0 y, p)

drawCircle :: Double -> IO ()
drawCircle radius = do
  let vertices = mapM_ vertex
                       [Vec2 (radius * cos(2*pi*a)) (radius * sin(2*pi*a)) |
                        a <- [0, 1/64 .. 63/64]]
  renderPrimitive LineLoop vertices

drawPts3 n buf = drawRangeElements Lines (0, n) 3 Float buf

up = Vec3 0 1 0

glvt3 :: Vec3 -> Vertex3 GLdouble
glvt3 (Vec3 x y z) = fmap glflt (Vertex3 x y z)

glvc3 :: Vec3 -> Vector3 GLdouble
glvc3 (Vec3 x y z) = fmap glflt (Vector3 x y z)


drawFrame :: IORef FrameState -> IO ()
drawFrame stateRef = do
  state <- readIORef stateRef
  drawScene state
  swapBuffers



mirror (Vec2 x y) = (Vec2 (-x) y)

data Tri = Tri { triCorner :: Vec2, triTop :: Double }
triBase, triSide :: Tri -> Line
triSide t = (triCorner t, Vec2 0 (triTop t))
triBase t = (triCorner t, mirror $ triCorner t)
triBottom :: Tri -> Double
triBottom = _2 . triCorner

triangleBetween :: Line -> Line -> Line -> [Vec2]
triangleBetween a b c = [intersectLine x y |
                         (x, y) <- [(a, b), (b, c), (c, a)]]

triLines :: Tri -> [Vec2]
triLines t =
  let (l, r) = triBase t
      top = Vec2 0 (triTop t)
  in [l, top, r, top, l, r]

label :: Vec2 -> String -> IO ()
label p s = preservingMatrix $ do
  glTranslate (extendZero p)
  glScale (1/3000)
  renderString MonoRoman s

data FrameState = FrameState {
  bgColor :: ClearColor,
  cameraPos :: Vec3,
  fsYs :: [Double],
  cs :: ComputedState
}

data ComputedState = ComputedState {
  csTris :: [(Tri, String)],
  csBindu :: Vec2,
  csConstructionPts :: [(Vec2, String)],
  csSquerrors :: [(String, Double)]
}

initialState :: FrameState
initialState = FrameState {
  bgColor = Color4 0.8 0.8 1.0 1.0,
  cameraPos = Vec3 0 0 1,
  fsYs = [
    0.3001145060079917,
    -0.30456468284635857,
    0.8358218564619556,
    -0.8757793496076738,
    -0.11504699897999972],
  cs = computeTris $ fsYs initialState
}

computeTris :: [Double] -> ComputedState
computeTris ys =

  let [y1, y2, y3, y4, y5] = ys

      centreLine@(bottom, top) = ((Vec2 0 (-1)), (Vec2 0 1))
      t2 = Tri (snd $ baseLine $ y1) (-1)
      t2' = Tri (snd $ baseLine $ y2) 1

      p4 = intersectLine (triBase t2) (triSide t2')
      l4 = (Vec2 0 y4, p4)
      p4' = intersectLine (triBase t2') (triSide t2)
      l4' = (Vec2 0 y3, p4')

      p5 = intersectLine l4 (baseLine y2)
      l5 = (Vec2 0 y1, p5)
      t5 = Tri (intersectLine l5 (baseLine y4)) y1

      p6 = intersectLine (baseLine y5) l4
      p6' = intersectLine l5 (triSide t2)
      t6 = Tri (intersectLine (p6', mirror p6') l4') y3

      p7 = intersectLine (baseLine y1) l4'
      l7 = (Vec2 0 y5, p7)
      t7 = Tri (intersectLine l7 (baseLine y3)) y5

      p8 = intersectLine l7 (triSide t2')
      t8 = Tri (intersectLine (p8, mirror p8) l4) y4

      t9 = Tri p6 (_2 p8)

      p10 = intersectLine (triSide t9) l7

      t11 = Tri (intersectLine (p10, mirror p10) l4') (triBottom t6)
      p11 = intersectLine (baseLine y5) (triSide t11)

      p12 = intersectLine l7 l5

      t13 = Tri (intersectLine (p12, mirror p12) (triSide t9)) y2

-- Find the incentre of an equilateral triangle
-- a is corner; b is top
-- bx == 0
-- ay - y == D(0, y)
-- D(p) = (a - p) . norm(perp(b-a))
-- D(0, y) = (ax, ay - y) . (ay - by, bx - ax) / l
--         = (ax, ay - y) . (ay - by, -ax) / l
--         = (ax*(ay-by) + (ay-y)*(-ax)) / l
--         = (ax*(ay-by) + -y*(-ax) + ay*(-ax)) / l
-- l*(ay - y) = ax*(ay-by) + -y*(-ax) + ay*(-ax)
-- y*(-ax - l) = ax*(ay-by) + ay*(-ax) - ay*l
-- y = (ax*(ay-by) + ay*(-ax) - ay*l)/(-ax - l)
--   = (ax*ay - ax*by - ay*ax - ay*l)/(-ax - l)
--   = (- ax*by - ay*l)/(-ax - l)
--   = (ax*by + ay*l)/(ax + l)
      bindu = Vec2 0 ((ax*by + ay*l)/(ax + l))
        where (Vec2 ax ay) = p12
              l = len (p12 &- Vec2 0 by)
              by = y5

      allTris = [
        (t2, "t2"),
        (t2', "t2'"),
        (t5, "t5"),
        (t6, "t6"),
        (t7, "t7"),
        (t8, "t8"),
        (t9, "t9"),
        (t11, "t11"),
        (t13, "t13")]
      allConstructionPoints = [
        (p4, "p4"),
        (p4', "p4'"),
        (p5, "p5"),
        (p6, "p6"),
        (p6', "p6'"),
        (p7, "p7"),
        (p8, "p8"),
        (p10, "p10"),
        (p11, "p11"),
        (p12, "p12")]

      p11_error = distPointLine p11 l5
      distPointLine p (a, b) = (p &. perp (a &- b)) -
                               (a &. perp (a &- b))

      squerrors = [
                  ("bindu", lensqr bindu),
                  ("p11", p11_error ** 2),
                  ("t5", (1 - (len $ triCorner t5))**2),
                  ("t7", (1 - (len $ triCorner t7))**2),
                  ("equi", (lineLen (triSide t7) - lineLen (triBase t7))**2)
                  ]

  in ComputedState {
       csTris = allTris,
       csConstructionPts = allConstructionPoints,
       csBindu = bindu,
       csSquerrors = squerrors
       }

epsilon :: Double
epsilon = 0.0000001

think :: FrameState -> FrameState
think oldState =
  let ys = fsYs oldState
      e = sum $ map snd $ csSquerrors $ cs oldState
      probePts = [h ++ (head t + epsilon):(tail t) |
                  (h, t) <- zip (inits ys) (filter (not . null) $ tails ys)]
      e's = [sum $ map snd $ csSquerrors $ computeTris pt | pt <- probePts]
      des = [e' - e | e' <- e's]
      lde = sqrt $ sum [de*de | de <- des]
      newYs = [y - epsilon * de / lde | (y, de) <- zip ys des]
--      x = traceShow (e's) epsilon
--      newYs = (head ys + x) : tail ys
  in oldState { fsYs = newYs, cs = computeTris newYs }

drawScene :: FrameState -> IO ()
drawScene state = do
  clearColor $= bgColor state
  clear [ColorBuffer, DepthBuffer]

  matrixMode $= Modelview 0
  loadIdentity
  lookAt (glvt3 $ cameraPos state) (glvt3 zero) (glvc3 up)

  let (ComputedState allTris' bindu allConstructionPts squerrors) = cs state
      allTris = map fst allTris'
      [t1, t2, t3, t4, t5, t6, t7, t8, t9] = allTris

  currentColor $= Color4 0.6 0.6 0.8 1
  let sideTris = [
        triangleBetween (triSide t2) (triBase t1) (triSide t1),
        triangleBetween (triSide t2) (triBase t6) (triSide t6),
        triangleBetween (triSide t2) (triBase t5) (triSide t5),

        triangleBetween (triSide t1) (triBase t2) (triSide t2),
        triangleBetween (triSide t1) (triBase t4) (triSide t4),
        triangleBetween (triSide t1) (triBase t3) (triSide t3),

        triangleBetween (triBase t4) (triSide t3) (triSide t6),
        triangleBetween (triBase t2) (triSide t4) (triSide t6),

        triangleBetween (triBase t1) (triSide t4) (triSide t6),
        triangleBetween (triBase t6) (triSide t4) (triSide t5),

        triangleBetween (triSide t7) (triBase t1) (triSide t5),
        triangleBetween (triSide t7) (triBase t8) (triSide t8),

        triangleBetween (triSide t8) (triBase t2) (triSide t3),
        triangleBetween (triSide t8) (triBase t7) (triSide t7),

        triangleBetween (triSide t3) (triBase t8) (triSide t5),
        triangleBetween (triSide t3) (triBase t9) (triSide t9),

        triangleBetween (triSide t9) (triBase t7) (triSide t3)
        ]
      centreTris = map mkCentreTri [
        (t3, t1),
        (t4, t6),
        (t2, t8),
        (t7, t9),
        (t9, t5),
        (t8, t3),
        (t1, t7),
        (t6, t4),
        (t5, t2)]
      mkCentreTri (b, s) = triangleBetween (triBase b) (triSide s) (mirror $ fst $ triSide s, mirror $ snd $ triSide s)
  renderPrimitive Triangles $ mapM_ vertex $ concat $ centreTris ++ sideTris ++ map (map mirror) sideTris

  currentColor $= Color4 0 0 0.8 1
  drawCircle 1
  renderPrimitive Lines $ mapM_ vertex $ concatMap triLines allTris

  currentColor $= Color4 1 1 1 1
  preservingMatrix $ do
    glTranslate (extendZero bindu)
    drawCircle (_2 bindu - _2 (fst $ last allConstructionPts))

  currentColor $= Color4 1 0 0 1
  label (Vec2 0 0) "Centre"
  mapM_ (uncurry label) $
   [(Vec2 0 y, "y" ++ show s) | (y, s) <- zip (fsYs state) [1..]] ++
   allConstructionPts

  sequence_ [label (Vec2 (-1) (-1 + 0.015 + 0.06 * l)) (m ++ ": " ++ show (e*10000)) |
             (l, (m, e)) <- zip [0..] squerrors]

  sequence_ [label (mirror $ triCorner t) ("t" ++ show l) | (t, l) <- zip allTris [1..]]

  renderPrimitive Lines $ sequence_ [vertex v | v <-
    [zero, bindu] ++
    concat [[mirror t, normalize $ mirror t] |
            t <- [triCorner (allTris!!2), triCorner (allTris!!4)]]]


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

  case (key, keyState) of
    (Char 'p', Down) -> print $ fsYs oldState
    otherwise -> return ()

  -- let newState =
  --       case (key, keyState) of
  --         (Char 'k', Down) -> Just oldState
  --         (Char 'j', Down) -> Just oldState
  --         otherwise -> Nothing
  -- when (isJust newState) $
  --      writeIORef stateRef $ fromJust newState
  -- when (not idleAnimation) $ drawFrame stateRef

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
