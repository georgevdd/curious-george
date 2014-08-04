module Main where
import Control.Monad (when)
import Graphics.PDF
import Graphics.UI.GLUT (Color4(..), GLfloat)
import Data.Vect.Double

import Penrosary hiding (main, strokeColor, drawTile, drawTri)
import qualified Penrosary as P

pointsPerMm = 72.0 / 25.4

pdfColor :: Color4 GLfloat -> Color
pdfColor (Color4 r g b _) = Rgb (realToFrac r) (realToFrac g) (realToFrac b)

main = do
    let rect = PDFRect 0 0 (ceiling $ pointsPerMm * 74.0) (ceiling $ pointsPerMm * 32.0)
    runPdf "penrosary.pdf" (standardDocInfo { author=toPDFString "georgevdd", compressed = False}) rect $ do
        myDocument

myDocument :: PDF ()
myDocument = do
    page1 <- addPage Nothing
    createPageContent page1

drawTile :: Half -> Draw ()
drawTile t@(Half Kite m) = drawTri t kiteColor
drawTile t@(Half Dart m) = drawTri t dartColor

drawTri :: Half -> Color4 GLfloat -> Draw ()
drawTri tile@(Half t m) color = when (shouldDrawAt m) $ do
  let vertices = map vertex $ (snd . realise) tile
      vertex (Vec3 x y _) = (30 * x :+ 30 * y)
  fillColor $ pdfColor color
  fill $ Polygon vertices
  strokeColor $ pdfColor P.strokeColor
  stroke $ Polygon vertices
  --currentColor $= color
  --renderPrimitive Triangles vertices
  --currentColor $= strokeColor
  --renderPrimitive LineStrip vertices


createPageContent :: PDFReference PDFPage -> PDF ()
createPageContent page = drawWithPage page $ do
    applyMatrix $ scale pointsPerMm pointsPerMm
    strokeColor red
    setWidth 0.25
    let tiles = clipDeflate [] sun' 8
        sun' = [Half Kite (m .*. linear (rotMatrix2 0)) | Half Kite m <- sun]
    mapM_ drawTile $ fst tiles ++ snd tiles
