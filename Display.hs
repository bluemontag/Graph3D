module Display (display) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import System.Random
import DataGraph

display ::  IORef (GLfloat, GLfloat) -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> Int -> Int -> DisplayCallback
display angle zoom position r1 r2 = do
   -- set the background color (dark solarized theme)
  clearColor $= Color4 0 0.1686 0.2117 1
  clear [ColorBuffer, DepthBuffer]
  -- Transformation to change the view
  loadIdentity -- reset any transformation
  -- tranlate
  (x,y) <- get position
  translate $ Vector3 x y 0
  -- zoom
  z <- get zoom
  scale z z z
  -- rotate
  (xangle,yangle) <- get angle
  rotate xangle $ Vector3 1.0 0.0 (0.0::GLfloat)
  rotate yangle $ Vector3 0.0 1.0 (0.0::GLfloat)
  g <- getStdGen
  -- Now that all transformation were made
  -- We create the object(s)
  forM_ (myPoints r1 r2) $ \(x,y,z) -> preservingMatrix $ drawVertex x y z
  --randOrder $= ((n+3)::Int)
  renderPrimitive Lines $ mapM_ drawEdge (myEdges r1 r2)
  swapBuffers -- refresh screen

drawVertex x y z = do
  translate $ Vector3 x y z
  color3f x y z
  renderObject Solid $ Sphere' 0.05 20 20

drawEdge ((x1, y1, z1), (x2, y2, z2)) = do
  vertex3f x1 y1 z1
  vertex3f x2 y2 z2


color3f r g b = color $ Color3 r g (b :: GLfloat)

vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)

--old functions

zipPairs :: [a] -> [(a,a)]
zipPairs list = zipPairs' (length list) list
zipPairs' n (x:xs) = case n of
                       0 -> []
                       1 -> []
                       m -> (x, head xs): zipPairs' (n-1) xs
zipPairs' n [] = []


cartProd :: Eq a => [a] -> [(a,a)]
cartProd list = [ (x,y) | x <- list, y <- list, y /= x ]



colorFromValue n =
  let
      t i = 0.7 + 0.3*cos( i / 10 )
  in
    color3f (t n) (t (n+5)) (t (n+10))

