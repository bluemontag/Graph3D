module DataGraph where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Graph
import System.Random

type GraphPoint = (GLfloat,GLfloat,GLfloat)


graph1 :: Graph
graph1 = buildG (0,10) [(1,2), 
                        (2,3), 
                        (3,1), (3,5), (3,4),
                        (4,1), (4,2), 
                        (5,4), 
                        (6,2), (6,5), 
                        (7,1), 
                        (8,2), (8,5), 
                        (9,3), 
                        (9,4), (9,5), 
                        (10,9), (10,8)]

points3D :: Graph -> Int -> Int -> [ GraphPoint ]
--before : points3D graph randList1 randList2 = [(fromIntegral x, (randList1!!x)*10, (randList2!!x)*10) | x <- (vertices graph) ]
points3D graph r1 r2 = [ getPoint r1 r2 v | v <- (vertices graph) ]

getPoint :: Int -> Int -> Data.Graph.Vertex -> GraphPoint
getPoint r1 r2 v = let
                     randList1 = randomList r1
                     randList2 = randomList r2
                   in ((fromIntegral v)/10, randList1!!v, randList2!!v)

myPoints :: Int -> Int -> [ GraphPoint ]
myPoints r1 r2 = points3D graph1 r1 r2

myEdges :: Int -> Int -> [ (GraphPoint, GraphPoint) ]
myEdges r1 r2 = [ (getPoint r1 r2 v1, getPoint r1 r2 v2) | (v1,v2) <- (edges graph1) ] ++ unitCube

origin = (0.0, 0.0, 0.0)

plusX (x, y, z) = (x+1, y, z)
plusY (x, y, z) = (x, y+1, z)
plusZ (x, y, z) = (x, y, z+1)

unitCube = [ (origin               ,     plusZ origin        ),    -- +z
             (plusZ origin         ,     plusX $ plusZ origin),    -- +x
             (plusX $ plusZ origin ,     plusX origin        ),    -- -z
             (plusX origin         ,     origin              ),    -- -x
             (origin               ,     plusX origin        ),    -- +x
             (plusX origin         ,     plusY $ plusX origin),    -- +y
             (plusY $ plusX origin ,     plusY origin        ),    -- -x
             (plusY origin         ,     origin              ),    -- -y
             (origin               ,     plusY origin        ),    -- +y
             (plusY origin         ,     plusZ $ plusY origin),    -- +z
             (plusZ $ plusY origin ,     plusZ origin        ),    -- -y
             (plusZ origin         ,     origin              )     -- -z
               ]      

randomList :: Int -> [Float]
randomList seed = randoms (mkStdGen seed) :: [Float]
