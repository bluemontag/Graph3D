module DataGraph where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Graph
import System.Random

graph1 = buildG (1,10) [(1,2), 
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

points3D :: Graph -> [Float] -> [Float] -> [(GLfloat,GLfloat,GLfloat)]
points3D graph randList1 randList2 = [(fromIntegral x, (randList1!!x)*10, (randList2!!x)*10) | x <- (vertices graph) ]

takeRand n g = (randomRs (0::GLfloat, 1::GLfloat) g)!!n 

--points :: StdGen -> Int -> [(GLfloat,GLfloat,GLfloat)]
--points g n = [ (x,y,z) | x <- [0, takeRand n g, 1], y <- [0, takeRand (n+1) g, 1], z<-[0, takeRand (n+2) g, 1] ]

myPoints r1 r2 = points3D graph1 (randomList r1) (randomList r2)

myEdges r1 r2 = let
                   randList1 = randomList r1
                   randList2 = randomList r2
                in [ ((fromIntegral v1, (randList1!!v1)*10, (randList2!!v1)*10), 
                     (fromIntegral v2, (randList1!!v2)*10, (randList2!!v2)*10)) | (v1,v2) <- edges graph1 ]

randomList :: Int -> [Float]
randomList seed = randoms (mkStdGen seed) :: [Float]
