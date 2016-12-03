import Graphics.UI.GLUT
import Data.IORef
import Bindings
import Graphics.UI.GLUT.Objects
import System.Random

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _window <- createWindow "Graph 3D"
  windowSize $= Size 1000 1000
  reshapeCallback $= Just reshape
  depthFunc $= Just Less -- the comparison function for depth the buffer
  -- Some state variables (I know it feels BAD)
  angle   <- newIORef ((35,0)::(GLfloat,GLfloat))
  zoom    <- newIORef (1::GLfloat)
  campos  <- newIORef ((0.7,0)::(GLfloat,GLfloat))
  rand1 <- randomIO :: IO Int
  rand2 <- randomIO :: IO Int
  keyboardMouseCallback $= Just (keyboardMouse angle zoom campos)
  idleCallback $= Just idle
  displayCallback $= display angle zoom campos rand1 rand2
  mainLoop