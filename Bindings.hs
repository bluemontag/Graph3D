module Bindings (idle, display, reshape, keyboardMouse) where
 
import Graphics.UI.GLUT
import Data.IORef
import Display
 
reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

idle :: IdleCallback
idle = postRedisplay Nothing

keyboardMouse :: IORef (GLfloat, GLfloat) -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse angle zoom campos key Down _ _ = case key of
    (Char 'p') -> modVar angle (mapFst (+0.5))
    (Char 'l') -> modVar angle (mapFst (+(-0.5)))
    (Char 'j') -> modVar angle (mapSnd (+0.5))
    (Char 'k') -> modVar angle (mapSnd (+(-0.5)))
    -- use o and i to zoom
    (Char '+') -> modVar zoom (*1.1)
    (Char '-') -> modVar zoom (*0.9)
    -- use arrow keys to move the camera
    (SpecialKey KeyLeft ) -> modVar campos (mapFst (+0.1))
    (SpecialKey KeyRight) -> modVar campos (mapFst (+(-0.1)))
    (SpecialKey KeyDown ) -> modVar campos (mapSnd (+0.1))
    (SpecialKey KeyUp   ) -> modVar campos (mapSnd (+(-0.1)))
    -- any other keys does nothing
    _ -> return ()
keyboardMouse _ _ _ _ _ _ _ = return ()

modVar v f = do
  v' <- get v
  v $= (f v')
mapFst f (x,y) = (f x, y)
mapSnd f (x,y) = (x  , f y)

