import Graphics.UI.GLUT

import Dodgy.Initialize

main :: IO ()
main = do
     (_progName, args) <- getArgsAndInitialize
     initDodgy args
