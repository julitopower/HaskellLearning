module OpenGlTest where

import Graphics.UI.GLUT

main:: IO()

-- main = do
--      (progName, args) <- getArgsAndInitialize
--      window <- createWindow "OMG this is too easy to be true"
--      displayCallback $= display
--      mainLoop

main = getArgsAndInitialize >>= (\(progName, args) -> 
       createWindow "Yet another" >>= (\window -> 
       displayCallback $= display >>= (\_ -> 
       mainLoop)))

-- main = getArgsAndInitialize >> 
--        createWindow "Another way" >> 
--        displayCallback $= display >> 
--        mainLoop

display :: DisplayCallback
display = clear [ColorBuffer] >>
	  flush

flipp :: (a -> b -> c) -> (b -> a -> c)
flipp  f = g
       where g y x = f x y

quicksort :: (Ord a) =>  [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	 quicksort low ++ x : quicksort high
	 where low = filter (<= x) xs
	       high = filter (> x) xs 
