module Main where

main :: IO ()
main = do
  putStr "Trying something out\n"
  putStr "Tell me something\n"
  filename <- getLine
  -- Haskell function application associates to the left. The $ function associates
  -- to the right and saves us a good number of brackes. We want to print the result
  -- of the expression on the right handside of the $
  putStr $  "Attempting to open " ++ filename ++ "\n"
  contents <- readFile filename
  putStr $ "There are " ++ nbOfLines contents ++ " lines in the file\n"
  putStr contents
  return ()

-- Here we are declaring the type of the function nbOfLines
-- It takes a String and returns a String
nbOfLines :: String -> String
nbOfLines = show.listSize.lines

-- Takes a list of any type and return an Int
listSize :: [a] -> Int
listSize [] = 0
listSize (_:xs) = 1 + (listSize xs)
