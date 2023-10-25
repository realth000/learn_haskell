module Chapter9.IO (runChapter9) where

runIOActionBinding = do
  putStrLn "Input a number:"
  name <- getLine
  putStrLn ("Number is " ++ name)

runIOEcho = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn line
      runIOEcho

runChapter9 :: IO ()
runChapter9 = runIOEcho
