module Chapter9.IO (runChapter9) where

import Control.Monad
import GHC.IO.IOMode (IOMode(ReadMode))
import Control.Exception (handle)
import System.IO

runIOActionBinding = do
  putStrLn "Input a number:"
  name <- getLine 
  putStrLn ("Number is " ++ name)

runIOEcho = do
  putStrLn "Input number"
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn line
      runIOEcho

runGetChar = do
  c <- getChar
  when (c /= 'q') $ do
    putChar c
    runGetChar

runGet3Char = do
  putStrLn "Input 3 chars"
  c3 <- sequence [getLine, getLine, getLine]
  print c3

runForever = forever $ do
  putStr "Give me some input: "
  l <- getLine
  putStrLn l

runReadFile = do
  putStrLn "Input file name:"
  name <- getLine
  withFile name ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)

runChapter9 :: IO ()
runChapter9 = runReadFile
