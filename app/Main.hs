module Main where

import Data.Text as T
import Parser
import System.Environment

getInput :: IO String
getInput = do
    args <- getArgs
    pure $ parseArgs args
  where
    parseArgs ("-i":input:_) = input
    parseArgs (_:xs) = parseArgs xs
    parseArgs [] = error "Provide the input as a single argument."

main :: IO ()
main = do
  input <- getInput
  let ast = runParser programP "" (T.pack input)
  putStrLn $ show ast
