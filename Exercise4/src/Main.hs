module Main where

import System.IO
import Ex4

main:: IO ()
main = do
  inputFile <- readFile "input.dat"
  let numbers = map read . lines $ inputFile :: [Integer]

  let result = zipWith ($) (cycle ops) numbers
  writeFile "output.dat" (unlines . map show $ result)