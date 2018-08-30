module Main where

import Lib
import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
import Data.Binary.Get (runGet)
import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  let storyFile = head args
  storyData <- BL.readFile storyFile
  let header = runGet parseHeader storyData
  -- putStrLn $ show header
  TIO.putStrLn $ showHeader header
