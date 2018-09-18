module Main where

import qualified Lib.Memory as M
import qualified Lib.Dictionary as D
import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
import Data.Binary.Get (runGet)
import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  let storyFile = head args
  storyH <- M.new storyFile
  TIO.putStrLn $ M.showHeader (M.getHeader storyH)
  TIO.putStr $ D.showDictionary (D.dictionary storyH)
