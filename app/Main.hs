module Main where

import qualified Language.ZMachine.Memory as M
import qualified Language.ZMachine.Dictionary as D
import System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  let storyFile = head args
  storyH <- M.new storyFile
  TIO.putStrLn $ M.showHeader (M.getHeader storyH)
  TIO.putStr $ D.showDictionary (D.dictionary storyH)
