module Main where

import RIO

import qualified Language.ZMachine.Memory as M
import qualified Language.ZMachine.Dictionary as D
import System.Environment (getArgs)

import qualified RIO.List as L
import qualified RIO.ByteString as B
import qualified RIO.Text as T

main :: IO ()
main = runSimpleApp $ do
  args <- liftIO $ getArgs
  let mStoryFile = L.headMaybe args
  case mStoryFile of
    Nothing -> B.putStr "No storyfile specified\n"
    Just storyFile -> do
      storyH <- liftIO $ M.new storyFile
      B.putStr . T.encodeUtf8 $ M.showHeader (M.getHeader storyH)
      B.putStr . T.encodeUtf8 $ D.showDictionary (D.dictionary storyH)
