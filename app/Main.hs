module Main where

import RIO

import Language.ZMachine.App
import qualified Language.ZMachine.Memory as M
import qualified Language.ZMachine.Dictionary as D
import System.Environment (getArgs)

import qualified RIO.List as L
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL


main :: IO ()
main = do
  logOptions' <- logOptionsHandle stdout False
  withLogFunc logOptions' $ \logFunc -> do
    args <- liftIO $ getArgs
    let mStoryFile = L.headMaybe args
    case mStoryFile of
      Nothing -> B.putStr "No story file specified\n"
      Just story -> do file <- BL.readFile story
                       let app = App { appLogger = logFunc
                                     , story = BL.toStrict file
                                     }
                       runRIO app dump

dump :: RIO App ()
dump = do env <- ask
          logInfo $ M.showHeader (M.getHeader env)
          logInfo $ D.showDictionary (D.dictionary env)
