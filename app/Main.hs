module Main where

import           RIO

import           Language.ZMachine.App
import qualified Language.ZMachine.Memory      as M
import qualified Language.ZMachine.Dictionary  as D
import qualified Language.ZMachine.Object      as OB
import qualified Language.ZMachine.Abbreviations
                                               as A
import           System.Environment             ( getArgs )

import qualified RIO.List                      as L
import qualified RIO.ByteString                as B
import qualified RIO.ByteString.Lazy           as BL


main :: IO ()
main = do
    runApp dump
    return ()

-- TODO : refactor to aid debugging
runApp :: RIO App a -> IO (Maybe a)
runApp action = do
    logOptions' <- logOptionsHandle stdout False

    withLogFunc logOptions' $ \logFunc -> do
        args <- liftIO $ getArgs
        let mStoryFile = L.headMaybe args

        case mStoryFile of
            Nothing -> do
                B.putStr "No story file specified\n"
                pure Nothing
            Just storyPath -> do
                file <- BL.readFile storyPath
                let app = App { appLogger = logFunc, story = BL.toStrict file }
                Just <$> runRIO app action


dump :: RIO App ()
dump = do
    dict          <- D.getDictionary
    header        <- M.getHeader
    abbreviations <- A.getAbbreviationTable
    objects       <- OB.getObjects

    logInfo . display $ header
    logInfo . display $ dict
  -- logInfo . display $ objects
