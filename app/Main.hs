module Main where

import           RIO

import Language.ZMachine.App
import qualified Language.ZMachine.Memory      as M
import qualified Language.ZMachine.Dictionary  as D
import qualified Language.ZMachine.Object      as OB
import Options.Applicative
import Data.Semigroup ((<>))


import qualified RIO.ByteString.Lazy           as BL



main :: IO ()
main = fromMaybe () <$> runApp dump


runApp :: RIO App a -> IO (Maybe a)
runApp appAction = do
    logOptions' <- logOptionsHandle stdout False

    withLogFunc logOptions' $ \logFunc ->
        let runWithOpts opts = do
                file <- BL.readFile (storyPath opts)
                let app = App { appLogger = logFunc
                                , story = BL.toStrict file
                                , appOptions = opts
                              }
                Just <$> runRIO app appAction
        in
            runWithOpts =<< execParser cmdParser

appParser :: Parser AppOptions
appParser = AppOptions
    <$> argument str (metavar "STORY" <> help "Story file path")
    <*> switch (long "dump-dict" <> short 'd' <> help "Dump dictionary")
    <*> switch (long "dump-objects" <> short 'o' <> help "Dump objects")
    <*> switch (long "dump-tree" <> short 't' <> help "Dump object tree")

cmdParser :: ParserInfo AppOptions
cmdParser = info (appParser <**> helper)
    ( fullDesc
    <> progDesc "ZMachine for Haskell"
    )

dump :: RIO App ()
dump = do
    opts <- appOptions <$> ask
    dict          <- D.getDictionary
    zHeader       <- M.getHeader
    objects       <- OB.getObjects

    logInfo . display $ zHeader
    if dumpDict opts then logInfo . display $ dict else pure ()
    if dumpObjects opts then logInfo . display $ objects else pure ()
    if dumpObjectTree opts then logInfo . OB.displayObjectTree $ OB.makeObjectTree objects else pure ()
    return ()