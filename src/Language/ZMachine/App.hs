module Language.ZMachine.App
    ( App(..)
    , AppOptions(..)
    )
where

import           RIO

import qualified RIO.ByteString                as B


--
-- App Definition
--

data App = App
    { appLogger :: !LogFunc
    , story     :: !B.ByteString
    , appOptions :: !AppOptions
    }

instance HasLogFunc App where
    logFuncL = lens appLogger (\x y -> x { appLogger = y })

data AppOptions = AppOptions
    { storyPath :: String 
    , dumpDict :: Bool 
    , dumpObjects :: Bool 
    , dumpObjectTree :: Bool
    }
