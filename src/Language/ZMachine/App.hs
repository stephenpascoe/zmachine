module Language.ZMachine.App
  ( App(..)
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
  }

instance HasLogFunc App where
  logFuncL = lens appLogger (\x y -> x { appLogger = y })
