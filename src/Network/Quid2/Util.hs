{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Network.Quid2.Util(
  dbg,warn,info,err,dbgS,logLevel,logLevelOut
  ,eitherToMaybe,isRight
  ,liftIO,forever,when,unless,try,tryE,forceE,SomeException
  ,async,cancel
  ,threadDelay,milliseconds,seconds,minutes
  ,module X
  ) where

import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.Async  (async, cancel)
import           Control.Exception         (SomeException, try)
import           Control.Monad
import           Control.Monad.IO.Class
import           GHC.IO.Handle             (Handle)
import           System.Log.Handler.Simple (verboseStreamHandler)
import           System.Log.Logger         as X

-- |Setup the global logging level
logLevel :: Priority -> IO ()
logLevel = updateGlobalLogger rootLoggerName . setLevel

logLevelOut :: Priority -> Handle -> IO ()
logLevelOut level handle = do
  out <- verboseStreamHandler handle level
  updateGlobalLogger rootLoggerName (setHandlers [out] . setLevel level)

forceE = either error id -- throwIO

tryE :: IO a -> IO (Either SomeException a)
tryE = try

eitherToMaybe :: Either t a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _) = Nothing

isRight :: Either t t1 -> Bool
isRight (Right _) = True
isRight _ = False

minutes :: Num c => c -> c
minutes = seconds . (60*)

seconds :: Num a => a -> a
seconds = (* 1000000)

milliseconds :: Num a => a -> a
milliseconds = (* 1000)

-- |Utilities for logging
dbgS :: String -> IO ()
dbgS = debugM "quid2-net"

dbg :: MonadIO m => [String] -> m ()
dbg = liftIO . dbgS . unwords

err :: MonadIO m => [String] -> m ()
err = liftIO . errorM "quid2-net" . unwords

warn :: MonadIO m => [String] -> m ()
warn = liftIO . warningM "quid2-net" . unwords

info :: MonadIO m => [String] -> m ()
info = liftIO . infoM "quid2-net" . unwords
