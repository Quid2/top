{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE GHCForeignImportPrim      #-}
{-# LANGUAGE JavaScriptFFI             #-}

module Network.Top.Util(
  dbg,warn,info,err,dbgS,logLevel
#ifdef ghcjs_HOST_OS
  ,Priority(..)
#else
  ,logLevelOut
  ,module X
#endif
  ,eitherToMaybe,isRight
  ,liftIO,forever,when,unless,try,tryE,forceE,SomeException
  ,async,cancel
  ,threadDelay,milliseconds,seconds,minutes
  ) where

import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.Async  (async, cancel)
import           Control.Exception         (SomeException, try)
import           Control.Monad
import           Control.Monad.IO.Class
import           GHC.IO.Handle             (Handle)

#ifdef ghcjs_HOST_OS
-- GHC-JS Version
import Data.IORef
import System.IO.Unsafe
import qualified Data.JSString as S

foreign import javascript unsafe "console.log($1)" clog :: S.JSString -> IO ()

foreign import javascript unsafe "console.info($1)" cinfo :: S.JSString -> IO ()

foreign import javascript unsafe "console.warn($1)" cwarn :: S.JSString -> IO ()

foreign import javascript unsafe "console.error($1)" cerr :: S.JSString -> IO ()

data Priority = DEBUG | INFO | WARNING | ERROR deriving (Show,Eq,Ord)

gLogLevel :: IORef Priority
{-# NOINLINE gLogLevel #-}
gLogLevel = unsafePerformIO (newIORef DEBUG)

logLevel :: Priority -> IO ()
logLevel = writeIORef gLogLevel

dbgS :: String -> IO ()
dbgS s = do
  l <- readIORef gLogLevel
  when (l == DEBUG) $ clog . S.pack $ s

dbg :: [String] -> IO ()
dbg = dbgS . unwords

info :: [String] -> IO ()
info ss =  do
  l <- readIORef gLogLevel
  when (l <=INFO) $ cinfo . S.pack . unwords $ ss

warn :: [String] -> IO ()
warn ss = do
  l <- readIORef gLogLevel
  when (l <=WARNING) $ cwarn . S.pack . unwords $ ss

err :: [String] -> IO ()
err = cerr . S.pack . unwords

#else
-- GHC Version

import           System.Log.Handler.Simple (verboseStreamHandler)
import           System.Log.Logger         as X

-- |Setup the global logging level
logLevel :: Priority -> IO ()
logLevel = updateGlobalLogger rootLoggerName . setLevel

logLevelOut :: Priority -> Handle -> IO ()
logLevelOut level handle = do
  out <- verboseStreamHandler handle level
  updateGlobalLogger rootLoggerName (setHandlers [out] . setLevel level)

-- |Utilities for logging
dbgS :: String -> IO ()
dbgS = debugM "top"

dbg :: MonadIO m => [String] -> m ()
dbg = liftIO . dbgS . unwords

err :: MonadIO m => [String] -> m ()
err = liftIO . errorM "top" . unwords

warn :: MonadIO m => [String] -> m ()
warn = liftIO . warningM "top" . unwords

info :: MonadIO m => [String] -> m ()
info = liftIO . infoM "top" . unwords
#endif

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

