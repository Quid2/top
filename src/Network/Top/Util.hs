{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE GHCForeignImportPrim      #-}
{-# LANGUAGE JavaScriptFFI             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Network.Top.Util(
  -- *Exceptions
  strictTry,try,tryE,forceE,SomeException

  -- *Time
  ,milliseconds,seconds,minutes
  ,withTimeout

  -- *Threads
  ,async,cancel,threadDelay

  -- *Monads
  ,liftIO,forever,when,unless

  -- *Logging (with native ghcjs support)
  ,dbg,warn,info,err,dbgS,logLevel

#ifdef ghcjs_HOST_OS
  ,Priority(..)
#else
  ,logLevelOut
  ,module X
#endif

  -- *Other
  ,eitherToMaybe


) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async, cancel)
import           Control.DeepSeq
import           Control.Exception        (SomeException, try)
import qualified Control.Exception        as E
import           Control.Monad
import           Control.Monad.IO.Class
import           GHC.IO.Handle            (Handle)
import           System.Timeout

---------- Logging

#ifdef ghcjs_HOST_OS

------------ GHC-JS Version
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
dbgS :: String -> IO ()
dbg :: [String] -> IO ()
info :: [String] -> IO ()
warn :: [String] -> IO ()
err :: [String] -> IO ()

logLevel = writeIORef gLogLevel

dbgS s = do
  l <- readIORef gLogLevel
  when (l == DEBUG) $ clog . S.pack $ s

dbg = dbgS . unwords

info ss =  do
  l <- readIORef gLogLevel
  when (l <=INFO) $ cinfo . S.pack . unwords $ ss

warn ss = do
  l <- readIORef gLogLevel
  when (l <=WARNING) $ cwarn . S.pack . unwords $ ss

err = cerr . S.pack . unwords

#else
------------ GHC Version
import           System.Log.Handler.Simple (verboseStreamHandler)
import           System.Log.Logger         as X

-- |Setup the global logging level
logLevel :: Priority -> IO ()
logLevel = updateGlobalLogger rootLoggerName . setLevel

logLevelOut :: Priority -> Handle -> IO ()
logLevelOut level handle = do
  out <- verboseStreamHandler handle level
  updateGlobalLogger rootLoggerName (setHandlers [out] . setLevel level)

-- |Log a message at DEBUG level
dbgS :: String -> IO ()
dbgS = debugM "top"

-- |Log multiple messages at DEBUG level
dbg :: MonadIO m => [String] -> m ()
dbg = liftIO . dbgS . unwords

-- |Log multiple messages at INFO level
info :: MonadIO m => [String] -> m ()
info = liftIO . infoM "top" . unwords

-- |Log multiple messages at WARNING level
warn :: MonadIO m => [String] -> m ()
warn = liftIO . warningM "top" . unwords

-- |Log multiple messages at ERROR level
err :: MonadIO m => [String] -> m ()
err = liftIO . errorM "top" . unwords

#endif

---------- Exceptions

-- |forceE == either error id
forceE :: Either String c -> c
forceE = either error id -- throwIO

-- |Like `try` but with returned exception fixed to `SomeException`
tryE :: IO a -> IO (Either SomeException a)
tryE = try

-- |Strict try, `deepseq`s the returned value
strictTry :: NFData a => IO a -> IO (Either E.SomeException a)
strictTry op = E.catch (op >>= \v -> return . Right $! deepseq v v) (\(err:: E.SomeException) -> return . Left $ err)

-- |Run an IO op with a timeout
withTimeout :: Int                  -- ^Timeout (in seconds)
            -> IO a                 -- ^Op to execute
            -> IO (Either String a) -- ^Right if op completed correctly, Left otherwise
-- withTimeout secs op = maybe (Left "Timeout") Right <$> timeout (seconds secs) op
withTimeout secs op = do
  em <- try $ timeout (seconds secs) op
  return $ case em of
    Left (e::SomeException) -> Left (show e)
    Right m -> maybe (Left "Timeout") Right m

-- |Convert an Either to a Maybe
eitherToMaybe :: Either t a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _) = Nothing

---------- Time

-- |Convert minutes to microseconds (μs)
minutes :: Num c => c -> c
minutes = seconds . (60*)

-- |Convert seconds to microseconds (μs)
seconds :: Num a => a -> a
seconds = (* 1000000)

-- |Convert milliseconds to microseconds (μs)
milliseconds :: Num a => a -> a
milliseconds = (* 1000)
