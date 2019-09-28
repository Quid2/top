{-# LANGUAGE ScopedTypeVariables #-}

-- |Run processed connected to Top
module Network.Top.Run
  ( runAppForever
  , runApp
  , runAppWith
  , run
  ) where

import           Data.ByteString   (ByteString, copy, unpack)
import           Network.Top.Types
import           Network.Top.Util
import           Network.Top.WSApp
import           ZM

-- |Permanently connect an application to a typed channel.
-- |Restart application in case of network or application failure.
-- |NOTE: does not provide a way to preserve application's state
runAppForever ::
     (Model (router a), Flat (router a), Show (router a), Flat a, Show a)
  => Config -- ^ Top configuration
  -> router a -- ^ Routing protocol
  -> App a r -- ^ Application to connect
  -> IO r -- ^ Value returned from the application
runAppForever cfg router = runForever (runApp cfg router)
  -- runAppForever cfg router app =

--   forever $ do
--     Left (ex :: SomeException) <-
--       try $
--       runApp cfg router $ \conn -> do
--         liftIO $ dbgS "connected"
--         app conn
--       -- Something went wrong, wait a few seconds and restart
--     dbg
--       [ "Exited loop with error"
--       , concat ["'", show ex, "'"]
--       , "retrying in a bit."
--       ]
--     threadDelay $ seconds 5
runForever connMaker app =
  forever $ do
    Left (ex :: SomeException) <-
      try $
      connMaker $ \conn -> do
        liftIO $ dbgS "connected"
        app conn
          -- Something went wrong, wait a few seconds and restart
    dbg
      [ "Exited loop with error"
      , concat ["'", show ex, "'"]
      , "retrying in a bit."
      ]
    threadDelay $ seconds 5

-- |Connect an application to a ByType channel using the default configuration.
run ::
     (Flat a, Show a, Model a)
  => App a r -- ^ Application to connect
  -> IO r -- ^ Final value returned from the application
run = runApp def ByType

-- |Connect an application to a typed channel.
runApp ::
     (Model (router a), Flat (router a), Show (router a), Flat a, Show a)
  => Config -- ^ Top configuration
  -> router a -- ^ Routing protocol
  -> App a r -- ^ Application to connect
  -> IO r -- ^ Final value returned from the application
runApp cfg router app = do
  dbg ["run", show router]
  runAppWith
    cfg
    (typedBLOB router)
    (\conn -> app (Connection (receive conn) (send conn) (close conn)))

-- |Connect an application to a typed channel
runAppWith ::
     Config -- ^ Top configuration
  -> TypedBLOB -- ^ Routing protocol
  -> App ByteString r -- ^ Application to connect
  -> IO r -- ^ Final value returned from the application
runAppWith cfg routerBin app = run cfg 1
  where
    run _ 4 = errIn "Too many redirects"
    run cfg n = do
      res <-
        runWSApp cfg $ \conn -> do
          send conn routerBin
          dbgS "Wait for CHATS answer"
          r :: WSChannelResult <- receive conn
          dbgS $ unwords ["Received CHATS answer", show r]
          case r of
            Failure why  -> errIn why
            Success      -> Right <$> app conn -- (Connection (receive conn) (send conn) (close conn))
          --Success -> Right <$> app (Connection (receive conn) (send conn) (close conn))
            RetryAt addr -> return (Left $ cfg {accessPoint = addr})
      case res of
        Left cfg' -> run cfg' (n + 1)
        Right r   -> return r
    errIn msg = dbg ["Failure", msg] >> error msg

-- |Send a value on a typed connection
send :: (Show a, Flat a) => WSConnection -> a -> IO ()
send conn v = do
  let e = flat v
  output conn e
  --dbg ["sent",show v,"as",show $ L.unpack e]

-- |Receive a value from a typed connection
receive :: (Show a, Flat a) => WSConnection -> IO a
receive conn = do
  e <- input conn
  --let e = copy eo
  let ue = unpack e
  let ev = unflat e
  dbg ["received data", take 200 $ show ue, " of length ", show $ length ue]
  dbg ["value is", take 200 $ show ev]
  either (\ex -> error $ unwords ["receive error", show ex]) return ev
-- receive :: Flat a => WSConnection -> IO (Maybe a)
-- receive conn = do
--   mbs <- input conn
--   return $ case mbs of
--     Nothing -> Nothing
--     Just bs -> eitherToMaybe $ unflat bs
-- -- |Setup a connection by sending a value specifying the routing protocol to be used
-- protocol :: (Show r, Model r, Flat r) => Connection ByteString -> r -> IO ()
-- --protocol = setProtocol (return ())
-- protocol conn routerType = do
--   dbg ["protocol",show routerType]
--   send conn . typedBLOB $ routerType
--   r::WSChannelResult <- receive conn
--   case r of
--     Failure why  -> err $ T.unpack why
--     Success      -> return ()
--     RetryAt addr -> err (show $ RetryAt addr)
--    where
--      err msg = error msg -- unwords ["Failed to establish connection:",msg]
-- setProtocol onSuccess onFailure  conn router = do
--   dbg ["protocol",show router]
--   send conn . typedBLOB $ router
--   r::WSChannelResult <- receive conn
--   case r of
--     Failure why  -> onFailure err $ T.unpack why
--     Success      -> onSuccess --
--     RetryAt addr -> err (show $ RetryAt addr)
--    where
--      err msg = error msg -- unwords ["Failed to establish connection:",msg]
-- setProtocol conn routerType = run 1
--   where
--     run 4 = errIn "Too many redirects"
--     run n = do
--       dbg ["protocol",show routerType,"attempt",show n]
--       send conn . typedBLOB $ routerType
--       r::WSChannelResult <- receive conn
--       case r of
--           Failure why  -> errIn (T.unpack why)
--           Success      -> Right <$> app conn -- (Connection (receive conn) (send conn) (close conn))
--           --Success -> Right <$> app (Connection (receive conn) (send conn) (close conn))
--           RetryAt addr -> return (Left $ Config addr)
--       case res of
--         Left cfg' -> run cfg' (n+1)
--         Right r   -> return r
--     errIn msg = dbg ["Failure",msg] >> error msg
