-- |WebSockets Connection (with GHCJS native support)
{-# LANGUAGE CPP #-}

module Network.Top.WSApp
  ( runWSApp
  ) where
#ifdef ghcjs_HOST_OS
import           Network.Top.WSApp.GHCJS (runWSApp)
#else
import           Network.Top.WSApp.GHC   (runWSApp)
#endif
