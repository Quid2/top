module Repo.Types (Repo(..),withRepo) where

import           Control.Exception
import           ZM

-- |A repository of absolute types
data Repo = Repo { get   :: AbsRef -> IO (Maybe AbsADT)
                 , put   :: AbsADT -> IO ()
                 , close :: IO () }

-- |Run an IO action on a Repo
withRepo :: IO Repo -> (Repo -> IO c) -> IO c
withRepo new = bracket new close

  