module Repo.Types (Repo(..)) where

import           ZM

-- |A repository of absolute types
data Repo = Repo { get   :: AbsRef -> IO (Maybe AbsADT)
                 , put   :: AbsADT -> IO ()
                 , close :: IO () }
