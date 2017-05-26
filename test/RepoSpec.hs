import           Control.Monad
import           Network.Top.Repo
import           Repo.Disk
import           Repo.Memory
import qualified Repo.Types       as R
import           System.Directory
import           Test.Tasty
import           Test.Tasty.HUnit
import           ZM

t = main

main = do

  mrepo <- memRepo
  drepo <- getTemporaryDirectory >>= dbRepo

  defaultMain $ testGroup "Repo" [
     testRepo mrepo
    ,testRepo drepo
    ]
  where
    testRepo repo = testCase "RepoTest" $ do
      let tm = absTypeModel (Proxy::Proxy Bool)
      let [tdef] = typeADTs tm
      R.put repo tdef
      tdef2 <- R.get repo (absRef tdef)
      when (Just tdef /= tdef2) $ assertFailure "ref not present"
      --  r <- ((tm ==) <$>) <$> solveType repo def (typeName tm)
      R.close repo
