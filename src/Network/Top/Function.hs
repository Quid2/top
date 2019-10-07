module Network.Top.Function
  ( Function
  , funServe
  , funReply
  , funCall
  , funCalls
  , sequenceErrors
  ) where

import           Data.Either
import           Network.Top.Pipes
import           Network.Top.Run
import           Network.Top.Types
import           Network.Top.Util
import           ZM
import           ZM.Type.Function

funServe f = async (runAppForever def ByType $ funReply f)

funReply ::
     (Show f, Show r, NFData r, Flat f)
  => (f -> IO r)
  -> Connection (Function f r)
  -> IO ()
funReply f conn = loop
  where
    loop = do
      msg <- input conn
      dbg ["funReply", take 100 $ show msg]
      case msg of
        (Call c) -> do
          dbg ["funReply Call", take 100 $ show c]
          er <- strictTry (f c)
          dbg ["funReply Reply", take 100 $ show er]
          case er of
            Right r -> output conn (Reply (shake128_48 c) r)
            Left e  -> err [show e]
        _ -> return ()
      loop

funCall ::
     (Flat f, Flat r, Show f, Show r)
  => f
  -> Connection (Function f r)
  -> IO (Either String r)
funCall c conn = do
  output conn (Call c)
  let key = shake128_48 c
  let loop = do
        msg <- input conn
        -- dbg ["funCall", show msg]
        case msg of
          (Reply k r)
            | k == key -> return r
          _ -> loop
  --withTimeout (timeoutInMilliSecs cfg) loop
  withTimeout 30 loop -- TOFIX: customised timeout

funCalls cs conn = sequenceErrors <$> mapM (flip funCall conn) cs

{- |
>>> sequenceErrors []
Right []

>>> sequenceErrors [Right "a"]
Right ["a"]

>>> sequenceErrors [Left "Bad"]
Left "Bad\n"

>>> sequenceErrors [Right "a",Left "Bad",Left "Worse",Right "b"]
Left "Bad\nWorse\n"
-}
sequenceErrors :: [Either String b] -> Either String [b]
sequenceErrors res =
  case partitionEithers res of
    ([], oks) -> Right oks
    (errs, _) -> Left (unlines errs)
-- allErrs :: [Either RepoError r] -> Either RepoError [r]
-- allErrs rs =
--   let errs = lefts rs
--    in if null errs
--         then sequence rs
--         else Left (unlines errs)
