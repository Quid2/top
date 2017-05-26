-- |Convert a Template Haskell pattern to a Pattern
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Data.Pattern.TH (patternQ, patternE) where

import           Data.Pattern.Types
import           Language.Haskell.TH        hiding (Match, Pat, Type)
import qualified Language.Haskell.TH        as TH
import           Language.Haskell.TH.Syntax hiding (Match, Type, Pat)
import           Network.Top.Types          ()

-- |Template Haskell function to convert an Haskell pattern to an `IPattern`
--
-- @
-- $(patternE [p|_|]) == PName PWild
-- @
--
-- Note: no support for negative integers or tuples with more than 5 elements
patternE :: Q TH.Pat -> Q Exp
patternE pat = asPatternM pat >>= lift

-- |Template Haskell function to convert an Haskell pattern to an `IPattern`
--
-- @
-- patternQ [p|_|] :: IO IPattern
-- PName PWild
-- @
--
patternQ :: Quasi m => Q TH.Pat -> m IPattern
patternQ = runQ . asPatternM

asPatternM  :: Monad m =>  m TH.Pat -> m IPattern
asPatternM = (conv <$>)
  where
    conv :: TH.Pat -> IPattern
    conv pat = case pat of
      ConP n [] | name n == "[]" -> PCon "Nil" []

      ConP n args -> PCon (name n) $ map conv args

      ListP ps -> convList $ map conv ps

      VarP n -> PName $ PVar (name n)

      WildP -> PName PWild

      ParensP p -> conv p

      InfixP p1 (Name (OccName ":" ) (NameG DataName (PkgName "ghc-prim") (ModName "GHC.Types"))) p2 -> (\a b -> PCon "Cons" [a,b]) (conv p1) (conv p2)

      TupP [p1,p2] -> (\a b -> PCon "Tuple2" [a,b]) (conv p1) (conv p2)
      TupP [p1,p2,p3] -> (\a b c -> PCon "Tuple3" [a,b,c]) (conv p1) (conv p2) (conv p3)
      TupP [p1,p2,p3,p4] -> (\a b c d -> PCon "Tuple4" [a,b,c,d]) (conv p1) (conv p2) (conv p3) (conv p4)
      TupP [p1,p2,p3,p4,p5] -> (\e1 e2 e3 e4 e5 -> PCon "Tuple5" [e1,e2,e3,e4,e5]) (conv p1) (conv p2) (conv p3) (conv p4) (conv p5)

      -- RecP --

      LitP l -> case l of
                           CharL c     -> PName . PChar $ c
                           StringL s   -> PName . PString $ s
                           IntegerL i  -> PName . PInt $ i
                           RationalL r -> PName . PRat $ r
                           _ -> error . unwords $ ["Unsupported literal",show l]

      _ -> error . unwords $ ["Unsupported pattern",show pat] -- pprint p,show p]


    convList :: [Pat v] -> Pat v
    convList []    = PCon "Nil" []
    convList (h:t) = PCon "Cons" [h,convList t]

    name :: Name -> String
    name (Name (OccName n) _) = n

-- asExp (PCon n ps) = AppE (AppE (c "Data.Pattern.Con") (LitE (StringL n))) (ListE (map asExp ps))
-- asExp (PName (V v)) = VarE (mkName v)
-- asExp (PName W) = AppE (c "Data.Pattern.Var") (c "W")

-- c = ConE . mkName




