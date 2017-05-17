-- |Convert a Template Haskell pattern to a Pattern
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
module Data.Pattern.TH (patternQ, patternE) where

import           Data.Pattern.Types
import           Language.Haskell.TH        hiding (Match, Pat, Type)
import qualified Language.Haskell.TH        as TH
import           Language.Haskell.TH.Syntax hiding (Match, Type, Pat)
import           Network.Top.Types          ()
{-
-}

-- $(byPattern [p|False|]) :: ByPattern Bool

-- ByPattern $(pattern [p|False|]) :: ByPattern Bool

-- Note: no support for negative integers or tuples with more than 5 elements
patternE :: Q TH.Pat -> Q Exp
patternE pat = asPatternM pat >>= lift

patternQ :: Quasi m => Q TH.Pat -> m (Pat PRef)
patternQ = runQ . asPatternM

asPatternM  :: Monad m =>  m TH.Pat -> m (Pat PRef)
asPatternM = (conv <$>)
  where
    conv :: TH.Pat -> Pat PRef
    conv pat = case pat of
      ConP n [] | name n == "[]" -> PCon "Nil" []

      ConP n args -> PCon (name n) $ map (conv) args

      ListP ps -> convList $ map conv ps

      VarP n -> PName $ PVar (name n)

      WildP -> PName PWild

      ParensP p -> conv p

      InfixP p1 (Name (OccName ":" ) (NameG DataName (PkgName "ghc-prim") (ModName "GHC.Types"))) p2 -> (\a b -> PCon "Cons" [a,b]) (conv p1) (conv p2)

      TupP [p1,p2] -> (\a b -> PCon "Tuple2" [a,b]) (conv p1) (conv p2)
      TupP [p1,p2,p3] -> (\a b c -> PCon "Tuple3" [a,b,c]) (conv p1) (conv p2) (conv p3)
      TupP [p1,p2,p3,p4] -> (\a b c d -> PCon "Tuple4" [a,b,c,d]) (conv p1) (conv p2) (conv p3) (conv p4)

      -- RecP --

      LitP l -> case l of
                           CharL c     -> PName . PChar $ c -- valPattern c
                           StringL s   -> PName . PString $ s -- valPattern s
                           IntegerL i  -> PName . PInt $ i -- onInt i
                           RationalL r -> PName . PRat $ r -- onRat r
                           _ -> error . unwords $ ["Unsupported literal",show l]

      _ -> error . unwords $ ["Unsupported pattern",show pat] -- pprint p,show p]


convList []    = PCon "Nil" []
convList (h:t) = PCon "Cons" [h,convList t]

name (Name (OccName n) _) = n

-- asExp (PCon n ps) = AppE (AppE (c "Data.Pattern.Con") (LitE (StringL n))) (ListE (map asExp ps))
-- asExp (PName (V v)) = VarE (mkName v)
-- asExp (PName W) = AppE (c "Data.Pattern.Var") (c "W")

c = ConE . mkName



