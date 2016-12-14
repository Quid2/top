-- |Convert a Template Haskell pattern to a Pattern
{-# LANGUAGE TemplateHaskell #-}
module Data.Pattern.TH (asPattern
                       ,patternQ
                       )where

import           Data.Foldable              (toList)
import           Data.Pattern.Types
-- import           Network.Top.Types
import           Language.Haskell.TH        hiding (Match, Type)
import           Language.Haskell.TH.Syntax hiding (Match, Type)

-- x = filter [p|\Message _ (subject:_) _ |]
-- \subject -> Con ... v1
filterPatternQ :: Quasi m => Q Pat -> m Exp
filterPatternQ patq = do
     p <- runQ $ convertPattern (PVar . V) (PVar W) patq
      -- print $ pmatch p --
     let vars = map (\(V v) -> v) . filter isVar $ toList p
     -- TODO: when done, remove haskell-src-meta
     -- let Right c = parseExp $ concat["\\",unwords $ vars,"-> onlyWildCards <$> (",showPatt p,")"]
     -- let c = concat["\\",unwords $ toList p,"->",showPatt p]
     let c = LamE (map (VarP . mkName) vars) (UInfixE (VarE (mkName "onlyWildCards")) (VarE (mkName "<$>")) (ParensE (asExp p)))
     -- print c >> print c2 >> print (c == c2)
     return c

patternQ :: Quasi m => Q Pat -> m (Pattern WildCard)
patternQ = runQ . asPattern

asPattern :: Monad m => m Pat -> m (Pattern WildCard)
asPattern = convertPattern (\n -> error $ unwords ["Variables are not allowed in patterns, use wildcards (_) only, found:",n]) (PVar WildCard)

-- Literals are converted to their flat representation (alternative: use proper definition?)
-- Anything else as a nested named pattern
-- convertPattern :: Quasi m => Q Pat -> m (Pattern String)
-- convertPatternM
--   :: Quasi m =>
--      (String -> Pattern v) -> Pattern v -> Q Pat -> m (Pattern v)
-- -- convertPattern onVar onWild p = runQ (p >>= convertM onVar onWild)
-- convertPatternM onVar onWild p = runQ (convertPattern_ onVar onWild p)

convertPattern onVar onWild p = p >>= convertM onVar onWild
  where
    convertM onVar onWild pat = case pat of
      ConP n [] | name n == "[]" -> return $ PCon "Nil" []
      ConP n args -> PCon (name n) <$> mapM (convertM onVar onWild) args
      VarP n -> return $ onVar (name n)
      WildP -> return onWild
      LitP l -> return . convLit $ l
      ParensP p -> convertM onVar onWild p
      InfixP p1 (Name (OccName ":" ) (NameG DataName (PkgName "ghc-prim") (ModName "GHC.Types"))) p2 -> (\a b -> PCon "Cons" [a,b]) <$> (convertM onVar onWild p1) <*> (convertM onVar onWild p2)
      ListP ps -> convList <$> mapM (convertM onVar onWild) ps
      TupP [p1,p2] -> (\a b -> PCon "Tuple2" [a,b]) <$> (convertM onVar onWild p1) <*> (convertM onVar onWild p2)
      TupP [p1,p2,p3] -> (\a b c -> PCon "Tuple3" [a,b,c]) <$> (convertM onVar onWild p1) <*> (convertM onVar onWild p2) <*> (convertM onVar onWild p3)
      TupP [p1,p2,p3,p4] -> (\a b c d -> PCon "Tuple4" [a,b,c,d]) <$> (convertM onVar onWild p1) <*> (convertM onVar onWild p2) <*> (convertM onVar onWild p3) <*> (convertM onVar onWild p4)
      p -> error . unwords $ ["Unsupported pattern",show p] -- pprint p,show p]

    name (Name (OccName n) _) = n

    convLit l = case l of
       CharL c    -> valPattern c
       StringL s  -> valPattern s
       -- BUG::always interpreted as Integral (signed Int) (should be mapped to right numerical type)
       IntegerL i -> valPattern i
       -- RationalL r -> valPattern r

    convList []    = PCon "Nil" []
    convList (h:t) = PCon "Cons" [h,convList t]

asExp (PCon n ps) = AppE (AppE (c "Data.Pattern.Con") (LitE (StringL n))) (ListE (map asExp ps))
asExp (PVar (V v)) = VarE (mkName v)
asExp (PVar W) = AppE (c "Data.Pattern.Var") (c "W")

c = ConE . mkName

