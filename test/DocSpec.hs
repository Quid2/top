module Main where

import           Data.List                      ( isSuffixOf )
import           System.FilePath.Find
import           Test.DocTest

main :: IO ()
-- main = find always ((extension ==? ".hs") &&? exceptFiles ["Data/Convertible/Base.hs","Data/Convertible/Utils.hs","Data/Convertible/Instances/Num.hs"]) "src" >>= doctest
main =
  find always
       ((extension ==? ".hs") &&? exceptFiles ["Data/Pattern/Matcher/X64.hs"])
       "src"
    >>= doctest

exceptFiles :: Foldable t => t String -> FindClause Bool
exceptFiles mdls =
  let excludes = liftOp (\fp modules -> not $ any (`isSuffixOf` fp) modules)
  in  filePath `excludes` mdls
      -- let excludes = liftOp (\fp mdls -> not $ any (\mdl -> isSuffixOf mdl (traceShowId fp)) mdls)
