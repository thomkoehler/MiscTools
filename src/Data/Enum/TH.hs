
{-# LANGUAGE TemplateHaskell #-}

module Data.Enum.TH
(
   enumADT,
   enumADTDef
)
where

import Language.Haskell.TH


enumADT :: String -> [(String, Integer)] -> Q [Dec]
enumADT typeNameS elems = do
  let typeName = mkName typeNameS
  let dataDecl = DataD [] typeName [] (map ((`NormalC` []) . mkName . fst) elems) [''Eq, ''Ord, ''Show]
  let toEnumMethods = (FunD (mkName "toEnum") (map toEnumClauses elems))
  let fromEnumMethods = (FunD (mkName "fromEnum") (map fromEnumClauses elems))
  let enumInstance = InstanceD [] (AppT (ConT (mkName "Enum")) (ConT typeName)) [toEnumMethods, fromEnumMethods]
  return [dataDecl, enumInstance]
  where
      toEnumClauses (name, value) = Clause [LitP (IntegerL value)] (NormalB (ConE (mkName name))) []
      fromEnumClauses (name, value) = Clause [ConP (mkName name) []] (NormalB (LitE (IntegerL value))) []


enumADTDef :: String -> String -> [(String, Integer)] -> Q [Dec]
enumADTDef typeNameS defFunctionS elems = do
  let defFunction = mkName defFunctionS
  let typeName = mkName typeNameS
  let xName = mkName "x"
  let dataDecl = DataD [] typeName [] (map ((`NormalC` []) . mkName . fst) elems) [''Eq, ''Ord, ''Show]
  let toEnumMethods = (FunD (mkName "toEnum") ((map toEnumClauses elems) ++ [Clause [VarP xName] (NormalB (AppE (VarE defFunction) (VarE xName))) []]))
  let fromEnumMethods = (FunD (mkName "fromEnum") (map fromEnumClauses elems))
  let enumInstance = InstanceD [] (AppT (ConT (mkName "Enum")) (ConT typeName)) [toEnumMethods, fromEnumMethods]
  return [dataDecl, enumInstance]
  where
      toEnumClauses (name, value) = Clause [LitP (IntegerL value)] (NormalB (ConE (mkName name))) []
      fromEnumClauses (name, value) = Clause [ConP (mkName name) []] (NormalB (LitE (IntegerL value))) []
