
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
  let dataDecl = DataD [] typeName [] Nothing (map ((`NormalC` []) . mkName . fst) elems) [DerivClause Nothing [ConT ''Eq, ConT ''Ord, ConT ''Show]]
  let toEnumMethods = FunD (mkName "toEnum") (map toEnumClauses elems)
  let enumInstance = InstanceD Nothing [] (AppT (ConT (mkName "Enum")) (ConT typeName)) [toEnumMethods, fromEnumMethods elems]
  return [dataDecl, enumInstance]
  where
      toEnumClauses (name, value) = Clause [LitP (IntegerL value)] (NormalB (ConE (mkName name))) []
      fromEnumClauses (name, value) = Clause [ConP (mkName name) []] (NormalB (LitE (IntegerL value))) []


enumADTDef :: String -> String -> [(String, Integer)] -> Q [Dec]
enumADTDef typeNameS defFunctionS elems = do
  let defFunction = mkName defFunctionS
  let typeName = mkName typeNameS
  let xName = mkName "x"
  let dataDecl = DataD [] typeName [] Nothing (map ((`NormalC` []) . mkName . fst) elems) [DerivClause Nothing [ConT ''Eq, ConT ''Ord, ConT ''Show]]
  let toEnumMethods = FunD (mkName "toEnum") (map toEnumClauses elems ++ [Clause [VarP xName] (NormalB (AppE (VarE defFunction) (VarE xName))) []])
  let enumInstance = InstanceD Nothing [] (AppT (ConT (mkName "Enum")) (ConT typeName)) [toEnumMethods, fromEnumMethods elems]
  return [dataDecl, enumInstance]
  where
      toEnumClauses (name, value) = Clause [LitP (IntegerL value)] (NormalB (ConE (mkName name))) []
      fromEnumClauses (name, value) = Clause [ConP (mkName name) []] (NormalB (LitE (IntegerL value))) []

      
fromEnumMethods :: [(String, Integer)] -> Dec
fromEnumMethods elems = FunD (mkName "fromEnum") (map fromEnumClauses elems)
  where
    fromEnumClauses (name, value) = Clause [ConP (mkName name) []] (NormalB (LitE (IntegerL value))) []