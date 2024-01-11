
{-
These are the tests for the queryExprs parsing which parses multiple
query expressions from one string.
-}

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.QueryExprs (queryExprsTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax

queryExprsTests :: TestItem
queryExprsTests = Group "query exprs" $ map (uncurry (TestStatements ansi2011))
    [("select 1",[ms])
    ,("select 1;",[ms])
    ,("select 1;select 1",[ms,ms])
    ,(" select 1;select 1; ",[ms,ms])
    ,("SELECT CURRENT_TIMESTAMP;"
     ,[SelectStatement $ toQueryExpr $ makeSelect
      {msSelectList = [(Iden [Name Nothing "CURRENT_TIMESTAMP"],Nothing)]}])
    ,("SELECT \"CURRENT_TIMESTAMP\";"
     ,[SelectStatement $ toQueryExpr $ makeSelect
      {msSelectList = [(Iden [Name (Just ("\"","\"")) "CURRENT_TIMESTAMP"],Nothing)]}])
    ]
  where
    ms = SelectStatement $ toQueryExpr $ makeSelect {msSelectList = [(NumLit "1",Nothing)]}
