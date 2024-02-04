
{-
These are the tests for the queryExprs parsing which parses multiple
query expressions from one string.
-}

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.QueryExprs (queryExprsTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestRunners
import Data.Text (Text)

queryExprsTests :: TestItem
queryExprsTests = Group "query exprs"
    [q "select 1" [ms]
    ,q "select 1;" [ms]
    ,q "select 1;select 1" [ms,ms]
    ,q " select 1;select 1; " [ms,ms]
    ,q "SELECT CURRENT_TIMESTAMP;"
      [SelectStatement $ toQueryExpr $ makeSelect
      {msSelectList = [(Iden [Name Nothing "CURRENT_TIMESTAMP"],Nothing)]}]
    ,q "SELECT \"CURRENT_TIMESTAMP\";"
      [SelectStatement $ toQueryExpr $ makeSelect
      {msSelectList = [(Iden [Name (Just ("\"","\"")) "CURRENT_TIMESTAMP"],Nothing)]}]
    ]
  where
    ms = SelectStatement $ toQueryExpr $ makeSelect {msSelectList = [(NumLit "1",Nothing)]}
    q :: HasCallStack => Text -> [Statement] -> TestItem
    q src ast = testStatements ansi2011 src ast
