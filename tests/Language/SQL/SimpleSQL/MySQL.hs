
-- Tests for mysql dialect parsing

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.MySQL (mySQLTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestRunners

mySQLTests :: TestItem
mySQLTests = Group "mysql dialect"
    [backtickQuotes
    ,limit]

{-
backtick quotes

limit syntax

[LIMIT {[offset,] row_count | row_count OFFSET offset}]
-}

backtickQuotes :: TestItem
backtickQuotes = Group "backtickQuotes"
    [testScalarExpr mysql "`test`" $ Iden [Name (Just ("`","`")) "test"]
    ,testParseScalarExprFails ansi2011 "`test`"]

limit :: TestItem
limit = Group "queries"
    [testQueryExpr mysql "select * from t limit 5"
     $ toQueryExpr $ sel {msFetchFirst = Just (NumLit "5")}
    ,testParseQueryExprFails mysql "select a from t fetch next 10 rows only;"
    ,testParseQueryExprFails ansi2011 "select * from t limit 5"]
  where
    sel = makeSelect
          {msSelectList = [(Star, Nothing)]
          ,msFrom = [TRSimple [Name Nothing "t"]]
          }
