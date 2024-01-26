
-- Tests for mysql dialect parsing

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.MySQL (mySQLTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax

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
backtickQuotes = Group "backtickQuotes" (map (uncurry (TestScalarExpr mysql))
    [("`test`", Iden [Name (Just ("`","`")) "test"])
    ]
    ++ [ParseScalarExprFails ansi2011 "`test`"]
    )

limit :: TestItem
limit = Group "queries" ( map (uncurry (TestQueryExpr mysql))
    [("select * from t limit 5"
     ,toQueryExpr $ sel {msFetchFirst = Just (NumLit "5")}
     )
    ]
    ++ [ParseQueryExprFails mysql "select a from t fetch next 10 rows only;"
       ,ParseQueryExprFails ansi2011 "select * from t limit 5"]
    )
  where
    sel = makeSelect
          {msSelectList = [(Star, Nothing)]
          ,msFrom = [TRSimple [Name Nothing "t"]]
          }
