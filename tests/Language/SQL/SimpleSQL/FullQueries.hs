
-- Some tests for parsing full queries.

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.FullQueries (fullQueriesTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestRunners
import Data.Text (Text)

fullQueriesTests :: TestItem
fullQueriesTests = Group "queries" $
    [q "select count(*) from t"
     $ toQueryExpr $ makeSelect
      {msSelectList = [(App [Name Nothing "count"] [Star], Nothing)]
      ,msFrom = [TRSimple [Name Nothing "t"]]
      }

    ,q "select a, sum(c+d) as s\n\
      \  from t,u\n\
      \  where a > 5\n\
      \  group by a\n\
      \  having count(1) > 5\n\
      \  order by s"
     $ toQueryExpr $ makeSelect
      {msSelectList = [(Iden [Name Nothing "a"], Nothing)
                      ,(App [Name Nothing "sum"]
                        [BinOp (Iden [Name Nothing "c"])
                               [Name Nothing "+"] (Iden [Name Nothing "d"])]
                       ,Just $ Name Nothing "s")]
      ,msFrom = [TRSimple [Name Nothing "t"], TRSimple [Name Nothing "u"]]
      ,msWhere = Just $ BinOp (Iden [Name Nothing "a"]) [Name Nothing ">"] (NumLit "5")
      ,msGroupBy = [SimpleGroup $ Iden [Name Nothing "a"]]
      ,msHaving = Just $ BinOp (App [Name Nothing "count"] [NumLit "1"])
                               [Name Nothing ">"] (NumLit "5")
      ,msOrderBy = [SortSpec (Iden [Name Nothing "s"]) DirDefault NullsOrderDefault]
      }
     
    ]
  where
    q :: HasCallStack => Text -> QueryExpr -> TestItem
    q src a = testQueryExpr ansi2011 src a
