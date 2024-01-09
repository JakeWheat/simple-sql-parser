
-- Some tests for parsing full queries.

module Language.SQL.SimpleSQL.FullQueries (fullQueriesTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax


fullQueriesTests :: TestItem
fullQueriesTests = Group "queries" $ map (uncurry (TestQueryExpr ansi2011))
    [("select count(*) from t"
     ,makeSelect
      {qeSelectList = [(App [Name Nothing "count"] [Star], Nothing)]
      ,qeFrom = [TRSimple [Name Nothing "t"]]
      }
     )

    ,("select a, sum(c+d) as s\n\
      \  from t,u\n\
      \  where a > 5\n\
      \  group by a\n\
      \  having count(1) > 5\n\
      \  order by s"
     ,makeSelect
      {qeSelectList = [(Iden [Name Nothing "a"], Nothing)
                      ,(App [Name Nothing "sum"]
                        [BinOp (Iden [Name Nothing "c"])
                               [Name Nothing "+"] (Iden [Name Nothing "d"])]
                       ,Just $ Name Nothing "s")]
      ,qeFrom = [TRSimple [Name Nothing "t"], TRSimple [Name Nothing "u"]]
      ,qeWhere = Just $ BinOp (Iden [Name Nothing "a"]) [Name Nothing ">"] (NumLit "5")
      ,qeGroupBy = [SimpleGroup $ Iden [Name Nothing "a"]]
      ,qeHaving = Just $ BinOp (App [Name Nothing "count"] [NumLit "1"])
                               [Name Nothing ">"] (NumLit "5")
      ,qeOrderBy = [SortSpec (Iden [Name Nothing "s"]) DirDefault NullsOrderDefault]
      }
     )
    ]
