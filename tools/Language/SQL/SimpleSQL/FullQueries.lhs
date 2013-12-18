
Some tests for parsing full queries.

> {-# LANGUAGE OverloadedStrings #-}
> module Language.SQL.SimpleSQL.FullQueries (fullQueriesTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax


> fullQueriesTests :: TestItem
> fullQueriesTests = Group "queries" $ map (uncurry TestQueryExpr)
>     [("select count(*) from t"
>      ,makeSelect
>       {qeSelectList = [(Nothing, App "count" [Star])]
>       ,qeFrom = [TRSimple "t"]
>       }
>      )

>     ,("select a, sum(c+d) as s\n\
>       \  from t,u\n\
>       \  where a > 5\n\
>       \  group by a\n\
>       \  having count(1) > 5\n\
>       \  order by s"
>      ,makeSelect
>       {qeSelectList = [(Nothing, Iden "a")
>                       ,(Just "s"
>                        ,App "sum" [BinOp (Iden "c")
>                                          "+" (Iden "d")])]
>       ,qeFrom = [TRSimple "t", TRSimple "u"]
>       ,qeWhere = Just $ BinOp (Iden "a") ">" (NumLit "5")
>       ,qeGroupBy = [SimpleGroup $ Iden "a"]
>       ,qeHaving = Just $ BinOp (App "count" [NumLit "1"])
>                                ">" (NumLit "5")
>       ,qeOrderBy = [SortSpec (Iden "s") Asc NullsOrderDefault]
>       }
>      )
>     ]
