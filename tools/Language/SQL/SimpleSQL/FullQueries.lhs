
Some tests for parsing full queries.

> module Language.SQL.SimpleSQL.FullQueries (fullQueriesTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax


> fullQueriesTests :: TestItem
> fullQueriesTests = Group "queries" $ map (uncurry TestQueryExpr)
>     [("select count(*) from t"
>      ,makeSelect
>       {qeSelectList = [(App [Name "count"] [Star], Nothing)]
>       ,qeFrom = [TRSimple [Name "t"]]
>       }
>      )

>     ,("select a, sum(c+d) as s\n\
>       \  from t,u\n\
>       \  where a > 5\n\
>       \  group by a\n\
>       \  having count(1) > 5\n\
>       \  order by s"
>      ,makeSelect
>       {qeSelectList = [(Iden [Name "a"], Nothing)
>                       ,(App [Name "sum"]
>                         [BinOp (Iden [Name "c"])
>                                [Name "+"] (Iden [Name "d"])]
>                        ,Just $ Name "s")]
>       ,qeFrom = [TRSimple [Name "t"], TRSimple [Name "u"]]
>       ,qeWhere = Just $ BinOp (Iden [Name "a"]) [Name ">"] (NumLit "5")
>       ,qeGroupBy = [SimpleGroup $ Iden [Name "a"]]
>       ,qeHaving = Just $ BinOp (App [Name "count"] [NumLit "1"])
>                                [Name ">"] (NumLit "5")
>       ,qeOrderBy = [SortSpec (Iden [Name "s"]) DirDefault NullsOrderDefault]
>       }
>      )
>     ]
