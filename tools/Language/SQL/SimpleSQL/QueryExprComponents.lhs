
These are the tests for the query expression components apart from the
table refs which are in a separate file.


These are a few misc tests which don't fit anywhere else.

> {-# LANGUAGE OverloadedStrings #-}
> module Language.SQL.SimpleSQL.QueryExprComponents (queryExprComponentTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax


> queryExprComponentTests :: TestItem
> queryExprComponentTests = Group "queryExprComponentTests"
>     [duplicates
>     ,selectLists
>     ,whereClause
>     ,having
>     ,orderBy
>     ,offsetFetch
>     ,combos
>     ,withQueries
>     ,values
>     ,tables
>     ]



> duplicates :: TestItem
> duplicates = Group "duplicates" $ map (uncurry TestQueryExpr)
>     [("select a from t" ,ms All)
>     ,("select all a from t" ,ms All)
>     ,("select distinct a from t", ms Distinct)
>     ]
>  where
>    ms d = makeSelect
>           {qeSetQuantifier = d
>           ,qeSelectList = [(Nothing,Iden "a")]
>           ,qeFrom = [TRSimple "t"]}

> selectLists :: TestItem
> selectLists = Group "selectLists" $ map (uncurry TestQueryExpr)
>     [("select 1",
>       makeSelect {qeSelectList = [(Nothing,NumLit "1")]})

>     ,("select a"
>      ,makeSelect {qeSelectList = [(Nothing,Iden "a")]})

>     ,("select a,b"
>      ,makeSelect {qeSelectList = [(Nothing,Iden "a")
>                                  ,(Nothing,Iden "b")]})

>     ,("select 1+2,3+4"
>      ,makeSelect {qeSelectList =
>                      [(Nothing,BinOp (NumLit "1") "+" (NumLit "2"))
>                      ,(Nothing,BinOp (NumLit "3") "+" (NumLit "4"))]})

>     ,("select a as a, /*comment*/ b as b"
>      ,makeSelect {qeSelectList = [(Just "a", Iden "a")
>                                  ,(Just "b", Iden "b")]})

>     ,("select a a, b b"
>      ,makeSelect {qeSelectList = [(Just "a", Iden "a")
>                                  ,(Just "b", Iden "b")]})

>     ,("select a + b * c"
>      ,makeSelect {qeSelectList =
>       [(Nothing,BinOp (Iden (Name "a")) (Name "+")
>        (BinOp (Iden (Name "b")) (Name "*") (Iden (Name "c"))))]})

>     ]

> whereClause :: TestItem
> whereClause = Group "whereClause" $ map (uncurry TestQueryExpr)
>     [("select a from t where a = 5"
>      ,makeSelect {qeSelectList = [(Nothing,Iden "a")]
>                  ,qeFrom = [TRSimple "t"]
>                  ,qeWhere = Just $ BinOp (Iden "a") "=" (NumLit "5")})
>     ]

> having :: TestItem
> having = Group "having" $ map (uncurry TestQueryExpr)
>     [("select a,sum(b) from t group by a having sum(b) > 5"
>      ,makeSelect {qeSelectList = [(Nothing, Iden "a")
>                                  ,(Nothing, App "sum" [Iden "b"])]
>                  ,qeFrom = [TRSimple "t"]
>                  ,qeGroupBy = [SimpleGroup $ Iden "a"]
>                  ,qeHaving = Just $ BinOp (App "sum" [Iden "b"])
>                                           ">" (NumLit "5")
>                  })
>     ]

> orderBy :: TestItem
> orderBy = Group "orderBy" $ map (uncurry TestQueryExpr)
>     [("select a from t order by a"
>      ,ms [SortSpec (Iden "a") Asc NullsOrderDefault])

>     ,("select a from t order by a, b"
>      ,ms [SortSpec (Iden "a") Asc NullsOrderDefault
>          ,SortSpec (Iden "b") Asc NullsOrderDefault])

>     ,("select a from t order by a asc"
>      ,ms [SortSpec (Iden "a") Asc NullsOrderDefault])

>     ,("select a from t order by a desc, b desc"
>      ,ms [SortSpec (Iden "a") Desc NullsOrderDefault
>          ,SortSpec (Iden "b") Desc NullsOrderDefault])

>     ,("select a from t order by a desc nulls first, b desc nulls last"
>      ,ms [SortSpec (Iden "a") Desc NullsFirst
>          ,SortSpec (Iden "b") Desc NullsLast])

>     ]
>   where
>     ms o = makeSelect {qeSelectList = [(Nothing,Iden "a")]
>                       ,qeFrom = [TRSimple "t"]
>                       ,qeOrderBy = o}

> offsetFetch :: TestItem
> offsetFetch = Group "offsetFetch" $ map (uncurry TestQueryExpr)
>     [-- ansi standard
>      ("select a from t offset 5 rows fetch next 10 rows only"
>      ,ms (Just $ NumLit "5") (Just $ NumLit "10"))
>     ,("select a from t offset 5 rows;"
>      ,ms (Just $ NumLit "5") Nothing)
>     ,("select a from t fetch next 10 row only;"
>      ,ms Nothing (Just $ NumLit "10"))
>     ,("select a from t offset 5 row fetch first 10 row only"
>      ,ms (Just $ NumLit "5") (Just $ NumLit "10"))
>      -- postgres
>     ,("select a from t limit 10 offset 5"
>      ,ms (Just $ NumLit "5") (Just $ NumLit "10"))
>     ]
>   where
>     ms o l = makeSelect
>              {qeSelectList = [(Nothing,Iden "a")]
>              ,qeFrom = [TRSimple "t"]
>              ,qeOffset = o
>              ,qeFetch = l}

> combos :: TestItem
> combos = Group "combos" $ map (uncurry TestQueryExpr)
>     [("select a from t union select b from u"
>      ,CombineQueryExpr ms1 Union All Respectively ms2)

>     ,("select a from t intersect select b from u"
>      ,CombineQueryExpr ms1 Intersect All Respectively ms2)

>     ,("select a from t except all select b from u"
>      ,CombineQueryExpr ms1 Except All Respectively ms2)

>     ,("select a from t union distinct corresponding \
>       \select b from u"
>      ,CombineQueryExpr ms1 Union Distinct Corresponding ms2)

>     ,("select a from t union select a from t union select a from t"
>      -- TODO: union should be left associative. I think the others also
>      -- so this needs to be fixed (new optionSuffix variation which
>      -- handles this)
>      ,CombineQueryExpr ms1 Union All Respectively
>        (CombineQueryExpr ms1 Union All Respectively ms1))
>     ]
>   where
>     ms1 = makeSelect
>           {qeSelectList = [(Nothing,Iden "a")]
>           ,qeFrom = [TRSimple "t"]}
>     ms2 = makeSelect
>           {qeSelectList = [(Nothing,Iden "b")]
>           ,qeFrom = [TRSimple "u"]}


> withQueries :: TestItem
> withQueries = Group "with queries" $ map (uncurry TestQueryExpr)
>     [("with u as (select a from t) select a from u"
>      ,With False [(Alias "u" Nothing, ms1)] ms2)

>     ,("with u(b) as (select a from t) select a from u"
>      ,With False [(Alias "u" (Just ["b"]), ms1)] ms2)

>     ,("with x as (select a from t),\n\
>       \     u as (select a from x)\n\
>       \select a from u"
>      ,With False [(Alias "x" Nothing, ms1), (Alias "u" Nothing,ms3)] ms2)

>     ,("with recursive u as (select a from t) select a from u"
>      ,With True [(Alias "u" Nothing, ms1)] ms2)
>     ]
>  where
>    ms c t = makeSelect
>             {qeSelectList = [(Nothing,Iden c)]
>             ,qeFrom = [TRSimple t]}
>    ms1 = ms "a" "t"
>    ms2 = ms "a" "u"
>    ms3 = ms "a" "x"

> values :: TestItem
> values = Group "values" $ map (uncurry TestQueryExpr)
>     [("values (1,2),(3,4)"
>       ,Values [[NumLit "1", NumLit "2"]
>               ,[NumLit "3", NumLit "4"]])
>     ]

> tables :: TestItem
> tables = Group "tables" $ map (uncurry TestQueryExpr)
>     [("table tbl", Table "tbl")
>     ]
