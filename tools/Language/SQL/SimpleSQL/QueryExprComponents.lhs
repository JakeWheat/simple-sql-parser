
These are the tests for the query expression components apart from the
table refs which are in a separate file.


These are a few misc tests which don't fit anywhere else.

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
> duplicates = Group "duplicates" $ map (uncurry (TestQueryExpr ansi2011))
>     [("select a from t" ,ms SQDefault)
>     ,("select all a from t" ,ms All)
>     ,("select distinct a from t", ms Distinct)
>     ]
>  where
>    ms d = makeSelect
>           {qeSetQuantifier = d
>           ,qeSelectList = [(Iden [Name "a"],Nothing)]
>           ,qeFrom = [TRSimple [Name "t"]]}

> selectLists :: TestItem
> selectLists = Group "selectLists" $ map (uncurry (TestQueryExpr ansi2011))
>     [("select 1",
>       makeSelect {qeSelectList = [(NumLit "1",Nothing)]})

>     ,("select a"
>      ,makeSelect {qeSelectList = [(Iden [Name "a"],Nothing)]})

>     ,("select a,b"
>      ,makeSelect {qeSelectList = [(Iden [Name "a"],Nothing)
>                                  ,(Iden [Name "b"],Nothing)]})

>     ,("select 1+2,3+4"
>      ,makeSelect {qeSelectList =
>                      [(BinOp (NumLit "1") [Name "+"] (NumLit "2"),Nothing)
>                      ,(BinOp (NumLit "3") [Name "+"] (NumLit "4"),Nothing)]})

>     ,("select a as a, /*comment*/ b as b"
>      ,makeSelect {qeSelectList = [(Iden [Name "a"], Just $ Name "a")
>                                  ,(Iden [Name "b"], Just $ Name "b")]})

>     ,("select a a, b b"
>      ,makeSelect {qeSelectList = [(Iden [Name "a"], Just $ Name "a")
>                                  ,(Iden [Name "b"], Just $ Name "b")]})

>     ,("select a + b * c"
>      ,makeSelect {qeSelectList =
>       [(BinOp (Iden [Name "a"]) [Name "+"]
>         (BinOp (Iden [Name "b"]) [Name "*"] (Iden [Name "c"]))
>        ,Nothing)]})

>     ]

> whereClause :: TestItem
> whereClause = Group "whereClause" $ map (uncurry (TestQueryExpr ansi2011))
>     [("select a from t where a = 5"
>      ,makeSelect {qeSelectList = [(Iden [Name "a"],Nothing)]
>                  ,qeFrom = [TRSimple [Name "t"]]
>                  ,qeWhere = Just $ BinOp (Iden [Name "a"]) [Name "="] (NumLit "5")})
>     ]

> having :: TestItem
> having = Group "having" $ map (uncurry (TestQueryExpr ansi2011))
>     [("select a,sum(b) from t group by a having sum(b) > 5"
>      ,makeSelect {qeSelectList = [(Iden [Name "a"],Nothing)
>                                  ,(App [Name "sum"] [Iden [Name "b"]],Nothing)]
>                  ,qeFrom = [TRSimple [Name "t"]]
>                  ,qeGroupBy = [SimpleGroup $ Iden [Name "a"]]
>                  ,qeHaving = Just $ BinOp (App [Name "sum"] [Iden [Name "b"]])
>                                           [Name ">"] (NumLit "5")
>                  })
>     ]

> orderBy :: TestItem
> orderBy = Group "orderBy" $ map (uncurry (TestQueryExpr ansi2011))
>     [("select a from t order by a"
>      ,ms [SortSpec (Iden [Name "a"]) DirDefault NullsOrderDefault])

>     ,("select a from t order by a, b"
>      ,ms [SortSpec (Iden [Name "a"]) DirDefault NullsOrderDefault
>          ,SortSpec (Iden [Name "b"]) DirDefault NullsOrderDefault])

>     ,("select a from t order by a asc"
>      ,ms [SortSpec (Iden [Name "a"]) Asc NullsOrderDefault])

>     ,("select a from t order by a desc, b desc"
>      ,ms [SortSpec (Iden [Name "a"]) Desc NullsOrderDefault
>          ,SortSpec (Iden [Name "b"]) Desc NullsOrderDefault])

>     ,("select a from t order by a desc nulls first, b desc nulls last"
>      ,ms [SortSpec (Iden [Name "a"]) Desc NullsFirst
>          ,SortSpec (Iden [Name "b"]) Desc NullsLast])

>     ]
>   where
>     ms o = makeSelect {qeSelectList = [(Iden [Name "a"],Nothing)]
>                       ,qeFrom = [TRSimple [Name "t"]]
>                       ,qeOrderBy = o}

> offsetFetch :: TestItem
> offsetFetch = Group "offsetFetch" $ map (uncurry (TestQueryExpr ansi2011))
>     [-- ansi standard
>      ("select a from t offset 5 rows fetch next 10 rows only"
>      ,ms (Just $ NumLit "5") (Just $ NumLit "10"))
>     ,("select a from t offset 5 rows;"
>      ,ms (Just $ NumLit "5") Nothing)
>     ,("select a from t fetch next 10 row only;"
>      ,ms Nothing (Just $ NumLit "10"))
>     ,("select a from t offset 5 row fetch first 10 row only"
>      ,ms (Just $ NumLit "5") (Just $ NumLit "10"))
>      -- postgres: disabled, will add back when postgres
>      -- dialect is added
>     --,("select a from t limit 10 offset 5"
>     -- ,ms (Just $ NumLit "5") (Just $ NumLit "10"))
>     ]
>   where
>     ms o l = makeSelect
>              {qeSelectList = [(Iden [Name "a"],Nothing)]
>              ,qeFrom = [TRSimple [Name "t"]]
>              ,qeOffset = o
>              ,qeFetchFirst = l}

> combos :: TestItem
> combos = Group "combos" $ map (uncurry (TestQueryExpr ansi2011))
>     [("select a from t union select b from u"
>      ,CombineQueryExpr ms1 Union SQDefault Respectively ms2)

>     ,("select a from t intersect select b from u"
>      ,CombineQueryExpr ms1 Intersect SQDefault Respectively ms2)

>     ,("select a from t except all select b from u"
>      ,CombineQueryExpr ms1 Except All Respectively ms2)

>     ,("select a from t union distinct corresponding \
>       \select b from u"
>      ,CombineQueryExpr ms1 Union Distinct Corresponding ms2)

>     ,("select a from t union select a from t union select a from t"
>      -- TODO: union should be left associative. I think the others also
>      -- so this needs to be fixed (new optionSuffix variation which
>      -- handles this)
>      ,CombineQueryExpr ms1 Union SQDefault Respectively
>        (CombineQueryExpr ms1 Union SQDefault Respectively ms1))
>     ]
>   where
>     ms1 = makeSelect
>           {qeSelectList = [(Iden [Name "a"],Nothing)]
>           ,qeFrom = [TRSimple [Name "t"]]}
>     ms2 = makeSelect
>           {qeSelectList = [(Iden [Name "b"],Nothing)]
>           ,qeFrom = [TRSimple [Name "u"]]}


> withQueries :: TestItem
> withQueries = Group "with queries" $ map (uncurry (TestQueryExpr ansi2011))
>     [("with u as (select a from t) select a from u"
>      ,With False [(Alias (Name "u") Nothing, ms1)] ms2)

>     ,("with u(b) as (select a from t) select a from u"
>      ,With False [(Alias (Name "u") (Just [Name "b"]), ms1)] ms2)

>     ,("with x as (select a from t),\n\
>       \     u as (select a from x)\n\
>       \select a from u"
>      ,With False [(Alias (Name "x") Nothing, ms1), (Alias (Name "u") Nothing,ms3)] ms2)

>     ,("with recursive u as (select a from t) select a from u"
>      ,With True [(Alias (Name "u") Nothing, ms1)] ms2)
>     ]
>  where
>    ms c t = makeSelect
>             {qeSelectList = [(Iden [Name c],Nothing)]
>             ,qeFrom = [TRSimple [Name t]]}
>    ms1 = ms "a" "t"
>    ms2 = ms "a" "u"
>    ms3 = ms "a" "x"

> values :: TestItem
> values = Group "values" $ map (uncurry (TestQueryExpr ansi2011))
>     [("values (1,2),(3,4)"
>       ,Values [[NumLit "1", NumLit "2"]
>               ,[NumLit "3", NumLit "4"]])
>     ]

> tables :: TestItem
> tables = Group "tables" $ map (uncurry (TestQueryExpr ansi2011))
>     [("table tbl", Table [Name "tbl"])
>     ]
