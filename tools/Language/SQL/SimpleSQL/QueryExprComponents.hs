
{-
These are the tests for the query expression components apart from the
table refs which are in a separate file.


These are a few misc tests which don't fit anywhere else.
-}

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.QueryExprComponents (queryExprComponentTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax


queryExprComponentTests :: TestItem
queryExprComponentTests = Group "queryExprComponentTests"
    [duplicates
    ,selectLists
    ,whereClause
    ,having
    ,orderBy
    ,offsetFetch
    ,combos
    ,withQueries
    ,values
    ,tables
    ]



duplicates :: TestItem
duplicates = Group "duplicates" $ map (uncurry (TestQueryExpr ansi2011))
    [("select a from t" ,ms SQDefault)
    ,("select all a from t" ,ms All)
    ,("select distinct a from t", ms Distinct)
    ]
 where
   ms d = makeSelect
          {qeSetQuantifier = d
          ,qeSelectList = [(Iden [Name Nothing "a"],Nothing)]
          ,qeFrom = [TRSimple [Name Nothing "t"]]}

selectLists :: TestItem
selectLists = Group "selectLists" $ map (uncurry (TestQueryExpr ansi2011))
    [("select 1",
      makeSelect {qeSelectList = [(NumLit "1",Nothing)]})

    ,("select a"
     ,makeSelect {qeSelectList = [(Iden [Name Nothing "a"],Nothing)]})

    ,("select a,b"
     ,makeSelect {qeSelectList = [(Iden [Name Nothing "a"],Nothing)
                                 ,(Iden [Name Nothing "b"],Nothing)]})

    ,("select 1+2,3+4"
     ,makeSelect {qeSelectList =
                     [(BinOp (NumLit "1") [Name Nothing "+"] (NumLit "2"),Nothing)
                     ,(BinOp (NumLit "3") [Name Nothing "+"] (NumLit "4"),Nothing)]})

    ,("select a as a, /*comment*/ b as b"
     ,makeSelect {qeSelectList = [(Iden [Name Nothing "a"], Just $ Name Nothing "a")
                                 ,(Iden [Name Nothing "b"], Just $ Name Nothing "b")]})

    ,("select a a, b b"
     ,makeSelect {qeSelectList = [(Iden [Name Nothing "a"], Just $ Name Nothing "a")
                                 ,(Iden [Name Nothing "b"], Just $ Name Nothing "b")]})

    ,("select a + b * c"
     ,makeSelect {qeSelectList =
      [(BinOp (Iden [Name Nothing "a"]) [Name Nothing "+"]
        (BinOp (Iden [Name Nothing "b"]) [Name Nothing "*"] (Iden [Name Nothing "c"]))
       ,Nothing)]})

    ]

whereClause :: TestItem
whereClause = Group "whereClause" $ map (uncurry (TestQueryExpr ansi2011))
    [("select a from t where a = 5"
     ,makeSelect {qeSelectList = [(Iden [Name Nothing "a"],Nothing)]
                 ,qeFrom = [TRSimple [Name Nothing "t"]]
                 ,qeWhere = Just $ BinOp (Iden [Name Nothing "a"]) [Name Nothing "="] (NumLit "5")})
    ]

having :: TestItem
having = Group "having" $ map (uncurry (TestQueryExpr ansi2011))
    [("select a,sum(b) from t group by a having sum(b) > 5"
     ,makeSelect {qeSelectList = [(Iden [Name Nothing "a"],Nothing)
                                 ,(App [Name Nothing "sum"] [Iden [Name Nothing "b"]],Nothing)]
                 ,qeFrom = [TRSimple [Name Nothing "t"]]
                 ,qeGroupBy = [SimpleGroup $ Iden [Name Nothing "a"]]
                 ,qeHaving = Just $ BinOp (App [Name Nothing "sum"] [Iden [Name Nothing "b"]])
                                          [Name Nothing ">"] (NumLit "5")
                 })
    ]

orderBy :: TestItem
orderBy = Group "orderBy" $ map (uncurry (TestQueryExpr ansi2011))
    [("select a from t order by a"
     ,ms [SortSpec (Iden [Name Nothing "a"]) DirDefault NullsOrderDefault])

    ,("select a from t order by a, b"
     ,ms [SortSpec (Iden [Name Nothing "a"]) DirDefault NullsOrderDefault
         ,SortSpec (Iden [Name Nothing "b"]) DirDefault NullsOrderDefault])

    ,("select a from t order by a asc"
     ,ms [SortSpec (Iden [Name Nothing "a"]) Asc NullsOrderDefault])

    ,("select a from t order by a desc, b desc"
     ,ms [SortSpec (Iden [Name Nothing "a"]) Desc NullsOrderDefault
         ,SortSpec (Iden [Name Nothing "b"]) Desc NullsOrderDefault])

    ,("select a from t order by a desc nulls first, b desc nulls last"
     ,ms [SortSpec (Iden [Name Nothing "a"]) Desc NullsFirst
         ,SortSpec (Iden [Name Nothing "b"]) Desc NullsLast])

    ]
  where
    ms o = makeSelect {qeSelectList = [(Iden [Name Nothing "a"],Nothing)]
                      ,qeFrom = [TRSimple [Name Nothing "t"]]
                      ,qeOrderBy = o}

offsetFetch :: TestItem
offsetFetch = Group "offsetFetch" $ map (uncurry (TestQueryExpr ansi2011))
    [-- ansi standard
     ("select a from t offset 5 rows fetch next 10 rows only"
     ,ms (Just $ NumLit "5") (Just $ NumLit "10"))
    ,("select a from t offset 5 rows;"
     ,ms (Just $ NumLit "5") Nothing)
    ,("select a from t fetch next 10 row only;"
     ,ms Nothing (Just $ NumLit "10"))
    ,("select a from t offset 5 row fetch first 10 row only"
     ,ms (Just $ NumLit "5") (Just $ NumLit "10"))
     -- postgres: disabled, will add back when postgres
     -- dialect is added
    --,("select a from t limit 10 offset 5"
    -- ,ms (Just $ NumLit "5") (Just $ NumLit "10"))
    ]
  where
    ms o l = makeSelect
             {qeSelectList = [(Iden [Name Nothing "a"],Nothing)]
             ,qeFrom = [TRSimple [Name Nothing "t"]]
             ,qeOffset = o
             ,qeFetchFirst = l}

combos :: TestItem
combos = Group "combos" $ map (uncurry (TestQueryExpr ansi2011))
    [("select a from t union select b from u"
     ,QueryExprSetOp ms1 Union SQDefault Respectively ms2)

    ,("select a from t intersect select b from u"
     ,QueryExprSetOp ms1 Intersect SQDefault Respectively ms2)

    ,("select a from t except all select b from u"
     ,QueryExprSetOp ms1 Except All Respectively ms2)

    ,("select a from t union distinct corresponding \
      \select b from u"
     ,QueryExprSetOp ms1 Union Distinct Corresponding ms2)

    ,("select a from t union select a from t union select a from t"
     -- TODO: union should be left associative. I think the others also
     -- so this needs to be fixed (new optionSuffix variation which
     -- handles this)
     ,QueryExprSetOp ms1 Union SQDefault Respectively
       (QueryExprSetOp ms1 Union SQDefault Respectively ms1))
    ]
  where
    ms1 = makeSelect
          {qeSelectList = [(Iden [Name Nothing "a"],Nothing)]
          ,qeFrom = [TRSimple [Name Nothing "t"]]}
    ms2 = makeSelect
          {qeSelectList = [(Iden [Name Nothing "b"],Nothing)]
          ,qeFrom = [TRSimple [Name Nothing "u"]]}


withQueries :: TestItem
withQueries = Group "with queries" $ map (uncurry (TestQueryExpr ansi2011))
    [("with u as (select a from t) select a from u"
     ,With False [(Alias (Name Nothing "u") Nothing, ms1)] ms2)

    ,("with u(b) as (select a from t) select a from u"
     ,With False [(Alias (Name Nothing "u") (Just [Name Nothing "b"]), ms1)] ms2)

    ,("with x as (select a from t),\n\
      \     u as (select a from x)\n\
      \select a from u"
     ,With False [(Alias (Name Nothing "x") Nothing, ms1), (Alias (Name Nothing "u") Nothing,ms3)] ms2)

    ,("with recursive u as (select a from t) select a from u"
     ,With True [(Alias (Name Nothing "u") Nothing, ms1)] ms2)
    ]
 where
   ms c t = makeSelect
            {qeSelectList = [(Iden [Name Nothing c],Nothing)]
            ,qeFrom = [TRSimple [Name Nothing t]]}
   ms1 = ms "a" "t"
   ms2 = ms "a" "u"
   ms3 = ms "a" "x"

values :: TestItem
values = Group "values" $ map (uncurry (TestQueryExpr ansi2011))
    [("values (1,2),(3,4)"
      ,Values [[NumLit "1", NumLit "2"]
              ,[NumLit "3", NumLit "4"]])
    ]

tables :: TestItem
tables = Group "tables" $ map (uncurry (TestQueryExpr ansi2011))
    [("table tbl", Table [Name Nothing "tbl"])
    ]
