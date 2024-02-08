
{-
These are the tests for the query expression components apart from the
table refs which are in a separate file.


These are a few misc tests which don't fit anywhere else.
-}

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.QueryExprComponents (queryExprComponentTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestRunners
import Data.Text (Text)

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
duplicates = Group "duplicates"
    [q "select a from t" $ ms SQDefault
    ,q "select all a from t" $ ms All
    ,q "select distinct a from t" $ ms Distinct
    ]
 where
   ms d = toQueryExpr $ makeSelect
          {msSetQuantifier = d
          ,msSelectList = [(Iden [Name Nothing "a"],Nothing)]
          ,msFrom = [TRSimple [Name Nothing "t"]]}

selectLists :: TestItem
selectLists = Group "selectLists"
    [q "select 1"
     $ toQueryExpr $ makeSelect {msSelectList = [(NumLit "1",Nothing)]}

    ,q "select a"
     $ toQueryExpr $ makeSelect {msSelectList = [(Iden [Name Nothing "a"],Nothing)]}

    ,q "select a,b"
     $ toQueryExpr $ makeSelect {msSelectList = [(Iden [Name Nothing "a"],Nothing)
                                 ,(Iden [Name Nothing "b"],Nothing)]}

    ,q "select 1+2,3+4"
     $ toQueryExpr $ makeSelect {msSelectList =
                     [(BinOp (NumLit "1") [Name Nothing "+"] (NumLit "2"),Nothing)
                     ,(BinOp (NumLit "3") [Name Nothing "+"] (NumLit "4"),Nothing)]}

    ,q "select a as a, /*comment*/ b as b"
     $ toQueryExpr $ makeSelect {msSelectList = [(Iden [Name Nothing "a"], Just $ Name Nothing "a")
                                 ,(Iden [Name Nothing "b"], Just $ Name Nothing "b")]}

    ,q "select a a, b b"
     $ toQueryExpr $ makeSelect {msSelectList = [(Iden [Name Nothing "a"], Just $ Name Nothing "a")
                                 ,(Iden [Name Nothing "b"], Just $ Name Nothing "b")]}

    ,q "select a + b * c"
     $ toQueryExpr $ makeSelect {msSelectList =
      [(BinOp (Iden [Name Nothing "a"]) [Name Nothing "+"]
        (BinOp (Iden [Name Nothing "b"]) [Name Nothing "*"] (Iden [Name Nothing "c"]))
       ,Nothing)]}
    ,q "select * from t"
     $ toQueryExpr $ makeSelect {msSelectList = [(Star,Nothing)]
                                ,msFrom = [TRSimple [Name Nothing "t"]]}

    ,q "select t.* from t"
     $ toQueryExpr $ makeSelect {msSelectList = [(QStar [Name Nothing "t"],Nothing)]
                                ,msFrom = [TRSimple [Name Nothing "t"]]}

    ,q "select t.*, a as b, u.* from t"
     $ toQueryExpr $ makeSelect
        {msSelectList =
         [(QStar [Name Nothing "t"],Nothing)
         ,(Iden [Name Nothing "a"], Just $ Name Nothing "b")
         ,(QStar [Name Nothing "u"],Nothing)]
        ,msFrom = [TRSimple [Name Nothing "t"]]}
    
    ]

whereClause :: TestItem
whereClause = Group "whereClause"
    [q "select a from t where a = 5"
     $ toQueryExpr $ makeSelect {msSelectList = [(Iden [Name Nothing "a"],Nothing)]
                 ,msFrom = [TRSimple [Name Nothing "t"]]
                 ,msWhere = Just $ BinOp (Iden [Name Nothing "a"]) [Name Nothing "="] (NumLit "5")}
    ]

having :: TestItem
having = Group "having"
    [q "select a,sum(b) from t group by a having sum(b) > 5"
     $ toQueryExpr $ makeSelect {msSelectList = [(Iden [Name Nothing "a"],Nothing)
                                 ,(App [Name Nothing "sum"] [Iden [Name Nothing "b"]],Nothing)]
                 ,msFrom = [TRSimple [Name Nothing "t"]]
                 ,msGroupBy = [SimpleGroup $ Iden [Name Nothing "a"]]
                 ,msHaving = Just $ BinOp (App [Name Nothing "sum"] [Iden [Name Nothing "b"]])
                                          [Name Nothing ">"] (NumLit "5")
                 }
    ]

orderBy :: TestItem
orderBy = Group "orderBy"
    [q "select a from t order by a"
     $ ms [SortSpec (Iden [Name Nothing "a"]) DirDefault NullsOrderDefault]

    ,q "select a from t order by a, b"
     $ ms [SortSpec (Iden [Name Nothing "a"]) DirDefault NullsOrderDefault
         ,SortSpec (Iden [Name Nothing "b"]) DirDefault NullsOrderDefault]

    ,q "select a from t order by a asc"
     $ ms [SortSpec (Iden [Name Nothing "a"]) Asc NullsOrderDefault]

    ,q "select a from t order by a desc, b desc"
     $ ms [SortSpec (Iden [Name Nothing "a"]) Desc NullsOrderDefault
         ,SortSpec (Iden [Name Nothing "b"]) Desc NullsOrderDefault]

    ,q "select a from t order by a desc nulls first, b desc nulls last"
     $ ms [SortSpec (Iden [Name Nothing "a"]) Desc NullsFirst
         ,SortSpec (Iden [Name Nothing "b"]) Desc NullsLast]

    ]
  where
    ms o = toQueryExpr $ makeSelect {msSelectList = [(Iden [Name Nothing "a"],Nothing)]
                      ,msFrom = [TRSimple [Name Nothing "t"]]
                      ,msOrderBy = o}

offsetFetch :: TestItem
offsetFetch = Group "offsetFetch"
    [-- ansi standard
     q "select a from t offset 5 rows fetch next 10 rows only"
     $ ms (Just $ NumLit "5") (Just $ NumLit "10")
    ,q "select a from t offset 5 rows;"
     $ ms (Just $ NumLit "5") Nothing
    ,q "select a from t fetch next 10 row only;"
     $ ms Nothing (Just $ NumLit "10")
    ,q "select a from t offset 5 row fetch first 10 row only"
     $ ms (Just $ NumLit "5") (Just $ NumLit "10")
     -- postgres: disabled, will add back when postgres
     -- dialect is added
    --,q "select a from t limit 10 offset 5"
    -- $ ms (Just $ NumLit "5") (Just $ NumLit "10"))
    ]
  where
    ms o l = toQueryExpr $ makeSelect
             {msSelectList = [(Iden [Name Nothing "a"],Nothing)]
             ,msFrom = [TRSimple [Name Nothing "t"]]
             ,msOffset = o
             ,msFetchFirst = l}

combos :: TestItem
combos = Group "combos"
    [q "select a from t union select b from u"
     $ QueryExprSetOp mst Union SQDefault Respectively msu

    ,q "select a from t intersect select b from u"
     $ QueryExprSetOp mst Intersect SQDefault Respectively msu

    ,q "select a from t except all select b from u"
     $ QueryExprSetOp mst Except All Respectively msu

    ,q "select a from t union distinct corresponding \
      \select b from u"
     $ QueryExprSetOp mst Union Distinct Corresponding msu

    ,q "select a from t union select a from t union select a from t"
     $ QueryExprSetOp (QueryExprSetOp mst Union SQDefault Respectively mst)
       Union SQDefault Respectively mst
    ]
  where
    mst = toQueryExpr $ makeSelect
          {msSelectList = [(Iden [Name Nothing "a"],Nothing)]
          ,msFrom = [TRSimple [Name Nothing "t"]]}
    msu = toQueryExpr $ makeSelect
          {msSelectList = [(Iden [Name Nothing "b"],Nothing)]
          ,msFrom = [TRSimple [Name Nothing "u"]]}


withQueries :: TestItem
withQueries = Group "with queries"
    [q "with u as (select a from t) select a from u"
     $ With False [(Alias (Name Nothing "u") Nothing, ms1)] ms2

    ,q "with u(b) as (select a from t) select a from u"
     $ With False [(Alias (Name Nothing "u") (Just [Name Nothing "b"]), ms1)] ms2

    ,q "with x as (select a from t),\n\
      \     u as (select a from x)\n\
      \select a from u"
     $ With False [(Alias (Name Nothing "x") Nothing, ms1), (Alias (Name Nothing "u") Nothing,ms3)] ms2

    ,q "with recursive u as (select a from t) select a from u"
     $ With True [(Alias (Name Nothing "u") Nothing, ms1)] ms2
    ]
 where
   ms c t = toQueryExpr $ makeSelect
            {msSelectList = [(Iden [Name Nothing c],Nothing)]
            ,msFrom = [TRSimple [Name Nothing t]]}
   ms1 = ms "a" "t"
   ms2 = ms "a" "u"
   ms3 = ms "a" "x"

values :: TestItem
values = Group "values"
    [q "values (1,2),(3,4)"
     $ Values [[NumLit "1", NumLit "2"]
              ,[NumLit "3", NumLit "4"]]
    ]

tables :: TestItem
tables = Group "tables"
    [q "table tbl" $ Table [Name Nothing "tbl"]
    ]

q :: HasCallStack => Text -> QueryExpr -> TestItem
q src ast = testQueryExpr ansi2011 src ast
