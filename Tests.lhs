
> module Tests (testData, runTests) where

> import Language.SQL.SimpleSQL.Syntax
> import Language.SQL.SimpleSQL.Pretty
> import Language.SQL.SimpleSQL.Parser
> import qualified Test.HUnit as H
> import Control.Monad
> import Tpch

> data TestItem = Group String [TestItem]
>               | TestScalarExpr String ScalarExpr
>               | TestQueryExpr String QueryExpr
>               | ParseQueryExpr String
>                 deriving (Eq,Show)

> scalarExprParserTests :: TestItem
> scalarExprParserTests = Group "scalarExprParserTests"
>     [literals
>     ,identifiers
>     ,star
>     ,app
>     ,caseexp
>     ,operators
>     ,parens
>     ,subqueries
>     ,aggregates
>     ,windowFunctions
>     ]

> literals :: TestItem
> literals = Group "literals" $ map (uncurry TestScalarExpr)
>     [("3", NumLit "3")
>      ,("3.", NumLit "3.")
>      ,("3.3", NumLit "3.3")
>      ,(".3", NumLit ".3")
>      ,("3.e3", NumLit "3.e3")
>      ,("3.3e3", NumLit "3.3e3")
>      ,(".3e3", NumLit ".3e3")
>      ,("3e3", NumLit "3e3")
>      ,("3e+3", NumLit "3e+3")
>      ,("3e-3", NumLit "3e-3")
>     ,("'string'", StringLit "string")
>     ,("'1'", StringLit "1")
>     ]

> identifiers :: TestItem
> identifiers = Group "identifiers" $ map (uncurry TestScalarExpr)
>     [("iden1", Iden "iden1")
>     ,("t.a", Iden2 "t" "a")
>     ]

> star :: TestItem
> star = Group "star" $ map (uncurry TestScalarExpr)
>     [("*", Star)
>     ,("t.*", Star2 "t")
>     ]

> app :: TestItem
> app = Group "app" $ map (uncurry TestScalarExpr)
>     [("f()", App "f" [])
>     ,("f(a)", App "f" [Iden "a"])
>     ,("f(a,b)", App "f" [Iden "a", Iden "b"])
>     ]

> caseexp :: TestItem
> caseexp = Group "caseexp" $ map (uncurry TestScalarExpr)
>     [("case a when 1 then 2 end"
>      ,Case (Just $ Iden "a") [(NumLit "1"
>                                     ,NumLit "2")] Nothing)
>     ,("case a when 1 then 2 when 3 then 4 end"
>      ,Case (Just $ Iden "a") [(NumLit "1", NumLit "2")
>                                    ,(NumLit "3", NumLit "4")] Nothing)
>     ,("case a when 1 then 2 when 3 then 4 else 5 end"
>      ,Case (Just $ Iden "a") [(NumLit "1", NumLit "2")
>                                    ,(NumLit "3", NumLit "4")] (Just $ NumLit "5"))
>     ,("case when a=1 then 2 when a=3 then 4 else 5 end"
>      ,Case Nothing [(Op "=" [Iden "a", NumLit "1"], NumLit "2")
>                    ,(Op "=" [Iden "a",NumLit "3"], NumLit "4")]
>                    (Just $ NumLit "5"))
>     ]

> operators :: TestItem
> operators = Group "operators"
>     [binaryOperators
>     ,unaryOperators
>     ,casts
>     ,miscOps]

> binaryOperators :: TestItem
> binaryOperators = Group "binaryOperators" $ map (uncurry TestScalarExpr)
>     [("a + b", Op "+" [Iden "a", Iden "b"])
>      -- sanity check fixities
>      -- todo: add more fixity checking
>     ,("a + b * c"
>      ,Op "+" [Iden "a"
>              ,Op "*" [Iden "b"
>                      ,Iden "c"]])
>     ,("a * b + c"
>      ,Op "+" [Op "*" [Iden "a", Iden "b"]
>              ,Iden "c"])
>     ]

> unaryOperators :: TestItem
> unaryOperators = Group "unaryOperators" $ map (uncurry TestScalarExpr)
>     [("not a", Op "not" [Iden "a"])
>     ,("not not a", Op "not" [Op "not" [Iden "a"]])
>     ,("+a", Op "+" [Iden "a"])
>     ,("-a", Op "-" [Iden "a"])
>     ]


> casts :: TestItem
> casts = Group "operators" $ map (uncurry TestScalarExpr)
>     [("cast('1' as int)"
>      ,Cast (StringLit "1") $ TypeName "int")
>     ,("int '3'"
>      ,CastOp "1" $ TypeName "int")
>     ,("cast('1' as double precision)"
>      ,Cast (StringLit "1") $ TypeName "double precision")
>     ,("double precision '3'"
>      ,CastOp "1" $ TypeName "double precision")
>     ]

> subqueries :: TestItem
> subqueries = Group "unaryOperators" $ map (uncurry TestScalarExpr)
>     [("exists (select * from t)", Op "not" [Iden "a"])
>     ,("(select a from t)", Op "not" [Op "not" [Iden "a"]])
>     ,("in (select a from t)", Op "+" [Iden "a"])
>     ,("not in (select a from t)", Op "+" [Iden "a"])
>     ,("a > ALL (select a from t)", Op "-" [Iden "a"])
>     ,("a > SOME (select a from t)", Op "-" [Iden "a"])
>     ,("a > ANY (select a from t)", Op "-" [Iden "a"])
>     ]

> miscOps :: TestItem
> miscOps = Group "unaryOperators" $ map (uncurry TestScalarExpr)
>     [("a in (1,2,3)", Op "not" [Iden "a"])
>     ,("a between b and c", Op "not" [])
>     ,("a not between b and c", Op "not" [])
>     ,("a is null", Op "not" [])
>     ,("a is not null", Op "not" [])
>     ,("a is distinct from b", Op "not" [])
>     ,("a is not distinct from b", Op "not" [])
>     ,("a is true", Op "not" [])
>     ,("a s not true", Op "not" [])
>     ,("a is false", Op "not" [])
>     ,("a is not false", Op "not" [])
>     ,("a is unknown", Op "not" [])
>     ,("a is not unknown", Op "not" [])
>     ,("a like b", Op "not" [])
>     ,("a not like b", Op "not" [])
>     ,("a is similar to b", Op "not" [])
>     ,("a is not similar to b", Op "not" [])
>     ,("a overlaps b", Op "not" [])
>     ,("extract(day from t)", Op "not" [])
>     ]

> aggregates :: TestItem
> aggregates = Group "aggregates" $ map (uncurry TestScalarExpr)
>     [("count(*)",NumLit "1")
>     ,("sum(a order by a)",NumLit "1")
>     ,("sum(all a)",NumLit "1")
>     ,("count(distinct a)",NumLit "1")
>     ]

> windowFunctions :: TestItem
> windowFunctions = Group "windowFunctions" $ map (uncurry TestScalarExpr)
>     [("max(a) over ()", NumLit "1")
>     ,("count(*) over ()", NumLit "1")
>     ,("max(a) over (partition by b)", NumLit "1")
>     ,("sum(a) over (order by b)", NumLit "1")
>     ,("sum(a) over (partition by b order by c)", NumLit "1")
>     ,("sum(a) over (partition by b order by c)", NumLit "1")
>      -- todo: check order by options, add frames
>     ]

> parens :: TestItem
> parens = Group "parens" $ map (uncurry TestScalarExpr)
>     [("(a)", Parens (Iden "a"))
>     ,("(a + b)", Parens (Op "+" [Iden "a", Iden "b"]))
>     ]

> queryExprParserTests :: TestItem
> queryExprParserTests = Group "queryExprParserTests"
>     [duplicates
>     ,selectLists
>     ,from
>     ,whereClause
>     ,groupByClause
>     ,having
>     ,orderBy
>     ,limit
>     ,combos
>     ,fullQueries
>     ]



> duplicates :: TestItem
> duplicates = Group "duplicates" $ map (uncurry TestQueryExpr)
>     [("select a from t" ,ms All)
>     ,("select all a from t" ,ms All)
>     ,("select distinct a from t", ms Distinct)
>     ]
>  where
>    ms d = makeSelect
>           {qeDuplicates = d
>           ,qeSelectList = [(Nothing,Iden "a")]
>           ,qeFrom = [SimpleTableRef "t"]}

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
>                      [(Nothing,Op "+" [NumLit "1",NumLit "2"])
>                      ,(Nothing,Op "+" [NumLit "3",NumLit "4"])]})
>     ,("select a as a, /*comment*/ b as b"
>      ,makeSelect {qeSelectList = [(Just "a", Iden "a")
>                                  ,(Just "b", Iden "b")]})
>     ,("select a a, b b"
>      ,makeSelect {qeSelectList = [(Just "a", Iden "a")
>                                  ,(Just "b", Iden "b")]})
>     ]

> from :: TestItem
> from = Group "from" $ map (uncurry TestQueryExpr)
>     [("select a from t"
>      ,ms [SimpleTableRef "t"])
>     ,("select a from t,u"
>      ,ms [SimpleTableRef "t", SimpleTableRef "u"])
>     ,("select a from t inner join u on expr"
>      ,ms [JoinTableRef Inner (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinOn $ Iden "expr")])
>     ,("select a from t left join u on expr"
>      ,ms [JoinTableRef JLeft (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinOn $ Iden "expr")])
>     ,("select a from t right join u on expr"
>      ,ms [JoinTableRef JRight (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinOn $ Iden "expr")])
>     ,("select a from t full join u on expr"
>      ,ms [JoinTableRef Full (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinOn $ Iden "expr")])
>     ,("select a from t cross join u"
>      ,ms [JoinTableRef Cross (SimpleTableRef "t")
>                             (SimpleTableRef "u") Nothing])
>     ,("select a from t natural inner join u"
>      ,ms [JoinTableRef Inner (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just JoinNatural)])
>     ,("select a from t inner join u using(a,b)"
>      ,ms [JoinTableRef Inner (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinUsing ["a", "b"])])
>     ,("select a from (select a from t)"
>      ,ms [JoinQueryExpr $ ms [SimpleTableRef "t"]])
>     ,("select a from t as u"
>      ,ms [JoinAlias (SimpleTableRef "t") "u"])
>     ,("select a from t u"
>      ,ms [JoinAlias (SimpleTableRef "t") "u"])
>     ,("select a from (t cross join u) as u"
>      ,ms [JoinAlias (JoinParens $ JoinTableRef Cross (SimpleTableRef "t")
>                             (SimpleTableRef "u") Nothing) "u"])
>     ]
>   where
>     ms f = makeSelect {qeSelectList = [(Nothing,Iden "a")]
>                       ,qeFrom = f}

> whereClause :: TestItem
> whereClause = Group "whereClause" $ map (uncurry TestQueryExpr)
>     [("select a from t where a = 5"
>      ,makeSelect {qeSelectList = [(Nothing,Iden "a")]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeWhere = Just $ Op "=" [Iden "a", NumLit "5"]})
>     ]

> groupByClause :: TestItem
> groupByClause = Group "groupByClause" $ map (uncurry TestQueryExpr)
>     [("select a,sum(b) from t group by a"
>      ,makeSelect {qeSelectList = [(Nothing, Iden "a")
>                                  ,(Nothing, App "sum" [Iden "b"])]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeGroupBy = [Iden "a"]
>                  })
>     ,("select a,b,sum(c) from t group by a,b"
>      ,makeSelect {qeSelectList = [(Nothing, Iden "a")
>                                  ,(Nothing, Iden "b")
>                                  ,(Nothing, App "sum" [Iden "c"])]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeGroupBy = [Iden "a",Iden "b"]
>                  })
>     ]

> having :: TestItem
> having = Group "having" $ map (uncurry TestQueryExpr)
>     [("select a,sum(b) from t group by a having sum(b) > 5"
>      ,makeSelect {qeSelectList = [(Nothing, Iden "a")
>                                  ,(Nothing, App "sum" [Iden "b"])]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeGroupBy = [Iden "a"]
>                  ,qeHaving = Just $ Op ">" [App "sum" [Iden "b"], NumLit "5"]
>                  })
>     ]

> orderBy :: TestItem
> orderBy = Group "orderBy" $ map (uncurry TestQueryExpr)
>     [("select a from t order by a"
>      ,ms [(Iden "a", Asc)])
>     ,("select a from t order by a, b"
>      ,ms [(Iden "a", Asc), (Iden "b", Asc)])
>     ,("select a from t order by a asc"
>      ,ms [(Iden "a", Asc)])
>     ,("select a from t order by a desc, b desc"
>      ,ms [(Iden "a", Desc), (Iden "b", Desc)])
>     ]
>   where
>     ms o = makeSelect {qeSelectList = [(Nothing,Iden "a")]
>                       ,qeFrom = [SimpleTableRef "t"]
>                       ,qeOrderBy = o}

> limit :: TestItem
> limit = Group "limit" $ map (uncurry TestQueryExpr)
>     [("select a from t limit 10"
>      ,ms (Just $ NumLit "10") Nothing)
>     ,("select a from t limit 10 offset 10"
>      ,ms (Just $ NumLit "10") (Just $ NumLit "10"))
>     ]
>   where
>     ms l o = makeSelect
>              {qeSelectList = [(Nothing,Iden "a")]
>              ,qeFrom = [SimpleTableRef "t"]
>              ,qeLimit = l
>              ,qeOffset = o}

> combos :: TestItem
> combos = Group "combos" $ map (uncurry TestQueryExpr)
>     [("select a from t union select b from u"
>      ,makeSelect)
>     ,("select a from t intersect select b from u"
>      ,makeSelect)
>     ,("select a from t except select b from u"
>      ,makeSelect)
>     ]

> fullQueries :: TestItem
> fullQueries = Group "queries" $ map (uncurry TestQueryExpr)
>     [("select count(*) from t"
>      ,makeSelect
>       {qeSelectList = [(Nothing, App "count" [Star])]
>       ,qeFrom = [SimpleTableRef "t"]
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
>                       ,(Just "s", App "sum" [Op "+" [Iden "c"
>                                                     ,Iden "d"]])]
>       ,qeFrom = [SimpleTableRef "t", SimpleTableRef "u"]
>       ,qeWhere = Just $ Op ">" [Iden "a", NumLit "5"]
>       ,qeGroupBy = [Iden "a"]
>       ,qeHaving = Just $ Op ">" [App "count" [NumLit "1"]
>                                 ,NumLit "5"]
>       ,qeOrderBy = [(Iden "s", Asc)]
>       }
>      )
>     ]

> tpchTests :: TestItem
> tpchTests =
>     Group "parse tpch"
>     $ map (ParseQueryExpr . snd) tpchQueries

> testData :: TestItem
> testData =
>     Group "parserTest"
>     [scalarExprParserTests
>     ,queryExprParserTests
>     ,tpchTests]


> runTests :: IO ()
> runTests = void $ H.runTestTT $ itemToTest testData

> itemToTest :: TestItem -> H.Test
> itemToTest (Group nm ts) =
>     H.TestLabel nm $ H.TestList $ map itemToTest ts
> itemToTest (TestScalarExpr str expected) =
>     toTest parseScalarExpr prettyScalarExpr str expected
> itemToTest (TestQueryExpr str expected) =
>     toTest parseQueryExpr prettyQueryExpr str expected
> itemToTest (ParseQueryExpr str) =
>     toPTest parseQueryExpr prettyQueryExpr str

> toTest :: (Eq a, Show a, Show e) =>
>           (String -> Maybe (Int,Int) -> String -> Either e a)
>        -> (a -> String)
>        -> String
>        -> a
>        -> H.Test
> toTest parser pp str expected = H.TestLabel str $ H.TestCase $ do
>         let egot = parser "" Nothing str
>         case egot of
>             Left e -> H.assertFailure $ show e
>             Right got -> do
>                 H.assertEqual "" expected got
>                 let str' = pp got
>                 let egot' = parser "" Nothing str'
>                 case egot' of
>                     Left e' -> H.assertFailure $ "pp roundtrip " ++ show e'
>                     Right got' -> H.assertEqual "pp roundtrip" expected got'

> toPTest :: (Eq a, Show a, Show e) =>
>           (String -> Maybe (Int,Int) -> String -> Either e a)
>        -> (a -> String)
>        -> String
>        -> H.Test
> toPTest parser pp str = H.TestLabel str $ H.TestCase $ do
>         let egot = parser "" Nothing str
>         case egot of
>             Left e -> H.assertFailure $ show e
>             Right got -> do
>                 let str' = pp got
>                 let egot' = parser "" Nothing str'
>                 case egot' of
>                     Left e' -> H.assertFailure $ "pp roundtrip " ++ show e'
>                     Right got' -> return ()
