
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
>      ,("'string'", StringLit "string")
>      ,("'1'", StringLit "1")
>      ,("interval '3' day", IntervalLit "3" "day" Nothing)
>      ,("interval '3' day (3)", IntervalLit "3" "day" $ Just 3)
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
>      ,Case Nothing [(BinOp "=" (Iden "a") (NumLit "1"), NumLit "2")
>                    ,(BinOp "=" (Iden "a") (NumLit "3"), NumLit "4")]
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
>     [("a + b", BinOp "+" (Iden "a") (Iden "b"))
>      -- sanity check fixities
>      -- todo: add more fixity checking
>     {-,("a + b * c"
>      ,Op "+" [Iden "a"
>              ,Op "*" [Iden "b"
>                      ,Iden "c"]])
>     ,("a * b + c"
>      ,Op "+" [Op "*" [Iden "a", Iden "b"]
>              ,Iden "c"])-}
>     ]

> unaryOperators :: TestItem
> unaryOperators = Group "unaryOperators" $ map (uncurry TestScalarExpr)
>     [("not a", PrefixOp "not" $ Iden "a")
>     ,("not not a", PrefixOp "not" $ PrefixOp "not" $ Iden "a")
>     ,("+a", PrefixOp "+" $ Iden "a")
>     ,("-a", PrefixOp "-" $ Iden "a")
>     ]


> casts :: TestItem
> casts = Group "operators" $ map (uncurry TestScalarExpr)
>     [("cast('1' as int)"
>      ,Cast (StringLit "1") $ TypeName "int")
>     ,("int '3'"
>      ,CastOp (TypeName "int") "3")
>     ,("cast('1' as double precision)"
>      ,Cast (StringLit "1") $ TypeName "double precision")
>     ,("double precision '3'"
>      ,CastOp (TypeName "double precision") "3")
>     ]

> subqueries :: TestItem
> subqueries = Group "unaryOperators" $ map (uncurry TestScalarExpr)
>     [("exists (select a from t)", SubQueryExpr SqExists ms)
>     ,("(select a from t)", SubQueryExpr SqSq ms)
>     ,("a in (select a from t)"
>      ,In True (Iden "a") (InQueryExpr ms))
>     ,("a not in (select a from t)"
>      ,In False (Iden "a") (InQueryExpr ms))
>     ,("a > all (select a from t)"
>      ,BinOp ">" (Iden "a") (SubQueryExpr SqAll ms))
>     ,("a = some (select a from t)"
>      ,BinOp "=" (Iden "a") (SubQueryExpr SqSome ms))
>     ,("a <= any (select a from t)"
>      ,BinOp "<=" (Iden "a") (SubQueryExpr SqAny ms))
>     ]
>   where
>     ms = makeSelect
>          {qeSelectList = [(Nothing,Iden "a")]
>          ,qeFrom = [SimpleTableRef "t"]
>          }

> miscOps :: TestItem
> miscOps = Group "unaryOperators" $ map (uncurry TestScalarExpr)
>     [("a in (1,2,3)"
>      ,In True (Iden "a") $ InList $ map NumLit ["1","2","3"])
>     ,("a between b and c", SpecialOp "between" [Iden "a"
>                                                ,Iden "b"
>                                                ,Iden "c"])
>     ,("a not between b and c", SpecialOp "not between" [Iden "a"
>                                                        ,Iden "b"
>                                                        ,Iden "c"])
>     ,("a is null", PostfixOp "is null" (Iden "a"))
>     ,("a is not null", PostfixOp "is not null" (Iden "a"))
>     ,("a is true", PostfixOp "is true" (Iden "a"))
>     ,("a is not true", PostfixOp "is not true" (Iden "a"))
>     ,("a is false", PostfixOp "is false" (Iden "a"))
>     ,("a is not false", PostfixOp "is not false" (Iden "a"))
>     ,("a is unknown", PostfixOp "is unknown" (Iden "a"))
>     ,("a is not unknown", PostfixOp "is not unknown" (Iden "a"))
>     ,("a is distinct from b", BinOp "is distinct from" (Iden "a") (Iden "b"))
>     ,("a is not distinct from b", BinOp "is not distinct from" (Iden "a") (Iden "b"))
>     ,("a like b", BinOp "like" (Iden "a") (Iden "b"))
>     ,("a not like b", BinOp "not like" (Iden "a") (Iden "b"))
>     ,("a is similar to b", BinOp "is similar to" (Iden "a") (Iden "b"))
>     ,("a is not similar to b", BinOp "is not similar to" (Iden "a") (Iden "b"))
>     ,("a overlaps b", BinOp "overlaps" (Iden "a") (Iden "b"))
>     ,("extract(day from t)", SpecialOp "extract" [Iden "day", Iden "t"])
>     ,("substring(x from 1 for 2)"
>      ,SpecialOp "substring" [Iden "x", NumLit "1", NumLit "2"])
>     ]

> aggregates :: TestItem
> aggregates = Group "aggregates" $ map (uncurry TestScalarExpr)
>     [("count(*)",App "count" [Star])
>     ,("sum(a order by a)"
>     ,AggregateApp "sum" Nothing [Iden "a"] [(Iden "a", Asc)])
>     ,("sum(all a)"
>     ,AggregateApp "sum" (Just All) [Iden "a"] [])
>     ,("count(distinct a)"
>     ,AggregateApp "count" (Just Distinct) [Iden "a"] [])
>     ]

> windowFunctions :: TestItem
> windowFunctions = Group "windowFunctions" $ map (uncurry TestScalarExpr)
>     [("max(a) over ()", WindowApp "max" [Iden "a"] [] [])
>     ,("count(*) over ()", WindowApp "count" [Star] [] [])
>     ,("max(a) over (partition by b)"
>      ,WindowApp "max" [Iden "a"] [Iden "b"] [])
>     ,("max(a) over (partition by b,c)"
>      ,WindowApp "max" [Iden "a"] [Iden "b",Iden "c"] [])
>     ,("sum(a) over (order by b)"
>      ,WindowApp "sum" [Iden "a"] [] [(Iden "b", Asc)])
>     ,("sum(a) over (order by b desc,c)"
>      ,WindowApp "sum" [Iden "a"] [] [(Iden "b", Desc)
>                                     ,(Iden "c", Asc)])
>     ,("sum(a) over (partition by b order by c)"
>      ,WindowApp "sum" [Iden "a"] [Iden "b"] [(Iden "c", Asc)])
>      -- todo: check order by options, add frames
>     ]

> parens :: TestItem
> parens = Group "parens" $ map (uncurry TestScalarExpr)
>     [("(a)", Parens (Iden "a"))
>     ,("(a + b)", Parens (BinOp "+" (Iden "a") (Iden "b")))
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
>     ,withQueries
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
>                      [(Nothing,BinOp "+" (NumLit "1") (NumLit "2"))
>                      ,(Nothing,BinOp "+" (NumLit "3") (NumLit "4"))]})
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
>      ,ms [JoinAlias (SimpleTableRef "t") "u" Nothing])
>     ,("select a from t u"
>      ,ms [JoinAlias (SimpleTableRef "t") "u" Nothing])
>     ,("select a from t u(b)"
>      ,ms [JoinAlias (SimpleTableRef "t") "u" $ Just ["b"]])
>     ,("select a from (t cross join u) as u"
>      ,ms [JoinAlias (JoinParens $ JoinTableRef Cross (SimpleTableRef "t")
>                             (SimpleTableRef "u") Nothing) "u" Nothing])
>     ]
>   where
>     ms f = makeSelect {qeSelectList = [(Nothing,Iden "a")]
>                       ,qeFrom = f}

> whereClause :: TestItem
> whereClause = Group "whereClause" $ map (uncurry TestQueryExpr)
>     [("select a from t where a = 5"
>      ,makeSelect {qeSelectList = [(Nothing,Iden "a")]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeWhere = Just $ BinOp "=" (Iden "a") (NumLit "5")})
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
>                  ,qeHaving = Just $ BinOp ">" (App "sum" [Iden "b"]) (NumLit "5")
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
>      ,CombineQueryExpr ms1 Union All Respectively ms2)
>     ,("select a from t intersect select b from u"
>      ,CombineQueryExpr ms1 Intersect All Respectively ms2)
>     ,("select a from t except all select b from u"
>      ,CombineQueryExpr ms1 Except All Respectively ms2)
>     ,("select a from t union distinct corresponding \
>       \select b from u"
>      ,CombineQueryExpr ms1 Union Distinct Corresponding ms2)
>     ]
>   where
>     ms1 = makeSelect
>           {qeSelectList = [(Nothing,Iden "a")]
>           ,qeFrom = [SimpleTableRef "t"]}
>     ms2 = makeSelect
>           {qeSelectList = [(Nothing,Iden "b")]
>           ,qeFrom = [SimpleTableRef "u"]}


> withQueries :: TestItem
> withQueries = Group "with queries" $ map (uncurry TestQueryExpr)
>     [("with u as (select a from t) select a from u"
>      ,With [("u", ms1)] ms2)
>     ,("with x as (select a from t),\n\
>       \     u as (select a from x)\n\
>       \select a from u"
>      ,With [("x", ms1), ("u",ms3)] ms2)
>     ]
>  where
>    ms c t = makeSelect
>             {qeSelectList = [(Nothing,Iden c)]
>             ,qeFrom = [SimpleTableRef t]}
>    ms1 = ms "a" "t"
>    ms2 = ms "a" "u"
>    ms3 = ms "a" "x"


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
>                       ,(Just "s", App "sum" [BinOp "+" (Iden "c")
>                                                        (Iden "d")])]
>       ,qeFrom = [SimpleTableRef "t", SimpleTableRef "u"]
>       ,qeWhere = Just $ BinOp ">" (Iden "a") (NumLit "5")
>       ,qeGroupBy = [Iden "a"]
>       ,qeHaving = Just $ BinOp ">" (App "count" [NumLit "1"])
>                                    (NumLit "5")
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
>     ,tpchTests
>     ]


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

> toTest :: (Eq a, Show a) =>
>           (String -> Maybe (Int,Int) -> String -> Either ParseError a)
>        -> (a -> String)
>        -> String
>        -> a
>        -> H.Test
> toTest parser pp str expected = H.TestLabel str $ H.TestCase $ do
>         let egot = parser "" Nothing str
>         case egot of
>             Left e -> H.assertFailure $ peFormattedError e
>             Right got -> do
>                 H.assertEqual "" expected got
>                 let str' = pp got
>                 let egot' = parser "" Nothing str'
>                 case egot' of
>                     Left e' -> H.assertFailure $ "pp roundtrip " ++ peFormattedError e'
>                     Right got' -> H.assertEqual "pp roundtrip" expected got'

> toPTest :: (Eq a, Show a) =>
>           (String -> Maybe (Int,Int) -> String -> Either ParseError a)
>        -> (a -> String)
>        -> String
>        -> H.Test
> toPTest parser pp str = H.TestLabel str $ H.TestCase $ do
>         let egot = parser "" Nothing str
>         case egot of
>             Left e -> H.assertFailure $ peFormattedError e
>             Right got -> do
>                 let str' = pp got
>                 let egot' = parser "" Nothing str'
>                 case egot' of
>                     Left e' -> H.assertFailure $ "pp roundtrip " ++ peFormattedError e'
>                     Right _got' -> return ()
