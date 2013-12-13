
> module Tests (testData, runTests) where

> import Language.SQL.SimpleSQL.Syntax
> import Language.SQL.SimpleSQL.Pretty
> import Language.SQL.SimpleSQL.Parser
> import qualified Test.HUnit as H
> import Control.Monad

> data TestItem = Group String [TestItem]
>               | TestScalarExpr String ScalarExpr
>               | TestQueryExpr String QueryExpr
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
>     ]

> literals :: TestItem
> literals = Group "literals" $ map (uncurry TestScalarExpr)
>     [("3", Literal "3")
>     ,("'string'", Literal "string")
>     ]

> identifiers :: TestItem
> identifiers = Group "identifiers" $ map (uncurry TestScalarExpr)
>     [("iden1", Identifier "iden1")
>     ,("t.a", Identifier2 "t" "a")
>     ]

> star :: TestItem
> star = Group "star" $ map (uncurry TestScalarExpr)
>     [("*", Star)
>     ,("t.*", Star2 "t")
>     ]

> app :: TestItem
> app = Group "app" $ map (uncurry TestScalarExpr)
>     [("f()", App "f" [])
>     ,("f(a)", App "f" [Identifier "a"])
>     ,("f(a,b)", App "f" [Identifier "a", Identifier "b"])
>     ]

> caseexp :: TestItem
> caseexp = Group "caseexp" $ map (uncurry TestScalarExpr)
>     [("case a when 1 then 2 end"
>      ,Case (Just $ Identifier "a") [(Literal "1", Literal "2")] Nothing)
>     ,("case a when 1 then 2 when 3 then 4 end"
>      ,Case (Just $ Identifier "a") [(Literal "1", Literal "2")
>                                    ,(Literal "3", Literal "4")] Nothing)
>     ,("case a when 1 then 2 when 3 then 4 else 5 end"
>      ,Case (Just $ Identifier "a") [(Literal "1", Literal "2")
>                                    ,(Literal "3", Literal "4")] (Just $ Literal "5"))
>     ,("case when a=1 then 2 when a=3 then 4 else 5 end"
>      ,Case Nothing [(Op "=" [Identifier "a", Literal "1"], Literal "2")
>                    ,(Op "=" [Identifier "a", Literal "3"], Literal "4")]
>                    (Just $ Literal "5"))
>     ]

> operators :: TestItem
> operators = Group "operators" $ map (uncurry TestScalarExpr)
>     [("a + b", Op "+" [Identifier "a", Identifier "b"])
>     ,("not not a", Op "not" [Op "not" [Identifier "a"]])
>     ]

> parens :: TestItem
> parens = Group "parens" $ map (uncurry TestScalarExpr)
>     [("(a)", Parens (Identifier "a"))
>     ,("(a + b)", Parens (Op "+" [Identifier "a", Identifier "b"]))
>     ]

> queryExprParserTests :: TestItem
> queryExprParserTests = Group "queryExprParserTests"
>     [selectLists
>     ,from
>     ,whereClause
>     ,groupByClause
>     ,having
>     ,orderBy
>     ,fullQueries
>     ]

> selectLists :: TestItem
> selectLists = Group "selectLists" $ map (uncurry TestQueryExpr)
>     [("select 1",
>       makeSelect {qeSelectList = [(Nothing,Literal "1")]})
>     ,("select a"
>      ,makeSelect {qeSelectList = [(Nothing,Identifier "a")]})
>     ,("select a,b"
>      ,makeSelect {qeSelectList = [(Nothing,Identifier "a")
>                                  ,(Nothing,Identifier "b")]})
>     ,("select 1+2,3+4"
>      ,makeSelect {qeSelectList =
>                      [(Nothing,Op "+" [Literal "1",Literal "2"])
>                      ,(Nothing,Op "+" [Literal "3",Literal "4"])]})
>     ,("select a as a, /*comment*/ b as b"
>      ,makeSelect {qeSelectList = [(Just "a", Identifier "a")
>                                  ,(Just "b", Identifier "b")]})
>     ,("select a a, b b"
>      ,makeSelect {qeSelectList = [(Just "a", Identifier "a")
>                                  ,(Just "b", Identifier "b")]})
>     ]

> from :: TestItem
> from = Group "from" $ map (uncurry TestQueryExpr)
>     [("select a from t"
>      ,ms [SimpleTableRef "t"])
>     ,("select a from t,u"
>      ,ms [SimpleTableRef "t", SimpleTableRef "u"])
>     ,("select a from t inner join u on expr"
>      ,ms [JoinTableRef Inner (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinOn $ Identifier "expr")])
>     ,("select a from t left join u on expr"
>      ,ms [JoinTableRef JLeft (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinOn $ Identifier "expr")])
>     ,("select a from t right join u on expr"
>      ,ms [JoinTableRef JRight (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinOn $ Identifier "expr")])
>     ,("select a from t full join u on expr"
>      ,ms [JoinTableRef Full (SimpleTableRef "t") (SimpleTableRef "u")
>                        (Just $ JoinOn $ Identifier "expr")])
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
>     ms f = makeSelect {qeSelectList = [(Nothing,Identifier "a")]
>                       ,qeFrom = f}

> whereClause :: TestItem
> whereClause = Group "whereClause" $ map (uncurry TestQueryExpr)
>     [("select a from t where a = 5"
>      ,makeSelect {qeSelectList = [(Nothing,Identifier "a")]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeWhere = Just $ Op "=" [Identifier "a", Literal "5"]})
>     ]

> groupByClause :: TestItem
> groupByClause = Group "groupByClause" $ map (uncurry TestQueryExpr)
>     [("select a,sum(b) from t group by a"
>      ,makeSelect {qeSelectList = [(Nothing, Identifier "a")
>                                  ,(Nothing, App "sum" [Identifier "b"])]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeGroupBy = [Identifier "a"]
>                  })
>     ,("select a,b,sum(c) from t group by a,b"
>      ,makeSelect {qeSelectList = [(Nothing, Identifier "a")
>                                  ,(Nothing, Identifier "b")
>                                  ,(Nothing, App "sum" [Identifier "c"])]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeGroupBy = [Identifier "a",Identifier "b"]
>                  })
>     ]

> having :: TestItem
> having = Group "having" $ map (uncurry TestQueryExpr)
>     [("select a,sum(b) from t group by a having sum(b) > 5"
>      ,makeSelect {qeSelectList = [(Nothing, Identifier "a")
>                                  ,(Nothing, App "sum" [Identifier "b"])]
>                  ,qeFrom = [SimpleTableRef "t"]
>                  ,qeGroupBy = [Identifier "a"]
>                  ,qeHaving = Just $ Op ">" [App "sum" [Identifier "b"], Literal "5"]
>                  })
>     ]

> orderBy :: TestItem
> orderBy = Group "orderBy" $ map (uncurry TestQueryExpr)
>     [("select a from t order by a"
>      ,ms [Identifier "a"])
>     ,("select a from t order by a, b"
>      ,ms [Identifier "a", Identifier "b"])
>     ]
>   where
>     ms o = makeSelect {qeSelectList = [(Nothing,Identifier "a")]
>                       ,qeFrom = [SimpleTableRef "t"]
>                       ,qeOrderBy = o}

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
>       {qeSelectList = [(Nothing, Identifier "a")
>                       ,(Just "s", App "sum" [Op "+" [Identifier "c"
>                                                     ,Identifier "d"]])]
>       ,qeFrom = [SimpleTableRef "t", SimpleTableRef "u"]
>       ,qeWhere = Just $ Op ">" [Identifier "a", Literal "5"]
>       ,qeGroupBy = [Identifier "a"]
>       ,qeHaving = Just $ Op ">" [App "count" [Literal "1"]
>                                 ,Literal "5"]
>       ,qeOrderBy = [Identifier "s"]
>       }
>      )
>     ]

> testData :: TestItem
> testData =
>     Group "parserTest"
>     [scalarExprParserTests
>     ,queryExprParserTests]


> runTests :: IO ()
> runTests = void $ H.runTestTT $ itemToTest testData

> itemToTest :: TestItem -> H.Test
> itemToTest (Group nm ts) =
>     H.TestLabel nm $ H.TestList $ map itemToTest ts
> itemToTest (TestScalarExpr str expected) =
>     toTest parseScalarExpr prettyScalarExpr str expected
> itemToTest (TestQueryExpr str expected) =
>     toTest parseQueryExpr prettyQueryExpr str expected

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
