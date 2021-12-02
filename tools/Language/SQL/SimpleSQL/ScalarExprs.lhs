
Tests for parsing scalar expressions

> module Language.SQL.SimpleSQL.ScalarExprs (scalarExprTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax
  
> scalarExprTests :: TestItem
> scalarExprTests = Group "scalarExprTests"
>     [literals
>     ,identifiers
>     ,star
>     ,parameter
>     ,dots
>     ,app
>     ,caseexp
>     ,convertfun     
>     ,operators
>     ,parens
>     ,subqueries
>     ,aggregates
>     ,windowFunctions
>     ,functionsWithReservedNames
>     ]

> literals :: TestItem
> literals = Group "literals" $ map (uncurry (TestScalarExpr ansi2011))
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
>      ,("'string'", StringLit "'" "'" "string")
>      ,("'string with a '' quote'", StringLit "'" "'" "string with a '' quote")
>      ,("'1'", StringLit "'" "'" "1")
>      ,("interval '3' day"
>       ,IntervalLit Nothing "3" (Itf "day" Nothing) Nothing)
>      ,("interval '3' day (3)"
>       ,IntervalLit Nothing "3" (Itf "day" $ Just (3,Nothing)) Nothing)
>      ,("interval '3 weeks'", TypedLit (TypeName [Name Nothing "interval"]) "3 weeks")
>     ]

> identifiers :: TestItem
> identifiers = Group "identifiers" $ map (uncurry (TestScalarExpr ansi2011))
>     [("iden1", Iden [Name Nothing "iden1"])
>     --,("t.a", Iden2 "t" "a")
>     ,("\"quoted identifier\"", Iden [Name (Just ("\"","\"")) "quoted identifier"])
>     ,("\"from\"", Iden [Name (Just ("\"","\"")) "from"])
>     ]

> star :: TestItem
> star = Group "star" $ map (uncurry (TestScalarExpr ansi2011))
>     [("*", Star)
>     --,("t.*", Star2 "t")
>     --,("ROW(t.*,42)", App "ROW" [Star2 "t", NumLit "42"])
>     ]

> parameter :: TestItem
> parameter = Group "parameter"
>     [TestScalarExpr ansi2011 "?" Parameter
>     ,TestScalarExpr postgres "$13" $ PositionalArg 13]


> dots :: TestItem
> dots = Group "dot" $ map (uncurry (TestScalarExpr ansi2011))
>     [("t.a", Iden [Name Nothing "t",Name Nothing "a"])
>     ,("t.*", BinOp (Iden [Name Nothing "t"]) [Name Nothing "."] Star)
>     ,("a.b.c", Iden [Name Nothing "a",Name Nothing "b",Name Nothing "c"])
>     ,("ROW(t.*,42)", App [Name Nothing "ROW"] [BinOp (Iden [Name Nothing "t"]) [Name Nothing "."] Star, NumLit "42"])
>     ]

> app :: TestItem
> app = Group "app" $ map (uncurry (TestScalarExpr ansi2011))
>     [("f()", App [Name Nothing "f"] [])
>     ,("f(a)", App [Name Nothing "f"] [Iden [Name Nothing "a"]])
>     ,("f(a,b)", App [Name Nothing "f"] [Iden [Name Nothing "a"], Iden [Name Nothing "b"]])
>     ]

> caseexp :: TestItem
> caseexp = Group "caseexp" $ map (uncurry (TestScalarExpr ansi2011))
>     [("case a when 1 then 2 end"
>      ,Case (Just $ Iden [Name Nothing "a"]) [([NumLit "1"]
>                               ,NumLit "2")] Nothing)

>     ,("case a when 1 then 2 when 3 then 4 end"
>      ,Case (Just $ Iden [Name Nothing "a"]) [([NumLit "1"], NumLit "2")
>                              ,([NumLit "3"], NumLit "4")] Nothing)

>     ,("case a when 1 then 2 when 3 then 4 else 5 end"
>      ,Case (Just $ Iden [Name Nothing "a"]) [([NumLit "1"], NumLit "2")
>                              ,([NumLit "3"], NumLit "4")]
>                              (Just $ NumLit "5"))

>     ,("case when a=1 then 2 when a=3 then 4 else 5 end"
>      ,Case Nothing [([BinOp (Iden [Name Nothing "a"]) [Name Nothing "="] (NumLit "1")], NumLit "2")
>                    ,([BinOp (Iden [Name Nothing "a"]) [Name Nothing "="] (NumLit "3")], NumLit "4")]
>                    (Just $ NumLit "5"))

>     ,("case a when 1,2 then 10 when 3,4 then 20 end"
>      ,Case (Just $ Iden [Name Nothing "a"]) [([NumLit "1",NumLit "2"]
>                                ,NumLit "10")
>                              ,([NumLit "3",NumLit "4"]
>                                ,NumLit "20")]
>                              Nothing)

>     ]

> convertfun :: TestItem 
> convertfun = Group "convert" $ map (uncurry (TestScalarExpr sqlserver))
>     [("CONVERT(varchar, 25.65)"
>      ,Convert (TypeName [Name Nothing "varchar"]) (NumLit "25.65") Nothing)
>     ,("CONVERT(datetime, '2017-08-25')"
>      ,Convert (TypeName [Name Nothing "datetime"]) (StringLit "'" "'" "2017-08-25") Nothing)
>     ,("CONVERT(varchar, '2017-08-25', 101)"
>      ,Convert (TypeName [Name Nothing "varchar"]) (StringLit "'" "'" "2017-08-25") (Just 101))
>     ]

> operators :: TestItem
> operators = Group "operators"
>     [binaryOperators
>     ,unaryOperators
>     ,casts
>     ,miscOps]

> binaryOperators :: TestItem
> binaryOperators = Group "binaryOperators" $ map (uncurry (TestScalarExpr ansi2011))
>     [("a + b", BinOp (Iden [Name Nothing "a"]) [Name Nothing "+"] (Iden [Name Nothing "b"]))
>      -- sanity check fixities
>      -- todo: add more fixity checking

>     ,("a + b * c"
>      ,BinOp  (Iden [Name Nothing "a"]) [Name Nothing "+"]
>              (BinOp (Iden [Name Nothing "b"]) [Name Nothing "*"] (Iden [Name Nothing "c"])))

>     ,("a * b + c"
>      ,BinOp (BinOp (Iden [Name Nothing "a"]) [Name Nothing "*"] (Iden [Name Nothing "b"]))
>             [Name Nothing "+"] (Iden [Name Nothing "c"]))
>     ]

> unaryOperators :: TestItem
> unaryOperators = Group "unaryOperators" $ map (uncurry (TestScalarExpr ansi2011))
>     [("not a", PrefixOp [Name Nothing "not"] $ Iden [Name Nothing "a"])
>     ,("not not a", PrefixOp [Name Nothing "not"] $ PrefixOp [Name Nothing "not"] $ Iden [Name Nothing "a"])
>     ,("+a", PrefixOp [Name Nothing "+"] $ Iden [Name Nothing "a"])
>     ,("-a", PrefixOp [Name Nothing "-"] $ Iden [Name Nothing "a"])
>     ]


> casts :: TestItem
> casts = Group "operators" $ map (uncurry (TestScalarExpr ansi2011))
>     [("cast('1' as int)"
>      ,Cast (StringLit "'" "'" "1") $ TypeName [Name Nothing "int"])

>     ,("int '3'"
>      ,TypedLit (TypeName [Name Nothing "int"]) "3")

>     ,("cast('1' as double precision)"
>      ,Cast (StringLit "'" "'" "1") $ TypeName [Name Nothing "double precision"])

>     ,("cast('1' as float(8))"
>      ,Cast (StringLit "'" "'" "1") $ PrecTypeName [Name Nothing "float"] 8)

>     ,("cast('1' as decimal(15,2))"
>      ,Cast (StringLit "'" "'" "1") $ PrecScaleTypeName [Name Nothing "decimal"] 15 2)


>     ,("double precision '3'"
>      ,TypedLit (TypeName [Name Nothing "double precision"]) "3")
>     ]

> subqueries :: TestItem
> subqueries = Group "unaryOperators" $ map (uncurry (TestScalarExpr ansi2011))
>     [("exists (select a from t)", SubQueryExpr SqExists ms)
>     ,("(select a from t)", SubQueryExpr SqSq ms)

>     ,("a in (select a from t)"
>      ,In True (Iden [Name Nothing "a"]) (InQueryExpr ms))

>     ,("a not in (select a from t)"
>      ,In False (Iden [Name Nothing "a"]) (InQueryExpr ms))

>     ,("a > all (select a from t)"
>      ,QuantifiedComparison (Iden [Name Nothing "a"]) [Name Nothing ">"] CPAll ms)

>     ,("a = some (select a from t)"
>      ,QuantifiedComparison (Iden [Name Nothing "a"]) [Name Nothing "="] CPSome ms)

>     ,("a <= any (select a from t)"
>      ,QuantifiedComparison (Iden [Name Nothing "a"]) [Name Nothing "<="] CPAny ms)
>     ]
>   where
>     ms = makeSelect
>          {qeSelectList = [(Iden [Name Nothing "a"],Nothing)]
>          ,qeFrom = [TRSimple [Name Nothing "t"]]
>          }

> miscOps :: TestItem
> miscOps = Group "unaryOperators" $ map (uncurry (TestScalarExpr ansi2011))
>     [("a in (1,2,3)"
>      ,In True (Iden [Name Nothing "a"]) $ InList $ map NumLit ["1","2","3"])

>     ,("a is null", PostfixOp [Name Nothing "is null"] (Iden [Name Nothing "a"]))
>     ,("a is not null", PostfixOp [Name Nothing "is not null"] (Iden [Name Nothing "a"]))
>     ,("a is true", PostfixOp [Name Nothing "is true"] (Iden [Name Nothing "a"]))
>     ,("a is not true", PostfixOp [Name Nothing "is not true"] (Iden [Name Nothing "a"]))
>     ,("a is false", PostfixOp [Name Nothing "is false"] (Iden [Name Nothing "a"]))
>     ,("a is not false", PostfixOp [Name Nothing "is not false"] (Iden [Name Nothing "a"]))
>     ,("a is unknown", PostfixOp [Name Nothing "is unknown"] (Iden [Name Nothing "a"]))
>     ,("a is not unknown", PostfixOp [Name Nothing "is not unknown"] (Iden [Name Nothing "a"]))
>     ,("a is distinct from b", BinOp (Iden [Name Nothing "a"]) [Name Nothing "is distinct from"] (Iden [Name Nothing "b"]))

>     ,("a is not distinct from b"
>      ,BinOp (Iden [Name Nothing "a"]) [Name Nothing "is not distinct from"] (Iden [Name Nothing "b"]))

>     ,("a like b", BinOp (Iden [Name Nothing "a"]) [Name Nothing "like"] (Iden [Name Nothing "b"]))
>     ,("a not like b", BinOp (Iden [Name Nothing "a"]) [Name Nothing "not like"] (Iden [Name Nothing "b"]))
>     ,("a is similar to b", BinOp (Iden [Name Nothing "a"]) [Name Nothing "is similar to"] (Iden [Name Nothing "b"]))

>     ,("a is not similar to b"
>      ,BinOp (Iden [Name Nothing "a"]) [Name Nothing "is not similar to"] (Iden [Name Nothing "b"]))

>     ,("a overlaps b", BinOp (Iden [Name Nothing "a"]) [Name Nothing "overlaps"] (Iden [Name Nothing "b"]))


special operators

>     ,("a between b and c", SpecialOp [Name Nothing "between"] [Iden [Name Nothing "a"]
>                                                ,Iden [Name Nothing "b"]
>                                                ,Iden [Name Nothing "c"]])

>     ,("a not between b and c", SpecialOp [Name Nothing "not between"] [Iden [Name Nothing "a"]
>                                                        ,Iden [Name Nothing "b"]
>                                                        ,Iden [Name Nothing "c"]])
>     ,("(1,2)"
>      ,SpecialOp [Name Nothing "rowctor"] [NumLit "1", NumLit "2"])


keyword special operators

>     ,("extract(day from t)"
>      , SpecialOpK [Name Nothing "extract"] (Just $ Iden [Name Nothing "day"]) [("from", Iden [Name Nothing "t"])])

>     ,("substring(x from 1 for 2)"
>      ,SpecialOpK [Name Nothing "substring"] (Just $ Iden [Name Nothing "x"]) [("from", NumLit "1")
>                                                ,("for", NumLit "2")])

>     ,("substring(x from 1)"
>      ,SpecialOpK [Name Nothing "substring"] (Just $ Iden [Name Nothing "x"]) [("from", NumLit "1")])

>     ,("substring(x for 2)"
>      ,SpecialOpK [Name Nothing "substring"] (Just $ Iden [Name Nothing "x"]) [("for", NumLit "2")])

>     ,("substring(x from 1 for 2 collate C)"
>      ,SpecialOpK [Name Nothing "substring"] (Just $ Iden [Name Nothing "x"])
>           [("from", NumLit "1")
>           ,("for", Collate (NumLit "2") [Name Nothing "C"])])

this doesn't work because of a overlap in the 'in' parser

>     ,("POSITION( string1 IN string2 )"
>      ,SpecialOpK [Name Nothing "position"] (Just $ Iden [Name Nothing "string1"]) [("in", Iden [Name Nothing "string2"])])

>     ,("CONVERT(char_value USING conversion_char_name)"
>      ,SpecialOpK [Name Nothing "convert"] (Just $ Iden [Name Nothing "char_value"])
>           [("using", Iden [Name Nothing "conversion_char_name"])])

>     ,("TRANSLATE(char_value USING translation_name)"
>      ,SpecialOpK [Name Nothing "translate"] (Just $ Iden [Name Nothing "char_value"])
>           [("using", Iden [Name Nothing "translation_name"])])

OVERLAY(string PLACING embedded_string FROM start
[FOR length])

>     ,("OVERLAY(string PLACING embedded_string FROM start)"
>      ,SpecialOpK [Name Nothing "overlay"] (Just $ Iden [Name Nothing "string"])
>           [("placing", Iden [Name Nothing "embedded_string"])
>           ,("from", Iden [Name Nothing "start"])])

>     ,("OVERLAY(string PLACING embedded_string FROM start FOR length)"
>      ,SpecialOpK [Name Nothing "overlay"] (Just $ Iden [Name Nothing "string"])
>           [("placing", Iden [Name Nothing "embedded_string"])
>           ,("from", Iden [Name Nothing "start"])
>           ,("for", Iden [Name Nothing "length"])])

TRIM( [ [{LEADING | TRAILING | BOTH}] [removal_char] FROM ]
target_string
[COLLATE collation_name] )



>     ,("trim(from target_string)"
>      ,SpecialOpK [Name Nothing "trim"] Nothing
>           [("both", StringLit "'" "'" " ")
>           ,("from", Iden [Name Nothing "target_string"])])

>     ,("trim(leading from target_string)"
>      ,SpecialOpK [Name Nothing "trim"] Nothing
>           [("leading", StringLit "'" "'" " ")
>           ,("from", Iden [Name Nothing "target_string"])])

>     ,("trim(trailing from target_string)"
>      ,SpecialOpK [Name Nothing "trim"] Nothing
>           [("trailing", StringLit "'" "'" " ")
>           ,("from", Iden [Name Nothing "target_string"])])

>     ,("trim(both from target_string)"
>      ,SpecialOpK [Name Nothing "trim"] Nothing
>           [("both", StringLit "'" "'" " ")
>           ,("from", Iden [Name Nothing "target_string"])])


>     ,("trim(leading 'x' from target_string)"
>      ,SpecialOpK [Name Nothing "trim"] Nothing
>           [("leading", StringLit "'" "'" "x")
>           ,("from", Iden [Name Nothing "target_string"])])

>     ,("trim(trailing 'y' from target_string)"
>      ,SpecialOpK [Name Nothing "trim"] Nothing
>           [("trailing", StringLit "'" "'" "y")
>           ,("from", Iden [Name Nothing "target_string"])])

>     ,("trim(both 'z' from target_string collate C)"
>      ,SpecialOpK [Name Nothing "trim"] Nothing
>           [("both", StringLit "'" "'" "z")
>           ,("from", Collate (Iden [Name Nothing "target_string"]) [Name Nothing "C"])])

>     ,("trim(leading from target_string)"
>      ,SpecialOpK [Name Nothing "trim"] Nothing
>           [("leading", StringLit "'" "'" " ")
>           ,("from", Iden [Name Nothing "target_string"])])


>     ]

> aggregates :: TestItem
> aggregates = Group "aggregates" $ map (uncurry (TestScalarExpr ansi2011))
>     [("count(*)",App [Name Nothing "count"] [Star])

>     ,("sum(a order by a)"
>     ,AggregateApp [Name Nothing "sum"] SQDefault [Iden [Name Nothing "a"]]
>                   [SortSpec (Iden [Name Nothing "a"]) DirDefault NullsOrderDefault] Nothing)

>     ,("sum(all a)"
>     ,AggregateApp [Name Nothing "sum"] All [Iden [Name Nothing "a"]] [] Nothing)

>     ,("count(distinct a)"
>     ,AggregateApp [Name Nothing "count"] Distinct [Iden [Name Nothing "a"]] [] Nothing)
>     ]

> windowFunctions :: TestItem
> windowFunctions = Group "windowFunctions" $ map (uncurry (TestScalarExpr ansi2011))
>     [("max(a) over ()", WindowApp [Name Nothing "max"] [Iden [Name Nothing "a"]] [] [] Nothing)
>     ,("count(*) over ()", WindowApp [Name Nothing "count"] [Star] [] [] Nothing)

>     ,("max(a) over (partition by b)"
>      ,WindowApp [Name Nothing "max"] [Iden [Name Nothing "a"]] [Iden [Name Nothing "b"]] [] Nothing)

>     ,("max(a) over (partition by b,c)"
>      ,WindowApp [Name Nothing "max"] [Iden [Name Nothing "a"]] [Iden [Name Nothing "b"],Iden [Name Nothing "c"]] [] Nothing)

>     ,("sum(a) over (order by b)"
>      ,WindowApp [Name Nothing "sum"] [Iden [Name Nothing "a"]] []
>           [SortSpec (Iden [Name Nothing "b"]) DirDefault NullsOrderDefault] Nothing)

>     ,("sum(a) over (order by b desc,c)"
>      ,WindowApp [Name Nothing "sum"] [Iden [Name Nothing "a"]] []
>           [SortSpec (Iden [Name Nothing "b"]) Desc NullsOrderDefault
>           ,SortSpec (Iden [Name Nothing "c"]) DirDefault NullsOrderDefault] Nothing)

>     ,("sum(a) over (partition by b order by c)"
>      ,WindowApp [Name Nothing "sum"] [Iden [Name Nothing "a"]] [Iden [Name Nothing "b"]]
>           [SortSpec (Iden [Name Nothing "c"]) DirDefault NullsOrderDefault] Nothing)

>     ,("sum(a) over (partition by b order by c range unbounded preceding)"
>      ,WindowApp [Name Nothing "sum"] [Iden [Name Nothing "a"]] [Iden [Name Nothing "b"]]
>       [SortSpec (Iden [Name Nothing "c"]) DirDefault NullsOrderDefault]
>       $ Just $ FrameFrom FrameRange UnboundedPreceding)

>     ,("sum(a) over (partition by b order by c range 5 preceding)"
>      ,WindowApp [Name Nothing "sum"] [Iden [Name Nothing "a"]] [Iden [Name Nothing "b"]]
>       [SortSpec (Iden [Name Nothing "c"]) DirDefault NullsOrderDefault]
>       $ Just $ FrameFrom FrameRange $ Preceding (NumLit "5"))

>     ,("sum(a) over (partition by b order by c range current row)"
>      ,WindowApp [Name Nothing "sum"] [Iden [Name Nothing "a"]] [Iden [Name Nothing "b"]]
>       [SortSpec (Iden [Name Nothing "c"]) DirDefault NullsOrderDefault]
>       $ Just $ FrameFrom FrameRange Current)

>     ,("sum(a) over (partition by b order by c rows 5 following)"
>      ,WindowApp [Name Nothing "sum"] [Iden [Name Nothing "a"]] [Iden [Name Nothing "b"]]
>       [SortSpec (Iden [Name Nothing "c"]) DirDefault NullsOrderDefault]
>       $ Just $ FrameFrom FrameRows $ Following (NumLit "5"))

>     ,("sum(a) over (partition by b order by c range unbounded following)"
>      ,WindowApp [Name Nothing "sum"] [Iden [Name Nothing "a"]] [Iden [Name Nothing "b"]]
>       [SortSpec (Iden [Name Nothing "c"]) DirDefault NullsOrderDefault]
>       $ Just $ FrameFrom FrameRange UnboundedFollowing)

>     ,("sum(a) over (partition by b order by c \n\
>       \range between 5 preceding and 5 following)"
>      ,WindowApp [Name Nothing "sum"] [Iden [Name Nothing "a"]] [Iden [Name Nothing "b"]]
>       [SortSpec (Iden [Name Nothing "c"]) DirDefault NullsOrderDefault]
>       $ Just $ FrameBetween FrameRange
>                             (Preceding (NumLit "5"))
>                             (Following (NumLit "5")))

>     ]

> parens :: TestItem
> parens = Group "parens" $ map (uncurry (TestScalarExpr ansi2011))
>     [("(a)", Parens (Iden [Name Nothing "a"]))
>     ,("(a + b)", Parens (BinOp (Iden [Name Nothing "a"]) [Name Nothing "+"] (Iden [Name Nothing "b"])))
>     ]

> functionsWithReservedNames :: TestItem
> functionsWithReservedNames = Group "functionsWithReservedNames" $ map t
>     ["abs"
>     ,"char_length"
>     ]
>   where
>     t fn = TestScalarExpr ansi2011 (fn ++ "(a)") $ App [Name Nothing fn] [Iden [Name Nothing "a"]]

