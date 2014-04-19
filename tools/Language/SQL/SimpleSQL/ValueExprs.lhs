
Tests for parsing value expressions

> module Language.SQL.SimpleSQL.ValueExprs (valueExprTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax

> valueExprTests :: TestItem
> valueExprTests = Group "valueExprTests"
>     [literals
>     ,identifiers
>     ,star
>     ,parameter
>     ,dots
>     ,app
>     ,caseexp
>     ,operators
>     ,parens
>     ,subqueries
>     ,aggregates
>     ,windowFunctions
>     ]

> literals :: TestItem
> literals = Group "literals" $ map (uncurry TestValueExpr)
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
>      ,("'string with a '' quote'", StringLit "string with a ' quote")
>      ,("'1'", StringLit "1")
>      ,("interval '3' day"
>       ,IntervalLit Nothing "3" (Itf "day" Nothing) Nothing)
>      ,("interval '3' day (3)"
>       ,IntervalLit Nothing "3" (Itf "day" $ Just (3,Nothing)) Nothing)
>      ,("interval '3 weeks'", TypedLit (TypeName [Name "interval"]) "3 weeks")
>     ]

> identifiers :: TestItem
> identifiers = Group "identifiers" $ map (uncurry TestValueExpr)
>     [("iden1", Iden [Name "iden1"])
>     --,("t.a", Iden2 "t" "a")
>     ,("\"quoted identifier\"", Iden [QName "quoted identifier"])
>     ]

> star :: TestItem
> star = Group "star" $ map (uncurry TestValueExpr)
>     [("*", Star)
>     --,("t.*", Star2 "t")
>     --,("ROW(t.*,42)", App "ROW" [Star2 "t", NumLit "42"])
>     ]

> parameter :: TestItem
> parameter = Group "parameter" $ map (uncurry TestValueExpr)
>     [("?", Parameter)
>     ]


> dots :: TestItem
> dots = Group "dot" $ map (uncurry TestValueExpr)
>     [("t.a", Iden [Name "t",Name "a"])
>     ,("t.*", BinOp (Iden [Name "t"]) [Name "."] Star)
>     ,("a.b.c", Iden [Name "a",Name "b",Name "c"])
>     ,("ROW(t.*,42)", App [Name "ROW"] [BinOp (Iden [Name "t"]) [Name "."] Star, NumLit "42"])
>     ]

> app :: TestItem
> app = Group "app" $ map (uncurry TestValueExpr)
>     [("f()", App [Name "f"] [])
>     ,("f(a)", App [Name "f"] [Iden [Name "a"]])
>     ,("f(a,b)", App [Name "f"] [Iden [Name "a"], Iden [Name "b"]])
>     ]

> caseexp :: TestItem
> caseexp = Group "caseexp" $ map (uncurry TestValueExpr)
>     [("case a when 1 then 2 end"
>      ,Case (Just $ Iden [Name "a"]) [([NumLit "1"]
>                               ,NumLit "2")] Nothing)

>     ,("case a when 1 then 2 when 3 then 4 end"
>      ,Case (Just $ Iden [Name "a"]) [([NumLit "1"], NumLit "2")
>                              ,([NumLit "3"], NumLit "4")] Nothing)

>     ,("case a when 1 then 2 when 3 then 4 else 5 end"
>      ,Case (Just $ Iden [Name "a"]) [([NumLit "1"], NumLit "2")
>                              ,([NumLit "3"], NumLit "4")]
>                              (Just $ NumLit "5"))

>     ,("case when a=1 then 2 when a=3 then 4 else 5 end"
>      ,Case Nothing [([BinOp (Iden [Name "a"]) [Name "="] (NumLit "1")], NumLit "2")
>                    ,([BinOp (Iden [Name "a"]) [Name "="] (NumLit "3")], NumLit "4")]
>                    (Just $ NumLit "5"))

>     ,("case a when 1,2 then 10 when 3,4 then 20 end"
>      ,Case (Just $ Iden [Name "a"]) [([NumLit "1",NumLit "2"]
>                                ,NumLit "10")
>                              ,([NumLit "3",NumLit "4"]
>                                ,NumLit "20")]
>                              Nothing)

>     ]

> operators :: TestItem
> operators = Group "operators"
>     [binaryOperators
>     ,unaryOperators
>     ,casts
>     ,miscOps]

> binaryOperators :: TestItem
> binaryOperators = Group "binaryOperators" $ map (uncurry TestValueExpr)
>     [("a + b", BinOp (Iden [Name "a"]) [Name "+"] (Iden [Name "b"]))
>      -- sanity check fixities
>      -- todo: add more fixity checking

>     ,("a + b * c"
>      ,BinOp  (Iden [Name "a"]) [Name "+"]
>              (BinOp (Iden [Name "b"]) [Name "*"] (Iden [Name "c"])))

>     ,("a * b + c"
>      ,BinOp (BinOp (Iden [Name "a"]) [Name "*"] (Iden [Name "b"]))
>             [Name "+"] (Iden [Name "c"]))
>     ]

> unaryOperators :: TestItem
> unaryOperators = Group "unaryOperators" $ map (uncurry TestValueExpr)
>     [("not a", PrefixOp [Name "not"] $ Iden [Name "a"])
>     ,("not not a", PrefixOp [Name "not"] $ PrefixOp [Name "not"] $ Iden [Name "a"])
>     ,("+a", PrefixOp [Name "+"] $ Iden [Name "a"])
>     ,("-a", PrefixOp [Name "-"] $ Iden [Name "a"])
>     ]


> casts :: TestItem
> casts = Group "operators" $ map (uncurry TestValueExpr)
>     [("cast('1' as int)"
>      ,Cast (StringLit "1") $ TypeName [Name "int"])

>     ,("int '3'"
>      ,TypedLit (TypeName [Name "int"]) "3")

>     ,("cast('1' as double precision)"
>      ,Cast (StringLit "1") $ TypeName [Name "double precision"])

>     ,("cast('1' as float(8))"
>      ,Cast (StringLit "1") $ PrecTypeName [Name "float"] 8)

>     ,("cast('1' as decimal(15,2))"
>      ,Cast (StringLit "1") $ PrecScaleTypeName [Name "decimal"] 15 2)


>     ,("double precision '3'"
>      ,TypedLit (TypeName [Name "double precision"]) "3")
>     ]

> subqueries :: TestItem
> subqueries = Group "unaryOperators" $ map (uncurry TestValueExpr)
>     [("exists (select a from t)", SubQueryExpr SqExists ms)
>     ,("(select a from t)", SubQueryExpr SqSq ms)

>     ,("a in (select a from t)"
>      ,In True (Iden [Name "a"]) (InQueryExpr ms))

>     ,("a not in (select a from t)"
>      ,In False (Iden [Name "a"]) (InQueryExpr ms))

>     ,("a > all (select a from t)"
>      ,QuantifiedComparison (Iden [Name "a"]) [Name ">"] CPAll ms)

>     ,("a = some (select a from t)"
>      ,QuantifiedComparison (Iden [Name "a"]) [Name "="] CPSome ms)

>     ,("a <= any (select a from t)"
>      ,QuantifiedComparison (Iden [Name "a"]) [Name "<="] CPAny ms)
>     ]
>   where
>     ms = makeSelect
>          {qeSelectList = [(Iden [Name "a"],Nothing)]
>          ,qeFrom = [TRSimple [Name "t"]]
>          }

> miscOps :: TestItem
> miscOps = Group "unaryOperators" $ map (uncurry TestValueExpr)
>     [("a in (1,2,3)"
>      ,In True (Iden [Name "a"]) $ InList $ map NumLit ["1","2","3"])

>     ,("a is null", PostfixOp [Name "is null"] (Iden [Name "a"]))
>     ,("a is not null", PostfixOp [Name "is not null"] (Iden [Name "a"]))
>     ,("a is true", PostfixOp [Name "is true"] (Iden [Name "a"]))
>     ,("a is not true", PostfixOp [Name "is not true"] (Iden [Name "a"]))
>     ,("a is false", PostfixOp [Name "is false"] (Iden [Name "a"]))
>     ,("a is not false", PostfixOp [Name "is not false"] (Iden [Name "a"]))
>     ,("a is unknown", PostfixOp [Name "is unknown"] (Iden [Name "a"]))
>     ,("a is not unknown", PostfixOp [Name "is not unknown"] (Iden [Name "a"]))
>     ,("a is distinct from b", BinOp (Iden [Name "a"]) [Name "is distinct from"] (Iden [Name "b"]))

>     ,("a is not distinct from b"
>      ,BinOp (Iden [Name "a"]) [Name "is not distinct from"] (Iden [Name "b"]))

>     ,("a like b", BinOp (Iden [Name "a"]) [Name "like"] (Iden [Name "b"]))
>     ,("a not like b", BinOp (Iden [Name "a"]) [Name "not like"] (Iden [Name "b"]))
>     ,("a is similar to b", BinOp (Iden [Name "a"]) [Name "is similar to"] (Iden [Name "b"]))

>     ,("a is not similar to b"
>      ,BinOp (Iden [Name "a"]) [Name "is not similar to"] (Iden [Name "b"]))

>     ,("a overlaps b", BinOp (Iden [Name "a"]) [Name "overlaps"] (Iden [Name "b"]))


special operators

>     ,("a between b and c", SpecialOp [Name "between"] [Iden [Name "a"]
>                                                ,Iden [Name "b"]
>                                                ,Iden [Name "c"]])

>     ,("a not between b and c", SpecialOp [Name "not between"] [Iden [Name "a"]
>                                                        ,Iden [Name "b"]
>                                                        ,Iden [Name "c"]])
>     ,("(1,2)"
>      ,SpecialOp [Name "rowctor"] [NumLit "1", NumLit "2"])


keyword special operators

>     ,("extract(day from t)"
>      , SpecialOpK [Name "extract"] (Just $ Iden [Name "day"]) [("from", Iden [Name "t"])])

>     ,("substring(x from 1 for 2)"
>      ,SpecialOpK [Name "substring"] (Just $ Iden [Name "x"]) [("from", NumLit "1")
>                                                ,("for", NumLit "2")])

>     ,("substring(x from 1)"
>      ,SpecialOpK [Name "substring"] (Just $ Iden [Name "x"]) [("from", NumLit "1")])

>     ,("substring(x for 2)"
>      ,SpecialOpK [Name "substring"] (Just $ Iden [Name "x"]) [("for", NumLit "2")])

>     ,("substring(x from 1 for 2 collate C)"
>      ,SpecialOpK [Name "substring"] (Just $ Iden [Name "x"])
>           [("from", NumLit "1")
>           ,("for", Collate (NumLit "2") [Name "C"])])

this doesn't work because of a overlap in the 'in' parser

>     ,("POSITION( string1 IN string2 )"
>      ,SpecialOpK [Name "position"] (Just $ Iden [Name "string1"]) [("in", Iden [Name "string2"])])

>     ,("CONVERT(char_value USING conversion_char_name)"
>      ,SpecialOpK [Name "convert"] (Just $ Iden [Name "char_value"])
>           [("using", Iden [Name "conversion_char_name"])])

>     ,("TRANSLATE(char_value USING translation_name)"
>      ,SpecialOpK [Name "translate"] (Just $ Iden [Name "char_value"])
>           [("using", Iden [Name "translation_name"])])

OVERLAY(string PLACING embedded_string FROM start
[FOR length])

>     ,("OVERLAY(string PLACING embedded_string FROM start)"
>      ,SpecialOpK [Name "overlay"] (Just $ Iden [Name "string"])
>           [("placing", Iden [Name "embedded_string"])
>           ,("from", Iden [Name "start"])])

>     ,("OVERLAY(string PLACING embedded_string FROM start FOR length)"
>      ,SpecialOpK [Name "overlay"] (Just $ Iden [Name "string"])
>           [("placing", Iden [Name "embedded_string"])
>           ,("from", Iden [Name "start"])
>           ,("for", Iden [Name "length"])])

TRIM( [ [{LEADING | TRAILING | BOTH}] [removal_char] FROM ]
target_string
[COLLATE collation_name] )



>     ,("trim(from target_string)"
>      ,SpecialOpK [Name "trim"] Nothing
>           [("both", StringLit " ")
>           ,("from", Iden [Name "target_string"])])

>     ,("trim(leading from target_string)"
>      ,SpecialOpK [Name "trim"] Nothing
>           [("leading", StringLit " ")
>           ,("from", Iden [Name "target_string"])])

>     ,("trim(trailing from target_string)"
>      ,SpecialOpK [Name "trim"] Nothing
>           [("trailing", StringLit " ")
>           ,("from", Iden [Name "target_string"])])

>     ,("trim(both from target_string)"
>      ,SpecialOpK [Name "trim"] Nothing
>           [("both", StringLit " ")
>           ,("from", Iden [Name "target_string"])])


>     ,("trim(leading 'x' from target_string)"
>      ,SpecialOpK [Name "trim"] Nothing
>           [("leading", StringLit "x")
>           ,("from", Iden [Name "target_string"])])

>     ,("trim(trailing 'y' from target_string)"
>      ,SpecialOpK [Name "trim"] Nothing
>           [("trailing", StringLit "y")
>           ,("from", Iden [Name "target_string"])])

>     ,("trim(both 'z' from target_string collate C)"
>      ,SpecialOpK [Name "trim"] Nothing
>           [("both", StringLit "z")
>           ,("from", Collate (Iden [Name "target_string"]) [Name "C"])])

>     ,("trim(leading from target_string)"
>      ,SpecialOpK [Name "trim"] Nothing
>           [("leading", StringLit " ")
>           ,("from", Iden [Name "target_string"])])


>     ]

> aggregates :: TestItem
> aggregates = Group "aggregates" $ map (uncurry TestValueExpr)
>     [("count(*)",App [Name "count"] [Star])

>     ,("sum(a order by a)"
>     ,AggregateApp [Name "sum"] SQDefault [Iden [Name "a"]]
>                   [SortSpec (Iden [Name "a"]) DirDefault NullsOrderDefault] Nothing)

>     ,("sum(all a)"
>     ,AggregateApp [Name "sum"] All [Iden [Name "a"]] [] Nothing)

>     ,("count(distinct a)"
>     ,AggregateApp [Name "count"] Distinct [Iden [Name "a"]] [] Nothing)
>     ]

> windowFunctions :: TestItem
> windowFunctions = Group "windowFunctions" $ map (uncurry TestValueExpr)
>     [("max(a) over ()", WindowApp [Name "max"] [Iden [Name "a"]] [] [] Nothing)
>     ,("count(*) over ()", WindowApp [Name "count"] [Star] [] [] Nothing)

>     ,("max(a) over (partition by b)"
>      ,WindowApp [Name "max"] [Iden [Name "a"]] [Iden [Name "b"]] [] Nothing)

>     ,("max(a) over (partition by b,c)"
>      ,WindowApp [Name "max"] [Iden [Name "a"]] [Iden [Name "b"],Iden [Name "c"]] [] Nothing)

>     ,("sum(a) over (order by b)"
>      ,WindowApp [Name "sum"] [Iden [Name "a"]] []
>           [SortSpec (Iden [Name "b"]) DirDefault NullsOrderDefault] Nothing)

>     ,("sum(a) over (order by b desc,c)"
>      ,WindowApp [Name "sum"] [Iden [Name "a"]] []
>           [SortSpec (Iden [Name "b"]) Desc NullsOrderDefault
>           ,SortSpec (Iden [Name "c"]) DirDefault NullsOrderDefault] Nothing)

>     ,("sum(a) over (partition by b order by c)"
>      ,WindowApp [Name "sum"] [Iden [Name "a"]] [Iden [Name "b"]]
>           [SortSpec (Iden [Name "c"]) DirDefault NullsOrderDefault] Nothing)

>     ,("sum(a) over (partition by b order by c range unbounded preceding)"
>      ,WindowApp [Name "sum"] [Iden [Name "a"]] [Iden [Name "b"]]
>       [SortSpec (Iden [Name "c"]) DirDefault NullsOrderDefault]
>       $ Just $ FrameFrom FrameRange UnboundedPreceding)

>     ,("sum(a) over (partition by b order by c range 5 preceding)"
>      ,WindowApp [Name "sum"] [Iden [Name "a"]] [Iden [Name "b"]]
>       [SortSpec (Iden [Name "c"]) DirDefault NullsOrderDefault]
>       $ Just $ FrameFrom FrameRange $ Preceding (NumLit "5"))

>     ,("sum(a) over (partition by b order by c range current row)"
>      ,WindowApp [Name "sum"] [Iden [Name "a"]] [Iden [Name "b"]]
>       [SortSpec (Iden [Name "c"]) DirDefault NullsOrderDefault]
>       $ Just $ FrameFrom FrameRange Current)

>     ,("sum(a) over (partition by b order by c rows 5 following)"
>      ,WindowApp [Name "sum"] [Iden [Name "a"]] [Iden [Name "b"]]
>       [SortSpec (Iden [Name "c"]) DirDefault NullsOrderDefault]
>       $ Just $ FrameFrom FrameRows $ Following (NumLit "5"))

>     ,("sum(a) over (partition by b order by c range unbounded following)"
>      ,WindowApp [Name "sum"] [Iden [Name "a"]] [Iden [Name "b"]]
>       [SortSpec (Iden [Name "c"]) DirDefault NullsOrderDefault]
>       $ Just $ FrameFrom FrameRange UnboundedFollowing)

>     ,("sum(a) over (partition by b order by c \n\
>       \range between 5 preceding and 5 following)"
>      ,WindowApp [Name "sum"] [Iden [Name "a"]] [Iden [Name "b"]]
>       [SortSpec (Iden [Name "c"]) DirDefault NullsOrderDefault]
>       $ Just $ FrameBetween FrameRange
>                             (Preceding (NumLit "5"))
>                             (Following (NumLit "5")))

>     ]

> parens :: TestItem
> parens = Group "parens" $ map (uncurry TestValueExpr)
>     [("(a)", Parens (Iden [Name "a"]))
>     ,("(a + b)", Parens (BinOp (Iden [Name "a"]) [Name "+"] (Iden [Name "b"])))
>     ]
