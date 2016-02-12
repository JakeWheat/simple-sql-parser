
Here are the tests for the group by component of query exprs

> module Language.SQL.SimpleSQL.GroupBy (groupByTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax


> groupByTests :: TestItem
> groupByTests = Group "groupByTests"
>     [simpleGroupBy
>     ,newGroupBy
>     ,randomGroupBy
>     ]

> simpleGroupBy :: TestItem
> simpleGroupBy = Group "simpleGroupBy" $ map (uncurry (TestQueryExpr ansi2011))
>     [("select a,sum(b) from t group by a"
>      ,makeSelect {qeSelectList = [(Iden [Name "a"],Nothing)
>                                  ,(App [Name "sum"] [Iden [Name "b"]],Nothing)]
>                  ,qeFrom = [TRSimple [Name "t"]]
>                  ,qeGroupBy = [SimpleGroup $ Iden [Name "a"]]
>                  })

>     ,("select a,b,sum(c) from t group by a,b"
>      ,makeSelect {qeSelectList = [(Iden [Name "a"],Nothing)
>                                  ,(Iden [Name "b"],Nothing)
>                                  ,(App [Name "sum"] [Iden [Name "c"]],Nothing)]
>                  ,qeFrom = [TRSimple [Name "t"]]
>                  ,qeGroupBy = [SimpleGroup $ Iden [Name "a"]
>                               ,SimpleGroup $ Iden [Name "b"]]
>                  })
>     ]

test the new group by (), grouping sets, cube and rollup syntax (not
sure which sql version they were introduced, 1999 or 2003 I think).

> newGroupBy :: TestItem
> newGroupBy = Group "newGroupBy" $ map (uncurry (TestQueryExpr ansi2011))
>     [("select * from t group by ()", ms [GroupingParens []])
>     ,("select * from t group by grouping sets ((), (a))"
>      ,ms [GroupingSets [GroupingParens []
>                        ,GroupingParens [SimpleGroup $ Iden [Name "a"]]]])
>     ,("select * from t group by cube(a,b)"
>      ,ms [Cube [SimpleGroup $ Iden [Name "a"], SimpleGroup $ Iden [Name "b"]]])
>     ,("select * from t group by rollup(a,b)"
>      ,ms [Rollup [SimpleGroup $ Iden [Name "a"], SimpleGroup $ Iden [Name "b"]]])
>     ]
>   where
>     ms g = makeSelect {qeSelectList = [(Star,Nothing)]
>                       ,qeFrom = [TRSimple [Name "t"]]
>                       ,qeGroupBy = g}

> randomGroupBy :: TestItem
> randomGroupBy = Group "randomGroupBy" $ map (ParseQueryExpr ansi2011)
>     ["select * from t GROUP BY a"
>     ,"select * from t GROUP BY GROUPING SETS((a))"
>     ,"select * from t GROUP BY a,b,c"
>     ,"select * from t GROUP BY GROUPING SETS((a,b,c))"
>     ,"select * from t GROUP BY ROLLUP(a,b)"
>     ,"select * from t GROUP BY GROUPING SETS((a,b),\n\
>      \(a),\n\
>      \() )"
>     ,"select * from t GROUP BY ROLLUP(b,a)"
>     ,"select * from t GROUP BY GROUPING SETS((b,a),\n\
>      \(b),\n\
>      \() )"
>     ,"select * from t GROUP BY CUBE(a,b,c)"
>     ,"select * from t GROUP BY GROUPING SETS((a,b,c),\n\
>      \(a,b),\n\
>      \(a,c),\n\
>      \(b,c),\n\
>      \(a),\n\
>      \(b),\n\
>      \(c),\n\
>      \() )"
>     ,"select * from t GROUP BY ROLLUP(Province, County, City)"
>     ,"select * from t GROUP BY ROLLUP(Province, (County, City))"
>     ,"select * from t GROUP BY ROLLUP(Province, (County, City))"
>     ,"select * from t GROUP BY GROUPING SETS((Province, County, City),\n\
>      \(Province),\n\
>      \() )"
>     ,"select * from t GROUP BY GROUPING SETS((Province, County, City),\n\
>      \(Province, County),\n\
>      \(Province),\n\
>      \() )"
>     ,"select * from t GROUP BY a, ROLLUP(b,c)"
>     ,"select * from t GROUP BY GROUPING SETS((a,b,c),\n\
>      \(a,b),\n\
>      \(a) )"
>     ,"select * from t GROUP BY a, b, ROLLUP(c,d)"
>     ,"select * from t GROUP BY GROUPING SETS((a,b,c,d),\n\
>      \(a,b,c),\n\
>      \(a,b) )"
>     ,"select * from t GROUP BY ROLLUP(a), ROLLUP(b,c)"
>     ,"select * from t GROUP BY GROUPING SETS((a,b,c),\n\
>      \(a,b),\n\
>      \(a),\n\
>      \(b,c),\n\
>      \(b),\n\
>      \() )"
>     ,"select * from t GROUP BY ROLLUP(a), CUBE(b,c)"
>     ,"select * from t GROUP BY GROUPING SETS((a,b,c),\n\
>      \(a,b),\n\
>      \(a,c),\n\
>      \(a),\n\
>      \(b,c),\n\
>      \(b),\n\
>      \(c),\n\
>      \() )"
>     ,"select * from t GROUP BY CUBE(a,b), ROLLUP(c,d)"
>     ,"select * from t GROUP BY GROUPING SETS((a,b,c,d),\n\
>      \(a,b,c),\n\
>      \(a,b),\n\
>      \(a,c,d),\n\
>      \(a,c),\n\
>      \(a),\n\
>      \(b,c,d),\n\
>      \(b,c),\n\
>      \(b),\n\
>      \(c,d),\n\
>      \(c),\n\
>      \() )"
>     ,"select * from t GROUP BY a, ROLLUP(a,b)"
>     ,"select * from t GROUP BY GROUPING SETS((a,b),\n\
>      \(a) )"
>     ,"select * from t GROUP BY Region,\n\
>      \ROLLUP(Sales_Person, WEEK(Sales_Date)),\n\
>      \CUBE(YEAR(Sales_Date), MONTH (Sales_Date))"
>     ,"select * from t GROUP BY ROLLUP (Region, Sales_Person, WEEK(Sales_Date),\n\
>      \YEAR(Sales_Date), MONTH(Sales_Date) )"

>     ,"SELECT WEEK(SALES_DATE) AS WEEK,\n\
>      \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
>      \SALES_PERSON, SUM(SALES) AS UNITS_SOLD\n\
>      \FROM SALES\n\
>      \WHERE WEEK(SALES_DATE) = 13\n\
>      \GROUP BY WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE), SALES_PERSON\n\
>      \ORDER BY WEEK, DAY_WEEK, SALES_PERSON"

>     ,"SELECT WEEK(SALES_DATE) AS WEEK,\n\
>      \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
>      \SALES_PERSON, SUM(SALES) AS UNITS_SOLD\n\
>      \FROM SALES\n\
>      \WHERE WEEK(SALES_DATE) = 13\n\
>      \GROUP BY GROUPING SETS ( (WEEK(SALES_DATE), SALES_PERSON),\n\
>      \(DAYOFWEEK(SALES_DATE), SALES_PERSON))\n\
>      \ORDER BY WEEK, DAY_WEEK, SALES_PERSON"

>     ,"SELECT WEEK(SALES_DATE) AS WEEK,\n\
>      \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
>      \SALES_PERSON, SUM(SALES) AS UNITS_SOLD\n\
>      \FROM SALES\n\
>      \WHERE WEEK(SALES_DATE) = 13\n\
>      \GROUP BY ROLLUP ( WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE), SALES_PERSON )\n\
>      \ORDER BY WEEK, DAY_WEEK, SALES_PERSON"

>     ,"SELECT WEEK(SALES_DATE) AS WEEK,\n\
>      \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
>      \SALES_PERSON, SUM(SALES) AS UNITS_SOLD\n\
>      \FROM SALES\n\
>      \WHERE WEEK(SALES_DATE) = 13\n\
>      \GROUP BY CUBE ( WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE), SALES_PERSON )\n\
>      \ORDER BY WEEK, DAY_WEEK, SALES_PERSON"

>     ,"SELECT SALES_PERSON,\n\
>      \MONTH(SALES_DATE) AS MONTH,\n\
>      \SUM(SALES) AS UNITS_SOLD\n\
>      \FROM SALES\n\
>      \GROUP BY GROUPING SETS ( (SALES_PERSON, MONTH(SALES_DATE)),\n\
>      \()\n\
>      \)\n\
>      \ORDER BY SALES_PERSON, MONTH"

>     ,"SELECT WEEK(SALES_DATE) AS WEEK,\n\
>      \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
>      \SUM(SALES) AS UNITS_SOLD\n\
>      \FROM SALES\n\
>      \GROUP BY ROLLUP ( WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE) )\n\
>      \ORDER BY WEEK, DAY_WEEK"

>     ,"SELECT MONTH(SALES_DATE) AS MONTH,\n\
>      \REGION,\n\
>      \SUM(SALES) AS UNITS_SOLD\n\
>      \FROM SALES\n\
>      \GROUP BY ROLLUP ( MONTH(SALES_DATE), REGION )\n\
>      \ORDER BY MONTH, REGION"

>     ,"SELECT WEEK(SALES_DATE) AS WEEK,\n\
>      \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
>      \MONTH(SALES_DATE) AS MONTH,\n\
>      \REGION,\n\
>      \SUM(SALES) AS UNITS_SOLD\n\
>      \FROM SALES\n\
>      \GROUP BY GROUPING SETS ( ROLLUP( WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE) ),\n\
>      \ROLLUP( MONTH(SALES_DATE), REGION ) )\n\
>      \ORDER BY WEEK, DAY_WEEK, MONTH, REGION"

>     ,"SELECT R1, R2,\n\
>      \WEEK(SALES_DATE) AS WEEK,\n\
>      \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
>      \MONTH(SALES_DATE) AS MONTH,\n\
>      \REGION, SUM(SALES) AS UNITS_SOLD\n\
>      \FROM SALES,(VALUES('GROUP 1','GROUP 2')) AS X(R1,R2)\n\
>      \GROUP BY GROUPING SETS ((R1, ROLLUP(WEEK(SALES_DATE),\n\
>      \DAYOFWEEK(SALES_DATE))),\n\
>      \(R2,ROLLUP( MONTH(SALES_DATE), REGION ) ))\n\
>      \ORDER BY WEEK, DAY_WEEK, MONTH, REGION"

>     {-,"SELECT COALESCE(R1,R2) AS GROUP,\n\
>      \WEEK(SALES_DATE) AS WEEK,\n\
>      \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
>      \MONTH(SALES_DATE) AS MONTH,\n\
>      \REGION, SUM(SALES) AS UNITS_SOLD\n\
>      \FROM SALES,(VALUES('GROUP 1','GROUP 2')) AS X(R1,R2)\n\
>      \GROUP BY GROUPING SETS ((R1, ROLLUP(WEEK(SALES_DATE),\n\
>      \DAYOFWEEK(SALES_DATE))),\n\
>      \(R2,ROLLUP( MONTH(SALES_DATE), REGION ) ))\n\
>      \ORDER BY GROUP, WEEK, DAY_WEEK, MONTH, REGION"-}
>      -- as group - needs more subtle keyword blacklisting

>     -- decimal as a function not allowed due to the reserved keyword
>     -- handling: todo, review if this is ansi standard function or
>     -- if there are places where reserved keywords can still be used
>     ,"SELECT MONTH(SALES_DATE) AS MONTH,\n\
>      \REGION,\n\
>      \SUM(SALES) AS UNITS_SOLD,\n\
>      \MAX(SALES) AS BEST_SALE,\n\
>      \CAST(ROUND(AVG(DECIMALx(SALES)),2) AS DECIMAL(5,2)) AS AVG_UNITS_SOLD\n\
>      \FROM SALES\n\
>      \GROUP BY CUBE(MONTH(SALES_DATE),REGION)\n\
>      \ORDER BY MONTH, REGION"

>     ]
