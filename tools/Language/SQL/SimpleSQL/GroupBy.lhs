
Here are the tests for the group by component of query exprs

> {-# LANGUAGE OverloadedStrings #-}
> module Language.SQL.SimpleSQL.GroupBy (groupByTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax


> groupByTests :: TestItem
> groupByTests = Group "groupByTests"
>     [simpleGroupBy
>     ,newGroupBy
>     ]

> simpleGroupBy :: TestItem
> simpleGroupBy = Group "simpleGroupBy" $ map (uncurry TestQueryExpr)
>     [("select a,sum(b) from t group by a"
>      ,makeSelect {qeSelectList = [(Nothing, Iden "a")
>                                  ,(Nothing, App "sum" [Iden "b"])]
>                  ,qeFrom = [TRSimple "t"]
>                  ,qeGroupBy = [SimpleGroup $ Iden "a"]
>                  })

>     ,("select a,b,sum(c) from t group by a,b"
>      ,makeSelect {qeSelectList = [(Nothing, Iden "a")
>                                  ,(Nothing, Iden "b")
>                                  ,(Nothing, App "sum" [Iden "c"])]
>                  ,qeFrom = [TRSimple "t"]
>                  ,qeGroupBy = [SimpleGroup $ Iden "a"
>                               ,SimpleGroup $ Iden "b"]
>                  })
>     ]

test the new group by (), grouping sets, cube and rollup syntax (not
sure which sql version they were introduced, 1999 or 2003 I think).

> newGroupBy :: TestItem
> newGroupBy = Group "newGroupBy" $ map (uncurry TestQueryExpr)
>     [("select * from t group by ()", ms [GroupingParens []])
>     ,("select * from t group by grouping sets ((), (a))"
>      ,ms [GroupingSets [GroupingParens []
>                        ,GroupingParens [SimpleGroup $ Iden "a"]]])
>     ,("select * from t group by cube(a,b)"
>      ,ms [Cube [SimpleGroup $ Iden "a", SimpleGroup $ Iden "b"]])
>     ,("select * from t group by rollup(a,b)"
>      ,ms [Rollup [SimpleGroup $ Iden "a", SimpleGroup $ Iden "b"]])


GROUP BY a
GROUP BY GROUPING SETS((a))
GROUP BY a,b,c
GROUP BY GROUPING SETS((a,b,c))
GROUP BY ROLLUP(a,b)
GROUP BY GROUPING SETS((a,b)
(a)
() )
GROUP BY ROLLUP(b,a)
GROUP BY GROUPING SETS((b,a)
(b)
() )
GROUP BY CUBE(a,b,c)
GROUP BY GROUPING SETS((a,b,c)
(a,b)
(a,c)
(b,c)
(a)
(b)
(c)
() )
GROUP BY ROLLUP(Province, County, City)
GROUP BY ROLLUP(Province, (County, City))
GROUP BY ROLLUP(Province, (County, City))
GROUP BY GROUPING SETS((Province, County, City)
(Province)
() )
GROUP BY GROUPING SETS((Province, County, City)
(Province, County)
(Province)
() )
GROUP BY a, ROLLUP(b,c)
GROUP BY GROUPING SETS((a,b,c)
(a,b)
(a) )
GROUP BY a, b, ROLLUP(c,d)
GROUP BY GROUPING SETS((a,b,c,d)
(a,b,c)
(a,b) )
GROUP BY ROLLUP(a), ROLLUP(b,c)
GROUP BY GROUPING SETS((a,b,c)
(a,b)
(a)
(b,c)
(b)
() )
GROUP BY ROLLUP(a), CUBE(b,c)
GROUP BY GROUPING SETS((a,b,c)
(a,b)
(a,c)
(a)
(b,c)
(b)
(c)
() )
GROUP BY CUBE(a,b), ROLLUP(c,d)
GROUP BY GROUPING SETS((a,b,c,d)
(a,b,c)
(a,b)
(a,c,d)
(a,c)
(a)
(b,c,d)
(b,c)
(b)
(c,d)
(c)
() )
GROUP BY a, ROLLUP(a,b)
GROUP BY GROUPING SETS((a,b)
(a) )
GROUP BY Region,
ROLLUP(Sales_Person, WEEK(Sales_Date)),
CUBE(YEAR(Sales_Date), MONTH (Sales_Date))
GROUP BY ROLLUP (Region, Sales_Person, WEEK(Sales_Date),
YEAR(Sales_Date), MONTH(Sales_Date) )


SELECT WEEK(SALES_DATE) AS WEEK,
DAYOFWEEK(SALES_DATE) AS DAY_WEEK,
SALES_PERSON, SUM(SALES) AS UNITS_SOLD
FROM SALES
WHERE WEEK(SALES_DATE) = 13
GROUP BY WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE), SALES_PERSON
ORDER BY WEEK, DAY_WEEK, SALES_PERSON

SELECT WEEK(SALES_DATE) AS WEEK,
DAYOFWEEK(SALES_DATE) AS DAY_WEEK,
SALES_PERSON, SUM(SALES) AS UNITS_SOLD
FROM SALES
WHERE WEEK(SALES_DATE) = 13
GROUP BY GROUPING SETS ( (WEEK(SALES_DATE), SALES_PERSON),
(DAYOFWEEK(SALES_DATE), SALES_PERSON))
ORDER BY WEEK, DAY_WEEK, SALES_PERSON

SELECT WEEK(SALES_DATE) AS WEEK,
DAYOFWEEK(SALES_DATE) AS DAY_WEEK,
SALES_PERSON, SUM(SALES) AS UNITS_SOLD
FROM SALES
WHERE WEEK(SALES_DATE) = 13
GROUP BY ROLLUP ( WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE), SALES_PERSON )
ORDER BY WEEK, DAY_WEEK, SALES_PERSON

SELECT WEEK(SALES_DATE) AS WEEK,
DAYOFWEEK(SALES_DATE) AS DAY_WEEK,
SALES_PERSON, SUM(SALES) AS UNITS_SOLD
FROM SALES
WHERE WEEK(SALES_DATE) = 13
GROUP BY CUBE ( WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE), SALES_PERSON )
ORDER BY WEEK, DAY_WEEK, SALES_PERSON

SELECT SALES_PERSON,
MONTH(SALES_DATE) AS MONTH,
SUM(SALES) AS UNITS_SOLD
FROM SALES
GROUP BY GROUPING SETS ( (SALES_PERSON, MONTH(SALES_DATE)),
()
)
ORDER BY SALES_PERSON, MONTH

SELECT WEEK(SALES_DATE) AS WEEK,
DAYOFWEEK(SALES_DATE) AS DAY_WEEK,
SUM(SALES) AS UNITS_SOLD
FROM SALES
GROUP BY ROLLUP ( WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE) )
ORDER BY WEEK, DAY_WEEK

SELECT MONTH(SALES_DATE) AS MONTH,
REGION,
SUM(SALES) AS UNITS_SOLD
FROM SALES
GROUP BY ROLLUP ( MONTH(SALES_DATE), REGION );
ORDER BY MONTH, REGION

SELECT WEEK(SALES_DATE) AS WEEK,
DAYOFWEEK(SALES_DATE) AS DAY_WEEK,
MONTH(SALES_DATE) AS MONTH,
REGION,
SUM(SALES) AS UNITS_SOLD
FROM SALES
GROUP BY GROUPING SETS ( ROLLUP( WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE) ),
ROLLUP( MONTH(SALES_DATE), REGION ) )
ORDER BY WEEK, DAY_WEEK, MONTH, REGION

SELECT R1, R2,
WEEK(SALES_DATE) AS WEEK,
DAYOFWEEK(SALES_DATE) AS DAY_WEEK,
MONTH(SALES_DATE) AS MONTH,
REGION, SUM(SALES) AS UNITS_SOLD
FROM SALES,(VALUES(’GROUP 1’,’GROUP 2’)) AS X(R1,R2)
GROUP BY GROUPING SETS ((R1, ROLLUP(WEEK(SALES_DATE),
DAYOFWEEK(SALES_DATE))),
(R2,ROLLUP( MONTH(SALES_DATE), REGION ) )
ORDER BY WEEK, DAY_WEEK, MONTH, REGION
)

SELECT COALESCE(R1,R2) AS GROUP,
WEEK(SALES_DATE) AS WEEK,
DAYOFWEEK(SALES_DATE) AS DAY_WEEK,
MONTH(SALES_DATE) AS MONTH,
REGION, SUM(SALES) AS UNITS_SOLD
FROM SALES,(VALUES(’GROUP 1’,’GROUP 2’)) AS X(R1,R2)
GROUP BY GROUPING SETS ((R1, ROLLUP(WEEK(SALES_DATE),
DAYOFWEEK(SALES_DATE))),
(R2,ROLLUP( MONTH(SALES_DATE), REGION ) )
ORDER BY GROUP, WEEK, DAY_WEEK, MONTH, REGION;
)

SELECT MONTH(SALES_DATE) AS MONTH,
REGION,
SUM(SALES) AS UNITS_SOLD,
MAX(SALES) AS BEST_SALE,
824
SQL Reference Volume 1
Examples of grouping sets, cube, and rollup queries
CAST(ROUND(AVG(DECIMAL(SALES)),2) AS DECIMAL(5,2)) AS AVG_UNITS_SOLD
FROM SALES
GROUP BY CUBE(MONTH(SALES_DATE),REGION)
ORDER BY MONTH, REGION

>     ]
>   where
>     ms g = makeSelect {qeSelectList = [(Nothing,Star)]
>                       ,qeFrom = [TRSimple "t"]
>                       ,qeGroupBy = g}
