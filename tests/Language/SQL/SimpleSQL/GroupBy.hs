
-- Here are the tests for the group by component of query exprs

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.GroupBy (groupByTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestRunners
import Data.Text (Text)


groupByTests :: TestItem
groupByTests = Group "groupByTests"
    [simpleGroupBy
    ,newGroupBy
    ,randomGroupBy
    ]

q :: HasCallStack => Text -> QueryExpr -> TestItem
q src a = testQueryExpr ansi2011 src a

p :: HasCallStack => Text -> TestItem
p src = testParseQueryExpr ansi2011 src



simpleGroupBy :: TestItem
simpleGroupBy = Group "simpleGroupBy"
    [q "select a,sum(b) from t group by a"
     $ toQueryExpr $ makeSelect {msSelectList = [(Iden [Name Nothing "a"],Nothing)
                                 ,(App [Name Nothing "sum"] [Iden [Name Nothing "b"]],Nothing)]
                 ,msFrom = [TRSimple [Name Nothing "t"]]
                 ,msGroupBy = [SimpleGroup $ Iden [Name Nothing "a"]]
                 }

    ,q "select a,b,sum(c) from t group by a,b"
     $ toQueryExpr $ makeSelect {msSelectList = [(Iden [Name Nothing "a"],Nothing)
                                 ,(Iden [Name Nothing "b"],Nothing)
                                 ,(App [Name Nothing "sum"] [Iden [Name Nothing "c"]],Nothing)]
                 ,msFrom = [TRSimple [Name Nothing "t"]]
                 ,msGroupBy = [SimpleGroup $ Iden [Name Nothing "a"]
                              ,SimpleGroup $ Iden [Name Nothing "b"]]
                 }
    ]

{-
test the new group by (), grouping sets, cube and rollup syntax (not
sure which sql version they were introduced, 1999 or 2003 I think).
-}

newGroupBy :: TestItem
newGroupBy = Group "newGroupBy"
    [q "select * from t group by ()" $ ms [GroupingParens []]
    ,q "select * from t group by grouping sets ((), (a))"
     $ ms [GroupingSets [GroupingParens []
                       ,GroupingParens [SimpleGroup $ Iden [Name Nothing "a"]]]]
    ,q "select * from t group by cube(a,b)"
     $ ms [Cube [SimpleGroup $ Iden [Name Nothing "a"], SimpleGroup $ Iden [Name Nothing "b"]]]
    ,q "select * from t group by rollup(a,b)"
     $ ms [Rollup [SimpleGroup $ Iden [Name Nothing "a"], SimpleGroup $ Iden [Name Nothing "b"]]]
    ]
  where
    ms g = toQueryExpr $ makeSelect {msSelectList = [(Star,Nothing)]
                      ,msFrom = [TRSimple [Name Nothing "t"]]
                      ,msGroupBy = g}

randomGroupBy :: TestItem
randomGroupBy = Group "randomGroupBy"
    [p "select * from t GROUP BY a"
    ,p "select * from t GROUP BY GROUPING SETS((a))"
    ,p "select * from t GROUP BY a,b,c"
    ,p "select * from t GROUP BY GROUPING SETS((a,b,c))"
    ,p "select * from t GROUP BY ROLLUP(a,b)"
    ,p "select * from t GROUP BY GROUPING SETS((a,b),\n\
     \(a),\n\
     \() )"
    ,p "select * from t GROUP BY ROLLUP(b,a)"
    ,p "select * from t GROUP BY GROUPING SETS((b,a),\n\
     \(b),\n\
     \() )"
    ,p "select * from t GROUP BY CUBE(a,b,c)"
    ,p "select * from t GROUP BY GROUPING SETS((a,b,c),\n\
     \(a,b),\n\
     \(a,c),\n\
     \(b,c),\n\
     \(a),\n\
     \(b),\n\
     \(c),\n\
     \() )"
    ,p "select * from t GROUP BY ROLLUP(Province, County, City)"
    ,p "select * from t GROUP BY ROLLUP(Province, (County, City))"
    ,p "select * from t GROUP BY ROLLUP(Province, (County, City))"
    ,p "select * from t GROUP BY GROUPING SETS((Province, County, City),\n\
     \(Province),\n\
     \() )"
    ,p "select * from t GROUP BY GROUPING SETS((Province, County, City),\n\
     \(Province, County),\n\
     \(Province),\n\
     \() )"
    ,p "select * from t GROUP BY a, ROLLUP(b,c)"
    ,p "select * from t GROUP BY GROUPING SETS((a,b,c),\n\
     \(a,b),\n\
     \(a) )"
    ,p "select * from t GROUP BY a, b, ROLLUP(c,d)"
    ,p "select * from t GROUP BY GROUPING SETS((a,b,c,d),\n\
     \(a,b,c),\n\
     \(a,b) )"
    ,p "select * from t GROUP BY ROLLUP(a), ROLLUP(b,c)"
    ,p "select * from t GROUP BY GROUPING SETS((a,b,c),\n\
     \(a,b),\n\
     \(a),\n\
     \(b,c),\n\
     \(b),\n\
     \() )"
    ,p "select * from t GROUP BY ROLLUP(a), CUBE(b,c)"
    ,p "select * from t GROUP BY GROUPING SETS((a,b,c),\n\
     \(a,b),\n\
     \(a,c),\n\
     \(a),\n\
     \(b,c),\n\
     \(b),\n\
     \(c),\n\
     \() )"
    ,p "select * from t GROUP BY CUBE(a,b), ROLLUP(c,d)"
    ,p "select * from t GROUP BY GROUPING SETS((a,b,c,d),\n\
     \(a,b,c),\n\
     \(a,b),\n\
     \(a,c,d),\n\
     \(a,c),\n\
     \(a),\n\
     \(b,c,d),\n\
     \(b,c),\n\
     \(b),\n\
     \(c,d),\n\
     \(c),\n\
     \() )"
    ,p "select * from t GROUP BY a, ROLLUP(a,b)"
    ,p "select * from t GROUP BY GROUPING SETS((a,b),\n\
     \(a) )"
    ,p "select * from t GROUP BY Region,\n\
     \ROLLUP(Sales_Person, WEEK(Sales_Date)),\n\
     \CUBE(YEAR(Sales_Date), MONTH (Sales_Date))"
    ,p "select * from t GROUP BY ROLLUP (Region, Sales_Person, WEEK(Sales_Date),\n\
     \YEAR(Sales_Date), MONTH(Sales_Date) )"

    ,p "SELECT WEEK(SALES_DATE) AS WEEK,\n\
     \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
     \SALES_PERSON, SUM(SALES) AS UNITS_SOLD\n\
     \FROM SALES\n\
     \WHERE WEEK(SALES_DATE) = 13\n\
     \GROUP BY WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE), SALES_PERSON\n\
     \ORDER BY WEEK, DAY_WEEK, SALES_PERSON"

    ,p "SELECT WEEK(SALES_DATE) AS WEEK,\n\
     \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
     \SALES_PERSON, SUM(SALES) AS UNITS_SOLD\n\
     \FROM SALES\n\
     \WHERE WEEK(SALES_DATE) = 13\n\
     \GROUP BY GROUPING SETS ( (WEEK(SALES_DATE), SALES_PERSON),\n\
     \(DAYOFWEEK(SALES_DATE), SALES_PERSON))\n\
     \ORDER BY WEEK, DAY_WEEK, SALES_PERSON"

    ,p "SELECT WEEK(SALES_DATE) AS WEEK,\n\
     \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
     \SALES_PERSON, SUM(SALES) AS UNITS_SOLD\n\
     \FROM SALES\n\
     \WHERE WEEK(SALES_DATE) = 13\n\
     \GROUP BY ROLLUP ( WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE), SALES_PERSON )\n\
     \ORDER BY WEEK, DAY_WEEK, SALES_PERSON"

    ,p "SELECT WEEK(SALES_DATE) AS WEEK,\n\
     \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
     \SALES_PERSON, SUM(SALES) AS UNITS_SOLD\n\
     \FROM SALES\n\
     \WHERE WEEK(SALES_DATE) = 13\n\
     \GROUP BY CUBE ( WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE), SALES_PERSON )\n\
     \ORDER BY WEEK, DAY_WEEK, SALES_PERSON"

    ,p "SELECT SALES_PERSON,\n\
     \MONTH(SALES_DATE) AS MONTH,\n\
     \SUM(SALES) AS UNITS_SOLD\n\
     \FROM SALES\n\
     \GROUP BY GROUPING SETS ( (SALES_PERSON, MONTH(SALES_DATE)),\n\
     \()\n\
     \)\n\
     \ORDER BY SALES_PERSON, MONTH"

    ,p "SELECT WEEK(SALES_DATE) AS WEEK,\n\
     \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
     \SUM(SALES) AS UNITS_SOLD\n\
     \FROM SALES\n\
     \GROUP BY ROLLUP ( WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE) )\n\
     \ORDER BY WEEK, DAY_WEEK"

    ,p "SELECT MONTH(SALES_DATE) AS MONTH,\n\
     \REGION,\n\
     \SUM(SALES) AS UNITS_SOLD\n\
     \FROM SALES\n\
     \GROUP BY ROLLUP ( MONTH(SALES_DATE), REGION )\n\
     \ORDER BY MONTH, REGION"

    ,p "SELECT WEEK(SALES_DATE) AS WEEK,\n\
     \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
     \MONTH(SALES_DATE) AS MONTH,\n\
     \REGION,\n\
     \SUM(SALES) AS UNITS_SOLD\n\
     \FROM SALES\n\
     \GROUP BY GROUPING SETS ( ROLLUP( WEEK(SALES_DATE), DAYOFWEEK(SALES_DATE) ),\n\
     \ROLLUP( MONTH(SALES_DATE), REGION ) )\n\
     \ORDER BY WEEK, DAY_WEEK, MONTH, REGION"

    ,p "SELECT R1, R2,\n\
     \WEEK(SALES_DATE) AS WEEK,\n\
     \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
     \MONTH(SALES_DATE) AS MONTH,\n\
     \REGION, SUM(SALES) AS UNITS_SOLD\n\
     \FROM SALES,(VALUES('GROUP 1','GROUP 2')) AS X(R1,R2)\n\
     \GROUP BY GROUPING SETS ((R1, ROLLUP(WEEK(SALES_DATE),\n\
     \DAYOFWEEK(SALES_DATE))),\n\
     \(R2,ROLLUP( MONTH(SALES_DATE), REGION ) ))\n\
     \ORDER BY WEEK, DAY_WEEK, MONTH, REGION"

    {-,p "SELECT COALESCE(R1,R2) AS GROUP,\n\
     \WEEK(SALES_DATE) AS WEEK,\n\
     \DAYOFWEEK(SALES_DATE) AS DAY_WEEK,\n\
     \MONTH(SALES_DATE) AS MONTH,\n\
     \REGION, SUM(SALES) AS UNITS_SOLD\n\
     \FROM SALES,(VALUES('GROUP 1','GROUP 2')) AS X(R1,R2)\n\
     \GROUP BY GROUPING SETS ((R1, ROLLUP(WEEK(SALES_DATE),\n\
     \DAYOFWEEK(SALES_DATE))),\n\
     \(R2,ROLLUP( MONTH(SALES_DATE), REGION ) ))\n\
     \ORDER BY GROUP, WEEK, DAY_WEEK, MONTH, REGION"-}
     -- as group - needs more subtle keyword blacklisting

    -- decimal as a function not allowed due to the reserved keyword
    -- handling: todo, review if this is ansi standard function or
    -- if there are places where reserved keywords can still be used
    ,p "SELECT MONTH(SALES_DATE) AS MONTH,\n\
     \REGION,\n\
     \SUM(SALES) AS UNITS_SOLD,\n\
     \MAX(SALES) AS BEST_SALE,\n\
     \CAST(ROUND(AVG(DECIMALx(SALES)),2) AS DECIMAL(5,2)) AS AVG_UNITS_SOLD\n\
     \FROM SALES\n\
     \GROUP BY CUBE(MONTH(SALES_DATE),REGION)\n\
     \ORDER BY MONTH, REGION"

    ]
