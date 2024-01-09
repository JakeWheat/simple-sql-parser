
module Language.SQL.SimpleSQL.CustomDialect (customDialectTests) where

import Language.SQL.SimpleSQL.TestTypes

customDialectTests :: TestItem
customDialectTests = Group "custom dialect tests" (map (uncurry ParseQueryExpr) passTests
    ++ map (uncurry ParseScalarExprFails) failTests )
  where
    failTests = [(ansi2011,"SELECT DATE('2000-01-01')")
                ,(ansi2011,"SELECT DATE")
                ,(dateApp,"SELECT DATE")
                ,(dateIden,"SELECT DATE('2000-01-01')")
                -- show this never being allowed as an alias
                ,(ansi2011,"SELECT a date")
                ,(dateApp,"SELECT a date")
                ,(dateIden,"SELECT a date")
                ]
    passTests = [(ansi2011,"SELECT a b")
                ,(noDateKeyword,"SELECT DATE('2000-01-01')")
                ,(noDateKeyword,"SELECT DATE")
                ,(dateApp,"SELECT DATE('2000-01-01')")
                ,(dateIden,"SELECT DATE")
                ]
    noDateKeyword = ansi2011 {diKeywords = filter (/="date") (diKeywords ansi2011)}
    dateIden = ansi2011 {diIdentifierKeywords = "date" : diIdentifierKeywords ansi2011}
    dateApp = ansi2011 {diAppKeywords = "date" : diAppKeywords ansi2011}
