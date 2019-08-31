
> module Language.SQL.SimpleSQL.CustomDialect (customDialectTests) where

> import Language.SQL.SimpleSQL.TestTypes

> customDialectTests :: TestItem
> customDialectTests = Group "custom dialect tests" (map (ParseQueryExpr myDialect) sometests
>     ++ [ParseScalarExprFails ansi2011 "SELECT DATE('2000-01-01')"])
>   where
>     myDialect = ansi2011 {diKeywords = filter (/="date") (diKeywords ansi2011)}
>     sometests = ["SELECT DATE('2000-01-01')"]
