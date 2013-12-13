
> module Tests where

> import Syntax
> import Pretty
> import Parser

> data TestItem = Group String [TestItem]
>               | TestScalarExpr String ScalarExpr
>               | TestQueryExpr String QueryExpr
>                 deriving (Eq,Show)

> scalarExprParserTests :: TestItem
> scalarExprParserTests = Group "scalarExprParserTests" []

> queryExprParserTests :: TestItem
> queryExprParserTests = Group "queryExprParserTests" []

> testData :: TestItem
> testData =
>     Group "parserTest"
>     [scalarExprParserTests
>     ,queryExprParserTests]
