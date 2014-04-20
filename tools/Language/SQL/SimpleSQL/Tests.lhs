
This is the main tests module which exposes the test data plus the
Test.Framework tests. It also contains the code which converts the
test data to the Test.Framework tests.

> module Language.SQL.SimpleSQL.Tests
>     (testData
>     ,tests
>     ,TestItem(..)
>     ) where

> import Test.Framework
> import Test.Framework.Providers.HUnit
> import qualified Test.HUnit as H

> --import Language.SQL.SimpleSQL.Syntax
> import Language.SQL.SimpleSQL.Pretty
> import Language.SQL.SimpleSQL.Parser

> import Language.SQL.SimpleSQL.TestTypes

> import Language.SQL.SimpleSQL.FullQueries
> import Language.SQL.SimpleSQL.GroupBy
> import Language.SQL.SimpleSQL.Postgres
> import Language.SQL.SimpleSQL.QueryExprComponents
> import Language.SQL.SimpleSQL.QueryExprs
> import Language.SQL.SimpleSQL.TableRefs
> import Language.SQL.SimpleSQL.ValueExprs
> import Language.SQL.SimpleSQL.Tpch

> import Language.SQL.SimpleSQL.SQL2003
> import Language.SQL.SimpleSQL.SQL2011

Order the tests to start from the simplest first. This is also the
order on the generated documentation.

> testData :: TestItem
> testData =
>     Group "parserTest"
>     [valueExprTests
>     ,queryExprComponentTests
>     ,queryExprsTests
>     ,tableRefTests
>     ,groupByTests
>     ,fullQueriesTests
>     ,postgresTests
>     ,tpchTests
>     ,sql2003Tests
>     ,sql2011Tests
>     ]

> tests :: Test.Framework.Test
> tests = itemToTest testData

> --runTests :: IO ()
> --runTests = void $ H.runTestTT $ itemToTest testData

> itemToTest :: TestItem -> Test.Framework.Test
> itemToTest (Group nm ts) =
>     testGroup nm $ map itemToTest ts
> itemToTest (TestValueExpr str expected) =
>     toTest parseValueExpr prettyValueExpr str expected
> itemToTest (TestQueryExpr str expected) =
>     toTest parseQueryExpr prettyQueryExpr str expected
> itemToTest (TestQueryExprs str expected) =
>     toTest parseQueryExprs prettyQueryExprs str expected
> itemToTest (ParseQueryExpr str) =
>     toPTest parseQueryExpr prettyQueryExpr str

> toTest :: (Eq a, Show a) =>
>           (String -> Maybe (Int,Int) -> String -> Either ParseError a)
>        -> (a -> String)
>        -> String
>        -> a
>        -> Test.Framework.Test
> toTest parser pp str expected = testCase str $ do
>         let egot = parser "" Nothing str
>         case egot of
>             Left e -> H.assertFailure $ peFormattedError e
>             Right got -> do
>                 H.assertEqual "" expected got
>                 let str' = pp got
>                 let egot' = parser "" Nothing str'
>                 case egot' of
>                     Left e' -> H.assertFailure $ "pp roundtrip"
>                                                  ++ "\n" ++ str'
>                                                  ++ peFormattedError e'
>                     Right got' -> H.assertEqual
>                                   ("pp roundtrip" ++ "\n" ++ str')
>                                    expected got'

> toPTest :: (Eq a, Show a) =>
>           (String -> Maybe (Int,Int) -> String -> Either ParseError a)
>        -> (a -> String)
>        -> String
>        -> Test.Framework.Test
> toPTest parser pp str = testCase str $ do
>         let egot = parser "" Nothing str
>         case egot of
>             Left e -> H.assertFailure $ peFormattedError e
>             Right got -> do
>                 let str' = pp got
>                 let egot' = parser "" Nothing str'
>                 case egot' of
>                     Left e' -> H.assertFailure $ "pp roundtrip "
>                                                  ++ "\n" ++ str' ++ "\n"
>                                                  ++ peFormattedError e'
>                     Right _got' -> return ()
