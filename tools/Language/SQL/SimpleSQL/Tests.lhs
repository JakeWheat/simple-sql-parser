
This is the main tests module which exposes the test data plus the
Test.Framework tests. It also contains the code which converts the
test data to the Test.Framework tests.

> module Language.SQL.SimpleSQL.Tests
>     (testData
>     ,tests
>     ,TestItem(..)
>     ) where

> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as H

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

> import Language.SQL.SimpleSQL.SQL2011

> import Language.SQL.SimpleSQL.MySQL

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
>     ,sql2011Tests
>     ,mySQLTests
>     ]

> tests :: T.TestTree
> tests = itemToTest testData

> --runTests :: IO ()
> --runTests = void $ H.runTestTT $ itemToTest testData

> itemToTest :: TestItem -> T.TestTree
> itemToTest (Group nm ts) =
>     T.testGroup nm $ map itemToTest ts
> itemToTest (TestValueExpr d str expected) =
>     toTest parseValueExpr prettyValueExpr d str expected
> itemToTest (TestQueryExpr d str expected) =
>     toTest parseQueryExpr prettyQueryExpr d str expected
> itemToTest (TestQueryExprs d str expected) =
>     toTest parseQueryExprs prettyQueryExprs d str expected
> itemToTest (ParseQueryExpr d str) =
>     toPTest parseQueryExpr prettyQueryExpr d str

> itemToTest (ParseQueryExprFails d str) =
>     toFTest parseQueryExpr prettyQueryExpr d str

> itemToTest (ParseValueExprFails d str) =
>     toFTest parseValueExpr prettyValueExpr d str


> toTest :: (Eq a, Show a) =>
>           (Dialect -> String -> Maybe (Int,Int) -> String -> Either ParseError a)
>        -> (Dialect -> a -> String)
>        -> Dialect
>        -> String
>        -> a
>        -> T.TestTree
> toTest parser pp d str expected = H.testCase str $ do
>         let egot = parser d "" Nothing str
>         case egot of
>             Left e -> H.assertFailure $ peFormattedError e
>             Right got -> do
>                 H.assertEqual "" expected got
>                 let str' = pp d got
>                 let egot' = parser d "" Nothing str'
>                 case egot' of
>                     Left e' -> H.assertFailure $ "pp roundtrip"
>                                                  ++ "\n" ++ str'
>                                                  ++ peFormattedError e'
>                     Right got' -> H.assertEqual
>                                   ("pp roundtrip" ++ "\n" ++ str')
>                                    expected got'

> toPTest :: (Eq a, Show a) =>
>           (Dialect -> String -> Maybe (Int,Int) -> String -> Either ParseError a)
>        -> (Dialect -> a -> String)
>        -> Dialect
>        -> String
>        -> T.TestTree
> toPTest parser pp d str = H.testCase str $ do
>         let egot = parser d "" Nothing str
>         case egot of
>             Left e -> H.assertFailure $ peFormattedError e
>             Right got -> do
>                 let str' = pp d got
>                 let egot' = parser d "" Nothing str'
>                 case egot' of
>                     Left e' -> H.assertFailure $ "pp roundtrip "
>                                                  ++ "\n" ++ str' ++ "\n"
>                                                  ++ peFormattedError e'
>                     Right _got' -> return ()


> toFTest :: (Eq a, Show a) =>
>           (Dialect -> String -> Maybe (Int,Int) -> String -> Either ParseError a)
>        -> (Dialect -> a -> String)
>        -> Dialect
>        -> String
>        -> T.TestTree
> toFTest parser pp d str = H.testCase str $ do
>         let egot = parser d "" Nothing str
>         case egot of
>             Left e -> return ()
>             Right got ->
>               H.assertFailure $ "parse didn't fail: " ++ show d ++ "\n" ++ str
