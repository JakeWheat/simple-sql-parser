
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
> import Language.SQL.SimpleSQL.Parse
> import Language.SQL.SimpleSQL.Lex

> import Language.SQL.SimpleSQL.TestTypes

> import Language.SQL.SimpleSQL.FullQueries
> import Language.SQL.SimpleSQL.GroupBy
> import Language.SQL.SimpleSQL.Postgres
> import Language.SQL.SimpleSQL.QueryExprComponents
> import Language.SQL.SimpleSQL.QueryExprs
> import Language.SQL.SimpleSQL.TableRefs
> import Language.SQL.SimpleSQL.ScalarExprs
> import Language.SQL.SimpleSQL.Odbc
> import Language.SQL.SimpleSQL.Tpch
> import Language.SQL.SimpleSQL.LexerTests

> import Language.SQL.SimpleSQL.SQL2011Queries
> import Language.SQL.SimpleSQL.SQL2011AccessControl
> import Language.SQL.SimpleSQL.SQL2011Bits
> import Language.SQL.SimpleSQL.SQL2011DataManipulation
> import Language.SQL.SimpleSQL.SQL2011Schema

> import Language.SQL.SimpleSQL.MySQL

Order the tests to start from the simplest first. This is also the
order on the generated documentation.

> testData :: TestItem
> testData =
>     Group "parserTest"
>     [lexerTests
>     ,scalarExprTests
>     ,odbcTests
>     ,queryExprComponentTests
>     ,queryExprsTests
>     ,tableRefTests
>     ,groupByTests
>     ,fullQueriesTests
>     ,postgresTests
>     ,tpchTests
>     ,sql2011QueryTests
>     ,sql2011DataManipulationTests
>     ,sql2011SchemaTests
>     ,sql2011AccessControlTests
>     ,sql2011BitsTests
>     ,mySQLTests
>     ]

> tests :: T.TestTree
> tests = itemToTest testData

> --runTests :: IO ()
> --runTests = void $ H.runTestTT $ itemToTest testData

> itemToTest :: TestItem -> T.TestTree
> itemToTest (Group nm ts) =
>     T.testGroup nm $ map itemToTest ts
> itemToTest (TestScalarExpr d str expected) =
>     toTest parseScalarExpr prettyScalarExpr d str expected
> itemToTest (TestQueryExpr d str expected) =
>     toTest parseQueryExpr prettyQueryExpr d str expected
> itemToTest (TestStatement d str expected) =
>     toTest parseStatement prettyStatement d str expected
> itemToTest (TestStatements d str expected) =
>     toTest parseStatements prettyStatements d str expected
> itemToTest (ParseQueryExpr d str) =
>     toPTest parseQueryExpr prettyQueryExpr d str

> itemToTest (ParseQueryExprFails d str) =
>     toFTest parseQueryExpr prettyQueryExpr d str

> itemToTest (ParseScalarExprFails d str) =
>     toFTest parseScalarExpr prettyScalarExpr d str

> itemToTest (LexTest d s ts) = makeLexerTest d s ts
> itemToTest (LexFails d s) = makeLexingFailsTest d s

> makeLexerTest :: Dialect -> String -> [Token] -> T.TestTree
> makeLexerTest d s ts = H.testCase s $ do
>     let lx = either (error . show) id $ lexSQL d "" Nothing s
>     H.assertEqual "" ts $ map snd lx
>     let s' = prettyTokens d $ map snd lx
>     H.assertEqual "pretty print" s s'

> makeLexingFailsTest :: Dialect -> String -> T.TestTree
> makeLexingFailsTest d s = H.testCase s $ do
>     case lexSQL d "" Nothing s of
>          Right x -> H.assertFailure $ "lexing should have failed: " ++ s ++ "\ngot: " ++ show x
>          Left _ -> return ()


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
> toFTest parser _pp d str = H.testCase str $ do
>         let egot = parser d "" Nothing str
>         case egot of
>             Left _e -> return ()
>             Right _got ->
>               H.assertFailure $ "parse didn't fail: " ++ show d ++ "\n" ++ str
