
{-
This is the main tests module which exposes the test data plus the
Test.Framework tests. It also contains the code which converts the
test data to the Test.Framework tests.
-}

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.Tests
    (testData
    ,tests
    ,TestItem(..)
    ) where

import Test.Hspec
    (SpecWith
    ,describe
    ,parallel
    )

import Language.SQL.SimpleSQL.TestTypes

import Language.SQL.SimpleSQL.FullQueries
import Language.SQL.SimpleSQL.GroupBy
import Language.SQL.SimpleSQL.Postgres
import Language.SQL.SimpleSQL.QueryExprComponents
import Language.SQL.SimpleSQL.QueryExprs
import Language.SQL.SimpleSQL.TableRefs
import Language.SQL.SimpleSQL.ScalarExprs
import Language.SQL.SimpleSQL.Odbc
import Language.SQL.SimpleSQL.Tpch
import Language.SQL.SimpleSQL.LexerTests
import Language.SQL.SimpleSQL.EmptyStatement
import Language.SQL.SimpleSQL.CreateIndex

import Language.SQL.SimpleSQL.SQL2011Queries
import Language.SQL.SimpleSQL.SQL2011AccessControl
import Language.SQL.SimpleSQL.SQL2011Bits
import Language.SQL.SimpleSQL.SQL2011DataManipulation
import Language.SQL.SimpleSQL.SQL2011Schema

import Language.SQL.SimpleSQL.MySQL
import Language.SQL.SimpleSQL.Oracle
import Language.SQL.SimpleSQL.CustomDialect
import Language.SQL.SimpleSQL.ErrorMessages

import qualified Data.Text as T

{-
Order the tests to start from the simplest first. This is also the
order on the generated documentation.
-}

testData :: TestItem
testData =
    Group "parserTest"
    [lexerTests
    ,scalarExprTests
    ,odbcTests
    ,queryExprComponentTests
    ,queryExprsTests
    ,tableRefTests
    ,groupByTests
    ,fullQueriesTests
    ,postgresTests
    ,tpchTests
    ,sql2011QueryTests
    ,sql2011DataManipulationTests
    ,sql2011SchemaTests
    ,sql2011AccessControlTests
    ,sql2011BitsTests
    ,mySQLTests
    ,oracleTests
    ,customDialectTests
    ,emptyStatementTests
    ,createIndexTests
    ,errorMessageTests
    ]

tests :: SpecWith ()
tests = parallel $ itemToTest testData

itemToTest :: TestItem -> SpecWith ()
itemToTest (Group nm ts) =
    describe (T.unpack nm) $ mapM_ itemToTest ts
itemToTest (TestScalarExpr _ _ _ t) = t
itemToTest (TestQueryExpr _ _ _ t) = t
itemToTest (TestStatement _ _ _ t) = t
itemToTest (TestStatements _ _ _ t) = t
itemToTest (ParseQueryExpr _ _ t) = t
itemToTest (ParseQueryExprFails _ _ t) = t
itemToTest (ParseScalarExprFails _ _ t) = t
itemToTest (LexTest _ _ _ t) = t
itemToTest (LexFails _ _ t) = t
itemToTest (GeneralParseFailTest _ _ t) = t
