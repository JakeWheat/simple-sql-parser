
{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.TestRunners
    (testLex
    ,lexFails
    ,testScalarExpr
    ,testQueryExpr
    ,testStatement
    ,testStatements
    ,testParseQueryExpr
    ,testParseQueryExprFails
    ,testParseScalarExprFails
    ,HasCallStack
    ) where

import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Pretty
import Language.SQL.SimpleSQL.Parse
import qualified Language.SQL.SimpleSQL.Lex as Lex

import Data.Text (Text)
import qualified Data.Text as T

import Language.SQL.SimpleSQL.Expectations
    (shouldParseL
    ,shouldFail
    ,shouldParseA
    ,shouldSucceed
    )
    
import Test.Hspec
    (it
    ,HasCallStack
    )

testLex :: HasCallStack => Dialect -> Text -> [Lex.Token] -> TestItem
testLex d input a =
    LexTest d input a $ do
        it (T.unpack input) $ Lex.lexSQL d False "" Nothing input `shouldParseL` a
        it (T.unpack $ "pp: " <> input) $ Lex.lexSQL d False "" Nothing (Lex.prettyTokens d a) `shouldParseL` a

lexFails :: HasCallStack => Dialect -> Text -> TestItem
lexFails d input =
    LexFails d input $ 
        it (T.unpack input) $ shouldFail $ Lex.lexSQL d False "" Nothing input

testScalarExpr :: HasCallStack => Dialect -> Text -> ScalarExpr -> TestItem
testScalarExpr d input a =
    TestScalarExpr d input a $ do
        it (T.unpack input) $ parseScalarExpr d "" Nothing input `shouldParseA` a
        it (T.unpack $ "pp: " <> input) $ parseScalarExpr d "" Nothing (prettyScalarExpr d a) `shouldParseA` a

testQueryExpr :: HasCallStack => Dialect -> Text -> QueryExpr -> TestItem
testQueryExpr d input a =
    TestQueryExpr d input a $ do
        it (T.unpack input) $ parseQueryExpr d "" Nothing input `shouldParseA` a
        it (T.unpack $ "pp: " <> input) $ parseQueryExpr d "" Nothing (prettyQueryExpr d a) `shouldParseA` a

testParseQueryExpr :: HasCallStack => Dialect -> Text -> TestItem
testParseQueryExpr d input =
    let a = parseQueryExpr d "" Nothing input
    in ParseQueryExpr d input $ do
        it (T.unpack input) $ shouldSucceed (T.unpack . prettyError) a
        case a of
            Left _ -> pure ()
            Right a' ->
                it (T.unpack $ "pp: " <> input) $
                parseQueryExpr d "" Nothing (prettyQueryExpr d a') `shouldParseA` a'

testParseQueryExprFails :: HasCallStack => Dialect -> Text -> TestItem
testParseQueryExprFails d input =
    ParseQueryExprFails d input $ 
        it (T.unpack input) $ shouldFail $ parseQueryExpr d "" Nothing input

testParseScalarExprFails :: HasCallStack => Dialect -> Text -> TestItem
testParseScalarExprFails d input =
    ParseScalarExprFails d input $ 
        it (T.unpack input) $ shouldFail $ parseScalarExpr d "" Nothing input

testStatement :: HasCallStack => Dialect -> Text -> Statement -> TestItem
testStatement d input a =
    TestStatement d input a $ do
        it (T.unpack input) $ parseStatement d "" Nothing input `shouldParseA` a
        it (T.unpack $ "pp: " <> input) $ parseStatement d "" Nothing (prettyStatement d a) `shouldParseA` a

testStatements :: HasCallStack => Dialect -> Text -> [Statement] -> TestItem
testStatements d input a =
    TestStatements d input a $ do
        it (T.unpack input) $ parseStatements d "" Nothing input `shouldParseA` a
        it (T.unpack $ "pp: " <> input) $ parseStatements d "" Nothing (prettyStatements d a) `shouldParseA` a

