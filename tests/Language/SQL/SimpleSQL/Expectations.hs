
module Language.SQL.SimpleSQL.Expectations
    (shouldParseA
    ,shouldParseL
    ,shouldParse1
    ,shouldFail
    ,shouldSucceed
    ,shouldFailWith
    ) where


import Language.SQL.SimpleSQL.Parse
import qualified Language.SQL.SimpleSQL.Lex as Lex

import qualified Data.Text as T
import Data.Text (Text)

import Test.Hspec.Expectations
    (Expectation
    ,HasCallStack
    ,expectationFailure
    )

import Test.Hspec
    (shouldBe
    )

shouldParseA :: (HasCallStack,Eq a, Show a) => Either ParseError a -> a -> Expectation
shouldParseA = shouldParse1 (T.unpack . prettyError)

shouldParseL :: (HasCallStack,Eq a, Show a) => Either Lex.ParseError a -> a -> Expectation
shouldParseL = shouldParse1 (T.unpack . Lex.prettyError)

shouldParse1 :: (HasCallStack, Show a, Eq a) =>
               (e -> String)
            -> Either e a
            -> a
            -> Expectation    
shouldParse1 prettyErr r v = case r of
  Left e ->
    expectationFailure $
      "expected: "
        ++ show v
        ++ "\nbut parsing failed with error:\n"
        ++ prettyErr e
  Right x -> x `shouldBe` v

shouldFail :: (HasCallStack, Show a) => Either e a -> Expectation    
shouldFail r = case r of
  Left _ -> (1 :: Int) `shouldBe` 1
  Right a -> expectationFailure $ "expected parse failure, but succeeded with " <> show a

shouldFailWith :: (HasCallStack, Show a) => (e -> Text) -> Either e a -> Text -> Expectation    
shouldFailWith p r e = case r of
  Left e1 -> p e1 `shouldBe` e
  Right a -> expectationFailure $ "expected parse failure, but succeeded with " <> show a

shouldSucceed :: (HasCallStack) => (e -> String) -> Either e a -> Expectation
shouldSucceed pe r = case r of
  Left e -> expectationFailure $ "expected parse success, but got: " <> pe e
  Right _ -> (1 :: Int) `shouldBe` 1
