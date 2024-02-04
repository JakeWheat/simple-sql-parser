{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.EmptyStatement where

import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.TestRunners
import Data.Text (Text)

emptyStatementTests :: TestItem
emptyStatementTests = Group "empty statement"
  [ s ";" EmptyStatement
  , t ";" [EmptyStatement]
  , t ";;" [EmptyStatement, EmptyStatement]
  , t ";;;" [EmptyStatement, EmptyStatement, EmptyStatement]
  , s "/* comment */ ;" EmptyStatement
  , t "" []
  , t "/* comment */" []
  , t "/* comment */ ;" [EmptyStatement]
  , t "/* comment */ ; /* comment */ ;"
      [EmptyStatement, EmptyStatement]
  , t "/* comment */ ; /* comment */ ; /* comment */ ;"
      [EmptyStatement, EmptyStatement, EmptyStatement]
  ]
  where
    s :: HasCallStack => Text -> Statement -> TestItem
    s src a = testStatement ansi2011 src a
    t :: HasCallStack => Text -> [Statement] -> TestItem
    t src a = testStatements ansi2011 src a
