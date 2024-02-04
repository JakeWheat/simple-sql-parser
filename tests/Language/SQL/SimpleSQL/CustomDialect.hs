
{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.CustomDialect (customDialectTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.TestRunners
import Data.Text (Text)

customDialectTests :: TestItem
customDialectTests = Group "custom dialect tests" $
    [q ansi2011 "SELECT a b"
    ,q noDateKeyword "SELECT DATE('2000-01-01')"
    ,q noDateKeyword "SELECT DATE"
    ,q dateApp "SELECT DATE('2000-01-01')"
    ,q dateIden "SELECT DATE"
    ,f ansi2011 "SELECT DATE('2000-01-01')"
    ,f ansi2011 "SELECT DATE"
    ,f dateApp "SELECT DATE"
    ,f dateIden "SELECT DATE('2000-01-01')"
    -- show this never being allowed as an alias
    ,f ansi2011 "SELECT a date"
    ,f dateApp "SELECT a date"
    ,f dateIden "SELECT a date"
    ]
  where
    noDateKeyword = ansi2011 {diKeywords = filter (/="date") (diKeywords ansi2011)}
    dateIden = ansi2011 {diIdentifierKeywords = "date" : diIdentifierKeywords ansi2011}
    dateApp = ansi2011 {diAppKeywords = "date" : diAppKeywords ansi2011}
    q :: HasCallStack => Dialect -> Text -> TestItem
    q d src = testParseQueryExpr d src
    f :: HasCallStack => Dialect -> Text -> TestItem
    f d src = testParseQueryExprFails d src
