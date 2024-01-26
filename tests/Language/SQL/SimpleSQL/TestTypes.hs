
{-
This is the types used to define the tests as pure data. See the
Tests.hs module for the 'interpreter'.
-}

module Language.SQL.SimpleSQL.TestTypes
    (TestItem(..)
    ,module Language.SQL.SimpleSQL.Dialect
    ) where

import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Lex (Token)
import Language.SQL.SimpleSQL.Dialect

import Data.Text (Text)

{-
TODO: maybe make the dialect args into [dialect], then each test
checks all the dialects mentioned work, and all the dialects not
mentioned give a parse error. Not sure if this will be too awkward due
to lots of tricky exceptions/variationsx.
-}

data TestItem = Group Text [TestItem]
              | TestScalarExpr Dialect Text ScalarExpr
              | TestQueryExpr Dialect Text QueryExpr
              | TestStatement Dialect Text Statement
              | TestStatements Dialect Text [Statement]

{-
this just checks the sql parses without error, mostly just a
intermediate when I'm too lazy to write out the parsed AST. These
should all be TODO to convert to a testqueryexpr test.
-}

              | ParseQueryExpr Dialect Text

-- check that the string given fails to parse

              | ParseQueryExprFails Dialect Text
              | ParseScalarExprFails Dialect Text
              | LexTest Dialect Text [Token]
              | LexFails Dialect Text
                deriving (Eq,Show)
