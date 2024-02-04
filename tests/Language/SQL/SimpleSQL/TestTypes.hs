
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

import Test.Hspec (SpecWith)


import Data.Text (Text)

{-
TODO: maybe make the dialect args into [dialect], then each test
checks all the dialects mentioned work, and all the dialects not
mentioned give a parse error. Not sure if this will be too awkward due
to lots of tricky exceptions/variationsx.

The test items are designed to allow code to grab all the examples
in easily usable data types, but since hspec has this neat feature
where it will give a source location for a test failure, each testitem
apart from group already has the SpecWith attached to run that test,
that way we can attach the source location to each test item
-}

data TestItem = Group Text [TestItem]
              | TestScalarExpr Dialect Text ScalarExpr (SpecWith ())
              | TestQueryExpr Dialect Text QueryExpr (SpecWith ())
              | TestStatement Dialect Text Statement (SpecWith ())
              | TestStatements Dialect Text [Statement] (SpecWith ())

{-
this just checks the sql parses without error, mostly just a
intermediate when I'm too lazy to write out the parsed AST. These
should all be TODO to convert to a testqueryexpr test.
-}

              | ParseQueryExpr Dialect Text (SpecWith ())

-- check that the string given fails to parse

              | ParseQueryExprFails Dialect Text (SpecWith ())
              | ParseScalarExprFails Dialect Text (SpecWith ())
              | LexTest Dialect Text [Token] (SpecWith ())
              | LexFails Dialect Text (SpecWith ())
              | GeneralParseFailTest Text Text (SpecWith ())

