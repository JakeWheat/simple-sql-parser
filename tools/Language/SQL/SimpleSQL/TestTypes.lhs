
This is the types used to define the tests as pure data. See the
Tests.lhs module for the 'interpreter'.

> {-# LANGUAGE FlexibleInstances #-}
> module Language.SQL.SimpleSQL.TestTypes where

> import Language.SQL.SimpleSQL.Syntax

> import Data.String

> data TestItem = Group String [TestItem]
>               | TestValueExpr String ValueExpr
>               | TestQueryExpr String QueryExpr
>               | TestQueryExprs String [QueryExpr]

this just checks the sql parses without error, mostly just a
intermediate when I'm too lazy to write out the parsed AST. These
should all be TODO to convert to a testqueryexpr test.

>               | ParseQueryExpr String
>                 deriving (Eq,Show)

hack to make the tests a bit simpler

> instance IsString Name where
>    fromString = Name

> instance IsString [Name] where
>    fromString = (:[]) . Name
