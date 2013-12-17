
These are the tests for the queryExprs parsing which parses multiple
query expressions from one string.

> module Language.SQL.SimpleSQL.QueryExprs (queryExprsTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax

> queryExprsTests :: TestItem
> queryExprsTests = Group "query exprs" $ map (uncurry TestQueryExprs)
>     [("select 1",[ms])
>     ,("select 1;",[ms])
>     ,("select 1;select 1",[ms,ms])
>     ,(" select 1;select 1; ",[ms,ms])
>     ]
>   where
>     ms = makeSelect {qeSelectList = [(Nothing,NumLit "1")]}
