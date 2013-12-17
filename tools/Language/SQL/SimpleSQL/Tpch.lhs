
Some tests for parsing the tpch queries


> module Language.SQL.SimpleSQL.Tpch (tpchTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Tpch

> tpchTests :: TestItem
> tpchTests =
>     Group "parse tpch"
>     $ map (ParseQueryExpr . snd) tpchQueries
