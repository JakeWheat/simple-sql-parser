
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.SQL.SimpleSQL.QueryExprParens (queryExprParensTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestRunners
import Data.Text (Text)
import qualified Text.RawString.QQ as R

queryExprParensTests :: TestItem
queryExprParensTests = Group "query expr parens"
    [q "(select *  from t)" $ QueryExprParens $ ms "t"
    ,q "select * from t except (select * from u except select * from v)"
     $ (ms "t") `sexcept` QueryExprParens (ms "u" `sexcept` ms "v")
        
    ,q "(select * from t except select * from u) except select * from v"
     $ QueryExprParens (ms "t" `sexcept` ms "u") `sexcept` ms "v"

    ,q [R.r|
select * from t
union
with a as (select * from u)
select * from a
|]
     $ ms "t" `sunion` with [("a", ms "u")] (ms "a")

    ,q [R.r|
select * from t
union
(with a as (select * from u)
 select * from a)
|]
     $ ms "t" `sunion` QueryExprParens (with [("a", ms "u")] (ms "a"))
    ]
  where
    q :: HasCallStack => Text -> QueryExpr -> TestItem
    q src ast = testQueryExpr ansi2011 src ast
    ms t = toQueryExpr $ makeSelect
            {msSelectList = [(Star,Nothing)]
            ,msFrom = [TRSimple [Name Nothing t]]}
    sexcept = so Except
    sunion = so Union
    so op a b = QueryExprSetOp a op SQDefault Respectively b
    with es s =
        With False (flip map es $ \(n,sn) -> (Alias (Name Nothing n) Nothing ,sn)) s