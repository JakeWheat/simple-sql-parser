
{-
These are the tests for parsing focusing on the from part of query
expression
-}

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.TableRefs (tableRefTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestRunners
import Data.Text (Text)

tableRefTests :: TestItem
tableRefTests = Group "tableRefTests"
    [q "select a from t"
     $ ms [TRSimple [Name Nothing "t"]]

     ,q "select a from f(a)"
      $ ms [TRFunction [Name Nothing "f"] [Iden [Name Nothing "a"]]]

    ,q "select a from t,u"
     $ ms [TRSimple [Name Nothing "t"], TRSimple [Name Nothing "u"]]

    ,q "select a from s.t"
     $ ms [TRSimple [Name Nothing "s", Name Nothing "t"]]

-- these lateral queries make no sense but the syntax is valid

    ,q "select a from lateral a"
     $ ms [TRLateral $ TRSimple [Name Nothing "a"]]

    ,q "select a from lateral a,b"
     $ ms [TRLateral $ TRSimple [Name Nothing "a"], TRSimple [Name Nothing "b"]]

    ,q "select a from a, lateral b"
     $ ms [TRSimple [Name Nothing "a"], TRLateral $ TRSimple [Name Nothing "b"]]

    ,q "select a from a natural join lateral b"
     $ ms [TRJoin (TRSimple [Name Nothing "a"]) True JInner
                 (TRLateral $ TRSimple [Name Nothing "b"])
                 Nothing]

    ,q "select a from lateral a natural join lateral b"
     $ ms [TRJoin (TRLateral $ TRSimple [Name Nothing "a"]) True JInner
                 (TRLateral $ TRSimple [Name Nothing "b"])
                 Nothing]


    ,q "select a from t inner join u on expr"
     $ ms [TRJoin (TRSimple [Name Nothing "t"]) False JInner (TRSimple [Name Nothing "u"])
                       (Just $ JoinOn $ Iden [Name Nothing "expr"])]

    ,q "select a from t join u on expr"
     $ ms [TRJoin (TRSimple [Name Nothing "t"]) False JInner (TRSimple [Name Nothing "u"])
                       (Just $ JoinOn $ Iden [Name Nothing "expr"])]

    ,q "select a from t left join u on expr"
     $ ms [TRJoin (TRSimple [Name Nothing "t"]) False JLeft (TRSimple [Name Nothing "u"])
                       (Just $ JoinOn $ Iden [Name Nothing "expr"])]

    ,q "select a from t right join u on expr"
     $ ms [TRJoin (TRSimple [Name Nothing "t"]) False JRight (TRSimple [Name Nothing "u"])
                       (Just $ JoinOn $ Iden [Name Nothing "expr"])]

    ,q "select a from t full join u on expr"
     $ ms [TRJoin (TRSimple [Name Nothing "t"]) False JFull (TRSimple [Name Nothing "u"])
                       (Just $ JoinOn $ Iden [Name Nothing "expr"])]

    ,q "select a from t cross join u"
     $ ms [TRJoin (TRSimple [Name Nothing "t"]) False
                       JCross (TRSimple [Name Nothing "u"]) Nothing]

    ,q "select a from t natural inner join u"
     $ ms [TRJoin (TRSimple [Name Nothing "t"]) True JInner (TRSimple [Name Nothing "u"])
                       Nothing]

    ,q "select a from t inner join u using(a,b)"
     $ ms [TRJoin (TRSimple [Name Nothing "t"]) False JInner (TRSimple [Name Nothing "u"])
                       (Just $ JoinUsing [Name Nothing "a", Name Nothing "b"])]

    ,q "select a from (select a from t)"
     $ ms [TRQueryExpr $ ms [TRSimple [Name Nothing "t"]]]

    ,q "select a from t as u"
     $ ms [TRAlias (TRSimple [Name Nothing "t"]) (Alias (Name Nothing "u") Nothing)]

    ,q "select a from t u"
     $ ms [TRAlias (TRSimple [Name Nothing "t"]) (Alias (Name Nothing "u") Nothing)]

    ,q "select a from t u(b)"
     $ ms [TRAlias (TRSimple [Name Nothing "t"]) (Alias (Name Nothing "u") $ Just [Name Nothing "b"])]

    ,q "select a from (t cross join u) as u"
     $ ms [TRAlias (TRParens $
                   TRJoin (TRSimple [Name Nothing "t"]) False JCross (TRSimple [Name Nothing "u"]) Nothing)
                          (Alias (Name Nothing "u") Nothing)]
     -- todo: not sure if the associativity is correct

    ,q "select a from t cross join u cross join v"
       $ ms [TRJoin
           (TRJoin (TRSimple [Name Nothing "t"]) False
                   JCross (TRSimple [Name Nothing "u"]) Nothing)
           False JCross (TRSimple [Name Nothing "v"]) Nothing]
    ]
  where
    ms f = toQueryExpr $ makeSelect {msSelectList = [(Iden [Name Nothing "a"],Nothing)]
                                    ,msFrom = f}
    q :: HasCallStack => Text -> QueryExpr -> TestItem
    q src ast = testQueryExpr ansi2011 src ast
