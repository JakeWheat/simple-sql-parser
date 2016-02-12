
These are the tests for parsing focusing on the from part of query
expression

> module Language.SQL.SimpleSQL.TableRefs (tableRefTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax


> tableRefTests :: TestItem
> tableRefTests = Group "tableRefTests" $ map (uncurry (TestQueryExpr ansi2011))
>     [("select a from t"
>      ,ms [TRSimple [Name "t"]])

>      ,("select a from f(a)"
>       ,ms [TRFunction [Name "f"] [Iden [Name "a"]]])

>     ,("select a from t,u"
>      ,ms [TRSimple [Name "t"], TRSimple [Name "u"]])

>     ,("select a from s.t"
>      ,ms [TRSimple [Name "s", Name "t"]])

these lateral queries make no sense but the syntax is valid

>     ,("select a from lateral a"
>      ,ms [TRLateral $ TRSimple [Name "a"]])

>     ,("select a from lateral a,b"
>      ,ms [TRLateral $ TRSimple [Name "a"], TRSimple [Name "b"]])

>     ,("select a from a, lateral b"
>      ,ms [TRSimple [Name "a"], TRLateral $ TRSimple [Name "b"]])

>     ,("select a from a natural join lateral b"
>      ,ms [TRJoin (TRSimple [Name "a"]) True JInner
>                  (TRLateral $ TRSimple [Name "b"])
>                  Nothing])

>     ,("select a from lateral a natural join lateral b"
>      ,ms [TRJoin (TRLateral $ TRSimple [Name "a"]) True JInner
>                  (TRLateral $ TRSimple [Name "b"])
>                  Nothing])


>     ,("select a from t inner join u on expr"
>      ,ms [TRJoin (TRSimple [Name "t"]) False JInner (TRSimple [Name "u"])
>                        (Just $ JoinOn $ Iden [Name "expr"])])

>     ,("select a from t join u on expr"
>      ,ms [TRJoin (TRSimple [Name "t"]) False JInner (TRSimple [Name "u"])
>                        (Just $ JoinOn $ Iden [Name "expr"])])

>     ,("select a from t left join u on expr"
>      ,ms [TRJoin (TRSimple [Name "t"]) False JLeft (TRSimple [Name "u"])
>                        (Just $ JoinOn $ Iden [Name "expr"])])

>     ,("select a from t right join u on expr"
>      ,ms [TRJoin (TRSimple [Name "t"]) False JRight (TRSimple [Name "u"])
>                        (Just $ JoinOn $ Iden [Name "expr"])])

>     ,("select a from t full join u on expr"
>      ,ms [TRJoin (TRSimple [Name "t"]) False JFull (TRSimple [Name "u"])
>                        (Just $ JoinOn $ Iden [Name "expr"])])

>     ,("select a from t cross join u"
>      ,ms [TRJoin (TRSimple [Name "t"]) False
>                        JCross (TRSimple [Name "u"]) Nothing])

>     ,("select a from t natural inner join u"
>      ,ms [TRJoin (TRSimple [Name "t"]) True JInner (TRSimple [Name "u"])
>                        Nothing])

>     ,("select a from t inner join u using(a,b)"
>      ,ms [TRJoin (TRSimple [Name "t"]) False JInner (TRSimple [Name "u"])
>                        (Just $ JoinUsing [Name "a", Name "b"])])

>     ,("select a from (select a from t)"
>      ,ms [TRQueryExpr $ ms [TRSimple [Name "t"]]])

>     ,("select a from t as u"
>      ,ms [TRAlias (TRSimple [Name "t"]) (Alias (Name "u") Nothing)])

>     ,("select a from t u"
>      ,ms [TRAlias (TRSimple [Name "t"]) (Alias (Name "u") Nothing)])

>     ,("select a from t u(b)"
>      ,ms [TRAlias (TRSimple [Name "t"]) (Alias (Name "u") $ Just [Name "b"])])

>     ,("select a from (t cross join u) as u"
>      ,ms [TRAlias (TRParens $
>                    TRJoin (TRSimple [Name "t"]) False JCross (TRSimple [Name "u"]) Nothing)
>                           (Alias (Name "u") Nothing)])
>      -- todo: not sure if the associativity is correct

>     ,("select a from t cross join u cross join v",
>        ms [TRJoin
>            (TRJoin (TRSimple [Name "t"]) False
>                    JCross (TRSimple [Name "u"]) Nothing)
>            False JCross (TRSimple [Name "v"]) Nothing])
>     ]
>   where
>     ms f = makeSelect {qeSelectList = [(Iden [Name "a"],Nothing)]
>                       ,qeFrom = f}
