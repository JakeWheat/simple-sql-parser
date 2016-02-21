
> module Language.SQL.SimpleSQL.Odbc (odbcTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax

> odbcTests :: TestItem
> odbcTests = Group "odbc" [
>        Group "datetime" [
>            e "{d '2000-01-01'}" (OdbcLiteral OLDate "2000-01-01")
>           ,e "{t '12:00:01.1'}" (OdbcLiteral OLTime "12:00:01.1")
>           ,e "{ts '2000-01-01 12:00:01.1'}"
>                (OdbcLiteral OLTimestamp "2000-01-01 12:00:01.1")
>        ]
>        ,Group "functions" [
>              e "{fn CHARACTER_LENGTH(string_exp)}"
>              $ OdbcFunc (ap "CHARACTER_LENGTH" [iden "string_exp"])
>             ,e "{fn EXTRACT(day from t)}"
>             $ OdbcFunc (SpecialOpK [Name Nothing "extract"] (Just $ Iden [Name Nothing "day"]) [("from", Iden [Name Nothing "t"])])
>             ,e "{fn now()}"
>              $ OdbcFunc (ap "now" [])
>             ,e "{fn CONVERT('2000-01-01', SQL_DATE)}"
>              $ OdbcFunc (ap "CONVERT"
>               [StringLit "'" "'" "2000-01-01"
>               ,iden "SQL_DATE"])
>             ,e "{fn CONVERT({fn CURDATE()}, SQL_DATE)}"
>              $ OdbcFunc (ap "CONVERT"
>               [OdbcFunc (ap "CURDATE" [])
>               ,iden "SQL_DATE"])
>             ]
>        {-,Group "outer join" [
>              ParseQueryExpr defaultParseFlags
>              "select * from {oj t1 left outer join t2 on true}"
>              $ makeSelect
>             {selSelectList = sl [si $ Star ea]
>             ,selTref = [OdbcTableRef ea (JoinTref ea (tref "t1") Unnatural LeftOuter Nothing
>                                          (tref "t2") (Just $ JoinOn ea (BooleanLit ea True)))]}]
>        ,Group "check parsing bugs" [
>              ParseQueryExpr defaultParseFlags
>              "select {fn CONVERT(cint,SQL_BIGINT)} from t;"
>              $ makeSelect
>             {selSelectList = sl [si $ OdbcFunc ea (App ea (name "CONVERT")
>                                                            [ei "cint"
>                                                            ,ei "SQL_BIGINT"])]
>             ,selTref = [tref "t"]}]-}
>        ]
>   where
>     e = TestValueExpr ansi2011 {allowOdbc = True}
>     --tsql = ParseProcSql defaultParseFlags {pfDialect=sqlServerDialect}
>     ap n = App [Name Nothing n]
>     iden n = Iden [Name Nothing n]

