
{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.Odbc (odbcTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestRunners
import Data.Text (Text)

odbcTests :: TestItem
odbcTests = Group "odbc" [
       Group "datetime" [
           e "{d '2000-01-01'}" (OdbcLiteral OLDate "2000-01-01")
          ,e "{t '12:00:01.1'}" (OdbcLiteral OLTime "12:00:01.1")
          ,e "{ts '2000-01-01 12:00:01.1'}"
               (OdbcLiteral OLTimestamp "2000-01-01 12:00:01.1")
       ]
       ,Group "functions" [
             e "{fn CHARACTER_LENGTH(string_exp)}"
             $ OdbcFunc (ap "CHARACTER_LENGTH" [iden "string_exp"])
            ,e "{fn EXTRACT(day from t)}"
            $ OdbcFunc (SpecialOpK [Name Nothing "extract"] (Just $ Iden [Name Nothing "day"]) [("from", Iden [Name Nothing "t"])])
            ,e "{fn now()}"
             $ OdbcFunc (ap "now" [])
            ,e "{fn CONVERT('2000-01-01', SQL_DATE)}"
             $ OdbcFunc (ap "CONVERT"
              [StringLit "'" "'" "2000-01-01"
              ,iden "SQL_DATE"])
            ,e "{fn CONVERT({fn CURDATE()}, SQL_DATE)}"
             $ OdbcFunc (ap "CONVERT"
              [OdbcFunc (ap "CURDATE" [])
              ,iden "SQL_DATE"])
            ]
       ,Group "outer join" [
             q
             "select * from {oj t1 left outer join t2 on expr}"
             $ toQueryExpr $ makeSelect
                   {msSelectList = [(Star,Nothing)]
                   ,msFrom = [TROdbc $ TRJoin (TRSimple [Name Nothing "t1"]) False JLeft (TRSimple [Name Nothing "t2"])
                                         (Just $ JoinOn $ Iden [Name Nothing "expr"])]}]
       ,Group "check parsing bugs" [
             q
             "select {fn CONVERT(cint,SQL_BIGINT)} from t;"
             $ toQueryExpr $ makeSelect
                   {msSelectList = [(OdbcFunc (ap "CONVERT"
                                                      [iden "cint"
                                                      ,iden "SQL_BIGINT"]), Nothing)]
                   ,msFrom = [TRSimple [Name Nothing "t"]]}]
       ]
  where
    e :: HasCallStack => Text -> ScalarExpr -> TestItem
    e src ast = testScalarExpr ansi2011{diOdbc = True} src ast

    q :: HasCallStack => Text -> QueryExpr -> TestItem
    q src ast = testQueryExpr ansi2011{diOdbc = True} src ast

    --tsql = ParseProcSql defaultParseFlags {pfDialect=sqlServerDialect}
    ap n = App [Name Nothing n]
    iden n = Iden [Name Nothing n]

