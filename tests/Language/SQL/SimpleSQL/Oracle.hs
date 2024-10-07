
-- Tests for oracle dialect parsing

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.Oracle (oracleTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestRunners

oracleTests :: TestItem
oracleTests = Group "oracle dialect"
    [oracleLobUnits]


oracleLobUnits :: TestItem
oracleLobUnits = Group "oracleLobUnits"
    [testScalarExpr oracle "cast (a as varchar2(3 char))"
     $ Cast (Iden [Name Nothing "a"]) (
         PrecLengthTypeName [Name Nothing "varchar2"] 3 Nothing (Just PrecCharacters))
    ,testScalarExpr oracle "cast (a as varchar2(3 byte))"
     $ Cast (Iden [Name Nothing "a"]) (
         PrecLengthTypeName [Name Nothing "varchar2"] 3 Nothing (Just PrecOctets))
    ,testStatement oracle
      "create table t (a varchar2(55 BYTE));"
     $ CreateTable [Name Nothing "t"]
       [TableColumnDef $ ColumnDef (Name Nothing "a")
        (PrecLengthTypeName [Name Nothing "varchar2"] 55 Nothing (Just PrecOctets))
        Nothing []]
       False
    ]

