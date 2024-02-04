
{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.CreateIndex where

import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.TestRunners
import Data.Text (Text)

createIndexTests :: TestItem
createIndexTests = Group "create index tests"
    [s "create index a on tbl(c1)"
      $ CreateIndex False [nm "a"] [nm "tbl"] [nm "c1"]
    ,s "create index a.b on sc.tbl (c1, c2)"
      $ CreateIndex False [nm "a", nm "b"] [nm "sc", nm "tbl"] [nm "c1", nm "c2"]
    ,s "create unique index a on tbl(c1)"
      $ CreateIndex True [nm "a"] [nm "tbl"] [nm "c1"]
    ]
  where
    nm = Name Nothing
    s :: HasCallStack => Text -> Statement -> TestItem
    s src ast = testStatement ansi2011 src ast
