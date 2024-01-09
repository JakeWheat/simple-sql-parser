
module Language.SQL.SimpleSQL.CreateIndex where

import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.TestTypes

createIndexTests :: TestItem
createIndexTests = Group "create index tests"
    [TestStatement ansi2011 "create index a on tbl(c1)"
      $ CreateIndex False [nm "a"] [nm "tbl"] [nm "c1"]
    ,TestStatement ansi2011 "create index a.b on sc.tbl (c1, c2)"
      $ CreateIndex False [nm "a", nm "b"] [nm "sc", nm "tbl"] [nm "c1", nm "c2"]
    ,TestStatement ansi2011 "create unique index a on tbl(c1)"
      $ CreateIndex True [nm "a"] [nm "tbl"] [nm "c1"]
    ]
  where
    nm = Name Nothing
