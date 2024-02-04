
# Overview

A parser for SQL in Haskell. Also includes a pretty printer which
formats SQL.

This is the documentation for version 0.7.0. Documentation for other
versions is available here:
<http://jakewheat.github.io/simple-sql-parser/>.

Status: usable for parsing a substantial amount of SQL. Adding support
for new SQL is easy. Expect a little bit of churn on the AST types
when support for new SQL features is added.

This version is tested with GHC 9.8.1, 9.6.4, and 9.4.8.

# Examples

Parse a SQL statement:

~~~~{.haskell sb-session='cabal repl --repl-options=-XOverloadedStrings' sb-prompt='ghci> ' sb-no-initial-text=}
ghci> import Language.SQL.SimpleSQL.Parse
ghci> import qualified Data.Text as T
ghci> either (T.unpack . prettyError) show $ parseStatement ansi2011 "" Nothing "select a + b * c"
"SelectStatement (Select {qeSetQuantifier = SQDefault, qeSelectList = [(BinOp (Iden [Name Nothing \"a\"]) [Name Nothing \"+\"] (BinOp (Iden [Name Nothing \"b\"]) [Name Nothing \"*\"] (Iden [Name Nothing \"c\"])),Nothing)], qeFrom = [], qeWhere = Nothing, qeGroupBy = [], qeHaving = Nothing, qeOrderBy = [], qeOffset = Nothing, qeFetchFirst = Nothing})"
~~~~

The result printed readably:

~~~~{.haskell sb-run='cabal run -fparserexe SimpleSQLParserTool -- parse -c "select a + b * c"' sb-cwd='..'}
[ SelectStatement
    Select
      { qeSetQuantifier = SQDefault
      , qeSelectList =
          [ ( BinOp
                (Iden [ Name Nothing "a" ])
                [ Name Nothing "+" ]
                (BinOp
                   (Iden [ Name Nothing "b" ])
                   [ Name Nothing "*" ]
                   (Iden [ Name Nothing "c" ]))
            , Nothing
            )
          ]
      , qeFrom = []
      , qeWhere = Nothing
      , qeGroupBy = []
      , qeHaving = Nothing
      , qeOrderBy = []
      , qeOffset = Nothing
      , qeFetchFirst = Nothing
      }
]
~~~~

Formatting SQL, TPC-H query 21:

~~~~{.sql sb-file='tpch21.sql'}
select
        s_name,
        count(*) as numwait
from
        supplier,
        lineitem l1,
        orders,
        nation
where
        s_suppkey = l1.l_suppkey
        and o_orderkey = l1.l_orderkey
        and o_orderstatus = 'F'
        and l1.l_receiptdate > l1.l_commitdate
        and exists (
                select
                        *
                from
                        lineitem l2
                where
                        l2.l_orderkey = l1.l_orderkey
                        and l2.l_suppkey <> l1.l_suppkey
        )
        and not exists (
                select
                        *
                from
                        lineitem l3
                where
                        l3.l_orderkey = l1.l_orderkey
                        and l3.l_suppkey <> l1.l_suppkey
                        and l3.l_receiptdate > l3.l_commitdate
        )
        and s_nationkey = n_nationkey
        and n_name = 'INDIA'
group by
        s_name
order by
        numwait desc,
        s_name
fetch first 100 rows only;
~~~~

Output from the simple-sql-parser pretty printer:

~~~~{.haskell sb-run='cabal run -fparserexe SimpleSQLParserTool -- format website/tpch21.sql' sb-cwd='..'}
select s_name, count(*) as numwait
from supplier,
     lineitem as l1,
     orders,
     nation
where s_suppkey = l1.l_suppkey
      and o_orderkey = l1.l_orderkey
      and o_orderstatus = 'F'
      and l1.l_receiptdate > l1.l_commitdate
      and exists (select *
                  from lineitem as l2
                  where l2.l_orderkey = l1.l_orderkey
                        and l2.l_suppkey <> l1.l_suppkey)
      and not exists (select *
                      from lineitem as l3
                      where l3.l_orderkey = l1.l_orderkey
                            and l3.l_suppkey <> l1.l_suppkey
                            and l3.l_receiptdate > l3.l_commitdate)
      and s_nationkey = n_nationkey
      and n_name = 'INDIA'
group by s_name
order by numwait desc, s_name
fetch first 100 rows only;

~~~~

# Supported SQL overview

* query expressions
  * select lists
  * from clause
  * where clause
  * group by clause
* having clause
  * order by clause
  * offset and fetch
  * set operators
  * common table expressions
  * wide range of scalar expressions
* DDL (ansi dialect)
  * create, drop schema
  * create, alter, drop table
  * create, drop view
  * create, alter, drop domain
  * create, drop assertion
  * create, alter, drop sequence
* non-query DML
  * delete
  * truncate
  * insert
  * update
* Access control
  * grant, revoke - permissions and roles
  * create, drop role
* Transaction management
  * begin, commit, rollback, savepoints

See the [supported_sql.html](supported_sql.html) page for details on the supported SQL.

Here is all the [test_cases.html](test_cases.html) rendered in a webpage so you can get
an idea of what it supports, and what various instances of SQL parse to.

# Installation

This package is on hackage, use it in the usual way. You can install
the SimpleSQLParserTool demo exe using:

~~~~
cabal install -fparserexe simple-sql-parser
~~~~

# Reporting bugs

Please report bugs here: <https://github.com/JakeWheat/simple-sql-parser/issues>

A good bug report (or feature request) should have an example of the
SQL which is failing. You can expect bugs to get fixed.

Feature requests are welcome, but be aware that there is no-one
generally available to work on these, so you should either make a pull
request, or find someone willing to implement the features and make a
pull request.

Bug reports of confusing or poor parse errors are also encouraged.

There is a related tutorial on implementing a SQL parser here:
<http://jakewheat.github.io/intro_to_parsing/> (TODO: this is out of
date, in the process of being updated)

# Modifying the library

Get the latest development version:

~~~~
git clone https://github.com/JakeWheat/simple-sql-parser.git
cd simple-sql-parser
cabal build
~~~~

You can run the tests using cabal:

~~~~
cabal test
~~~~

Or use the makefile target

~~~~
make test
~~~~

To skip some of the slow lexer tests, which you usually only need to
run before each commit, use:

~~~~
make fast-test
~~~~

When you add support for new syntax: add some tests. If you modify or
fix something, and it doesn't have tests, add some. If the syntax
isn't in ANSI SQL, guard it behind a dialect flag. If you add
support for something from a new dialect, add that dialect.

Check all the tests still pass, then send a pull request on Github.

# Links

* Haddock: [haddock/index.html](haddock/index.html)
* Supported SQL: [supported_sql.html](supported_sql.html)
* Test cases: [test_cases.html](test_cases.html)
* Homepage: <http://jakewheat.github.io/simple-sql-parser/latest>
* Hackage: <http://hackage.haskell.org/package/simple-sql-parser>
* Source repository: <https://github.com/JakeWheat/simple-sql-parser>
* Bug tracker: <https://github.com/JakeWheat/simple-sql-parser/issues>
* Changes: <https://github.com/JakeWheat/simple-sql-parser/blob/master/changelog>
* Other versions: <http://jakewheat.github.io/simple-sql-parser/>
* Contact: jakewheat@tutanota.com

The simple-sql-parser is a lot less simple than it used to be. If you
just need to parse much simpler SQL than this, or want to start with a
simpler parser and modify it slightly, you could also look at the
basic query parser in the intro_to_parsing project, the code is here:
<https://github.com/JakeWheat/intro_to_parsing/blob/master/SimpleSQLQueryParser0.lhs>
(TODO: this is out of date, in the process of being updated).
