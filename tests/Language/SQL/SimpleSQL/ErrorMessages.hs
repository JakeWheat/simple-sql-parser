
{-

Quick tests for error messages, all the tests use the entire formatted
output of parse failures to compare, it's slightly fragile. Most of
the tests use a huge golden file which contains tons of parse error
examples.

-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.SQL.SimpleSQL.ErrorMessages
    (errorMessageTests
    ) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Parse
import qualified Language.SQL.SimpleSQL.Lex as L
import Language.SQL.SimpleSQL.TestRunners
--import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Expectations
import Test.Hspec (it)
import Debug.Trace

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Test.Hspec.Golden
    (Golden(..)
    )

import qualified Text.RawString.QQ as R
import System.FilePath ((</>))
import Text.Show.Pretty (ppShow)

errorMessageTests :: TestItem
errorMessageTests = Group "error messages"
    [gp (parseQueryExpr ansi2011 "" Nothing) prettyError [R.r|

select
a
from t
where
  something
order by 1,2,3 where

        |]
        [R.r|8:16:
  |
8 | order by 1,2,3 where
  |                ^^^^^
unexpected where
|]
   ,gp (L.lexSQL ansi2011 False "" Nothing)  L.prettyError [R.r|
           
select
a
from t
where
  something
order by 1,2,3 $@

        |]
        [R.r|8:16:
  |
8 | order by 1,2,3 $@
  |                ^
unexpected '$'
|]
   ,let fn = "expected-parse-errors"
        got = generateParseResults parseErrorData
    in GoldenErrorTest fn parseErrorData $ it "parse error regressions" $ myGolden (T.unpack fn) got
        ]
  where
    gp :: (Show a, HasCallStack) => (Text -> Either e a) -> (e -> Text) -> Text -> Text -> TestItem
    gp parse pret src err =
        GeneralParseFailTest src err $
           it (T.unpack src) $
           let f1 = parse src
               ex = shouldFailWith pret
               quickTrace =
                   case f1 of
                       Left f | pret f /= err ->
                            trace (T.unpack ("check\n[" <> pret f <>"]\n["<> err <> "]\n"))
                       _ -> id
           in quickTrace (f1 `ex` err)

------------------------------------------------------------------------------

-- golden parse error tests
    
myGolden :: String -> Text -> Golden Text
myGolden name actualOutput =
  Golden {
    output = actualOutput,
    encodePretty = show,
    writeToFile = T.writeFile,
    readFromFile = T.readFile,
    goldenFile = name </> "golden",
    actualFile = Just (name </> "actual"),
    failFirstTime = False
  }

parseErrorData :: [(Text,Text,Text)]
parseErrorData =
    concat
    [simpleExpressions1
    ,pgExprs
    ,sqlServerIden
    ,mysqliden
    ,paramvariations
    ,odbcexpr
    ,odbcqexpr
    ,queryExprExamples
    ,statementParseErrorExamples]

generateParseResults :: [(Text,Text,Text)] -> Text
generateParseResults dat =
    let testLine (parser,dialect,src) =
            let d = case dialect of
                    "ansi2011" -> ansi2011
                    "postgres" -> postgres
                    "sqlserver" -> sqlserver
                    "mysql" -> mysql
                    "params" -> ansi2011{diAtIdentifier=True, diHashIdentifier= True}
                    "odbc" -> ansi2011{diOdbc=True}
                    _ -> error $ "unknown dialect: " <> T.unpack dialect
                res = case parser of
                        "queryExpr" ->
                            either prettyError (T.pack . ppShow)
                            $ parseQueryExpr d "" Nothing src
                        "scalarExpr" ->
                            either prettyError (T.pack . ppShow)
                            $ parseScalarExpr d "" Nothing src
                        "statement" ->
                            either prettyError (T.pack . ppShow)
                            $ parseStatement d "" Nothing src
                        _ -> error $ "unknown parser: " <> T.unpack parser
                -- prepend a newline to multi line fields, so they show
                -- nice in a diff in meld or similar
                resadj = if '\n' `T.elem` res
                         then T.cons '\n' res
                         else res
            in T.unlines [parser, dialect, src, resadj]
    in T.unlines $ map testLine dat

parseExampleStrings :: Text -> [Text]
parseExampleStrings = filter (not . T.null) . map T.strip . T.splitOn ";"

simpleExpressions1 :: [(Text,Text,Text)]
simpleExpressions1 =
    concat $ flip map (parseExampleStrings simpleExprData) $ \e ->
    [("scalarExpr", "ansi2011", e)
    ,("queryExpr", "ansi2011", "select " <> e)
    ,("queryExpr", "ansi2011", "select " <> e <> ",")
    ,("queryExpr", "ansi2011", "select " <> e <> " from")]
  where
    simpleExprData = [R.r|
'test
;
'test''t
;
'test''
;
3.23e-
;
.
;
3.23e
;
a.3
;
3.a
;
3.2a
;
4iden
;
4iden.
;
iden.4iden
;
4iden.*
;
from
;
from.a
;
a.from
;
not
;
4 +
;
4 + from
;
(5
;
(5 +
;
(5 + 6
;
(5 + from)
;
case
;
case a
;
case a when b c end
;
case a when b then c
;
case a else d end
;
case a from c end
;
case a when from then to end
;
/* blah
;
/* blah /* stuff */
;
/* *
;
/* /
;
$$something$
;
$$something
;
$$something
x
;
$a$something$b$
;
$a$
;
'''
;
'''''
;
"a
;
"a""
;
"""
;
"""""
;
""
;
*/
;
:3
;
@3
;
#3
;
:::
;
|||
;
...
;
"
;
]
;
)
;
[test
;
[]
;
[[test]]
;
`open
;
```
;
``
;
}
;
mytype(4 '4';
;
app(3
;
app(
;
app(something
;
app(something,
;
count(*
;
count(* filter (where something > 5)
;
count(*) filter (where something > 5
;
count(*) filter (
;
sum(a over (order by b)
;
sum(a) over (order by b
;
sum(a) over (
;
rank(a,c within group (order by b)
;
rank(a,c) within group (order by b
;
rank(a,c) within group (
;
array[
;
(a
;
(
;
a >*
;
a >* b
;
( ( a
;
( ( a )
;
( ( a  + )
|]

pgExprs :: [(Text,Text,Text)]
pgExprs = flip map (parseExampleStrings src) $ \e ->
    ("scalarExpr", "postgres", e)
  where src = [R.r|
$$something$
;
$$something
;
$$something
x
;
$a$something$b$
;
$a$
;
:::
;
|||
;
...
;

|]

sqlServerIden :: [(Text,Text,Text)]
sqlServerIden = flip map (parseExampleStrings src) $ \e ->
    ("scalarExpr", "sqlserver", e)
  where src = [R.r|
]
;
[test
;
[]
;
[[test]]

|]

mysqliden :: [(Text,Text,Text)]
mysqliden = flip map (parseExampleStrings src) $ \e ->
    ("scalarExpr", "mysql", e)
  where src = [R.r|
`open
;
```
;
``

|]

paramvariations :: [(Text,Text,Text)]
paramvariations = flip map (parseExampleStrings src) $ \e ->
    ("scalarExpr", "params", e)
  where src = [R.r|
:3
;
@3
;
#3

|]


odbcexpr :: [(Text,Text,Text)]
odbcexpr = flip map (parseExampleStrings src) $ \e ->
    ("scalarExpr", "odbc", e)
  where src = [R.r|
{d '2000-01-01'
;
{fn CHARACTER_LENGTH(string_exp)

|]

odbcqexpr :: [(Text,Text,Text)]
odbcqexpr = flip map (parseExampleStrings src) $ \e ->
    ("queryExpr", "odbc", e)
  where src = [R.r|
select * from {oj t1 left outer join t2 on expr

|]


    
queryExprExamples :: [(Text,Text,Text)]
queryExprExamples = flip map (parseExampleStrings src) $ \e ->
    ("queryExpr", "ansi2011", e)
  where src = [R.r|
select a select
;
select a from t,
;
select a from t select
;
select a from t(a)
;
select a from (t
;
select a from (t having
;
select a from t a b
;
select a from t as
;
select a from t as having
;
select a from (1234)
;
select a from (1234
;
select a from a wrong join b
;
select a from a natural wrong join b
;
select a from a left wrong join b
;
select a from a left wrong join b
;
select a from a join b select
;
select a from a join b on select
;
select a from a join b on (1234
;
select a from a join b using(a
;
select a from a join b using(a,
;
select a from a join b using(a,)
;
select a from a join b using(1234
;
select a from t order no a
;
select a from t order by a where c
;
select 'test
'
;
select a as
;
select a as from t
;
select a as, 
;
select a,
;
select a, from t
;
select a as from
;
select a as from from
;
select a as from2 from
;
select a fromt
;
select a b fromt

;
select a from t u v
;
select a from t as
;
select a from t, 
;
select a from group by b
;
select a from t join group by a
;
select a from t join
;
select a from (@
;
select a from ()
;
select a from t left join u on
;
select a from t left join u on group by a
;
select a from t left join u using
;
select a from t left join u using (
;
select a from t left join u using (a
;
select a from t left join u using (a,
;
select a from (select a from)
;
select a from (select a

;
select a from t where
;
select a from t group by a having b where
;
select a from t where (a
;
select a from t where group by b

;
select a from t group by
;
select a from t group
;
select a from t group by a as
;
select a from t group by a,
;
select a from t group by order by
;
select a <<== b from t
;
/*
;
select * as a
;
select t.* as a
;
select 3 + *
;
select case when * then 1 end
;
select (*)
;
select * from (select a
        from t
;
select * from (select a(stuff)
        from t

;
select *
     from (select a,b
           from t
           where a = 1
                 and b > a

;
select *
     from (select a,b
           from t
           where a = 1
                 and b > a
           from t)

|]


statementParseErrorExamples :: [(Text,Text,Text)]
statementParseErrorExamples = flip map (parseExampleStrings src) $ \e ->
    ("statement", "ansi2011", e)
  where src = [R.r|
create
;
drop
;
delete this
;
delete where 7
;
delete from where t
;
truncate nothing
;
truncate nothing nothing
;
truncate table from
;
truncate table t u
;
insert t select u
;
insert into t insert
;
insert into t (1,2)
;
insert into t(
;
insert into t(1
;
insert into t(a
;
insert into t(a,
;
insert into t(a,b)
;
insert into t(a,b) values
;
insert into t(a,b) values (
;
insert into t(a,b) values (1
;
insert into t(a,b) values (1,
;
insert into t(a,b) values (1,2) and stuff
;
update set 1
;
update t u
;
update t u v
;
update t set a
;
update t set a=
;
update t set a=1,
;
update t set a=1 where
;
update t set a=1 where 1 also
;
create table
;
create table t (
  a
)
;
create table t (
  a
;
create table t (
  a,
)
;
create table t (
)
;
create table t (
;
create table t
;
create table t. (
;
truncate table t.
;
drop table t. where
;
update t. set
;
delete from t. where
;
insert into t. values
;
with a as (select * from t
select 1
;
with a as (select * from t
;
with a as (
;
with a (
;
with as (select * from t)
select 1
;
with (select * from t) as a
select 1


|]

