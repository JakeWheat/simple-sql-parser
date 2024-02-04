{-

tool to compare before and after on error messages, suggested use:
add any extra parse error examples below
run it on baseline code
run it on the modified code
use meld on the two resulting csvs
  bear in mind that " will appear as "" because of csv escaping

this is how to generate a csv of errors:

cabal -ftestexe build error-messages-tool && cabal -ftestexe run error-messages-tool -- generate | cabal -ftestexe run error-messages-tool -- test > res.csv

TODO:
think about making a regression test with this
can add some more tools:
there's a join mode to join two sets of results, could add a filter
  to remove rows that are the same
  but finding the different rows in meld seems to work well enough
figure out if you can display visual diffs between pairs of cells in localc
implement the tagging feature, one idea for working with it:
you generate a bunch of error messages
you eyeball the list, and mark some as good, some as bad
then when you update, you can do a compare which filters
  to keep any errors that have changed, and any that haven't
  changed but are not marked as good
etc.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

import Data.Text (Text)
import qualified Data.Text as T

import Text.Show.Pretty (ppShow)
import qualified Text.RawString.QQ as R

import Language.SQL.SimpleSQL.Parse
    (prettyError
    ,parseQueryExpr
    ,parseScalarExpr
--    ,parseStatement
--    ,parseStatements
    ,ansi2011
--    ,ParseError(..)
    )
--import qualified Language.SQL.SimpleSQL.Lex as L

import Language.SQL.SimpleSQL.Dialect
    (postgres
    ,Dialect(..)
    ,sqlserver
    ,mysql
    )

import System.Environment (getArgs)
import Data.Csv
    (encode
    ,decode
    ,HasHeader(..))

import qualified Data.ByteString.Lazy as B hiding (pack)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)
import qualified Data.Vector as V
import Data.Vector (Vector)

import           Database.SQLite.Simple
    (open
    ,execute_
    ,executeMany
    ,query_
    )

main :: IO ()
main = do

    as <- getArgs
    case as of
        ["generate"] -> B.putStrLn generateData
        ["test"] -> do
            txt <- B.getContents
            B.putStrLn $ runTests txt
        ["compare", f1, f2] -> do
            c1 <- B.readFile f1
            c2 <- B.readFile f2
            B.putStrLn =<< compareFiles c1 c2
        _ -> error $ "unsupported arguments: " <> show as

------------------------------------------------------------------------------

-- compare two files
{-

take two inputs
assume they have (testrunid, parser, dialect, src, res,tags) lines
do a full outer join between them, on
  parser,dialect,src
so you have
parser,dialect,src,res a, tags a, res b, tags b

then output this as the result

see what happens if you highlight the differences in localc, edit some
tags, then save as csv - does the highlighting just disappear leaving
the interesting data only?

-}
    

compareFiles :: ByteString -> ByteString -> IO ByteString
compareFiles csva csvb = do
    let data1 :: [(Text,Text,Text,Text,Text,Text)]
        data1 = either (error . show) V.toList $ decode NoHeader csva
        data2 :: [(Text,Text,Text,Text,Text,Text)]
        data2 = either (error . show) V.toList $ decode NoHeader csvb
    conn <- open ":memory:"
    execute_ conn [R.r|
create table data1 (
    testrunida text,
    parser text,
    dialect text,
    source text,
    result_a text,
    tags_a text)|]
    execute_ conn [R.r|
create table data2 (
    testrunidb text,
    parser text,
    dialect text,
    source text,
    result_b text,
    tags_b text)|]
    
    executeMany conn "insert into data1 values (?,?,?,?,?,?)" data1
    executeMany conn "insert into data2 values (?,?,?,?,?,?)" data2
    r <- query_ conn [R.r|
select
   parser, dialect, source, result_a, tags_a, result_b, tags_b
from data1 natural full outer join data2|] :: IO [(Text,Text,Text,Text,Text,Text,Text)]

    pure $ encode r

------------------------------------------------------------------------------

-- running tests

runTests :: ByteString -> ByteString
runTests csvsrc =
    let csv :: Vector (Text,Text,Text)
        csv = either (error . show) id $ decode NoHeader csvsrc

        testrunid = ("0" :: Text)
    
        testLine (parser,dialect,src) =
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
                        _ -> error $ "unknown parser: " <> T.unpack parser
                -- prepend a newline to multi line fields, so they show
                -- nice in a diff in meld or similar
                resadj = if '\n' `T.elem` res
                         then T.cons '\n' res
                         else res
            in (testrunid, parser, dialect, src, resadj,"" :: Text)

        allres = V.map testLine csv
    in encode $ V.toList allres
                
------------------------------------------------------------------------------

-- generating data

generateData :: ByteString
generateData =
    encode $ concat
    [simpleExpressions1
    ,pgExprs
    ,sqlServerIden
    ,mysqliden
    ,paramvariations
    ,odbcexpr
    ,odbcqexpr
    ,otherParseErrorExamples]

--------------------------------------

-- example data

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


    
otherParseErrorExamples :: [(Text,Text,Text)]
otherParseErrorExamples = flip map (parseExampleStrings src) $ \e ->
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

|]
