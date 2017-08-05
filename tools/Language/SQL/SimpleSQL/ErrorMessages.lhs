
Want to work on the error messages. Ultimately, parsec won't give the
best error message for a parser combinator library in haskell. Should
check out the alternatives such as polyparse and uu-parsing.

For now the plan is to try to get the best out of parsec. Skip heavy
work on this until the parser is more left factored?

Ideas:

1. generate large lists of invalid syntax
2. create table of the sql source and the error message
3. save these tables and compare from version to version. Want to
   catch improvements and regressions and investigate. Have to do this
   manually

= generating bad sql source

take good sql statements or expressions. Convert them into sequences
of tokens - want to preserve the whitespace and comments perfectly
here. Then modify these lists by either adding a token, removing a
token, or modifying a token (including creating bad tokens of raw
strings which don't represent anything than can be tokenized.

Now can see the error message for all of these bad strings. Probably
have to generate and prune this list manually in stages since there
will be too many.

Contexts:

another area to focus on is contexts: for instance, we have a set of
e.g. 1000 bad scalar expressions with error messages. Now can put
those bad scalar expressions into various contexts and see that the
error messages are still good.

plan:

1. create a list of all the value expression, with some variations for
   each
2. manually create some error variations for each expression
3. create a renderer which will create a csv of the expressions and
   the errors
   this is to load as a spreadsheet to investigate more
4. create a renderer for the csv which will create a markdown file for
   the website. this is to demonstrate the error messages in the
   documentation

Then create some contexts for all of these: inside another value
expression, or inside a query expression. Do the same: render and
review the error messages.

Then, create some query expressions to focus on the non value
expression parts.


> module Language.SQL.SimpleSQL.ErrorMessages where

> {-import Language.SQL.SimpleSQL.Parser
> import Data.List
> import Text.Groom

> valueExpressions :: [String]
> valueExpressions =
>     ["10.."
>     ,"..10"
>     ,"10e1e2"
>     ,"10e--3"
>     ,"1a"
>     ,"1%"

>     ,"'b'ad'"
>     ,"'bad"
>     ,"bad'"

>     ,"interval '5' ay"
>     ,"interval '5' day (4.4)"
>     ,"interval '5' day (a)"
>     ,"intervala '5' day"
>     ,"interval 'x' day (3"
>     ,"interval 'x' day 3)"

>     ,"1badiden"
>     ,"$"
>     ,"!"
>     ,"*.a"

>     ,"??"
>     ,"3?"
>     ,"?a"

>     ,"row"
>     ,"row 1,2"
>     ,"row(1,2"
>     ,"row 1,2)"
>     ,"row(1 2)"

>     ,"f("
>     ,"f)"

>     ,"f(a"
>     ,"f a)"
>     ,"f(a b)"

TODO:
case
operators

>    ,"a + (b + c"

casts
subqueries: + whole set of parentheses use
in list
'keyword' functions
aggregates
window functions


>    ]

> queryExpressions :: [String]
> queryExpressions =
>     map sl1 valueExpressions
>     ++ map sl2 valueExpressions
>     ++ map sl3 valueExpressions
>     ++
>     ["select a from t inner jin u"]
>   where
>     sl1 x = "select " ++ x ++ " from t"
>     sl2 x = "select " ++ x ++ ", y from t"
>     sl3 x = "select " ++ x ++ " fom t"

> valExprs :: [String] -> [(String,String)]
> valExprs = map parseOne
>   where
>     parseOne x = let p = parseValueExpr "" Nothing x
>                  in (x,either peFormattedError (\x -> "ERROR: parsed ok " ++ groom x) p)


> queryExprs :: [String] -> [(String,String)]
> queryExprs = map parseOne
>   where
>     parseOne x = let p = parseQueryExpr "" Nothing x
>                  in (x,either peFormattedError (\x -> "ERROR: parsed ok " ++ groom x) p)


> pExprs :: [String] -> [String] -> String
> pExprs x y =
>     let l = valExprs x ++ queryExprs y
>     in intercalate "\n\n\n\n" $ map (\(a,b) -> a ++ "\n" ++ b) l
> -}
