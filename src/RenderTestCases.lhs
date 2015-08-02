
Converts the test data to markdown

> import Language.SQL.SimpleSQL.Tests
> import Text.Show.Pretty
> import Control.Monad.State
> import Language.SQL.SimpleSQL.Parser
> import Language.SQL.SimpleSQL.Lexer

> data TableItem = Heading Int String
>                | Row String String

> doc :: Int -> TestItem -> [TableItem]
> doc n (Group nm is) =
>     Heading n nm
>     : concatMap (doc (n + 1)) is
> doc _ (TestValueExpr _ str e) =
>     [Row str (ppShow e)]
> doc _ (TestQueryExpr _ str e) =
>     [Row str (ppShow e)]
> doc _ (TestStatement _ str e) =
>     [Row str (ppShow e)]
> doc _ (TestStatements _ str e) =
>     [Row str (ppShow e)]
> doc _ (ParseQueryExpr d str) =
>     [Row str (ppShow $ parseQueryExpr d "" Nothing str)]
> doc _ (ParseQueryExprFails d str) =
>     [Row str (ppShow $ parseQueryExpr d "" Nothing str)]
> doc _ (ParseValueExprFails d str) =
>     [Row str (ppShow $ parseValueExpr d "" Nothing str)]

> doc _ (LexerTest d str t) =
>   -- todo: figure out how to handle this:
>   -- too many entries, but want to show the lexing
>   -- a bit
>     -- [Row str (ppShow $ lexSQL d "" Nothing str)]
>     []
>     -- should probably think about doing something similar
>     -- with other generated combination tests such as the typename
>     -- tests

TODO: should put the dialect in the html output


> render :: [TableItem] -> IO ()
> render = go False
>   where
>     go t (Heading level title : is) = do
>         when t $ putStrLn "</table>"
>         putStrLn $ replicate level '#' ++ " " ++ title
>         go False is
>     go t (Row sql hask : is) = do
>         unless t $ putStrLn "<table>"
>         let sql' = "\n~~~~{.sql}\n" ++ sql ++ "\n~~~~\n"
>             hask' = "\n~~~~{.haskell}\n" ++ hask ++ "\n~~~~\n"
>         putStrLn $ "<tr><td>" ++ sql'
>                    ++ "</td><td>" ++ hask' ++ "</td></tr>"
>         go True is
>     go t [] = when t $ putStrLn "</table>"

> main :: IO ()
> main = render $ doc 1 testData
