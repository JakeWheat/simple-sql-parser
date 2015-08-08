
Converts the test data to asciidoc

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
>         when t $ putStrLn "|==="
>         -- slight hack
>         when (level > 1) $
>             putStrLn $ "\n" ++ replicate level '=' ++ " " ++ title
>         go False is
>     go t (Row sql hask : is) = do
>         unless t $ putStrLn "[cols=\"2\"]\n|==="
>         let sql' = "\n[source,sql]\n----\n" ++ sql ++ "\n----\n"
>             hask' = "\n[source,haskell]\n----\n" ++ hask ++ "\n----\n"
>         putStrLn $ "a| " ++ escapePipe sql'
>                    ++ "a| " ++ escapePipe hask' ++ " "
>         go True is
>     go t [] = when t $ putStrLn "|==="
>     escapePipe [] = []
>     escapePipe ('\\':'|':xs) = '\\' : '\\' : '\\' : '|' : escapePipe xs
>     escapePipe ('|':xs) = '\\' : '|' : escapePipe xs
>     escapePipe (x:xs) = x : escapePipe xs

> main :: IO ()
> main = do
>        putStrLn "\n:toc:\n\
>                 \:toc-placement: macro\n\
>                 \:sectnums:\n\
>                 \:toclevels: 10\n\
>                 \:sectnumlevels: 10\n\
>                 \:source-highlighter: pygments\n\n\
>                 \= simple-sql-parser examples/test cases\n\n\
>                 \toc::[]\n"
>        render $ doc 1 testData
