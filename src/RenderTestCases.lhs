
Converts the test data to markdown

> import Tests
> import Text.Groom
> import Control.Monad.State
> import Language.SQL.SimpleSQL.Parser

> data TableItem = Heading Int String
>                | Row String String

> doc :: Int -> TestItem -> [TableItem]
> doc n (Group nm is) =
>     Heading n nm
>     : concatMap (doc (n + 1)) is
> doc _ (TestScalarExpr str e) =
>     [Row str (groom e)]
> doc _ (TestQueryExpr str e) =
>     [Row str (groom e)]
> doc _ (TestQueryExprs str e) =
>     [Row str (groom e)]
> doc _ (ParseQueryExpr str) =
>     [Row str (groom $ parseQueryExpr "" Nothing str)]


> render :: [TableItem] -> IO ()
> render ts =
>     go False ts
>   where
>     go t ((Heading level title):is) = do
>         when t $ putStrLn "</table>"
>         putStrLn $ replicate level '#' ++ " " ++ title
>         go False is
>     go t ((Row sql hask):is) = do
>         when (not t) $ putStrLn "<table>"
>         let sql' = "\n~~~~{.sql}\n" ++ sql ++ "\n~~~~\n"
>             hask' = "\n~~~~{.haskell}\n" ++ hask ++ "\n~~~~\n"
>         putStrLn $ "<tr><td>" ++ sql'
>                    ++ "</td><td>" ++ hask' ++ "</td></tr>"
>         go True is
>     go t [] = when t $ putStrLn "</table>"

> main :: IO ()
> main = render $ doc 1 testData
