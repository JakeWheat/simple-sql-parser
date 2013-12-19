
> import System.Environment

> import Language.SQL.SimpleSQL.Pretty
> import Language.SQL.SimpleSQL.Parser

> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>       [f] -> do
>              src <- readFile f
>              either (error . peFormattedError)
>                     (putStrLn . prettyQueryExprs)
>                     $ parseQueryExprs f Nothing src
>       _ -> error "please pass filename to indent"
