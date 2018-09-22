
> import System.Environment

> import Language.SQL.SimpleSQL.Pretty
> import Language.SQL.SimpleSQL.Parser
> import Language.SQL.SimpleSQL.Syntax

> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>       [f] -> do
>              src <- readFile f
>              either (error . peFormattedError)
>                     (putStrLn . prettyQueryExprs SQL2011)
>                     $ parseQueryExprs SQL2011 f Nothing src
>       _ -> error "please pass filename to indent"
