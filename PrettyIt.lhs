
> import System.Environment

> import Language.SQL.SimpleSQL.Pretty
> import Language.SQL.SimpleSQL.Parser
> import Data.List

> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>       [f] -> do
>              src <- readFile f
>              either (error . peFormattedError)
>                     (putStrLn . intercalate "\n" . map prettyQueryExpr)
>                     $ parseQueryExprs f Nothing src
>       _ -> error "please pass filename to prettify"
