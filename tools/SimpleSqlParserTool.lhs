
Simple command line tool to experiment with simple-sql-parser

Commands:

parse: parse sql from file, stdin or from command line
lex: lex sql same
indent: parse then pretty print sql

> {-# LANGUAGE TupleSections #-}
> import System.Environment
> import Control.Monad
> import Data.Maybe
> import System.Exit
> import Data.List
> import Text.Show.Pretty

> import Language.SQL.SimpleSQL.Pretty
> import Language.SQL.SimpleSQL.Parser
> import Language.SQL.SimpleSQL.Syntax
> import Language.SQL.SimpleSQL.Lexer


> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>         [] -> do
>               showHelp $ Just "no command given"
>         (c:as) -> do
>              let cmd = lookup c commands
>              maybe (showHelp (Just "command not recognised"))
>                    (\(_,cmd') -> cmd' as)
>                    cmd

> commands :: [(String, (String,[String] -> IO ()))]
> commands =
>     [("help", helpCommand)
>     ,("parse", parseCommand)
>     ,("lex", lexCommand)
>     ,("indent", indentCommand)]

> showHelp :: Maybe String -> IO ()
> showHelp msg = do
>           maybe (return ()) (\e -> putStrLn $ "Error: " ++ e) msg
>           putStrLn "Usage:\n SimpleSqlParserTool command args"
>           forM_ commands $ \(c, (h,_)) -> do
>                putStrLn $ c ++ "\t" ++ h
>           when (isJust msg) $ exitFailure

> helpCommand :: (String,[String] -> IO ())
> helpCommand =
>     ("show help for this progam", \_ -> showHelp Nothing)

> getInput :: [String] -> IO (FilePath,String)
> getInput as =
>     case as of
>       ["-"] -> error "read stdin"
>       ("-c":as') -> return ("-", unwords as')
>       [filename] -> (filename,) <$> readFile filename
>       _ -> showHelp (Just "arguments not recognised") >> error ""

> parseCommand :: (String,[String] -> IO ())
> parseCommand =
>   ("parse SQL from file/stdin/command line (use -c to parse from command line)"
>   ,\args -> do
>       (f,src) <- getInput args
>       either (error . peFormattedError)
>           (putStrLn . ppShow)
>           $ parseQueryExprs SQL2011 f Nothing src
>   )

> lexCommand :: (String,[String] -> IO ())
> lexCommand =
>   ("lex SQL from file/stdin/command line (use -c to parse from command line)"
>   ,\args -> do
>       (f,src) <- getInput args
>       either (error . peFormattedError)
>              (putStrLn . intercalate ",\n" . map show)
>              $ lexSQL SQL2011 f Nothing src
>   )


> indentCommand :: (String,[String] -> IO ())
> indentCommand =
>   ("parse then pretty print SQL from file/stdin/command line (use -c to parse from command line)"
>   ,\args -> do
>       (f,src) <- getInput args
>       either (error . peFormattedError)
>           (putStrLn . prettyQueryExprs SQL2011)
>           $ parseQueryExprs SQL2011 f Nothing src

>   )
