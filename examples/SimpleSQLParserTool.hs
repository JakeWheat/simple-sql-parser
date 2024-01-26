
{-
Simple command line tool to experiment with simple-sql-parser

Commands:

parse: parse sql from file, stdin or from command line
lex: lex sql same
indent: parse then pretty print sql

TODO: this is supposed to be a simple example, but it's a total mess
write some simple helpers so it's all in text?

-}

{-# LANGUAGE TupleSections #-}
import System.Environment (getArgs)
import Control.Monad (forM_, when)
import Data.Maybe (isJust)
import System.Exit (exitFailure)
import Data.List (intercalate)
import Text.Show.Pretty (ppShow)
--import Control.Applicative

import qualified Data.Text as T

import Language.SQL.SimpleSQL.Pretty
    (prettyStatements)
import Language.SQL.SimpleSQL.Parse
    (parseStatements
    ,prettyError)
import qualified Language.SQL.SimpleSQL.Lex as L
import Language.SQL.SimpleSQL.Dialect (ansi2011)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
              showHelp $ Just "no command given"
        (c:as) -> do
             let cmd = lookup c commands
             maybe (showHelp (Just "command not recognised"))
                   (\(_,cmd') -> cmd' as)
                   cmd

commands :: [(String, (String,[String] -> IO ()))]
commands =
    [("help", helpCommand)
    ,("parse", parseCommand)
    ,("lex", lexCommand)
    ,("format", formatCommand)]

showHelp :: Maybe String -> IO ()
showHelp msg = do
          maybe (return ()) (\e -> putStrLn $ "Error: " ++ e) msg
          putStrLn "Usage:\n SimpleSQLParserTool command args"
          forM_ commands $ \(c, (h,_)) -> do
               putStrLn $ c ++ "\t" ++ h
          when (isJust msg) $ exitFailure

helpCommand :: (String,[String] -> IO ())
helpCommand =
    ("show help for this progam", \_ -> showHelp Nothing)

getInput :: [String] -> IO (FilePath,String)
getInput as =
    case as of
      ["-"] -> ("",) <$> getContents
      ("-c":as') -> return ("", unwords as')
      [filename] -> (filename,) <$> readFile filename
      _ -> showHelp (Just "arguments not recognised") >> error ""

parseCommand :: (String,[String] -> IO ())
parseCommand =
  ("parse SQL from file/stdin/command line (use -c to parse from command line)"
  ,\args -> do
      (f,src) <- getInput args
      either (error . T.unpack . prettyError)
          (putStrLn . ppShow)
          $ parseStatements ansi2011 (T.pack f) Nothing (T.pack src)
  )

lexCommand :: (String,[String] -> IO ())
lexCommand =
  ("lex SQL from file/stdin/command line (use -c to parse from command line)"
  ,\args -> do
      (f,src) <- getInput args
      either (error . T.unpack . L.prettyError)
             (putStrLn . intercalate ",\n" . map show)
             $ L.lexSQL ansi2011 (T.pack f) Nothing (T.pack src)
  )


formatCommand :: (String,[String] -> IO ())
formatCommand =
  ("parse then pretty print SQL from file/stdin/command line (use -c to parse from command line)"
  ,\args -> do
      (f,src) <- getInput args
      either (error . T.unpack . prettyError)
          (putStrLn . T.unpack . prettyStatements ansi2011)
          $ parseStatements ansi2011 (T.pack f) Nothing (T.pack src)

  )
