
-- Simple example to show parsing some SQL then pretty printing the AST

{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Text.Show.Pretty
import System.IO

import Language.SQL.SimpleSQL.Parse
       (parseStatements
       ,ParseError
       ,prettyError
       ,ansi2011)

import Language.SQL.SimpleSQL.Syntax (Statement)
import qualified Data.Text as T

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
              -- read from stdin
              c <- getContents
              doIt c
        ["-s", sql] -> do
              -- parse arg given
              doIt sql
        [f] ->
              -- read file
              withFile f ReadMode (\h -> do
                  x <- hGetContents h
                  doIt x)
        _ -> do
            putStrLn "use no arguments to stream sql from stdin, e.g.:\n\
                     \  cat some.sql | SimpleSQLParserExample\n\
                     \n\
                     \use -s to parse sql on command line, e.g.:\n\
                     \  SimpleSQLParserExample -s \"select * from t\"\n\
                     \use a single arg to parse a file, e.g.\n\
                     \  SimpleSQLParserExample some.sql"

doIt :: String -> IO ()
doIt src = do
    let parsed :: Either ParseError [Statement]
        parsed = parseStatements ansi2011 "" Nothing (T.pack src)
    either (error . T.unpack . prettyError)
           (putStrLn . ppShow)
           parsed
