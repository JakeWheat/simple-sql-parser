
-- Converts the test data to asciidoc

{-# LANGUAGE OverloadedStrings #-}
import Language.SQL.SimpleSQL.Tests
import Text.Show.Pretty
import Control.Monad.State
import qualified Language.SQL.SimpleSQL.Parse as P
import qualified Language.SQL.SimpleSQL.Lex as L
import Data.List
import Control.Monad (when, unless)
import Data.Text (Text)
import qualified Data.Text as T

import Prelude hiding (putStrLn)
import Data.Text.IO (putStrLn)

data TableItem = Heading Int Text
               | Row Text Text

doc :: Int -> TestItem -> [TableItem]
-- filter out some groups of tests
doc n (Group nm _) | "generated" `T.isInfixOf` nm = []
doc n (Group nm is) =
    Heading n nm
    : concatMap (doc (n + 1)) is
doc _ (TestScalarExpr _ str e) =
    [Row str (T.pack $ ppShow e)]
doc _ (TestQueryExpr _ str e) =
    [Row str (T.pack $ ppShow e)]
doc _ (TestStatement _ str e) =
    [Row str (T.pack $ ppShow e)]
doc _ (TestStatements _ str e) =
    [Row str (T.pack $ ppShow e)]
doc _ (ParseQueryExpr d str) =
    [Row str (showResult $ P.parseQueryExpr d "" Nothing str)]
doc _ (ParseQueryExprFails d str) =
    [Row str (showResult $ P.parseQueryExpr d "" Nothing str)]
doc _ (ParseScalarExprFails d str) =
    [Row str (showResult $ P.parseScalarExpr d "" Nothing str)]

doc _ (LexTest d str t) =
    [Row str (showResultL $ L.lexSQL d "" Nothing str)]

doc _ (LexFails d str) =
    [Row str (showResultL $ L.lexSQL d "" Nothing str)]

showResult :: Show a => Either P.ParseError a -> Text
showResult = either (("Left\n" <>) . P.prettyError) (T.pack . ppShow)

showResultL :: Show a => Either L.ParseError a -> Text
showResultL = either (("Left\n" <>) . L.prettyError) (T.pack . ppShow)


-- TODO: should put the dialect in the html output


render :: [TableItem] -> IO ()
render = go False
  where
    go t (Heading level title : is) = do
        when t $ putStrLn "|==="
        -- slight hack
        when (level > 1) $
            putStrLn $ "\n" <> T.replicate level "=" <> " " <> title
        go False is
    go t (Row sql hask : is) = do
        unless t $ putStrLn "[cols=\"2\"]\n|==="
        let sql' = "\n[source,sql]\n----\n" <> sql <> "\n----\n"
            hask' = "\n[source,haskell]\n----\n" <> hask <> "\n----\n"
        putStrLn $ "a| " <> escapePipe sql'
                   <> "a| " <> escapePipe hask' <> " "
        go True is
    go t [] = when t $ putStrLn "|==="
    escapePipe t = T.pack $ escapePipe' $ T.unpack t
    escapePipe' [] = []
    escapePipe' ('\\':'|':xs) = '\\' : '\\' : '\\' : '|' : escapePipe' xs
    escapePipe' ('|':xs) = '\\' : '|' : escapePipe' xs
    escapePipe' (x:xs) = x : escapePipe' xs

main :: IO ()
main = do
       putStrLn "\n:toc:\n\
                \:toc-placement: macro\n\
                \:sectnums:\n\
                \:toclevels: 10\n\
                \:sectnumlevels: 10\n\
                \:source-highlighter: pygments\n\n\
                \= simple-sql-parser examples/test cases\n\n\
                \toc::[]\n"
       render $ doc 1 testData
