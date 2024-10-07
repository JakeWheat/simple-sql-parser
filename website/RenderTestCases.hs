-- Converts the test data to markdown
-- it uses raw html for the table parts

{-# LANGUAGE OverloadedStrings #-}
import Language.SQL.SimpleSQL.Tests
import Text.Show.Pretty (ppShow)
import qualified Language.SQL.SimpleSQL.Parse as P
import qualified Language.SQL.SimpleSQL.Lex as L
import qualified Data.Text as T

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

data TableItem = Heading Int L.Text
               | Row L.Text L.Text

doc :: Int -> TestItem -> [TableItem]
-- filter out some groups of tests
doc _ (Group nm _) | "generated" `T.isInfixOf` nm = []
doc n (Group nm is) =
    Heading n (L.fromStrict nm)
    : concatMap (doc (n + 1)) is
doc _ (TestScalarExpr _ str e _) =
    [Row (L.fromStrict str) (L.pack $ ppShow e)]
doc _ (TestQueryExpr _ str e _) =
    [Row (L.fromStrict str) (L.pack $ ppShow e)]
doc _ (TestStatement _ str e _) =
    [Row (L.fromStrict str) (L.pack $ ppShow e)]
doc _ (TestStatements _ str e _) =
    [Row (L.fromStrict str) (L.pack $ ppShow e)]
doc _ (ParseQueryExpr d str _) =
    [Row (L.fromStrict str) (showResult $ P.parseQueryExpr d "" Nothing str)]
doc _ (ParseQueryExprFails d str _) =
    [Row (L.fromStrict str) (showResult $ P.parseQueryExpr d "" Nothing str)]
doc _ (ParseScalarExprFails d str _) =
    [Row (L.fromStrict str) (showResult $ P.parseScalarExpr d "" Nothing str)]

doc _ (LexTest d str _ _) =
    [Row (L.fromStrict str) (showResultL $ L.lexSQL d False "" Nothing str)]

doc _ (LexFails d str _) =
    [Row (L.fromStrict str) (showResultL $ L.lexSQL d False "" Nothing str)]
doc _ (GeneralParseFailTest {}) = []
-- todo: find some way to render error message examples in a readable way
doc _ (GoldenErrorTest {}) = []

showResult :: Show a => Either P.ParseError a -> L.Text
showResult = either (("Left\n" <>) . L.fromStrict . P.prettyError) (L.pack . ppShow)

showResultL :: Show a => Either L.ParseError a -> L.Text
showResultL = either (("Left\n" <>) . L.fromStrict .  L.prettyError) (L.pack . ppShow)


-- TODO: should put the dialect in the html output


render :: [TableItem] -> L.Text
render = go False
  where
    go _t (Heading level title : is) =
        "</table>\n"
        <>
        -- slight hack
        (if (level > 1)
         then "\n" <> L.replicate (fromIntegral $ level - 1) "#" <> " " <> title <> "\n"
         else "")
        <> go False is
    go t (Row sql hask : is) =
        (if (not t)
         then "<table>\n"
         else "")
        <> let sql' = "\n~~~~{.sql}\n" <> sql <> "\n~~~~\n"
               hask' = "\n~~~~{.haskell}\n" <> hask <> "\n~~~~\n"
           in  "<tr><td>\n" <> sql' <> "</td><td>\n" <> hask' <> "</td></tr>\n"
        <> go True is
    go _t [] = "</table>\n"
    {-escapePipe t = T.pack $ escapePipe' $ T.unpack t
    escapePipe' [] = []
    escapePipe' ('\\':'|':xs) = '\\' : '\\' : '\\' : '|' : escapePipe' xs
    escapePipe' ('|':xs) = '\\' : '|' : escapePipe' xs
    escapePipe' (x:xs) = x : escapePipe' xs-}

main :: IO ()
main = L.putStrLn $ render $ doc 1 testData

