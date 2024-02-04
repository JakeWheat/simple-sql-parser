
{-
See the file examples/ErrorMessagesTool.hs for some work on this

TODO:

add simple test to check the error and quoting on later line in multi
line input for lexing and parsing; had a regression here that made it
to a release

-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.SQL.SimpleSQL.ErrorMessages
    (errorMessageTests
    ) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Parse
import qualified Language.SQL.SimpleSQL.Lex as L
import Language.SQL.SimpleSQL.TestRunners
--import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Expectations
import Test.Hspec (it)
import Debug.Trace

import Data.Text (Text)
import qualified Data.Text as T

import qualified Text.RawString.QQ as R

errorMessageTests :: TestItem
errorMessageTests = Group "error messages"
    [gp (parseQueryExpr ansi2011 "" Nothing) prettyError [R.r|

select
a
from t
where
  something
order by 1,2,3 where

        |]
        [R.r|8:16:
  |
8 | order by 1,2,3 where
  |                ^^^^^
unexpected where
|]
   ,gp (L.lexSQL ansi2011 False "" Nothing)  L.prettyError [R.r|
           
select
a
from t
where
  something
order by 1,2,3 $@

        |]
        [R.r|8:16:
  |
8 | order by 1,2,3 $@
  |                ^
unexpected '$'
|]
        ]
  where
    
    gp :: (Show a, HasCallStack) => (Text -> Either e a) -> (e -> Text) -> Text -> Text -> TestItem
    gp parse pret src err =
        GeneralParseFailTest src err $
           it (T.unpack src) $
           let f1 = parse src
               ex = shouldFailWith pret
               quickTrace =
                   case f1 of
                       Left f | pret f /= err ->
                            trace (T.unpack ("check\n[" <> pret f <>"]\n["<> err <> "]\n"))
                       _ -> id
           in quickTrace (f1 `ex` err)
