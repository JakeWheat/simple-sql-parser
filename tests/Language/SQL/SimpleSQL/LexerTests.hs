

-- Test for the lexer


{-
TODO:
figure out a way to do quickcheck testing:
1. generate valid tokens and check they parse

2. combine two generated tokens together for the combo testing

this especially will work much better for the postgresql extensible
operator tests which doing exhaustively takes ages and doesn't bring
much benefit over testing a few using quickcheck.
-}

{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.LexerTests (lexerTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Lex
    (Token(..)
    ,tokenListWillPrintAndLex
    )
import Language.SQL.SimpleSQL.TestRunners

import qualified Data.Text as T
import Data.Text (Text)
    
--import Debug.Trace
--import Data.Char (isAlpha)
-- import Data.List

lexerTests :: TestItem
lexerTests = Group "lexerTests" $
    [bootstrapTests
    ,ansiLexerTests
    ,postgresLexerTests
    ,sqlServerLexerTests
    ,oracleLexerTests
    ,mySqlLexerTests
    ,odbcLexerTests
    ]

-- quick sanity tests to see something working
bootstrapTests :: TestItem
bootstrapTests = Group "bootstrap tests" $
    [t "iden" [Identifier Nothing "iden"]

    ,t "\"a1normal \"\" iden\"" [Identifier (Just ("\"","\"")) "a1normal \"\" iden"]

    ,t "'string'" [SqlString "'" "'" "string"]

    ,t "  " [Whitespace "  "]
    ,t "\t  " [Whitespace "\t  "]
    ,t "  \n  " [Whitespace "  \n  "]
    
    ,t "--" [LineComment "--"]
    ,t "--\n" [LineComment "--\n"]
    ,t "--stuff" [LineComment "--stuff"]
    ,t "-- stuff" [LineComment "-- stuff"]
    ,t "-- stuff\n" [LineComment "-- stuff\n"]
    ,t "--\nstuff" [LineComment "--\n", Identifier Nothing "stuff"]
    ,t "-- com \nstuff" [LineComment "-- com \n", Identifier Nothing "stuff"]

    ,t "/*test1*/" [BlockComment "/*test1*/"]
    ,t "/**/" [BlockComment "/**/"]
    ,t "/***/" [BlockComment "/***/"]
    ,t "/* * */" [BlockComment "/* * */"]
    ,t "/*test*/" [BlockComment "/*test*/"]
    ,t "/*te/*st*/*/" [BlockComment "/*te/*st*/*/"]
    ,t "/*te*st*/" [BlockComment "/*te*st*/"]
    ,t "/*lines\nmore lines*/" [BlockComment "/*lines\nmore lines*/"]
    ,t "/*test1*/\n" [BlockComment "/*test1*/", Whitespace "\n"]
    ,t "/*test1*/stuff" [BlockComment "/*test1*/", Identifier Nothing "stuff"]

    ,t "1" [SqlNumber "1"]
    ,t "42" [SqlNumber "42"]

    ,tp "$1" [PositionalArg 1]
    ,tp "$200" [PositionalArg 200]

    ,t ":test" [PrefixedVariable ':' "test"]
       
    ] ++ map (\a -> t a [Symbol a]) (
     ["!=", "<>", ">=", "<=", "||"]
     ++ map T.singleton ("(),-+*/<>=." :: [Char]))
  where
    t :: HasCallStack => Text -> [Token] -> TestItem
    t src ast = testLex ansi2011 src ast
    tp :: HasCallStack => Text -> [Token] -> TestItem
    tp src ast = testLex ansi2011{diPositionalArg=True} src ast


ansiLexerTable :: [(Text,[Token])]
ansiLexerTable =
    -- single char symbols
    map (\s -> (T.singleton s,[Symbol $ T.singleton s])) "+-^*/%~&|?<>[]=,;()"
    -- multi char symbols
    ++ map (\s -> (s,[Symbol s])) [">=","<=","!=","<>","||"]
    ++ (let idens = ["a", "_a", "test", "table", "Stuff", "STUFF"]
        -- simple identifiers
        in map (\i -> (i, [Identifier Nothing i])) idens
           <> map (\i -> ("\"" <> i <> "\"", [Identifier (Just ("\"","\"")) i])) idens
           -- todo: in order to make lex . pretty id, need to
           -- preserve the case of the u
           <> map (\i -> ("u&\"" <> i <> "\"", [Identifier (Just ("u&\"","\"")) i])) idens
           -- host param
           <> map (\i -> (T.cons ':' i, [PrefixedVariable ':' i])) idens
       )
    -- quoted identifiers with embedded double quotes
    -- the lexer doesn't unescape the quotes
    ++ [("\"anormal \"\" iden\"", [Identifier (Just ("\"","\"")) "anormal \"\" iden"])]
    -- strings
    -- the lexer doesn't apply escapes at all
    ++ [("'string'", [SqlString "'" "'" "string"])
       ,("'normal '' quote'", [SqlString "'" "'" "normal '' quote"])
       ,("'normalendquote '''", [SqlString "'" "'" "normalendquote ''"])
       ,("'\n'", [SqlString "'" "'" "\n"])]
    -- csstrings
    ++ map (\c -> (c <> "'test'", [SqlString (c <> "'") "'" "test"]))
       ["n", "N","b", "B","x", "X", "u&"]
    -- numbers
    ++ [("10", [SqlNumber "10"])
       ,(".1", [SqlNumber ".1"])
       ,("5e3", [SqlNumber "5e3"])
       ,("5e+3", [SqlNumber "5e+3"])
       ,("5e-3", [SqlNumber "5e-3"])
       ,("10.2", [SqlNumber "10.2"])
       ,("10.2e7", [SqlNumber "10.2e7"])]
    -- whitespace
    ++ concat [[(T.singleton a,[Whitespace $ T.singleton a])
               ,(T.singleton a <> T.singleton b, [Whitespace (T.singleton a <> T.singleton b)])]
              | a <- " \n\t", b <- " \n\t"]
    -- line comment
    ++ map (\c -> (c, [LineComment c]))
       ["--", "-- ", "-- this is a comment", "-- line com\n"]
    -- block comment
    ++ map (\c -> (c, [BlockComment c]))
       ["/**/", "/* */","/* this is a comment */"
       ,"/* this *is/ a comment */"
       ]


ansiLexerTests :: TestItem
ansiLexerTests = Group "ansiLexerTests" $
    [Group "ansi lexer token tests" $ [l s t |  (s,t) <- ansiLexerTable]
    ,Group "ansi generated combination lexer tests" $
        [ l (s <> s1) (t <> t1)
        | (s,t) <- ansiLexerTable
        , (s1,t1) <- ansiLexerTable
        , tokenListWillPrintAndLex ansi2011 $ t <> t1

        ]
    ,Group "ansiadhoclexertests" $
        [l "" []
        ,l "-- line com\nstuff"  [LineComment "-- line com\n",Identifier Nothing "stuff"]
        ] ++
        [-- want to make sure this gives a parse error
         f "*/"
         -- combinations of pipes: make sure they fail because they could be
         -- ambiguous and it is really unclear when they are or not, and
         -- what the result is even when they are not ambiguous
        ,f "|||"
        ,f "||||"
        ,f "|||||"
         -- another user experience thing: make sure extra trailing
         -- number chars are rejected rather than attempting to parse
         -- if the user means to write something that is rejected by this code,
         -- then they can use whitespace to make it clear and then it will parse
        ,f "12e3e4"
        ,f "12e3e4"
        ,f "12e3e4"
        ,f "12e3.4"
        ,f "12.4.5"
        ,f "12.4e5.6"
        ,f "12.4e5e7"]
    ]
  where
    l :: HasCallStack => Text -> [Token] -> TestItem
    l src ast = testLex ansi2011 src ast
    f :: HasCallStack => Text -> TestItem
    f src = lexFails ansi2011 src


{-
todo: lexing tests
do quickcheck testing:
can try to generate valid tokens then check they parse

same as above: can also try to pair tokens, create an accurate
  function to say which ones can appear adjacent, and test

I think this plus the explicit lists of tokens like above which do
basic sanity + explicit edge casts will provide a high level of
assurance.
-}



postgresLexerTable :: [(Text,[Token])]
postgresLexerTable =
    -- single char symbols
    map (\s -> (T.singleton s,[Symbol $ T.singleton s])) "+-^*/%~&|?<>[]=,;():"
    -- multi char symbols
    ++ map (\s -> (s,[Symbol s])) [">=","<=","!=","<>","||", "::","..",":="]
    -- generic symbols

    ++ (let idens = ["a", "_a", "test", "table", "Stuff", "STUFF"]
        -- simple identifiers
        in map (\i -> (i, [Identifier Nothing i])) idens
           ++ map (\i -> ("\"" <> i <> "\"", [Identifier (Just ("\"","\"")) i])) idens
           -- todo: in order to make lex . pretty id, need to
           -- preserve the case of the u
           ++ map (\i -> ("u&\"" <> i <> "\"", [Identifier (Just ("u&\"","\"")) i])) idens
           -- host param
           ++ map (\i -> (T.cons ':' i, [PrefixedVariable ':' i])) idens
       )
    -- positional var
    ++ [("$1", [PositionalArg 1])]
    -- quoted identifiers with embedded double quotes
    ++ [("\"normal \"\" iden\"", [Identifier (Just ("\"","\"")) "normal \"\" iden"])]
    -- strings
    ++ [("'string'", [SqlString "'" "'" "string"])
       ,("'normal '' quote'", [SqlString "'" "'" "normal '' quote"])
       ,("'normalendquote '''", [SqlString "'" "'" "normalendquote ''"])
       ,("'\n'", [SqlString "'" "'" "\n"])
       ,("E'\n'", [SqlString "E'" "'" "\n"])
       ,("e'this '' quote'", [SqlString "e'" "'" "this '' quote"])
       ,("e'this \\' quote'", [SqlString "e'" "'" "this \\' quote"])
       ,("'not this \\' quote", [SqlString "'" "'" "not this \\"
                                ,Whitespace " "
                                ,Identifier Nothing "quote"])
       ,("$$ string 1 $$", [SqlString "$$" "$$" " string 1 "])
       ,("$$ string $ 2 $$", [SqlString "$$" "$$" " string $ 2 "])
       ,("$a$ $$string 3$$ $a$", [SqlString "$a$" "$a$" " $$string 3$$ "])
       ]
    -- csstrings
    ++ map (\c -> (c <> "'test'", [SqlString (c <> "'") "'" "test"]))
       ["n", "N","b", "B","x", "X", "u&", "e", "E"]
    -- numbers
    ++ [("10", [SqlNumber "10"])
       ,(".1", [SqlNumber ".1"])
       ,("5e3", [SqlNumber "5e3"])
       ,("5e+3", [SqlNumber "5e+3"])
       ,("5e-3", [SqlNumber "5e-3"])
       ,("10.2", [SqlNumber "10.2"])
       ,("10.2e7", [SqlNumber "10.2e7"])]
    -- whitespace
    ++ concat [[(T.singleton a,[Whitespace $ T.singleton a])
               ,(T.singleton a <> T.singleton b, [Whitespace $ T.singleton a <> T.singleton b])]
              | a <- " \n\t", b <- " \n\t"]
    -- line comment
    ++ map (\c -> (c, [LineComment c]))
       ["--", "-- ", "-- this is a comment", "-- line com\n"]
    -- block comment
    ++ map (\c -> (c, [BlockComment c]))
       ["/**/", "/* */","/* this is a comment */"
       ,"/* this *is/ a comment */"
       ]

{-
An operator name is a sequence of up to NAMEDATALEN-1 (63 by default) characters from the following list:

+ - * / < > = ~ ! @ # % ^ & | ` ?

There are a few restrictions on operator names, however:
-- and /* cannot appear anywhere in an operator name, since they will be taken as the start of a comment.

A multiple-character operator name cannot end in + or -, unless the name also contains at least one of these characters:

~ ! @ # % ^ & | ` ?

todo: 'negative' tests
symbol then --
symbol then /*
operators without one of the exception chars
  followed by + or - without whitespace

also: do the testing for the ansi compatibility special cases
-}

postgresShortOperatorTable :: [(Text,[Token])]
postgresShortOperatorTable =
    [ (x, [Symbol x]) | x <- someValidPostgresOperators 2]


postgresExtraOperatorTable :: [(Text,[Token])]
postgresExtraOperatorTable =
    [ (x, [Symbol x]) | x <- someValidPostgresOperators 4]


someValidPostgresOperators :: Int -> [Text]
someValidPostgresOperators l =
       [ x
       | n <- [1..l]
       , x <- combos "+-*/<>=~!@#%^&|`?" n
       , not ("--" `T.isInfixOf` x || "/*" `T.isInfixOf` x || "*/" `T.isInfixOf` x)
       , not (T.last x `T.elem` "+-")
         || or (map (`T.elem` x) "~!@#%^&|`?")
       ]

{-
These are postgres operators, which if followed immediately by a + or
-, will lex as separate operators rather than one operator including
the + or -.
-}

somePostgresOpsWhichWontAddTrailingPlusMinus :: Int -> [Text]
somePostgresOpsWhichWontAddTrailingPlusMinus l =
       [ x
       | n <- [1..l]
       , x <- combos "+-*/<>=" n
       , not ("--" `T.isInfixOf` x || "/*" `T.isInfixOf` x || "*/" `T.isInfixOf` x)
       , not (T.last x `T.elem` "+-")
       ]

postgresLexerTests :: TestItem
postgresLexerTests = Group "postgresLexerTests" $
    [Group "postgres lexer token tests" $
     [l s t | (s,t) <- postgresLexerTable]
    ,Group "postgres generated lexer token tests" $
     [l s t | (s,t) <- postgresShortOperatorTable ++ postgresExtraOperatorTable]
    ,Group "postgres generated combination lexer tests" $
    [ l (s <> s1) (t <> t1)
    | (s,t) <- postgresLexerTable ++ postgresShortOperatorTable
    , (s1,t1) <- postgresLexerTable ++ postgresShortOperatorTable
    , tokenListWillPrintAndLex postgres $ t ++ t1

    ]
    ,Group "generated postgres edgecase lexertests" $
     [l s t
     | (s,t) <- edgeCaseCommentOps
                ++ edgeCasePlusMinusOps
                ++ edgeCasePlusMinusComments]

    ,Group "adhoc postgres lexertests" $
      -- need more tests for */ to make sure it is caught if it is in the middle of a
      -- sequence of symbol letters
        [f "*/"
        ,f ":::"
        ,f "::::"
        ,f ":::::"
        ,f "@*/"
        ,f "-*/"
        ,f "12e3e4"
        ,f "12e3e4"
        ,f "12e3e4"
        ,f "12e3.4"
        ,f "12.4.5"
        ,f "12.4e5.6"
        ,f "12.4e5e7"
         -- special case allow this to lex to 1 .. 2
         -- this is for 'for loops' in plpgsql
        ,l "1..2" [SqlNumber "1", Symbol "..", SqlNumber "2"]
        ]
    ]
 where
   edgeCaseCommentOps =
     [ (x <> "/*<test*/", [Symbol x, BlockComment "/*<test*/"])
     | x <- eccops
     , not (T.last x == '*')
     ] ++
     [ (x <> "--<test", [Symbol x, LineComment "--<test"])
     | x <- eccops
     , not (T.last x == '-')
     ]
   eccops = someValidPostgresOperators 2
   edgeCasePlusMinusOps = concat
     [ [ (x <> "+", [Symbol x, Symbol "+"])
       , (x <> "-", [Symbol x, Symbol "-"]) ]
     | x <- somePostgresOpsWhichWontAddTrailingPlusMinus 2
     ]
   edgeCasePlusMinusComments =
     [("---", [LineComment "---"])
     ,("+--", [Symbol "+", LineComment "--"])
     ,("-/**/", [Symbol "-", BlockComment "/**/"])
     ,("+/**/", [Symbol "+", BlockComment "/**/"])
     ]
   l :: HasCallStack => Text -> [Token] -> TestItem
   l src ast = testLex postgres src ast
   f :: HasCallStack => Text -> TestItem
   f src = lexFails postgres src

sqlServerLexerTests :: TestItem
sqlServerLexerTests = Group "sqlServerLexTests" $
    [l s t | (s,t) <-
    [("@variable", [(PrefixedVariable '@' "variable")])
    ,("#variable", [(PrefixedVariable '#' "variable")])
    ,("[quoted identifier]", [(Identifier (Just ("[", "]")) "quoted identifier")])
    ]]
 where
    l :: HasCallStack => Text -> [Token] -> TestItem
    l src ast = testLex sqlserver src ast

oracleLexerTests :: TestItem
oracleLexerTests = Group "oracleLexTests" $
    [] -- nothing oracle specific atm

mySqlLexerTests :: TestItem
mySqlLexerTests = Group "mySqlLexerTests" $
    [ l s t | (s,t) <-
    [("`quoted identifier`", [(Identifier (Just ("`", "`")) "quoted identifier")])
    ]
    ]
 where
    l :: HasCallStack => Text -> [Token] -> TestItem
    l src ast = testLex mysql src ast

odbcLexerTests :: TestItem
odbcLexerTests = Group "odbcLexTests" $
    [ lo s t | (s,t) <-
    [("{}", [Symbol "{", Symbol "}"])
    ]]
    ++ [lno "{"
       ,lno "}"]
 where
    lo :: HasCallStack => Text -> [Token] -> TestItem
    lo src ast = testLex (sqlserver {diOdbc = True}) src ast
    lno :: HasCallStack => Text -> TestItem
    lno src = lexFails (sqlserver{diOdbc = False}) src


combos :: [Char] -> Int -> [Text]
combos _ 0 = [T.empty]
combos l n = [ T.cons x tl | x <- l, tl <- combos l (n - 1) ]

