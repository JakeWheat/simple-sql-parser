

Test for the lexer

> module Language.SQL.SimpleSQL.LexerTests (lexerTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Lex (Token(..))
> --import Debug.Trace
> import Data.Char (isAlpha)

> lexerTests :: TestItem
> lexerTests = Group "lexerTests" $
>     [Group "lexer token tests" [ansiLexerTests
>                                ,postgresLexerTests]]

> ansiLexerTable :: [(String,[Token])]
> ansiLexerTable =
>     -- single char symbols
>     map (\s -> ([s],[Symbol [s]])) "+-^*/%~&|?<>[]=,;()"
>     -- multi char symbols
>     ++ map (\s -> (s,[Symbol s])) [">=","<=","!=","<>","||"]
>     ++ (let idens = ["a", "_a", "test", "table", "Stuff", "STUFF"]
>         -- simple identifiers
>         in map (\i -> (i, [Identifier Nothing i])) idens
>            ++ map (\i -> ("\"" ++ i ++ "\"", [Identifier (Just ("\"","\"")) i])) idens
>            -- todo: in order to make lex . pretty id, need to
>            -- preserve the case of the u
>            ++ map (\i -> ("u&\"" ++ i ++ "\"", [Identifier (Just ("u&\"","\"")) i])) idens
>            -- host param
>            ++ map (\i -> (':':i, [HostParam i])) idens
>        )
>     -- quoted identifiers with embedded double quotes
>     -- the lexer doesn't unescape the quotes
>     ++ [("\"normal \"\" iden\"", [Identifier (Just ("\"","\"")) "normal \"\" iden"])]
>     -- strings
>     -- the lexer doesn't apply escapes at all
>     ++ [("'string'", [SqlString "'" "'" "string"])
>        ,("'normal '' quote'", [SqlString "'" "'" "normal '' quote"])
>        ,("'normalendquote '''", [SqlString "'" "'" "normalendquote ''"])]
>     -- csstrings
>     ++ map (\c -> (c ++ "'test'", [SqlString (c ++ "'") "'" "test"]))
>        ["n", "N","b", "B","x", "X", "u&"]
>     -- numbers
>     ++ [("10", [SqlNumber "10"])
>        ,(".1", [SqlNumber ".1"])
>        ,("5e3", [SqlNumber "5e3"])
>        ,("5e+3", [SqlNumber "5e+3"])
>        ,("5e-3", [SqlNumber "5e-3"])
>        ,("10.2", [SqlNumber "10.2"])
>        ,("10.2e7", [SqlNumber "10.2e7"])]
>     -- whitespace
>     ++ concat [[([a],[Whitespace [a]])
>                ,([a,b], [Whitespace [a,b]])]
>               | a <- " \n\t", b <- " \n\t"]
>     -- line comment
>     ++ map (\c -> (c, [LineComment c]))
>        ["--", "-- ", "-- this is a comment", "-- line com\n"]
>     -- block comment
>     ++ map (\c -> (c, [BlockComment c]))
>        ["/**/", "/* */","/* this is a comment */"
>        ,"/* this *is/ a comment */"
>        ]

> ansiLexerTests :: TestItem
> ansiLexerTests = Group "ansiLexerTests" $
>     [Group "ansi lexer token tests" $ [LexerTest ansi2011 s t |  (s,t) <- ansiLexerTable]
>     ,Group "ansi generated combination lexer tests" $
>     [ LexerTest ansi2011 (s ++ s1) (t ++ t1)
>     | (s,t) <- ansiLexerTable
>     , (s1,t1) <- ansiLexerTable

which combinations won't work:
<> <= >= || two single symbols which make a double char symbol
identifier + identifier if both are quoted or unquoted
string string
csstring string
line comment anything (can add newline?)
number number (todo: double check more carefully)

>     , isGood $ t ++ t1

>     ]
>     ,Group "adhoc lexer tests" $
>        map (uncurry $ LexerTest ansi2011)
>        [("", [])
>        ,("-- line com\nstuff", [LineComment "-- line com\n",Identifier Nothing "stuff"])
>        ]
>      ]

>  where
>    isGood :: [Token] -> Bool
>    isGood l = {-let b =-} and $ map not [p l | p <- map listPred badCombos]
>               -- in trace ("isGood " ++ show (l,b)) b
>    badCombos :: [((Token -> Bool),(Token -> Bool))]
>    badCombos = [symbolPair "<" ">"
>                ,symbolPair "<" "="
>                ,symbolPair ">" "="
>                ,symbolPair "!" "="
>                ,symbolPair "|" "|"
>                ,symbolPair "||" "|"
>                ,symbolPair "|" "||"
>                ,symbolPair "||" "||"
>                ,symbolPair "<" ">="

>                ,symbolPair "-" "-"
>                ,symbolPair "/" "*"
>                ,symbolPair "*" "/"

>                ,(isIdentifier, isIdentifier)
>                ,(isDQIdentifier, isDQIdentifier)
>                ,(isCQIdentifier, isDQIdentifier)
>                ,(isString, isNonCsString)
>                ,(isEofLineComment, const True)
>                ,(isNumber, isNumber)
>                ,(isHostParam,isIdentifier)
>                ,(isHostParam,isCsString)
>                ,(isHostParam,isCQIdentifier)
>                ,(isIdentifier,isCsString)
>                ,(isIdentifier,isCQIdentifier)
>                ,(isWhitespace, isWhitespace)
>                ,(isIdentifier, isNumber)
>                ,(isHostParam, isNumber)
>                ,(isMinus, isLineComment)
>                ]
>    isIdentifier (Identifier Nothing _) = True
>    isIdentifier _ = False
>    isDQIdentifier (Identifier (Just ("\"",_)) _) = True
>    isDQIdentifier _ = False
>    isCQIdentifier (Identifier (Just ((x:_),_)) _) | isAlpha x = True
>    isCQIdentifier _ = False
>    isCsString (SqlString (x:_) _ _) | isAlpha x = True
>    isCsString _ = False
>    isString (SqlString _ _ _) = True
>    isString _ = False
>    isNonCsString (SqlString [] _ _) = True
>    isNonCsString (SqlString (x:_) _ _) | not (isAlpha x) = True
>    isNonCsString _ = False
>    isEofLineComment (LineComment s) = last s /= '\n'
>    isEofLineComment _ = False
>    isLineComment (LineComment {}) = True
>    isLineComment _ = False
>    isNumber (SqlNumber{}) = True
>    isNumber _ = False
>    isHostParam (HostParam{}) = True
>    isHostParam _ = False
>    isWhitespace (Whitespace{}) = True
>    isWhitespace _ = False
>    isMinus (Symbol "-") = True
>    isMinus _ = False
>    symbolPair a b = ((==Symbol a), (==Symbol b))
>    listPred :: ((Token -> Bool),(Token -> Bool)) -> [Token] -> Bool
>    listPred _ [] = False
>    listPred _ [_] = False
>    listPred (p,p1) (t:t1:ts) | p t && p1 t1 = True
>                              | otherwise = listPred (p,p1) (t1:ts)

todo: lexing tests
do quickcheck testing:
can try to generate valid tokens then check they parse

same as above: can also try to pair tokens, create an accurate
  function to say which ones can appear adjacent, and test

I think this plus the explicit lists of tokens like above which do
basic sanity + explicit edge casts will provide a high level of
assurance.



> postgresLexerTable :: [(String,[Token])]
> postgresLexerTable =
>     -- single char symbols
>     map (\s -> ([s],[Symbol [s]])) "+-^*/%~&|?<>[]=,;():"
>     -- multi char symbols
>     ++ map (\s -> (s,[Symbol s])) [">=","<=","!=","<>","||", "::","..",":="]
>     -- todo: add many examples of generic symbols
>     -- also: do the testing for the ansi compatibility special cases
>     ++ (let idens = ["a", "_a", "test", "table", "Stuff", "STUFF"]
>         -- simple identifiers
>         in map (\i -> (i, [Identifier Nothing i])) idens
>            ++ map (\i -> ("\"" ++ i ++ "\"", [Identifier (Just ("\"","\"")) i])) idens
>            -- todo: in order to make lex . pretty id, need to
>            -- preserve the case of the u
>            ++ map (\i -> ("u&\"" ++ i ++ "\"", [Identifier (Just ("u&\"","\"")) i])) idens
>            -- host param
>            ++ map (\i -> (':':i, [HostParam i])) idens
>        )
>     -- positional var
>     ++ [("$1", [PositionalArg 1])]
>     -- quoted identifiers with embedded double quotes
>     ++ [("\"normal \"\" iden\"", [Identifier (Just ("\"","\"")) "normal \"\" iden"])]
>     -- strings
>     ++ [("'string'", [SqlString "'" "'" "string"])
>        ,("'normal '' quote'", [SqlString "'" "'" "normal '' quote"])
>        ,("'normalendquote '''", [SqlString "'" "'" "normalendquote ''"])
>        ,("e'this '' quote'", [SqlString "e'" "'" "this '' quote"])
>        ,("e'this \\' quote'", [SqlString "e'" "'" "this \\' quote"])
>         -- todo: implement only allowing \' in e quoted strings
>        {-,("'not this \\' quote", [SqlString "'" "'" "not this \\"
>                                  ,Whitespace " "
>                                  ,Identifier Nothing "quote"])-}
>        ]
>     -- csstrings
>     ++ map (\c -> (c ++ "'test'", [SqlString (c ++ "'") "'" "test"]))
>        ["n", "N","b", "B","x", "X", "u&", "e", "E"]
>     -- numbers
>     ++ [("10", [SqlNumber "10"])
>        ,(".1", [SqlNumber ".1"])
>        ,("5e3", [SqlNumber "5e3"])
>        ,("5e+3", [SqlNumber "5e+3"])
>        ,("5e-3", [SqlNumber "5e-3"])
>        ,("10.2", [SqlNumber "10.2"])
>        ,("10.2e7", [SqlNumber "10.2e7"])]
>     -- whitespace
>     ++ concat [[([a],[Whitespace [a]])
>                ,([a,b], [Whitespace [a,b]])]
>               | a <- " \n\t", b <- " \n\t"]
>     -- line comment
>     ++ map (\c -> (c, [LineComment c]))
>        ["--", "-- ", "-- this is a comment", "-- line com\n"]
>     -- block comment
>     ++ map (\c -> (c, [BlockComment c]))
>        ["/**/", "/* */","/* this is a comment */"
>        ,"/* this *is/ a comment */"
>        ]

> postgresLexerTests :: TestItem
> postgresLexerTests = Group "postgresLexerTests" $
>     [Group "postgres lexer token tests" $ [LexerTest postgres s t |  (s,t) <- postgresLexerTable]
>     ]
