

Test for the lexer

> module Language.SQL.SimpleSQL.LexerTests (lexerTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Lex (Token(..))
> --import Debug.Trace
> import Data.Char (isAlpha)

> ansiLexerTable :: [(String,[Token])]
> ansiLexerTable =
>     -- single char symbols
>     map (\s -> ([s],[Symbol [s]])) "+-^*/%~&|?<>[]=,;()"
>     -- multi char symbols
>     ++ map (\s -> (s,[Symbol s])) [">=","<=","!=","<>","||"]
>     ++ (let idens = ["a", "_a", "test", "table", "Stuff", "STUFF"]
>         -- simple identifiers
>         in map (\i -> (i, [Identifier i])) idens
>            ++ map (\i -> ("\"" ++ i ++ "\"", [QuotedIdentifier "\"" "\"" i])) idens
>            -- todo: in order to make lex . pretty id, need to
>            -- preserve the case of the u
>            ++ map (\i -> ("u&\"" ++ i ++ "\"", [QuotedIdentifier "u&\"" "\"" i])) idens
>            -- host param
>            ++ map (\i -> (':':i, [HostParam i])) idens
>        )
>     -- quoted identifiers with embedded double quotes
>     ++ [("\"normal \"\" iden\"", [QuotedIdentifier "\"" "\"" "normal \" iden"])]
>     -- strings
>     ++ [("'string'", [SqlString "'" "'" "string"])
>        ,("'normal '' quote'", [SqlString "'" "'" "normal ' quote"])
>        ,("'normalendquote '''", [SqlString "'" "'" "normalendquote '"])]
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

> lexerTests :: TestItem
> lexerTests = Group "lexerTests" $
>     [Group "lexer token tests" [ansiLexerTests]]


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
>        ,("-- line com\nstuff", [LineComment "-- line com\n",Identifier "stuff"])
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
>    isIdentifier (Identifier _) = True
>    isIdentifier _ = False
>    isDQIdentifier (QuotedIdentifier "\"" _ _) = True
>    isDQIdentifier _ = False
>    isCQIdentifier (QuotedIdentifier (x:_) _ _) | isAlpha x = True
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
