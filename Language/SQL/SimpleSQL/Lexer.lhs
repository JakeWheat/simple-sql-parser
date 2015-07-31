
Lexer TODO:

left factor to get rid of trys

> -- | This is the module contains a Lexer for SQL.
> {-# LANGUAGE TupleSections #-}
> module Language.SQL.SimpleSQL.Lexer
>     (lexSQL
>     ,Token(..)
>     ,prettyToken
>     ,prettyTokens
>     ,Position
>     ,ParseError(..)
>     ,Dialect(..)) where

> import Language.SQL.SimpleSQL.Syntax (Dialect(..))

> import Text.Parsec (option,string,manyTill,anyChar
>                    ,try,string,many1,oneOf,digit,(<|>),choice,char,eof
>                    ,many,runParser,lookAhead,satisfy)
> import Language.SQL.SimpleSQL.Combinators
> import Language.SQL.SimpleSQL.Errors
> import Control.Applicative hiding ((<|>), many)
> import Data.Char
> import Control.Monad
> import Prelude hiding (takeWhile)
> import Text.Parsec.String (Parser)


> -- | Represents a lexed token
> data Token
>     -- | a symbol is one of the following
>     -- * multi char symbols <> <= >= != ||
>     -- * single char symbols: * + -  < >  ^ / %  ~ & | ? ( ) [ ] , ; ( )
>     --
>     = Symbol String
>
>     -- | This is an identifier or keyword.
>     --
>     | Identifier String
>
>     -- | This is an identifier quoted with "
>     | QIdentifier String
>     -- | This is an identifier quoted with u&"
>     | UQIdentifier String

>     -- | This is a dialect specific quoted identifier with the quote
>     -- characters explicit. The first and second fields are the
>     -- starting and ending quote characters.n
>     | DQIdentifier String String String
>
>     -- | This is a host param symbol, e.g. :param
>     | HostParam String
>
>     -- | This is a string literal.
>     | SqlString String
>
>     -- | This is a character set string literal. The first field is
>     -- the charatecter set (one of nNbBxX).
>     | CSSqlString String String
>
>     -- | a number literal (integral or otherwise), stored in original format
>     -- unchanged
>     | SqlNumber String
>
>     -- | non-significant whitespace (space, tab, newline) (strictly speaking,
>     -- it is up to the client to decide whether the whitespace is significant
>     -- or not)
>     | Whitespace String
>
>     -- | a commented line using --, contains every character starting with the
>     -- \'--\' and including the terminating newline character if there is one
>     -- - this will be missing if the last line in the source is a line comment
>     -- with no trailing newline
>     | LineComment String
>
>     -- | a block comment, \/* stuff *\/, includes the comment delimiters
>     | BlockComment String
>
>       deriving (Eq,Show)



> -- | Accurate pretty printing, if you lex a bunch of tokens,
> -- then pretty print them, should should get back exactly the
> -- same string
> prettyToken :: Dialect -> Token -> String
> prettyToken _ (Symbol s) = s
> prettyToken _ (Identifier t) = t
> prettyToken _ (QIdentifier t) =
>     "\"" ++ doubleChars '"' t ++ "\""
> prettyToken _ (UQIdentifier t) =
>     "u&\"" ++ doubleChars '"' t ++ "\""
> prettyToken _ (DQIdentifier s e t) =
>     s ++ t ++ e
> prettyToken _ (HostParam p) = ':':p
> prettyToken _ (SqlString t) = "'" ++ doubleChars '\'' t ++ "'"
> prettyToken _ (CSSqlString cs t) = cs ++ "'" ++ t ++ "'"
> prettyToken _ (SqlNumber r) = r
> prettyToken _ (Whitespace t) = t
> prettyToken _ (LineComment l) = l
> prettyToken _ (BlockComment c) = c

> prettyTokens :: Dialect -> [Token] -> String
> prettyTokens d ts = concat $ map (prettyToken d) ts

> doubleChars :: Char -> String -> String
> doubleChars _ [] = []
> doubleChars c (d:ds) | c == d = c:d:doubleChars c ds
>                      | otherwise = d:doubleChars c ds

TODO: try to make all parsers applicative only

> type Position = (String,Int,Int)

> addPosition :: Position -> String -> Position
> addPosition = addPosition'

> addPosition' :: Position -> String -> Position
> addPosition' (f,l,c) [] = (f,l,c)
> addPosition' (f,l,_) ('\n':xs) = addPosition' (f,l+1,0) xs
> addPosition' (f,l,c) (_:xs) = addPosition' (f,l,c+1) xs



> lexSQL :: Dialect -> Position -> String -> Either ParseError [(Position,Token)]
> lexSQL dialect pos@(fn,_,_) txt =
>     either (Left . convParseError fn) Right
>     $ runParser (many_p pos <* eof) () "" txt
>   where

>      many_p pos' = some_p pos' `mplus` return []
>      some_p pos' = do
>        tok <- sqlToken dialect pos'
>        let pos'' = advancePos dialect pos' (snd tok)
>        (tok:) <$> many_p pos''

> advancePos :: Dialect -> Position -> Token -> Position
> advancePos dialect pos tok =
>     let pt = prettyToken dialect tok
>     in addPosition pos pt

> -- | parser for a sql token
> sqlToken :: Dialect -> Position -> Parser (Position,Token)
> sqlToken d p =
>     (p,) <$> choice [sqlString d
>                     ,identifier d
>                     ,hostParam d
>                     ,lineComment d
>                     ,blockComment d
>                     ,sqlNumber d
>                     ,symbol d
>                     ,sqlWhitespace d]

> takeWhile1 :: (Char -> Bool) -> Parser String
> takeWhile1 p = many1 (satisfy p)

> takeWhile :: (Char -> Bool) -> Parser String
> takeWhile p = many (satisfy p)

> takeTill :: (Char -> Bool) -> Parser String
> takeTill p =
>     try (manyTill anyChar (peekSatisfy p))

> peekSatisfy :: (Char -> Bool) -> Parser ()
> peekSatisfy p = do
>     void $ try $ lookAhead (satisfy p)

> identifier :: Dialect -> Parser Token
> identifier d =
>     choice
>     [QIdentifier <$> qiden
>     ,UQIdentifier <$> ((try (string "u&" <|> string "U&")) *> qiden)
>     ,Identifier <$> identifierString
>     ,DQIdentifier "`" "`" <$> mySqlQIden
>     ]
>   where
>     qiden = char '"' *> qidenSuffix ""
>     qidenSuffix t = do
>         s <- takeTill (=='"')
>         void $ char '"'
>         -- deal with "" as literal double quote character
>         choice [do
>                 void $ char '"'
>                 qidenSuffix $ concat [t,s,"\""]
>                ,return $ concat [t,s]]
>     -- mysql can quote identifiers with `
>     mySqlQIden = do
>         guard (d == MySQL)
>         char '`' *> takeWhile1 (/='`') <* char '`'

> identifierString :: Parser String
> identifierString =
>     startsWith (\c -> c == '_' || isAlpha c)
>                (\c -> c == '_' || isAlphaNum c)


> sqlString :: Dialect -> Parser Token
> sqlString _ =
>     choice [csString
>            ,normalString
>            ]
>   where
>     normalString = SqlString {-"'"-} <$> (char '\'' *> normalStringSuffix "")
>     normalStringSuffix t = do
>         s <- takeTill (=='\'')
>         void $ char '\''
>         -- deal with '' as literal quote character
>         choice [do
>                 void $ char '\''
>                 normalStringSuffix $ concat [t,s,"'"]
>                ,return $ concat [t,s]]
>     csString = CSSqlString <$> try (cs  <* char '\'') <*> normalStringSuffix ""
>     cs = choice [(:[]) <$> oneOf "nNbBxX"
>                 ,string "u&"
>                 ,string "U&"]

> hostParam :: Dialect -> Parser Token
> hostParam _ = HostParam <$> (char ':' *> identifierString)


> sqlNumber :: Dialect -> Parser Token
> sqlNumber _ = SqlNumber <$>
>     (int <??> (pp dot <??.> pp int)
>      <|> try ((++) <$> dot <*> int))
>     <??> pp expon
>   where
>     int = many1 digit
>     dot = string "."
>     expon = (:) <$> oneOf "eE" <*> sInt
>     sInt = (++) <$> option "" (string "+" <|> string "-") <*> int
>     pp = (<$$> (++))

digits
digits.[digits][e[+-]digits]
[digits].digits[e[+-]digits]
digitse[+-]digits

where digits is one or more decimal digits (0 through 9). At least one
digit must be before or after the decimal point, if one is used. At
least one digit must follow the exponent marker (e), if one is
present. There cannot be any spaces or other characters embedded in
the constant. Note that any leading plus or minus sign is not actually
considered part of the constant; it is an operator applied to the
constant.


A symbol is one of the two character symbols, or one of the single
character symbols in the two lists below.

> symbol :: Dialect -> Parser Token
> symbol _ = Symbol <$> choice (many1 (char '.') :
>                  map (try . string) [">=","<=","!=","<>","||"]
>                  ++ map (string . (:[])) "+-^*/%~&|?<>[]=,;()")

> sqlWhitespace :: Dialect -> Parser Token
> sqlWhitespace _ = Whitespace <$> many1 (satisfy isSpace)

> lineComment :: Dialect -> Parser Token
> lineComment _ =
>     (\s -> LineComment $ concat ["--",s]) <$>
>     (try (string "--") *> choice
>                     [flip snoc '\n' <$> takeTill (=='\n') <* char '\n'
>                     ,takeWhile (/='\n') <* eof
>                     ])
>   where
>     snoc :: String -> Char -> String
>     snoc s a = s ++ [a]

> blockComment :: Dialect -> Parser Token
> blockComment _ =
>     (\s -> BlockComment $ concat ["/*",s]) <$>
>     (try (string "/*") *> commentSuffix 0)
>   where
>     commentSuffix :: Int -> Parser String
>     commentSuffix n = do
>       -- read until a possible end comment or nested comment
>       x <- takeWhile (\e -> e /= '/' && e /= '*')
>       choice [-- close comment: if the nesting is 0, done
>               -- otherwise recurse on commentSuffix
>               try (string "*/") *> let t = concat [x,"*/"]
>                                    in if n == 0
>                                       then return t
>                                       else (\s -> concat [t,s]) <$> commentSuffix (n - 1)
>               -- nested comment, recurse
>              ,try (string "/*") *> ((\s -> concat [x,"/*",s]) <$> commentSuffix (n + 1))
>               -- not an end comment or nested comment, continue
>              ,(:) <$> anyChar <*> commentSuffix n]


> startsWith :: (Char -> Bool) -> (Char -> Bool) -> Parser String
> startsWith p ps = do
>   c <- satisfy p
>   choice [(:) c <$> (takeWhile1 ps)
>          ,return [c]]
