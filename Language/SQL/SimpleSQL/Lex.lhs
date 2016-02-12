
The parser uses a separate lexer for two reasons:

1. sql syntax is very awkward to parse, the separate lexer makes it
easier to handle this in most places (in some places it makes it
harder or impossible, the fix is to switch to something better than
parsec

2. using a separate lexer gives a huge speed boost

> -- | This is the module contains a Lexer for SQL.
> {-# LANGUAGE TupleSections #-}
> module Language.SQL.SimpleSQL.Lex
>     (Token(..)
>     ,lexSQL
>     ,prettyToken
>     ,prettyTokens
>     ,ParseError(..)
>     ,Dialect(..)) where

> import Language.SQL.SimpleSQL.Dialect

> import Text.Parsec (option,string,manyTill,anyChar
>                    ,try,string,many1,oneOf,digit,(<|>),choice,char,eof
>                    ,many,runParser,lookAhead,satisfy
>                    ,setPosition,getPosition
>                    ,setSourceColumn,setSourceLine
>                    ,sourceName, setSourceName
>                    ,sourceLine, sourceColumn)
> import Language.SQL.SimpleSQL.Combinators
> import Language.SQL.SimpleSQL.Errors
> import Control.Applicative hiding ((<|>), many)
> import Data.Char
> import Control.Monad
> import Prelude hiding (takeWhile)
> import Text.Parsec.String (Parser)
> import Data.Maybe


> -- | Represents a lexed token
> data Token
>     -- | A symbol is one of the following
>     --
>     -- * multi char symbols <> <= >= != ||
>     -- * single char symbols: * + -  < >  ^ / %  ~ & | ? ( ) [ ] , ; ( )
>     --
>     = Symbol String
>
>     -- | This is an identifier or keyword.
>     --
>     | Identifier String
>
>     -- | This is a quoted identifier, the quotes can be " or u&,
>     -- etc. or something dialect specific like []
>     -- the first two fields are the start and end quotes
>     | QuotedIdentifier String -- start quote
>                        String -- end quote
>                        String -- content
>     -- | This is a host param symbol, e.g. :param
>     | HostParam String
>
>     -- | This is a string literal. The first two fields are the --
>     -- start and end quotes, which are usually both ', but can be
>     -- the character set (one of nNbBxX, or u&, U&), or a dialect
>     -- specific string quoting (such as $$ in postgres)
>     | SqlString String String String
>     -- | A number literal (integral or otherwise), stored in original format
>     -- unchanged
>     | SqlNumber String
>
>     -- | Whitespace, one or more of space, tab or newline.
>     | Whitespace String
>
>     -- | A commented line using --, contains every character starting with the
>     -- \'--\' and including the terminating newline character if there is one
>     -- - this will be missing if the last line in the source is a line comment
>     -- with no trailing newline
>     | LineComment String
>
>     -- | A block comment, \/* stuff *\/, includes the comment delimiters
>     | BlockComment String
>
>       deriving (Eq,Show)



> -- | Pretty printing, if you lex a bunch of tokens, then pretty
> -- print them, should should get back exactly the same string
> prettyToken :: Dialect -> Token -> String
> prettyToken _ (Symbol s) = s
> prettyToken _ (Identifier t) = t
> prettyToken _ (QuotedIdentifier q1 q2 t) =
>     q1 ++
>     -- todo: a bit hacky, do a better design
>     (if '"' `elem` q1 then doubleChars '"' t else t)
>     ++ q2
> --prettyToken _ (UQIdentifier t) =
> --    "u&\"" ++ doubleChars '"' t ++ "\""
> --prettyToken _ (DQIdentifier s e t) =
> --    s ++ t ++ e
> prettyToken _ (HostParam p) = ':':p
> prettyToken _ (SqlString s e t) =
>     s ++ (if '\'' `elem` s then doubleChars '\'' t else t) ++ e
> --prettyToken _ (CSSqlString cs t) = cs ++ "'" ++ t ++ "'"
> prettyToken _ (SqlNumber r) = r
> prettyToken _ (Whitespace t) = t
> prettyToken _ (LineComment l) = l
> prettyToken _ (BlockComment c) = c

> prettyTokens :: Dialect -> [Token] -> String
> prettyTokens d ts = concat $ map (prettyToken d) ts

When parsing a quoted identifier, you can have a double quote
character in the identifier like this: "quotes""identifier" ->
quoted"identifer. The double double quotes character is changed to a
single character in the lexer and expanded back to two characters in
the pretty printer. This also applies to strings, which can embed a
single quote like this: 'string''with quote'.

> doubleChars :: Char -> String -> String
> doubleChars _ [] = []
> doubleChars c (d:ds) | c == d = c:d:doubleChars c ds
>                      | otherwise = d:doubleChars c ds

TODO: try to make all parsers applicative only

> -- | Lex some SQL to a list of tokens.
> lexSQL :: Dialect
>                   -- ^ dialect of SQL to use
>                -> FilePath
>                   -- ^ filename to use in error messages
>                -> Maybe (Int,Int)
>                   -- ^ line number and column number of the first character
>                   -- in the source to use in error messages
>                -> String
>                   -- ^ the SQL source to lex
>                -> Either ParseError [((String,Int,Int),Token)]
> lexSQL dialect fn' p src =
>     let (l',c') = fromMaybe (1,1) p
>     in either (Left . convParseError src) Right
>        $ runParser (setPos (fn',l',c') *> many (sqlToken dialect) <* eof) () fn' src
>   where
>     setPos (fn,l,c) = do
>         fmap (flip setSourceName fn
>                . flip setSourceLine l
>                . flip setSourceColumn c) getPosition
>           >>= setPosition

> -- | parser for a sql token
> sqlToken :: Dialect -> Parser ((String,Int,Int),Token)
> sqlToken d = do
>     p' <- getPosition
>     let p = (sourceName p',sourceLine p', sourceColumn p')

The order of parsers is important: strings and quoted identifiers can
start out looking like normal identifiers, so we try to parse these
first and use a little bit of try. Line and block comments start like
symbols, so we try these before symbol. Numbers can start with a . so
this is also tried before symbol (a .1 will be parsed as a number, but
. otherwise will be parsed as a symbol).

>     (p,) <$> choice [sqlString d
>                     ,identifier d
>                     ,hostParam d
>                     ,lineComment d
>                     ,blockComment d
>                     ,sqlNumber d
>                     ,symbol d
>                     ,sqlWhitespace d]

Parses identifiers:

simple_identifier_23
u&"unicode quoted identifier"
"quoted identifier"
"quoted identifier "" with double quote char"
`mysql quoted identifier`

> identifier :: Dialect -> Parser Token
> identifier d =
>     choice
>     [QuotedIdentifier "\"" "\"" <$> qiden
>      -- try is used here to avoid a conflict with identifiers
>      -- and quoted strings which also start with a 'u'
>     ,QuotedIdentifier "u&\"" "\"" <$> (try (string "u&") *> qiden)
>     ,QuotedIdentifier "U&\"" "\"" <$> (try (string "U&") *> qiden)
>     ,Identifier <$> identifierString
>      -- todo: dialect protection
>     ,QuotedIdentifier "`" "`" <$> mySqlQIden
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
>         guard (diSyntaxFlavour d == MySQL)
>         char '`' *> takeWhile1 (/='`') <* char '`'

This parses a valid identifier without quotes.

> identifierString :: Parser String
> identifierString =
>     startsWith (\c -> c == '_' || isAlpha c)
>                (\c -> c == '_' || isAlphaNum c)


Parse a SQL string. Examples:

'basic string'
'string with '' a quote'
n'international text'
b'binary string'
x'hexidecimal string'


> sqlString :: Dialect -> Parser Token
> sqlString _ =
>     choice [csString
>            ,normalString
>            ]
>   where
>     normalString = SqlString "'" "'" <$> (char '\'' *> normalStringSuffix "")
>     normalStringSuffix t = do
>         s <- takeTill (=='\'')
>         void $ char '\''
>         -- deal with '' as literal quote character
>         choice [do
>                 void $ char '\''
>                 normalStringSuffix $ concat [t,s,"'"]
>                ,return $ concat [t,s]]
>     -- try is used to to avoid conflicts with
>     -- identifiers which can start with n,b,x,u
>     -- once we read the quote type and the starting '
>     -- then we commit to a string
>     csString = SqlString <$> try cs <*> return "'" <*> normalStringSuffix ""
>     cs = choice $ (map (\x -> string ([x] ++ "'")) "nNbBxX")
>                   ++ [string "u&'"
>                      ,string "U&'"]

> hostParam :: Dialect -> Parser Token
> hostParam _ = HostParam <$> (char ':' *> identifierString)



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

> sqlNumber :: Dialect -> Parser Token
> sqlNumber _ = SqlNumber <$>
>     (int <??> (pp dot <??.> pp int)
>      -- try is used in case we read a dot
>      -- and it isn't part of a number
>      -- if there are any following digits, then we commit
>      -- to it being a number and not something else
>      <|> try ((++) <$> dot <*> int))
>     <??> pp expon
>   where
>     int = many1 digit
>     dot = string "."
>     expon = (:) <$> oneOf "eE" <*> sInt
>     sInt = (++) <$> option "" (string "+" <|> string "-") <*> int
>     pp = (<$$> (++))


A symbol is one of the two character symbols, or one of the single
character symbols in the two lists below.

> symbol :: Dialect -> Parser Token
> symbol _ = Symbol <$>
>     choice (
>             many1 (char '.') :
>                  -- try is used because most of the first
>                  -- characters of the two character symbols
>                  -- can also be part of a single character symbol
>                  -- maybe this would be better with left factoring?
>                 map (try . string) [">=","<=","!=","<>","||"]
>                 ++ map (string . (:[])) "+-^*/%~&|?<>[]=,;()")

> sqlWhitespace :: Dialect -> Parser Token
> sqlWhitespace _ = Whitespace <$> many1 (satisfy isSpace)

> lineComment :: Dialect -> Parser Token
> lineComment _ =
>     (\s -> LineComment $ concat ["--",s]) <$>
>     -- try is used here in case we see a - symbol
>     -- once we read two -- then we commit to the comment token
>     (try (string "--") *> (
>         -- todo: there must be a better way to do this
>      conc <$> manyTill anyChar (lookAhead lineCommentEnd) <*> lineCommentEnd))
>   where
>     conc a Nothing = a
>     conc a (Just b) = a ++ b
>     lineCommentEnd =
>         Just "\n" <$ char '\n'
>         <|> Nothing <$ eof

Try is used in the block comment for the two symbol bits because we
want to backtrack if we read the first symbol but the second symbol
isn't there.

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
>              ,(\c s -> x ++ [c] ++ s) <$> anyChar <*> commentSuffix n]


Some helper combinators

> startsWith :: (Char -> Bool) -> (Char -> Bool) -> Parser String
> startsWith p ps = do
>   c <- satisfy p
>   choice [(:) c <$> (takeWhile1 ps)
>          ,return [c]]

> takeWhile1 :: (Char -> Bool) -> Parser String
> takeWhile1 p = many1 (satisfy p)

> takeWhile :: (Char -> Bool) -> Parser String
> takeWhile p = many (satisfy p)

> takeTill :: (Char -> Bool) -> Parser String
> takeTill p =
>     manyTill anyChar (peekSatisfy p)

> peekSatisfy :: (Char -> Bool) -> Parser ()
> peekSatisfy p = do
>     void $ lookAhead (satisfy p)
