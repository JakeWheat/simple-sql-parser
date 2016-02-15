
The parser uses a separate lexer for two reasons:

1. sql syntax is very awkward to parse, the separate lexer makes it
easier to handle this in most places (in some places it makes it
harder or impossible, the fix is to switch to something better than
parsec)

2. using a separate lexer gives a huge speed boost because it reduces
backtracking. (We could get this by making the parsing code a lot more
complex also.)

= Lexing and dialects

The main dialect differences:

symbols follow different rules in different dialects

e.g. postgresql has a flexible extensible-ready syntax for operators
which are parsed here as symbols

sql server using [] for quoting identifiers, and so they don't parse
as symbols here (in other dialects including ansi, these are used for
array operations)

quoting of identifiers is different in different dialects

there are various other identifier differences:
ansi has :host_param
there are variants on these like in @sql_server adn in #oracle

string quoting follows different rules in different dialects,
e.g. postgresql has $$ quoting

todo: public documentation on dialect definition - and dialect flags



> -- | This is the module contains a Lexer for SQL.
> {-# LANGUAGE TupleSections #-}
> module Language.SQL.SimpleSQL.Lex
>     (Token(..)
>     ,lexSQL
>     ,prettyToken
>     ,prettyTokens
>     ,ParseError(..)
>     ,Dialect(..)
>     ,tokensWillPrintAndLex
>     ,tokenListWillPrintAndLex
>     ) where

> import Language.SQL.SimpleSQL.Dialect

> import Text.Parsec (option,string,manyTill,anyChar
>                    ,try,string,many1,oneOf,digit,(<|>),choice,char,eof
>                    ,many,runParser,lookAhead,satisfy
>                    ,setPosition,getPosition
>                    ,setSourceColumn,setSourceLine
>                    ,sourceName, setSourceName
>                    ,sourceLine, sourceColumn
>                    ,notFollowedBy)
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
>     -- | A symbol (in ansi dialect) is one of the following
>     --
>     -- * multi char symbols <> <= >= != ||
>     -- * single char symbols: * + -  < >  ^ / %  ~ & | ? ( ) [ ] , ; ( )
>     --
>     = Symbol String
>
>     -- | This is an identifier or keyword. The first field is
>     -- the quotes used, or nothing if no quotes were used. The quotes
>     -- can be " or u& or something dialect specific like []
>     | Identifier (Maybe (String,String)) String
>
>     -- | This is a host param symbol, e.g. :param
>     | HostParam String
>
>     -- | This is a prefixed variable symbol, such as @var or #var (not used in ansi dialect)
>     | PrefixedVariable Char String
>
>     -- | This is a positional arg identifier e.g. $1
>     | PositionalArg Int
>
>     -- | This is a string literal. The first two fields are the --
>     -- start and end quotes, which are usually both ', but can be
>     -- the character set (one of nNbBxX, or u&, U&), or a dialect
>     -- specific string quoting (such as $$ in postgres)
>     | SqlString String String String
>
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
> prettyToken _ (Identifier Nothing t) = t
> prettyToken _ (Identifier (Just (q1,q2)) t) = q1 ++ t ++ q2
> prettyToken _ (HostParam p) = ':':p
> prettyToken _ (PrefixedVariable c p) = c:p
> prettyToken _ (PositionalArg p) = '$':show p
> prettyToken _ (SqlString s e t) = s ++ t ++ e
> prettyToken _ (SqlNumber r) = r
> prettyToken _ (Whitespace t) = t
> prettyToken _ (LineComment l) = l
> prettyToken _ (BlockComment c) = c

> prettyTokens :: Dialect -> [Token] -> String
> prettyTokens d ts = concat $ map (prettyToken d) ts

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
>                     ,positionalArg d
>                     ,dontParseEndBlockComment d
>                     ,prefixedVariable d
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
>     [Identifier (Just ("\"","\"")) <$> qiden
>      -- try is used here to avoid a conflict with identifiers
>      -- and quoted strings which also start with a 'u'
>     ,Identifier (Just ("u&\"","\"")) <$> (try (string "u&") *> qiden)
>     ,Identifier (Just ("U&\"","\"")) <$> (try (string "U&") *> qiden)
>     ,Identifier Nothing <$> identifierString
>      -- todo: dialect protection
>     ,guard (diSyntaxFlavour d == MySQL) >>
>      Identifier (Just ("`","`"))
>      <$> (char '`' *> takeWhile1 (/='`') <* char '`')
>     ,guard (diSyntaxFlavour d == SQLServer) >>
>      Identifier (Just ("[","]"))
>      <$> (char '[' *> takeWhile1 (`notElem` "[]") <* char ']')
>     ]
>   where
>     qiden = char '"' *> qidenSuffix ""
>     qidenSuffix t = do
>         s <- takeTill (=='"')
>         void $ char '"'
>         -- deal with "" as literal double quote character
>         choice [do
>                 void $ char '"'
>                 qidenSuffix $ concat [t,s,"\"\""]
>                ,return $ concat [t,s]]


This parses a valid identifier without quotes.

> identifierString :: Parser String
> identifierString =
>     startsWith (\c -> c == '_' || isAlpha c) isIdentifierChar

this can be moved to the dialect at some point

> isIdentifierChar :: Char -> Bool
> isIdentifierChar c = c == '_' || isAlphaNum c

Parse a SQL string. Examples:

'basic string'
'string with '' a quote'
n'international text'
b'binary string'
x'hexidecimal string'


> sqlString :: Dialect -> Parser Token
> sqlString d = dollarString <|> csString <|> normalString
>   where
>     dollarString = do
>         guard $ diSyntaxFlavour d == Postgres
>         -- use try because of ambiguity with symbols and with
>         -- positional arg
>         s <- choice
>              [do
>               i <- try (char '$' *> identifierString <* char '$')
>               return $ "$" ++ i ++ "$"
>              ,try (string "$$")
>              ]
>         str <- manyTill anyChar (try $ string s)
>         return $ SqlString s s str
>     normalString = SqlString "'" "'" <$> (char '\'' *> normalStringSuffix False "")
>     normalStringSuffix allowBackslash t = do
>         s <- takeTill $ if allowBackslash
>                         then (`elem` "'\\")
>                         else (== '\'')
>         -- deal with '' or \' as literal quote character
>         choice [do
>                 ctu <- choice ["''" <$ try (string "''")
>                               ,"\\'" <$ string "\\'"
>                               ,"\\" <$ char '\\']
>                 normalStringSuffix allowBackslash $ concat [t,s,ctu]
>                ,concat [t,s] <$ char '\'']
>     -- try is used to to avoid conflicts with
>     -- identifiers which can start with n,b,x,u
>     -- once we read the quote type and the starting '
>     -- then we commit to a string
>     -- it's possible that this will reject some valid syntax
>     -- but only pathalogical stuff, and I think the improved
>     -- error messages and user predictability make it a good
>     -- pragmatic choice
>     csString
>       | diSyntaxFlavour d == Postgres =
>         choice [SqlString <$> try (string "e'" <|> string "E'")
>                           <*> return "'" <*> normalStringSuffix True ""
>                ,csString']
>       | otherwise = csString'
>     csString' = SqlString
>                 <$> try cs
>                 <*> return "'"
>                 <*> normalStringSuffix False ""
>     csPrefixes = "nNbBxX"
>     cs = choice $ (map (\x -> string ([x] ++ "'")) csPrefixes)
>                   ++ [string "u&'"
>                      ,string "U&'"]

> hostParam :: Dialect -> Parser Token

use try for postgres because we also support : and :: as symbols
There might be a problem with parsing e.g. a[1:b]

> hostParam d | diSyntaxFlavour d == Postgres =
>     HostParam <$> try (char ':' *> identifierString)

> hostParam _ = HostParam <$> (char ':' *> identifierString)

> prefixedVariable :: Dialect -> Parser Token
> prefixedVariable  d | diSyntaxFlavour d == SQLServer =
>     PrefixedVariable <$> char '@' <*> identifierString
> prefixedVariable  d | diSyntaxFlavour d == Oracle =
>     PrefixedVariable <$> char '#' <*> identifierString
> prefixedVariable _ = guard False *> fail "unpossible"

> positionalArg :: Dialect -> Parser Token
> positionalArg d | diSyntaxFlavour d == Postgres =
>   -- use try to avoid ambiguities with other syntax which starts with dollar
>   PositionalArg <$> try (char '$' *> (read <$> many1 digit))
> positionalArg _ = guard False *> fail "unpossible"


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
> sqlNumber _ =
>     SqlNumber <$> completeNumber
>     -- this is for definitely avoiding possibly ambiguous source
>     <* notFollowedBy (oneOf "eE.")
>   where
>     completeNumber =
>       (int <??> (pp dot <??.> pp int)
>       -- try is used in case we read a dot
>       -- and it isn't part of a number
>       -- if there are any following digits, then we commit
>       -- to it being a number and not something else
>       <|> try ((++) <$> dot <*> int))
>       <??> pp expon

>     int = many1 digit
>     dot = string "."
>     expon = (:) <$> oneOf "eE" <*> sInt
>     sInt = (++) <$> option "" (string "+" <|> string "-") <*> int
>     pp = (<$$> (++))


A symbol is one of the two character symbols, or one of the single
character symbols in the two lists below.

> symbol :: Dialect -> Parser Token
> symbol d | diSyntaxFlavour d == Postgres =
>     Symbol <$> choice (otherSymbol ++ [singlePlusMinus,opMoreChars])

rules

An operator name is a sequence of up to NAMEDATALEN-1 (63 by default) characters from the following list:

+ - * / < > = ~ ! @ # % ^ & | ` ?

There are a few restrictions on operator names, however:
-- and /* cannot appear anywhere in an operator name, since they will be taken as the start of a comment.

A multiple-character operator name cannot end in + or -, unless the name also contains at least one of these characters:

~ ! @ # % ^ & | ` ?

>  where
>    -- other symbols are all the tokens which parse as symbols in
>    -- this lexer which aren't considered operators in postgresql
>    -- a single ? is parsed as a operator here instead of an other
>    -- symbol because this is the least complex way to do it
>    otherSymbol = many1 (char '.') :
>                  (map (try . string) ["::", ":="]
>                   ++ map (string . (:[])) "[],;():"
>                   ++ if allowOdbc d
>                      then [string "{", string "}"]
>                      else []
>                  )

exception char is one of:
~ ! @ # % ^ & | ` ?
which allows the last character of a multi character symbol to be + or
-

>    allOpSymbols = "+-*/<>=~!@#%^&|`?"
>    -- these are the symbols when if part of a multi character
>    -- operator permit the operator to end with a + or - symbol
>    exceptionOpSymbols = "~!@#%^&|`?"

>    -- special case for parsing a single + or - symbol
>    singlePlusMinus = try $ do
>      c <- oneOf "+-"
>      notFollowedBy $ oneOf allOpSymbols
>      return [c]

>    -- this is used when we are parsing a potentially multi symbol
>    -- operator and we have alread seen one of the 'exception chars'
>    -- and so we can end with a + or -
>    moreOpCharsException = do
>        c <- oneOf (filter (`notElem` "-/*") allOpSymbols)
>             -- make sure we don't parse a comment starting token
>             -- as part of an operator
>             <|> try (char '/' <* notFollowedBy (char '*'))
>             <|> try (char '-' <* notFollowedBy (char '-'))
>             -- and make sure we don't parse a block comment end
>             -- as part of another symbol
>             <|> try (char '*' <* notFollowedBy (char '/'))
>        (c:) <$> option [] moreOpCharsException

>    opMoreChars = choice
>        [-- parse an exception char, now we can finish with a + -
>         (:)
>         <$> oneOf exceptionOpSymbols
>         <*> option [] moreOpCharsException
>        ,(:)
>         <$> (-- parse +, make sure it isn't the last symbol
>              try (char '+' <* lookAhead (oneOf allOpSymbols))
>              <|> -- parse -, make sure it isn't the last symbol
>                  -- or the start of a -- comment
>              try (char '-'
>                   <* notFollowedBy (char '-')
>                   <* lookAhead (oneOf allOpSymbols))
>              <|> -- parse / check it isn't the start of a /* comment
>              try (char '/' <* notFollowedBy (char '*'))
>              <|> -- make sure we don't parse */ as part of a symbol
>              try (char '*' <* notFollowedBy (char '/'))
>              <|> -- any other ansi operator symbol
>              oneOf "<>=")
>         <*> option [] opMoreChars
>        ]

> symbol d | diSyntaxFlavour d == SQLServer =
>    Symbol <$> choice (otherSymbol ++ regularOp)
>  where
>    otherSymbol = many1 (char '.') :
>                  (map (string . (:[])) ",;():?"
>                   ++ if allowOdbc d
>                      then [string "{", string "}"]
>                      else [])

try is used because most of the first characters of the two character
symbols can also be part of a single character symbol

>    regularOp = map (try . string) [">=","<=","!=","<>"]
>                ++ map (string . (:[])) "+-^*/%~&<>="
>                ++ [char '|' *>
>                    choice ["||" <$ char '|' <* notFollowedBy (char '|')
>                           ,return "|"]]

> symbol d =
>    Symbol <$> choice (otherSymbol ++ regularOp)
>  where
>    otherSymbol = many1 (char '.') :
>                  (map (string . (:[])) "[],;():?"
>                   ++ if allowOdbc d
>                      then [string "{", string "}"]
>                      else [])

try is used because most of the first characters of the two character
symbols can also be part of a single character symbol

>    regularOp = map (try . string) [">=","<=","!=","<>"]
>                ++ map (string . (:[])) "+-^*/%~&<>=[]"
>                ++ [char '|' *>
>                    choice ["||" <$ char '|' <* notFollowedBy (char '|')
>                           ,return "|"]]



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


This is to improve user experience: provide an error if we see */
outside a comment. This could potentially break postgres ops with */
in (which is a stupid thing to do). In other cases, the user should
write * / instead (I can't think of any cases when this would be valid
syntax though).

> dontParseEndBlockComment :: Dialect -> Parser Token
> dontParseEndBlockComment _ =
>     -- don't use try, then it should commit to the error
>     try (string "*/") *> fail "comment end without comment start"


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
> takeTill p = manyTill anyChar (peekSatisfy p)

> peekSatisfy :: (Char -> Bool) -> Parser ()
> peekSatisfy p = void $ lookAhead (satisfy p)

This utility function will accurately report if the two tokens are
pretty printed, if they should lex back to the same two tokens. This
function is used in testing (and can be used in other places), and
must not be implemented by actually trying to print and then lex
(because then we would have the risk of thinking two tokens cannot be
together when there is bug in the lexer and it should be possible to
put them together.

question: maybe pretty printing the tokens separately and then
analysing the concrete syntax without concatting the two printed
tokens together is a better way of doing this?

maybe do some quick checking to make sure this function only gives
true negatives: check pairs which return false actually fail to lex or
give different symbols in return

a good sanity test for this function is to change it to always return
true, then check that the automated tests return the same number of
successes.

> tokenListWillPrintAndLex :: Dialect -> [Token] -> Bool
> tokenListWillPrintAndLex _ [] = True
> tokenListWillPrintAndLex _ [_] = True
> tokenListWillPrintAndLex d (a:b:xs) =
>     tokensWillPrintAndLex d a b && tokenListWillPrintAndLex d (b:xs)

> tokensWillPrintAndLex :: Dialect -> Token -> Token -> Bool
> tokensWillPrintAndLex d a b

a : followed by an identifier character will look like a host param
followed by = or : makes a different symbol

>     | Symbol ":" <- a
>     , checkFirstBChar (\x -> isIdentifierChar x || x `elem` ":=") = False

two symbols next to eachother will fail if the symbols can combine and
(possibly just the prefix) look like a different symbol

>     | Dialect {diSyntaxFlavour = Postgres} <- d
>     , Symbol a' <- a
>     , Symbol b' <- b
>     , b' `notElem` ["+", "-"] || or (map (`elem` a') "~!@#%^&|`?") = False

check two adjacent symbols in non postgres where the combination
possibilities are much more limited. This is ansi behaviour, it might
be different when the other dialects are done properly

>    | Symbol a' <- a
>    , Symbol b' <- b
>    , (a',b') `elem` [("<",">")
>                     ,("<","=")
>                     ,(">","=")
>                     ,("!","=")
>                     ,("|","|")
>                     ,("||","|")
>                     ,("|","||")
>                     ,("||","||")
>                     ,("<",">=")
>                     ] = False

two whitespaces will be combined

>    | Whitespace {} <- a
>    , Whitespace {} <- b = False

line comment without a newline at the end will eat the next token

>    | LineComment {} <- a
>    , checkLastAChar (/='\n') = False

check the last character of the first token and the first character of
the second token forming a comment start or end symbol

>    | let f '-' '-' = True
>          f '/' '*' = True
>          f '*' '/' = True
>          f _ _ = False
>      in checkBorderChars f = False

a symbol will absorb a following .
TODO: not 100% on this always being bad

>    |  Symbol {} <- a
>    , checkFirstBChar (=='.') = False

unquoted identifier followed by an identifier letter

>    | Identifier Nothing _ <- a
>    , checkFirstBChar isIdentifierChar = False

a quoted identifier using ", followed by a " will fail

>    | Identifier (Just (_,"\"")) _ <- a
>    , checkFirstBChar (=='"') = False

host param followed by an identifier char will be absorbed

>    | HostParam {} <- a
>    , checkFirstBChar isIdentifierChar = False

prefixed variable same:

>    | PrefixedVariable {} <- a
>    , checkFirstBChar isIdentifierChar = False

a positional arg will absorb a following digit

>    | PositionalArg {} <- a
>    , checkFirstBChar isDigit = False

a string ending with ' followed by a token starting with ' will be absorbed

>    | SqlString _ "'" _ <- a
>    , checkFirstBChar (=='\'') = False

a number followed by a . will fail or be absorbed

>    | SqlNumber {} <- a
>    , checkFirstBChar (=='.') = False

a number followed by an e or E will fail or be absorbed

>    | SqlNumber {} <- a
>    , checkFirstBChar (\x -> x =='e' || x == 'E') = False

two numbers next to eachother will fail or be absorbed

>    | SqlNumber {} <- a
>    , SqlNumber {} <- b = False
>
>    | otherwise = True


>   where
>     prettya = prettyToken d a
>     prettyb = prettyToken d b
>     -- helper function to run a predicate on the
>     -- last character of the first token and the first
>     -- character of the second token
>     checkBorderChars f
>         | (_:_) <- prettya
>         , (fb:_) <- prettyb
>         , la <- last prettya
>         = f la fb
>     checkBorderChars _ = False
>     checkFirstBChar f = case prettyb of
>                           (b':_) -> f b'
>                           _ -> False
>     checkLastAChar f = case prettya of
>                           (_:_) -> f $ last prettya
>                           _ -> False




TODO:

make the tokenswill print more dialect accurate. Maybe add symbol
  chars and identifier chars to the dialect definition and use them from
  here

start adding negative / different parse dialect tests

add token tables and tests for oracle, sql server
review existing tables

look for refactoring opportunities, especially the token
generation tables in the tests

do some user documentation on lexing, and lexing/dialects

start thinking about a more separated design for the dialect handling

make sure other symbols repeated are protected like | || where neccessary
     such as :
