
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
>     ,Dialect(..)) where

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
>     -- | A symbol is one of the following
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

>     -- | This is a host param symbol, e.g. :param
>     | HostParam String


>     -- | This is a positional arg identifier e.g. $1
>     | PositionalArg Int
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
> prettyToken _ (Identifier Nothing t) = t
> prettyToken _ (Identifier (Just (q1,q2)) t) = q1 ++ t ++ q2
> prettyToken _ (HostParam p) = ':':p
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
>     ,Identifier (Just ("`","`")) <$> mySqlQIden
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

> positionalArg :: Dialect -> Parser Token
> positionalArg d | diSyntaxFlavour d == Postgres =
>   -- use try to avoid ambiguities with other syntax which starts with dollar
>   PositionalArg <$> try (char '$' *> (read <$> many1 digit))
> positionalArg _ = guard False *> error "unpossible"


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
>                   ++ map (string . (:[])) "[],;():")

exception char is one of:
~ ! @ # % ^ & | ` ?
which allows the last character of a multi character symbol to be + or
-

>    allOpSymbols = "+-*/<>=~!@#%^&|`?"
>    -- all symbols except - and / which can be used to start
>    -- a comment token
>    allOpSymbolsNoCommentStarters = filter (`notElem` "-/") allOpSymbols
>    -- these are the symbols when if part of a multi character
>    -- operator permit the operator to end with a + or - symbol
>    exceptionOpSymbols = "~!@#%^&|`?"
> 
>    -- special case for parsing a single + or - symbol
>    singlePlusMinus = try $ do
>      c <- choice $ map char "+-"
>      -- todo: deal with e.g. --- +-- +/* ?
>      notFollowedBy $ choice $ map char allOpSymbols
>      return [c]

>    -- this is used when we are parsing a potentially multi symbol
>    -- operator and we have alread seen one of the 'exception chars'
>    -- and so we can end with a + or -
>    moreOpCharsException = do
>        c <- choice (map char allOpSymbolsNoCommentStarters
>                     -- make sure we don't parse a comment starting token
>                     -- as part of an operator
>                     ++ [try (char '/' <* notFollowedBy (char '*'))
>                        ,try (char '-' <* notFollowedBy (char '-'))])
>        (c:) <$> option [] moreOpCharsException

>    opMoreChars = choice
>        [do
>         -- parse an exception char, now we can finish with a + -
>         c <- choice $ map char exceptionOpSymbols
>         (c:) <$> option [] moreOpCharsException
>        ,do
>         -- parse + or -, make sure it isn't the last symbol
>         c <- try (char '+'
>                   -- make sure there is another symbol
>                   <* lookAhead (choice $ map char allOpSymbols))
>         (c:) <$> option [] opMoreChars
>        ,do
>         c <- try (char '-'
>                   -- check for comment
>                   <* notFollowedBy (char '-')
>                   -- make sure there is another symbol
>                   <* lookAhead (choice $ map char allOpSymbols))
>         (c:) <$> option [] opMoreChars
>        ,do
>         -- parse one of the other ansi operator symbols
>         c <- choice (-- check / isn't start of comment /*
>                      try (char '/' <* notFollowedBy (char '*'))
>                      : map char "*<>=")
>         (c:) <$> option [] opMoreChars
>        ]


> symbol _ =
>    Symbol <$> choice (otherSymbol ++ regularOp)
>  where
>    otherSymbol = many1 (char '.') :
>                  map (string . (:[])) "[],;():?"

try is used because most of the first characters of the two character
symbols can also be part of a single character symbol

>    regularOp = map (try . string) [">=","<=","!=","<>","||"]
>                ++ map (string . (:[])) "+-^*/%~&|<>="



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


postgresql notes:
u&
SELECT 'foo'
'bar';
is equivalent to:
SELECT 'foobar';

SELECT 'foo'      'bar';
is invalid

(this should be in ansi also)

definitely do major review and docs:
when can escapes and prefixes be using with syntactic string literals
when can they be combined
when can e.g. dollar quoting be used
what escaping should there be, including unicode escapes


E'string'

with a range of escapes which should appear in the dialect data type

dollar quoted strings
never with prefixes/escapes

B''
X''

numbers

:: cast


type 'string' -  literals only, not array types
  ansi allows for some specific types
'string'::type
cast('string' as type)

can use dollar quoting here
typename('string') (not all types)
check these in the parser for keyword issues

extended operator rules

$1 positional parameter
()
[]
,
;
: array slices and variable names/hostparam
*
.

some operator precedence notes

SELECT 3 OPERATOR(pg_catalog.+) 4;

diff from ansi:

all the same symbols + more + different rules about parsing multi char
symbols (ansi is trivial here, postgresql is not trivial and
extensible)

identifiers: same, doublecheck the u&
hostparam: same, but with implementation issues because : is also a
  symbol in postgresql. this might be a little tricky to deal with

string literals:
u&?, does pg support n?

numbers: same
whitespace, comments: same

make sure there is a list of lexical syntax which is valid in postgres
and not in ansi, and vice versa, and have explicit tests for
these. There might also be situations here where a string is valid in
both, but lexes differently. There is definitely cases like this in
the main syntax.


action plan:
no abstract syntax changes are needed
write down a spec for ansi and for postgresql lexical syntax
create a list of tests for postgresql
  include eveything from ansi which is the same: maybe refactor the
  tests to make this maintainable

design for escaping issues
  (affects ansi also)
design for string literal-like syntax and for continuation strings
  (affects ansi also)

the test approach in general is first to parse basic examples of each
kind of token, then to manually come up with some edge cases to test,
and then to generate a good representative set of tokens (probably the
same set as the previous two categories), and create the cross product
of pairs of these tokens, eliminate ones when the tokens are next to
each other and it doesn't parse as the two separate tokens, using
manually written rules (want to be super accurate here - no false
positives or negatives), then test these all parse good as
well. Separating out the lexing in this way and doing this approach I
think gives a very good chance of minimising bugs in the basic
parsing, especially in the hairy bits.


= lexical syntax

One possible gotcha: there isn't a one-one correpsondence between e.g
identifiers and string literals in the lexical syntax, and identifiers
and string literals in the main syntax.

== ansi

=== symbol
=== identifier
+ escaping
=== quoted identifier
+ escaping, prefixes
=== host param

=== string literal-like
+ escaping, prefixes

=== number literals

=== whitespace

=== comments

== postgresql

=== symbol

=== identifier

== postgresql

=== symbol
extended set of symbols + extensibility + special cases
: is a symbol and also part of host param

=== identifier

same as ansi? is the character set the same?

=== quoted identifier

same as ansi?

=== host param

same as ansi (check char set)

=== string literal-like

dollar quoting
E quoting
missing n'?


=== number literals

same as ansi, i think

=== whitespace

same as ansi

=== comments

same as ansi

=== additions
$1 positional parameter

---- find what else is in hssqlppp to support mysql, oracle, sql
     server

