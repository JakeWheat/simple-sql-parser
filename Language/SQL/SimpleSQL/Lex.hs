
{-
The parser uses a separate lexer for two reasons:

1. sql syntax is very awkward to parse, the separate lexer makes it
easier to handle this in most places (in some places it makes it
harder or impossible, the fix is to switch to something better than
parsec)

2. using a separate lexer gives a huge speed boost because it reduces
backtracking. (We could get this by making the parsing code a lot more
complex also.)

3. we can test the lexer relatively exhaustively, then even when we
don't do nearly as comprehensive testing on the syntax level, we still
have a relatively high assurance of the low level of bugs. This is
much more difficult to get parity with when testing the syntax parser
directly without the separately testing lexing stage.

TODO:

optimisations:

check for left factor opportunities
check for places where it parses a few substrings from the source,
  then puts them back together with a concatenate of some flavour
  -> this is better if can find a way to parse the entire string
  from the source and lift it in one go into the lexical token
before this is done, a smaller optimisation is when any code matches
  a constant string in the lexer, use that constant string instead
  of the string from the parser, it might make a small difference in
  a few places
maybe every token should carry the exact source as well as any fields
  it's been broken into - so pretty printing is trivial


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

lexing tests are starting to take a really long time, so split the
tests so it is much easier to run all the tests except the lexing
tests which only need to be run when working on the lexer (which
should be relatively uncommon), or doing a commit or finishing off a
series of commits,

start writing the error message tests:
  generate/write a large number of syntax errors
  create a table with the source and the error message
  try to compare some different versions of code to compare the
    quality of the error messages by hand

  get this checked in so improvements and regressions in the error
    message quality can be tracked a little more easily (although it will
    still be manual)

try again to add annotation to the ast

-}

-- | Lexer for SQL.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.SQL.SimpleSQL.Lex
    (Token(..)
    ,WithPos(..)
    ,lexSQL
    ,lexSQLWithPositions
    ,prettyToken
    ,prettyTokens
    ,ParseError
    ,prettyError
    ,tokenListWillPrintAndLex
    ,ansi2011
    ,SQLStream(..)
    ) where

import Language.SQL.SimpleSQL.Dialect
    (Dialect(..)
    ,ansi2011
    )

import Text.Megaparsec
    (Parsec
    ,runParser'

    ,PosState(..)
    ,TraversableStream(..)
    ,VisualStream(..)
    
    ,ParseErrorBundle(..)
    ,errorBundlePretty

    ,SourcePos(..)
    ,getSourcePos
    ,getOffset
    ,pstateSourcePos
    ,statePosState
    ,mkPos

    ,choice
    ,satisfy
    ,takeWhileP
    ,takeWhile1P
    ,(<?>)
    ,eof
    ,many
    ,try
    ,option
    ,(<|>)
    ,notFollowedBy
    ,manyTill
    ,anySingle
    ,lookAhead
    )
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
    (string
    ,char
    )
import Text.Megaparsec.State (initialState)

import qualified Data.List          as DL
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy(..))
import Data.Void (Void)

import Control.Applicative ((<**>))
import Data.Char
    (isAlphaNum
    ,isAlpha
    ,isSpace
    ,isDigit
    )
import Control.Monad (void, guard)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

------------------------------------------------------------------------------

-- syntax

-- | Represents a lexed token
data Token
    -- | A symbol (in ansi dialect) is one of the following
    --
    -- * multi char symbols <> \<= \>= != ||
    -- * single char symbols: * + -  < >  ^ / %  ~ & | ? ( ) [ ] , ; ( )
    --
    = Symbol Text
    -- | This is an identifier or keyword. The first field is
    -- the quotes used, or nothing if no quotes were used. The quotes
    -- can be " or u& or something dialect specific like []
    | Identifier (Maybe (Text,Text)) Text
    -- | This is a prefixed variable symbol, such as :var, @var or #var
    -- (only :var is used in ansi dialect)
    | PrefixedVariable Char Text
    -- | This is a positional arg identifier e.g. $1
    | PositionalArg Int
    -- | This is a string literal. The first two fields are the --
    -- start and end quotes, which are usually both ', but can be
    -- the character set (one of nNbBxX, or u&, U&), or a dialect
    -- specific string quoting (such as $$ in postgres)
    | SqlString Text Text Text
    -- | A number literal (integral or otherwise), stored in original format
    -- unchanged
    | SqlNumber Text
    -- | Whitespace, one or more of space, tab or newline.
    | Whitespace Text
    -- | A commented line using --, contains every character starting with the
    -- \'--\' and including the terminating newline character if there is one
    -- - this will be missing if the last line in the source is a line comment
    -- with no trailing newline
    | LineComment Text
    -- | A block comment, \/* stuff *\/, includes the comment delimiters
    | BlockComment Text
      deriving (Eq,Show,Ord)

------------------------------------------------------------------------------

-- main api functions

-- | Lex some SQL to a list of tokens.
lexSQLWithPositions
    :: Dialect
    -- ^ dialect of SQL to use
    -> Text
    -- ^ filename to use in error messages
    -> Maybe (Int,Int)
    -- ^ line number and column number of the first character
    -- in the source to use in error messages
    -> Text
    -- ^ the SQL source to lex
    -> Either ParseError [WithPos Token]
lexSQLWithPositions dialect fn p src = myParse fn p (many (sqlToken dialect) <* (eof <?> "")) src


-- | Lex some SQL to a list of tokens.
lexSQL
    :: Dialect
    -- ^ dialect of SQL to use
    -> Text
    -- ^ filename to use in error messages
    -> Maybe (Int,Int)
    -- ^ line number and column number of the first character
    -- in the source to use in error messages
    -> Text
    -- ^ the SQL source to lex
    -> Either ParseError [Token]
lexSQL dialect fn p src =
    map tokenVal <$> lexSQLWithPositions dialect fn p src

myParse :: Text -> Maybe (Int,Int) -> Parser a -> Text -> Either ParseError a
myParse name sp' p s =
        let sp = fromMaybe (1,1) sp'
            ps = SourcePos (T.unpack name) (mkPos $ fst sp) (mkPos $ snd sp)
            is = initialState (T.unpack name) s
            sps = (statePosState is) {pstateSourcePos = ps}
            is' = is {statePosState = sps}
        in snd $ runParser' p is'

prettyError :: ParseError -> Text
prettyError = T.pack . errorBundlePretty

------------------------------------------------------------------------------

-- parsing boilerplate

type ParseError = ParseErrorBundle Text Void

type Parser = Parsec Void Text

-- | Positional information added to tokens to preserve source positions
-- for the parser
data WithPos a = WithPos
  { startPos :: SourcePos
  , endPos :: SourcePos
  , tokenLength :: Int
  , tokenVal :: a
  } deriving (Eq, Ord, Show)

------------------------------------------------------------------------------

-- pretty print

-- | Pretty printing, if you lex a bunch of tokens, then pretty
-- print them, should should get back exactly the same string
prettyToken :: Dialect -> Token -> Text
prettyToken _ (Symbol s) = s
prettyToken _ (Identifier Nothing t) = t
prettyToken _ (Identifier (Just (q1,q2)) t) = q1 <> t <> q2
prettyToken _ (PrefixedVariable c p) = T.cons c p
prettyToken _ (PositionalArg p) = T.cons '$' $ T.pack $ show p
prettyToken _ (SqlString s e t) = s <> t <> e
prettyToken _ (SqlNumber r) = r
prettyToken _ (Whitespace t) = t
prettyToken _ (LineComment l) = l
prettyToken _ (BlockComment c) = c

prettyTokens :: Dialect -> [Token] -> Text
prettyTokens d ts = T.concat $ map (prettyToken d) ts

------------------------------------------------------------------------------

-- token parsers

-- | parser for a sql token
sqlToken :: Dialect -> Parser (WithPos Token)
sqlToken d = (do
    -- possibly there's a more efficient way of doing the source positions?
    sp <- getSourcePos
    off <- getOffset
    t <- choice
         [sqlString d
         ,identifier d
         ,lineComment d
         ,blockComment d
         ,sqlNumber d
         ,positionalArg d
         ,dontParseEndBlockComment d
         ,prefixedVariable d
         ,symbol d
         ,sqlWhitespace d]
    off1 <- getOffset
    ep <- getSourcePos
    pure $ WithPos sp ep (off1 - off) t) <?> "valid lexical token"

--------------------------------------

{-
Parse a SQL string. Examples:

'basic string'
'string with '' a quote'
n'international text'
b'binary string'
x'hexidecimal string'
-}

sqlString :: Dialect -> Parser Token
sqlString d = dollarString <|> csString <|> normalString
  where
    dollarString = do
        guard $ diDollarString d
        -- use try because of ambiguity with symbols and with
        -- positional arg
        delim <- (\x -> T.concat ["$",x,"$"])
                 <$> try (char '$' *> option "" identifierString <* char '$')
        SqlString delim delim . T.pack <$> manyTill anySingle (try $ string delim)
    normalString = SqlString "'" "'" <$> (char '\'' *> normalStringSuffix False "")
    normalStringSuffix allowBackslash t = do
        s <- takeWhileP Nothing $ if allowBackslash
                                  then (`notElemChar` "'\\")
                                  else (/= '\'')
        -- deal with '' or \' as literal quote character
        choice [do
                ctu <- choice ["''" <$ try (string "''")
                              ,"\\'" <$ string "\\'"
                              ,"\\" <$ char '\\']
                normalStringSuffix allowBackslash $ T.concat [t,s,ctu]
               ,T.concat [t,s] <$ char '\'']
    -- try is used to to avoid conflicts with
    -- identifiers which can start with n,b,x,u
    -- once we read the quote type and the starting '
    -- then we commit to a string
    -- it's possible that this will reject some valid syntax
    -- but only pathalogical stuff, and I think the improved
    -- error messages and user predictability make it a good
    -- pragmatic choice
    csString
      | diEString d =
        choice [SqlString <$> try (string "e'" <|> string "E'")
                          <*> pure "'" <*> normalStringSuffix True ""
               ,csString']
      | otherwise = csString'
    csString' = SqlString
                <$> try cs
                <*> pure "'"
                <*> normalStringSuffix False ""
    csPrefixes = map (`T.cons` "'") "nNbBxX" ++ ["u&'", "U&'"]
    cs :: Parser Text
    cs = choice $ map string csPrefixes

--------------------------------------

{-
Parses identifiers:

simple_identifier_23
u&"unicode quoted identifier"
"quoted identifier"
"quoted identifier "" with double quote char"
`mysql quoted identifier`
-}

identifier :: Dialect -> Parser Token
identifier d =
    choice
    [quotedIden
    ,unicodeQuotedIden
    ,regularIden
    ,guard (diBackquotedIden d) >> mySqlQuotedIden
    ,guard (diSquareBracketQuotedIden d) >> sqlServerQuotedIden
    ]
  where
    regularIden = Identifier Nothing <$> identifierString
    quotedIden = Identifier (Just ("\"","\"")) <$> qidenPart
    mySqlQuotedIden = Identifier (Just ("`","`"))
                      <$> (char '`' *> takeWhile1P Nothing (/='`') <* char '`')
    sqlServerQuotedIden = Identifier (Just ("[","]"))
                          <$> (char '[' *> takeWhile1P Nothing (`notElemChar` "[]") <* char ']')
    -- try is used here to avoid a conflict with identifiers
    -- and quoted strings which also start with a 'u'
    unicodeQuotedIden = Identifier
                        <$> (f <$> try (oneOf "uU" <* string "&"))
                        <*> qidenPart
      where f x = Just (T.cons x "&\"", "\"")
    qidenPart = char '"' *> qidenSuffix ""
    qidenSuffix t = do
        s <- takeWhileP Nothing (/='"')
        void $ char '"'
        -- deal with "" as literal double quote character
        choice [do
                void $ char '"'
                qidenSuffix $ T.concat [t,s,"\"\""]
               ,pure $ T.concat [t,s]]

identifierString :: Parser Text
identifierString = (do
    c <- satisfy isFirstLetter
    choice
        [T.cons c <$> takeWhileP (Just "identifier char") isIdentifierChar
        ,pure $ T.singleton c]) <?> "identifier"
  where
     isFirstLetter c = c == '_' || isAlpha c

isIdentifierChar :: Char -> Bool
isIdentifierChar c = c == '_' || isAlphaNum c

--------------------------------------

lineComment :: Dialect -> Parser Token
lineComment _ = do
    try (string_ "--") <?> ""
    rest <- takeWhileP (Just "non newline character") (/='\n')
    -- can you optionally read the \n to terminate the takewhilep without reparsing it?
    suf <- option "" ("\n" <$ char_ '\n')
    pure $ LineComment $ T.concat ["--", rest, suf]

--------------------------------------

-- TODO: the parser before the switch to megaparsec parsed nested block comments
-- I don't know any dialects that use this, but I think it's useful, if needed,
-- add it back in under a dialect flag?
blockComment :: Dialect -> Parser Token
blockComment _ = (do
    try $ string_ "/*"
    BlockComment . T.concat . ("/*":) <$> more) <?> ""
  where
    more = choice
        [["*/"] <$ try (string_ "*/")  -- comment ended
        ,char_ '*' *> (("*":) <$> more) -- comment contains * but this isn't the comment end token
        -- not sure if there's an easy optimisation here
        ,(:) <$> takeWhile1P (Just "non comment terminator text") (/= '*') <*> more]

{-
This is to improve user experience: provide an error if we see */
outside a comment. This could potentially break postgres ops with */
in them (which is a stupid thing to do). In other cases, the user
should write * / instead (I can't think of any cases when this would
be valid syntax though).
-}

dontParseEndBlockComment :: Dialect -> Parser Token
dontParseEndBlockComment _ =
    -- don't use try, then it should commit to the error
    try (string "*/") *> fail "comment end without comment start"

--------------------------------------

{-
numbers

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


algorithm:
either
  parse 1 or more digits
    then an optional dot which isn't two dots
    then optional digits
  or: parse a dot which isn't two dots
    then digits
followed by an optional exponent
-}

sqlNumber :: Dialect -> Parser Token
sqlNumber d =
    SqlNumber <$> completeNumber
    -- this is for definitely avoiding possibly ambiguous source
    <* choice [-- special case to allow e.g. 1..2
               guard (diPostgresSymbols d)
               *> void (lookAhead $ try (string ".." <?> ""))
                  <|> void (notFollowedBy (oneOf "eE."))
              ,notFollowedBy (oneOf "eE.")
              ]
  where
    completeNumber =
      (digits <??> (pp dot <??.> pp digits)
      -- try is used in case we read a dot
      -- and it isn't part of a number
      -- if there are any following digits, then we commit
      -- to it being a number and not something else
      <|> try ((<>) <$> dot <*> digits))
      <??> pp expon

    -- make sure we don't parse two adjacent dots in a number
    -- special case for postgresql, we backtrack if we see two adjacent dots
    -- to parse 1..2, but in other dialects we commit to the failure
    dot = let p = string "." <* notFollowedBy (char '.')
          in if diPostgresSymbols d
             then try p
             else p
    expon = T.cons <$> oneOf "eE" <*> sInt
    sInt = (<>) <$> option "" (T.singleton <$> oneOf "+-") <*> digits
    pp = (<$$> (<>))
    p <??> q = p <**> option id q
    pa <$$> c = pa <**> pure (flip c)
    pa <??.> pb =
       let c = (<$>) . flip
       in (.) `c` pa <*> option id pb

digits :: Parser Text
digits = takeWhile1P (Just "digit") isDigit

--------------------------------------

positionalArg :: Dialect -> Parser Token
positionalArg d =
    guard (diPositionalArg d) >>
    -- use try to avoid ambiguities with other syntax which starts with dollar
    PositionalArg <$> try (char_ '$' *> (read . T.unpack <$> digits))

--------------------------------------

-- todo: I think the try here should read a prefix char, then a single valid
-- identifier char, then commit
prefixedVariable :: Dialect -> Parser Token
prefixedVariable d = try $ choice
    [PrefixedVariable <$> char ':' <*> identifierString
    ,guard (diAtIdentifier d) >>
     PrefixedVariable <$> char '@' <*> identifierString
    ,guard (diHashIdentifier d) >>
     PrefixedVariable <$> char '#' <*> identifierString
    ]

--------------------------------------

{-
Symbols

A symbol is an operator, or one of the misc symbols which include:
. .. := : :: ( ) ? ; , { } (for odbc)

The postgresql operator syntax allows a huge range of operators
compared with ansi and other dialects
-}

symbol :: Dialect -> Parser Token
symbol d  = Symbol <$> choice (concat
   [dots
   ,if diPostgresSymbols d
    then postgresExtraSymbols
    else []
   ,miscSymbol
   ,if diOdbc d then odbcSymbol else []
   ,if diPostgresSymbols d
    then generalizedPostgresqlOperator
    else basicAnsiOps
   ])
 where
   dots = [takeWhile1P (Just "dot") (=='.')]
   odbcSymbol = [string "{", string "}"]
   postgresExtraSymbols =
       [try (string ":=")
        -- parse :: and : and avoid allowing ::: or more
       ,try (string "::" <* notFollowedBy (char ':'))
       ,try (string ":" <* notFollowedBy (char ':'))]
   miscSymbol = map (string . T.singleton) $
                case () of
                    _ | diSqlServerSymbols d -> ",;():?"
                      | diPostgresSymbols d -> "[],;()"
                      | otherwise -> "[],;():?"

{-
try is used because most of the first characters of the two character
symbols can also be part of a single character symbol
-}

   basicAnsiOps = map (try . string) [">=","<=","!=","<>"]
                  ++ map (string . T.singleton) "+-^*/%~&<>="
                  ++ pipes
   pipes = -- what about using many1 (char '|'), then it will
           -- fail in the parser? Not sure exactly how
           -- standalone the lexer should be
           [char '|' *>
            choice ["||" <$ char '|' <* notFollowedBy (char '|')
                   ,pure "|"]]

{-
postgresql generalized operators

this includes the custom operators that postgres supports,
plus all the standard operators which could be custom operators
according to their grammar

rules

An operator name is a sequence of up to NAMEDATALEN-1 (63 by default) characters from the following list:

+ - * / < > = ~ ! @ # % ^ & | ` ?

There are a few restrictions on operator names, however:
-- and /* cannot appear anywhere in an operator name, since they will be taken as the start of a comment.

A multiple-character operator name cannot end in + or -, unless the name also contains at least one of these characters:

~ ! @ # % ^ & | ` ?

which allows the last character of a multi character symbol to be + or
-
-}

generalizedPostgresqlOperator :: [Parser Text]
generalizedPostgresqlOperator = [singlePlusMinus,opMoreChars]
  where
    allOpSymbols = "+-*/<>=~!@#%^&|`?"
    -- these are the symbols when if part of a multi character
    -- operator permit the operator to end with a + or - symbol
    exceptionOpSymbols = "~!@#%^&|`?"

    -- special case for parsing a single + or - symbol
    singlePlusMinus = try $ do
      c <- oneOf "+-"
      notFollowedBy $ oneOf allOpSymbols
      pure $ T.singleton c

    -- this is used when we are parsing a potentially multi symbol
    -- operator and we have alread seen one of the 'exception chars'
    -- and so we can end with a + or -
    moreOpCharsException = do
       c <- oneOf (filter (`notElemChar` "-/*") allOpSymbols)
            -- make sure we don't parse a comment starting token
            -- as part of an operator
            <|> try (char '/' <* notFollowedBy (char '*'))
            <|> try (char '-' <* notFollowedBy (char '-'))
            -- and make sure we don't parse a block comment end
            -- as part of another symbol
            <|> try (char '*' <* notFollowedBy (char '/'))
       T.cons c <$> option "" moreOpCharsException

    opMoreChars = choice
       [-- parse an exception char, now we can finish with a + -
        T.cons
        <$> oneOf exceptionOpSymbols
        <*> option "" moreOpCharsException
       ,T.cons
        <$> (-- parse +, make sure it isn't the last symbol
             try (char '+' <* lookAhead (oneOf allOpSymbols))
             <|> -- parse -, make sure it isn't the last symbol
                 -- or the start of a -- comment
             try (char '-'
                  <* notFollowedBy (char '-')
                  <* lookAhead (oneOf allOpSymbols))
             <|> -- parse / check it isn't the start of a /* comment
             try (char '/' <* notFollowedBy (char '*'))
             <|> -- make sure we don't parse */ as part of a symbol
             try (char '*' <* notFollowedBy (char '/'))
             <|> -- any other ansi operator symbol
             oneOf "<>=")
        <*> option "" opMoreChars
       ]

--------------------------------------

sqlWhitespace :: Dialect -> Parser Token
sqlWhitespace _ = Whitespace <$> takeWhile1P (Just "whitespace") isSpace <?> ""

----------------------------------------------------------------------------

-- parser helpers

char_ :: Char -> Parser ()
char_ = void . char

string_ :: Text -> Parser ()
string_ = void . string

oneOf :: [Char] -> Parser Char
oneOf = M.oneOf

notElemChar :: Char -> [Char] -> Bool
notElemChar a b = a `notElem` (b :: [Char])

----------------------------------------------------------------------------


{-
This utility function will accurately report if the two tokens are
pretty printed, if they should lex back to the same two tokens. This
function is used in testing (and can be used in other places), and
must not be implemented by actually trying to print both tokens and
then lex them back from a single string (because then we would have
the risk of thinking two tokens cannot be together when there is bug
in the lexer, which the testing is supposed to find).

maybe do some quick checking to make sure this function only gives
true negatives: check pairs which return false actually fail to lex or
give different symbols in return: could use quickcheck for this

a good sanity test for this function is to change it to always return
true, then check that the automated tests return the same number of
successes. I don't think it succeeds this test at the moment
-}

-- | Utility function to tell you if a list of tokens
-- will pretty print then lex back to the same set of tokens.
-- Used internally, might be useful for generating SQL via lexical tokens.
tokenListWillPrintAndLex :: Dialect -> [Token] -> Bool
tokenListWillPrintAndLex _ [] = True
tokenListWillPrintAndLex _ [_] = True
tokenListWillPrintAndLex d (a:b:xs) =
    tokensWillPrintAndLex d a b && tokenListWillPrintAndLex d (b:xs)

tokensWillPrintAndLex :: Dialect -> Token -> Token -> Bool
tokensWillPrintAndLex d a b

{-
a : followed by an identifier character will look like a host param
followed by = or : makes a different symbol
-}

    | Symbol ":" <- a
    , checkFirstBChar (\x -> isIdentifierChar x || x `T.elem` ":=") = False

{-
two symbols next to eachother will fail if the symbols can combine and
(possibly just the prefix) look like a different symbol
-}

    | diPostgresSymbols d
    , Symbol a' <- a
    , Symbol b' <- b
    , b' `notElem` ["+", "-"] || any (`T.elem` a') ("~!@#%^&|`?" :: [Char]) = False

{-
check two adjacent symbols in non postgres where the combination
possibilities are much more limited. This is ansi behaviour, it might
be different when the other dialects are done properly
-}

   | Symbol a' <- a
   , Symbol b' <- b
   , (a',b') `elem` [("<",">")
                    ,("<","=")
                    ,(">","=")
                    ,("!","=")
                    ,("|","|")
                    ,("||","|")
                    ,("|","||")
                    ,("||","||")
                    ,("<",">=")
                    ] = False

-- two whitespaces will be combined

   | Whitespace {} <- a
   , Whitespace {} <- b = False

-- line comment without a newline at the end will eat the next token

   | LineComment {} <- a
   , checkLastAChar (/='\n') = False

{-
check the last character of the first token and the first character of
the second token forming a comment start or end symbol
-}

   | let f '-' '-' = True
         f '/' '*' = True
         f '*' '/' = True
         f _ _ = False
     in checkBorderChars f = False

{-
a symbol will absorb a following .
TODO: not 100% on this always being bad
-}

   | Symbol {} <- a
   , checkFirstBChar (=='.') = False

-- cannot follow a symbol ending in : with another token starting with :

   | let f ':' ':' = True
         f _ _ = False
     in checkBorderChars f = False

-- unquoted identifier followed by an identifier letter

   | Identifier Nothing _ <- a
   , checkFirstBChar isIdentifierChar = False

-- a quoted identifier using ", followed by a " will fail

   | Identifier (Just (_,"\"")) _ <- a
   , checkFirstBChar (=='"') = False

-- prefixed variable followed by an identifier char will be absorbed

   | PrefixedVariable {} <- a
   , checkFirstBChar isIdentifierChar = False

-- a positional arg will absorb a following digit

   | PositionalArg {} <- a
   , checkFirstBChar isDigit = False

-- a string ending with ' followed by a token starting with ' will be absorbed

   | SqlString _ "'" _ <- a
   , checkFirstBChar (=='\'') = False

-- a number followed by a . will fail or be absorbed

   | SqlNumber {} <- a
   , checkFirstBChar (=='.') = False

-- a number followed by an e or E will fail or be absorbed

   | SqlNumber {} <- a
   , checkFirstBChar (\x -> x =='e' || x == 'E') = False

-- two numbers next to eachother will fail or be absorbed

   | SqlNumber {} <- a
   , SqlNumber {} <- b = False


   | otherwise = True

  where
    prettya = prettyToken d a
    prettyb = prettyToken d b
    -- helper function to run a predicate on the
    -- last character of the first token and the first
    -- character of the second token
    checkBorderChars f =
        case (T.unsnoc prettya, T.uncons prettyb) of
            (Just (_,la), Just (fb,_)) -> f la fb
            _ -> False
    checkFirstBChar f = case T.uncons prettyb of
        Just (b',_) -> f b'
        _ -> False
    checkLastAChar f = case T.unsnoc prettya of
        Just (_,la) -> f la
        _ -> False

------------------------------------------------------------------------------

-- megaparsec stream boilerplate

-- | Wrapper to allow using the lexer as input to a megaparsec parser.
data SQLStream = SQLStream
  { sqlStreamInput :: String
  , unSQLStream :: [WithPos Token]
  }

instance M.Stream SQLStream where
  type Token  SQLStream = WithPos Token
  type Tokens SQLStream = [WithPos Token]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (SQLStream _ []) = Nothing
  take1_ (SQLStream str (t:ts)) = Just
    ( t
    , SQLStream (drop (tokensLength pxy (t NE.:|[])) str) ts
    )
  takeN_ n (SQLStream str s)
    | n <= 0    = Just ([], SQLStream str s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
        in case NE.nonEmpty x of
          Nothing -> Just (x, SQLStream str s')
          Just nex -> Just (x, SQLStream (drop (tokensLength pxy nex) str) s')
  takeWhile_ f (SQLStream str s) =
    let (x, s') = DL.span f s
    in case NE.nonEmpty x of
      Nothing -> (x, SQLStream str s')
      Just nex -> (x, SQLStream (drop (tokensLength pxy nex) str) s')

instance VisualStream SQLStream where
  showTokens Proxy = DL.intercalate " "
    . NE.toList
    . fmap (showMyToken . tokenVal)
  tokensLength Proxy xs = sum (tokenLength <$> xs)

instance TraversableStream SQLStream where
    -- I have no idea what all this is doing
  reachOffset o _x@(M.PosState {..}) =
    ( Just $ actualLine
    , PosState
        { pstateInput = SQLStream
            { sqlStreamInput = postStr
            , unSQLStream = post
            }
        , pstateOffset = max pstateOffset o
        , pstateSourcePos = newSourcePos
        , pstateTabWidth = pstateTabWidth
        , pstateLinePrefix = prefix
        }
    )
    where
      maybeitsthefullsource = sqlStreamInput pstateInput
      targetLineNo = M.unPos $ sourceLine newSourcePos
      actualLine = case drop (targetLineNo - 1) $ lines maybeitsthefullsource of
          (x:_) -> x
          [] -> "<empty line>"
      prefix =
        if sameLine
          then pstateLinePrefix ++ preLine
          else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> case unSQLStream pstateInput of
            [] -> pstateSourcePos
            xs -> endPos (last xs)
          (x:_) -> startPos x
      (pre, post) = splitAt (o - pstateOffset) (unSQLStream pstateInput)
      (preStr, postStr) = splitAt tokensConsumed (sqlStreamInput pstateInput)
      preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre

pxy :: Proxy SQLStream
pxy = Proxy

showMyToken :: Token -> String
-- todo: how to do this properly?
showMyToken = T.unpack . prettyToken ansi2011
