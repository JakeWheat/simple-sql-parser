
{-
= TOC:

notes
Public api
Names - parsing identifiers
Typenames
Scalar expressions
  simple literals
  star, param
  parens expression, row constructor and scalar subquery
  case, cast, exists, unique, array/ multiset constructor
  typed literal, app, special function, aggregate, window function
  suffixes: in, between, quantified comparison, match predicate, array
    subscript, escape, collate
  operators
  scalar expression top level
  helpers
query expressions
  select lists
  from clause
  other table expression clauses:
    where, group by, having, order by, offset and fetch
  common table expressions
  query expression
  set operations
lexers
utilities

= Notes about the code

The lexers appear at the bottom of the file. There tries to be a clear
separation between the lexers and the other parser which only use the
lexers, this isn't 100% complete at the moment and needs fixing.

== Left factoring

The parsing code is aggressively left factored, and try is avoided as
much as possible. Try is avoided because:

 * when it is overused it makes the code hard to follow
 * when it is overused it makes the parsing code harder to debug
 * it makes the parser error messages much worse

The code could be made a bit simpler with a few extra 'trys', but this
isn't done because of the impact on the parser error
messages. Apparently it can also help the speed but this hasn't been
looked into.

== Parser error messages

A lot of care has been given to generating good parser error messages
for invalid syntax. There are a few utils below which partially help
in this area.

There is a set of crafted bad expressions in ErrorMessages.hs, these
are used to guage the quality of the error messages and monitor
regressions by hand. The use of <?> is limited as much as possible:
each instance should justify itself by improving an actual error
message.

There is also a plan to write a really simple expression parser which
doesn't do precedence and associativity, and the fix these with a pass
over the ast. I don't think there is any other way to sanely handle
the common prefixes between many infix and postfix multiple keyword
operators, and some other ambiguities also. This should help a lot in
generating good error messages also.

Both the left factoring and error message work are greatly complicated
by the large number of shared prefixes of the various elements in SQL
syntax.

== Main left factoring issues

There are three big areas which are tricky to left factor:

 * typenames
 * scalar expressions which can start with an identifier
 * infix and suffix operators

=== typenames

There are a number of variations of typename syntax. The standard
deals with this by switching on the name of the type which is parsed
first. This code doesn't do this currently, but might in the
future. Taking the approach in the standard grammar will limit the
extensibility of the parser and might affect the ease of adapting to
support other sql dialects.

=== identifier scalar expressions

There are a lot of scalar expression nodes which start with
identifiers, and can't be distinguished the tokens after the initial
identifier are parsed. Using try to implement these variations is very
simple but makes the code much harder to debug and makes the parser
error messages really bad.

Here is a list of these nodes:

 * identifiers
 * function application
 * aggregate application
 * window application
 * typed literal: typename 'literal string'
 * interval literal which is like the typed literal with some extras

There is further ambiguity e.g. with typed literals with precision,
functions, aggregates, etc. - these are an identifier, followed by
parens comma separated scalar expressions or something similar, and it
is only later that we can find a token which tells us which flavour it
is.

There is also a set of nodes which start with an identifier/keyword
but can commit since no other syntax can start the same way:

 * case
 * cast
 * exists, unique subquery
 * array constructor
 * multiset constructor
 * all the special syntax functions: extract, position, substring,
  convert, translate, overlay, trim, etc.

The interval literal mentioned above is treated in this group at the
moment: if we see 'interval' we parse it either as a full interval
literal or a typed literal only.

Some items in this list might have to be fixed in the future, e.g. to
support standard 'substring(a from 3 for 5)' as well as regular
function substring syntax 'substring(a,3,5) at the same time.

The work in left factoring all this is mostly done, but there is still
a substantial bit to complete and this is by far the most difficult
bit. At the moment, the work around is to use try, the downsides of
which is the poor parsing error messages.

=== infix and suffix operators

== permissiveness

The parser is very permissive in many ways. This departs from the
standard which is able to eliminate a number of possibilities just in
the grammar, which this parser allows. This is done for a number of
reasons:

 * it makes the parser simple - less variations
 * it should allow for dialects and extensibility more easily in the
  future (e.g. new infix binary operators with custom precedence)
 * many things which are effectively checked in the grammar in the
  standard, can be checked using a typechecker or other simple static
  analysis

To use this code as a front end for a sql engine, or as a sql validity
checker, you will need to do a lot of checks on the ast. A
typechecker/static checker plus annotation to support being a compiler
front end is planned but not likely to happen too soon.

Some of the areas this affects:

typenames: the variation of the type name should switch on the actual
name given according to the standard, but this code only does this for
the special case of interval type names. E.g. you can write 'int
collate C' or 'int(15,2)' and this will parse as a character type name
or a precision scale type name instead of being rejected.

scalar expressions: every variation on scalar expressions uses the same
parser/syntax. This means we don't try to stop non boolean valued
expressions in boolean valued contexts in the parser. Another area
this affects is that we allow general scalar expressions in group by,
whereas the standard only allows column names with optional collation.

These are all areas which are specified (roughly speaking) in the
syntax rather than the semantics in the standard, and we are not
fixing them in the syntax but leaving them till the semantic checking
(which doesn't exist in this code at this time).
-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This is the module with the parser functions.
module Language.SQL.SimpleSQL.Parse
    (parseQueryExpr
    ,parseScalarExpr
    ,parseStatement
    ,parseStatements
    ,ParseError(..)
    ,prettyError
    ,ansi2011
    ) where

import Text.Megaparsec
    (ParsecT
    ,runParserT

    ,ParseErrorBundle(..)
    ,errorBundlePretty
    ,hidden
    ,failure
    ,ErrorItem(..)

    ,(<|>)
    ,token
    ,choice
    ,eof
    ,try
    ,sepBy
    ,sepBy1
    ,optional
    ,option
    ,some
    ,many
    ,between
    ,lookAhead
    )
import qualified Control.Monad.Combinators.Expr as E
import qualified Control.Monad.Permutations as P
import qualified Text.Megaparsec as M

import Control.Monad.Reader
    (Reader
    ,runReader
    ,ask
    ,asks
    )

import qualified Data.Set           as Set
import qualified Data.List.NonEmpty as NE
import Data.Void (Void)

import Control.Monad (guard, void)
import Control.Applicative ((<**>))
import Data.Char (isDigit)
import Data.List (sort,groupBy)
import Data.Function (on)
import Data.Maybe (catMaybes, isJust, mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Dialect
import qualified Language.SQL.SimpleSQL.Lex as L
--import Text.Megaparsec.Debug (dbg)
import Text.Read (readMaybe)

------------------------------------------------------------------------------

-- = Public API

-- | Parses a query expr, trailing semicolon optional.
parseQueryExpr
    :: Dialect
    -- ^ dialect of SQL to use
    -> Text
    -- ^ filename to use in error messages
    -> Maybe (Int,Int)
    -- ^ line number and column number of the first character
    -- in the source to use in error messages
    -> Text
    -- ^ the SQL source to parse
    -> Either ParseError QueryExpr
parseQueryExpr = wrapParse topLevelQueryExpr

-- | Parses a statement, trailing semicolon optional.
parseStatement
    :: Dialect
    -- ^ dialect of SQL to use
    -> Text
    -- ^ filename to use in error messages
    -> Maybe (Int,Int)
    -- ^ line number and column number of the first character
    -- in the source to use in error messages
    -> Text
    -- ^ the SQL source to parse
    -> Either ParseError Statement
parseStatement = wrapParse topLevelStatement

-- | Parses a list of statements, with semi colons between
-- them. The final semicolon is optional.
parseStatements
    :: Dialect
    -- ^ dialect of SQL to use
    -> Text
    -- ^ filename to use in error messages
    -> Maybe (Int,Int)
    -- ^ line number and column number of the first character
    -- in the source to use in error messages
    -> Text
    -- ^ the SQL source to parse
    -> Either ParseError [Statement]
parseStatements = wrapParse statements

-- | Parses a scalar expression.
parseScalarExpr
    :: Dialect
    -- ^ dialect of SQL to use
    -> Text
    -- ^ filename to use in error messages
    -> Maybe (Int,Int)
    -- ^ line number and column number of the first character
    -- in the source to use in error messages
    -> Text
    -- ^ the SQL source to parse
    -> Either ParseError ScalarExpr
parseScalarExpr = wrapParse scalarExpr

-- Megaparsec is too clever, so have to create a new type to represent
-- either a lex error or a parse error

data ParseError
    = LexError L.ParseError
    | ParseError (ParseErrorBundle L.SQLStream Void)

prettyError :: ParseError -> Text
prettyError (LexError e) = T.pack $ errorBundlePretty e
prettyError (ParseError e) = T.pack $ errorBundlePretty e

{-
This helper function takes the parser given and:

sets the position when parsing
automatically skips leading whitespace
checks the parser parses all the input using eof
converts the error return to the nice wrapper
-}

wrapParse :: Parser a
          -> Dialect
          -> Text
          -> Maybe (Int,Int)
          -> Text
          -> Either ParseError a
wrapParse parser d f p src = do
    lx <- either (Left . LexError) Right $ L.lexSQLWithPositions d True f p src
    either (Left . ParseError) Right $
        runReader (runParserT (parser <* (hidden eof)) (T.unpack f)
                   $ L.SQLStream (T.unpack src) $ filter notSpace lx) d
  where
    notSpace = notSpace' . L.tokenVal
    notSpace' (L.Whitespace {}) = False
    notSpace' (L.LineComment {}) = False
    notSpace' (L.BlockComment {}) = False
    notSpace' _ = True

------------------------------------------------------------------------------

-- parsing code

type Parser = ParsecT Void L.SQLStream (Reader Dialect)

{-
------------------------------------------------

= Names

Names represent identifiers and a few other things. The parser here
handles regular identifiers, dotten chain identifiers, quoted
identifiers and unicode quoted identifiers.

Dots: dots in identifier chains are parsed here and represented in the
Iden constructor usually. If parts of the chains are non identifier
scalar expressions, then this is represented by a BinOp "."
instead. Dotten chain identifiers which appear in other contexts (such
as function names, table names, are represented as [Name] only.

Identifier grammar:

unquoted:
underscore <|> letter : many (underscore <|> alphanum

example
_example123

quoted:

double quote, many (non quote character or two double quotes
together), double quote

"example quoted"
"example with "" quote"

unicode quoted is the same as quoted in this parser, except it starts
with U& or u&

u&"example quoted"
-}

name :: Text -> Parser Name
name lbl = label lbl $ do
    bl <- askDialect diKeywords
    uncurry Name <$> identifierTok bl

-- todo: replace (:[]) with a named function all over

names :: Text -> Parser [Name]
names lbl =
    label lbl (reverse <$> (((:[]) <$> name lbl) `chainrSuffix` anotherName))
  -- can't use a simple chain here since we
  -- want to wrap the . + name in a try
  -- this will change when this is left factored
  where
    anotherName :: Parser ([Name] -> [Name])
    anotherName = try ((:) <$> (hidden (symbol "." *> name lbl)))

{-
= Type Names

Typenames are used in casts, and also in the typed literal syntax,
which is a typename followed by a string literal.

Here are the grammar notes:

== simple type name

just an identifier chain or a multi word identifier (this is a fixed
list of possibilities, e.g. as 'character varying', see below in the
parser code for the exact list).

<simple-type-name> ::= <identifier-chain>
     | multiword-type-identifier

== Precision type name

<precision-type-name> ::= <simple-type-name> <left paren> <unsigned-int> <right paren>

e.g. char(5)

note: above and below every where a simple type name can appear, this
means a single identifier/quoted or a dotted chain, or a multi word
identifier

== Precision scale type name

<precision-type-name> ::= <simple-type-name> <left paren> <unsigned-int> <comma> <unsigned-int> <right paren>

e.g. decimal(15,2)

== Lob type name

this is a variation on the precision type name with some extra info on
the units:

<lob-type-name> ::=
   <simple-type-name> <left paren> <unsigned integer> [ <multiplier> ] [ <char length units> ] <right paren>

<multiplier>    ::=   K | M | G
<char length units>    ::=   CHARACTERS | CODE_UNITS | OCTETS

(if both multiplier and char length units are missing, then this will
parse as a precision type name)

e.g.
clob(5M octets)

== char type name

this is a simple type with optional precision which allows the
character set or the collation to appear as a suffix:

<char type name> ::=
    <simple type name>
    [ <left paren> <unsigned-int> <right paren> ]
    [ CHARACTER SET <identifier chain> ]
    [ COLLATE <identifier chain> ]

e.g.

char(5) character set my_charset collate my_collation

= Time typename

this is typename with optional precision and either 'with time zone'
or 'without time zone' suffix, e.g.:

<datetime type> ::=
    [ <left paren> <unsigned-int> <right paren> ]
    <with or without time zone>
<with or without time zone> ::= WITH TIME ZONE | WITHOUT TIME ZONE
    WITH TIME ZONE | WITHOUT TIME ZONE

= row type name

<row type> ::=
    ROW <left paren> <field definition> [ { <comma> <field definition> }... ] <right paren>

<field definition> ::= <identifier> <type name>

e.g.
row(a int, b char(5))

= interval type name

<interval type> ::= INTERVAL <interval datetime field> [TO <interval datetime field>]

<interval datetime field> ::=
  <datetime field> [ <left paren> <unsigned int> [ <comma> <unsigned int> ] <right paren> ]

= array type name

<array type> ::= <data type> ARRAY [ <left bracket> <unsigned integer> <right bracket> ]

= multiset type name

<multiset type>    ::=   <data type> MULTISET

A type name will parse into the 'smallest' constructor it will fit in
syntactically, e.g. a clob(5) will parse to a precision type name, not
a lob type name.

Unfortunately, to improve the error messages, there is a lot of (left)
factoring in this function, and it is a little dense.

the hideArg is used when the typename is used as part of a typed
literal expression, to hide what comes after the paren in
'typename('. This is so 'arbitrary_fn(' gives an 'expecting expression',
instead of 'expecting expression or number', which is odd.

-}

typeName :: Parser TypeName
typeName = typeName' False

typeName' :: Bool -> Parser TypeName
typeName' hideArg =
    label "typename" (
        (rowTypeName <|> intervalTypeName <|> otherTypeName)
        `chainrSuffix` tnSuffix)
  where
    rowTypeName =
        RowTypeName <$> (hidden (keyword_ "row") *> parens (commaSep1 rowField))
    rowField = (,) <$> name "type name" <*> typeName
    ----------------------------
    intervalTypeName =
        hidden (keyword_ "interval") *>
        (uncurry IntervalTypeName <$> intervalQualifier)
    ----------------------------
    otherTypeName =
        nameOfType <**>
            (typeNameWithParens
             <|> pure Nothing <**> (hidden timeTypeName <|> hidden charTypeName)
             <|> pure TypeName)
    nameOfType = reservedTypeNames <|> names "type name"
    charTypeName = charSet <**> (option [] tcollate <**> pure (flip4 CharTypeName))
                   <|> pure [] <**> (tcollate <**> pure (flip4 CharTypeName))
    typeNameWithParens =
        (hidden openParen *> (if hideArg then hidden unsignedInteger else unsignedInteger))
        <**> (closeParen *> hidden precMaybeSuffix
              <|> hidden (precScaleTypeName <|> precLengthTypeName) <* closeParen)
    precMaybeSuffix = (. Just) <$> (timeTypeName <|> charTypeName)
                      <|> pure (flip PrecTypeName)
    precScaleTypeName =
        (hidden comma *> (if hideArg then hidden unsignedInteger else unsignedInteger))
        <**> pure (flip3 PrecScaleTypeName)
    precLengthTypeName =
        Just <$> lobPrecSuffix
        <**> (optional lobUnits <**> pure (flip4 PrecLengthTypeName))
        <|> pure Nothing <**> ((Just <$> lobUnits) <**> pure (flip4 PrecLengthTypeName))
    timeTypeName = tz <**> pure (flip3 TimeTypeName)
    ----------------------------
    lobPrecSuffix = PrecK <$ keyword_ "k"
                    <|> PrecM <$ keyword_ "m"
                    <|> PrecG <$ keyword_ "g"
                    <|> PrecT <$ keyword_ "t"
                    <|> PrecP <$ keyword_ "p"
    lobUnits = PrecCharacters <$ keyword_ "characters"
               -- char and byte are the oracle spelling
               -- todo: move these to oracle dialect
               <|> PrecCharacters <$ keyword_ "char"
               <|> PrecOctets <$ keyword_ "octets"
               <|> PrecOctets <$ keyword_ "byte"
    tz = True <$ keywords_ ["with", "time","zone"]
         <|> False <$ keywords_ ["without", "time","zone"]
    charSet = keywords_ ["character", "set"] *> names "character set name"
    tcollate = keyword_ "collate" *> names "collation name"
    ----------------------------
    tnSuffix = multiset <|> array
    multiset = MultisetTypeName <$ keyword_ "multiset"
    array = keyword_ "array" *>
        (optional (brackets unsignedInteger) <**> pure (flip ArrayTypeName))
    ----------------------------
    -- this parser handles the fixed set of multi word
    -- type names, plus all the type names which are
    -- reserved words
    reservedTypeNames = do
        stn <- askDialect diSpecialTypeNames
        (:[]) . Name Nothing . T.unwords <$> makeKeywordTree stn


{-
= Scalar expressions

== simple literals

See the stringToken lexer below for notes on string literal syntax.
-}

stringLit :: Parser ScalarExpr
stringLit = (\(s,e,t) -> StringLit s e t) <$> stringTokExtend

numberLit :: Parser ScalarExpr
numberLit = NumLit <$> sqlNumberTok False

simpleLiteral :: Parser ScalarExpr
simpleLiteral = numberLit <|> stringLit

{-
== star, param, host param

=== star

used in select *, select x.*, and agg(*) variations, and some other
places as well. The parser makes an attempt to not parse star in
most contexts, to provide better experience when the user makes a mistake
in an expression containing * meaning multiple. It will parse a *
at the top level of a select item, or in arg in a app argument list.
-}

star :: Parser ScalarExpr
star =
    hidden $ choice
    [Star <$ symbol "*"
    -- much easier to use try here than to left factor where
    -- this is allowed and not allowed
    ,try (QStar <$> (names "qualified star" <* symbol "." <* symbol "*"))]

{-
== parameter

unnamed parameter or named parameter
use in e.g. select * from t where a = ?
select x from t where x > :param
-}

parameter :: Parser ScalarExpr
parameter = choice
    [Parameter <$ questionMark
    ,HostParameter
     <$> hostParamTok
     <*> hoptional (keyword "indicator" *> hostParamTok)]

-- == positional arg

positionalArg :: Parser ScalarExpr
positionalArg = PositionalArg <$> positionalArgTok

{-
== parens

scalar expression parens, row ctor and scalar subquery
-}

parensExpr :: Parser ScalarExpr
parensExpr = parens $ choice
    [SubQueryExpr SqSq <$> queryExpr
    ,ctor <$> commaSep1 scalarExpr]
  where
    ctor [a] = Parens a
    ctor as = SpecialOp [Name Nothing "rowctor"] as

{-
== case, cast, exists, unique, array/multiset constructor, interval

All of these start with a fixed keyword which is reserved, so no other
syntax can start with the same keyword.

=== case expression
-}

caseExpr :: Parser ScalarExpr
caseExpr =
    Case <$> (keyword_ "case" *> optional scalarExpr)
         <*> some whenClause
         <*> optional elseClause
         <* keyword_ "end"
  where
   whenClause = (,) <$> (keyword_ "when" *> commaSep1 scalarExpr)
                    <*> (keyword_ "then" *> scalarExpr)
   elseClause = keyword_ "else" *> scalarExpr

{-
=== cast

cast: cast(expr as type)
-}

cast :: Parser ScalarExpr
cast = keyword_ "cast" *>
       parens (Cast <$> scalarExpr
                    <*> (keyword_ "as" *> typeName))

{-
=== convert

convertSqlServer: SqlServer dialect CONVERT(data_type(length), expression, style)
-}

convertSqlServer :: Parser ScalarExpr
convertSqlServer = guardDialect diConvertFunction
                   *> keyword_ "convert" *>
                   parens (Convert <$> typeName <*> (comma *> scalarExpr)
                      <*> optional (comma *> unsignedInteger))

{-
=== exists, unique

subquery expression:
[exists|unique] (queryexpr)
-}

subquery :: Parser ScalarExpr
subquery = SubQueryExpr <$> sqkw <*> parens queryExpr
  where
    sqkw = SqExists <$ keyword_ "exists" <|> SqUnique <$ keyword_ "unique"

-- === array/multiset constructor

arrayCtor :: Parser ScalarExpr
arrayCtor = keyword_ "array" >>
    choice
    [ArrayCtor <$> parens queryExpr
    ,Array (Iden [Name Nothing "array"]) <$> brackets (commaSep scalarExpr)]

{-
As far as I can tell, table(query expr) is just syntax sugar for
multiset(query expr). It must be there for compatibility or something.
-}

multisetCtor :: Parser ScalarExpr
multisetCtor =
    choice
    [keyword_ "multiset" >>
     choice
     [MultisetQueryCtor <$> parens queryExpr
     ,MultisetCtor <$> brackets (commaSep scalarExpr)]
    ,keyword_ "table" >>
     MultisetQueryCtor <$> parens queryExpr]

nextValueFor :: Parser ScalarExpr
nextValueFor = keywords_ ["next","value","for"] >>
    NextValueFor <$> names "sequence generator name"

{-
=== interval

interval literals are a special case and we follow the grammar less
permissively here

parse SQL interval literals, something like
interval '5' day (3)
or
interval '5' month

if the literal looks like this:
interval 'something'

then it is parsed as a regular typed literal. It must have a
interval-datetime-field suffix to parse as an intervallit

It uses try because of a conflict with interval type names: todo, fix
this. also fix the monad -> applicative
-}

intervalLit :: Parser ScalarExpr
intervalLit =
    label "interval literal" $ try (keyword_ "interval" >> do
    s <- hoptional $ choice [Plus <$ symbol_ "+"
                           ,Minus <$ symbol_ "-"]
    lit <- singleQuotesOnlyStringTok
    q <- hoptional intervalQualifier
    mkIt s lit q)
  where
    mkIt Nothing val Nothing = pure $ TypedLit (TypeName [Name Nothing "interval"]) val
    mkIt s val (Just (a,b)) = pure $ IntervalLit s val a b
    mkIt (Just {}) _val Nothing = fail "cannot use sign without interval qualifier"

{-
== typed literal, app, special, aggregate, window, iden

All of these start with identifiers (some of the special functions
start with reserved keywords).

they are all variations on suffixes on the basic identifier parser

The windows is a suffix on the app parser

=== iden prefix term

all the scalar expressions which start with an identifier

(todo: really put all of them here instead of just some of them)
-}

idenExpr :: Parser ScalarExpr
idenExpr =
    -- todo: try reversing these
    -- then if it parses as a typename as part of a typed literal
    -- and not a regularapplike, then you'll get a better error message
    try typedLiteral <|> regularAppLike
  where
    -- parse regular iden or app
    -- if it could potentially be a typed literal typename 'literaltext'
    -- optionally try to parse that
    regularAppLike = do
        e <- (keywordFunctionOrIden
              <|> (names "identifier" <**> (hidden app <|> pure Iden)))
        let getInt s = readMaybe (T.unpack s)
        case e of
            Iden nm -> tryTypedLiteral (TypeName nm) <|> pure e
            App nm [NumLit prec]
                | Just prec' <- getInt prec ->
                tryTypedLiteral (PrecTypeName nm prec') <|> pure e
            App nm [NumLit prec,NumLit scale]
                | Just prec' <- getInt prec
                , Just scale' <- getInt scale ->
                tryTypedLiteral (PrecScaleTypeName nm prec' scale') <|> pure e
            _ -> pure e
    tryTypedLiteral tn =
        TypedLit tn <$> hidden singleQuotesOnlyStringTok
    typedLiteral =
        TypedLit <$> hidden (typeName' True) <*> singleQuotesOnlyStringTok
    keywordFunctionOrIden = do
        d <- askDialect id
        x <- hidden (keywordTok (diIdentifierKeywords d ++ diAppKeywords d))
        let i = T.toLower x `elem` diIdentifierKeywords d
            a = T.toLower x `elem` diAppKeywords d
        case () of
            _ | i && a -> pure [Name Nothing x] <**> (hidden app <|> pure Iden)
              | i -> pure (Iden [Name Nothing x])
              | a -> pure [Name Nothing x] <**> app
              | otherwise -> -- shouldn't get here
                    fail $ "unexpected keyword: " <> T.unpack x

{-
=== special

These are keyword operators which don't look like normal prefix,
postfix or infix binary operators. They mostly look like function
application but with keywords in the argument list instead of commas
to separate the arguments.

the special op keywords
parse an operator which is
operatorname(firstArg keyword0 arg0 keyword1 arg1 etc.)
-}

data SpecialOpKFirstArg = SOKNone
                        | SOKOptional
                        | SOKMandatory

specialOpK :: Text -- name of the operator
           -> SpecialOpKFirstArg -- has a first arg without a keyword
           -> [(Text,Bool)] -- the other args with their keywords
                              -- and whether they are optional
           -> Parser ScalarExpr
specialOpK opName firstArg kws =
    keyword_ opName >> do
    void openParen
    let pfa = do
              e <- scalarExpr
              -- check we haven't parsed the first
              -- keyword as an identifier
              case (e,kws) of
                  (Iden [Name Nothing i], (k,_):_)
                      | T.toLower i == k ->
                          fail $ "unexpected " ++ T.unpack i
                  _ -> pure ()
              pure e
    fa <- case firstArg of
         SOKNone -> pure Nothing
         SOKOptional -> optional (try pfa)
         SOKMandatory -> Just <$> pfa
    as <- mapM parseArg kws
    void closeParen
    pure $ SpecialOpK [Name Nothing opName] fa $ catMaybes as
  where
    parseArg (nm,mand) =
        let p = keyword_ nm >> scalarExpr
        in fmap (nm,) <$> if mand
                          then Just <$> p
                          else optional (try p)

{-
The actual operators:

EXTRACT( date_part FROM expression )

POSITION( string1 IN string2 )

SUBSTRING(extraction_string FROM starting_position [FOR length]
[COLLATE collation_name])

CONVERT(char_value USING conversion_char_name)

TRANSLATE(char_value USING translation_name)

OVERLAY(string PLACING embedded_string FROM start
[FOR length])

TRIM( [ [{LEADING | TRAILING | BOTH}] [removal_char] FROM ]
target_string
[COLLATE collation_name] )
-}

specialOpKs :: Parser ScalarExpr
specialOpKs = choice $ map try
    [extract, position, substring, convert, translate, overlay, trim]

extract :: Parser ScalarExpr
extract = specialOpK "extract" SOKMandatory [("from", True)]

position :: Parser ScalarExpr
position = specialOpK "position" SOKMandatory [("in", True)]

{-
strictly speaking, the substring must have at least one of from and
for, but the parser doens't enforce this
-}

substring :: Parser ScalarExpr
substring = specialOpK "substring" SOKMandatory
                [("from", False),("for", False)]

convert :: Parser ScalarExpr
convert = specialOpK "convert" SOKMandatory [("using", True)]


translate :: Parser ScalarExpr
translate = specialOpK "translate" SOKMandatory [("using", True)]

overlay :: Parser ScalarExpr
overlay = specialOpK "overlay" SOKMandatory
                [("placing", True),("from", True),("for", False)]

{-
trim is too different because of the optional char, so a custom parser
the both ' ' is filled in as the default if either parts are missing
in the source
-}

trim :: Parser ScalarExpr
trim =
    keyword "trim" >>
    parens (mkTrim
            <$> option "both" sides
            <*> option " " singleQuotesOnlyStringTok
            <*> (keyword_ "from" *> scalarExpr))
  where
    sides = choice ["leading" <$ keyword_ "leading"
                   ,"trailing" <$ keyword_ "trailing"
                   ,"both" <$ keyword_ "both"]
    mkTrim fa ch fr =
      SpecialOpK [Name Nothing "trim"] Nothing
          $ catMaybes [Just (fa,StringLit "'" "'" ch)
                      ,Just ("from", fr)]

{-
=== app, aggregate, window

This parses all these variations:
normal function application with just a csv of scalar exprs
aggregate variations (distinct, order by in parens, filter and where
  suffixes)
window apps (fn/agg followed by over)

This code is also a little dense like the typename code because of
left factoring, later they will even have to be partially combined
together.
-}

app :: Parser ([Name] -> ScalarExpr)
app =
    hidden openParen *> choice
    [hidden duplicates
     <**> (commaSep1 scalarExprOrStar
           <**> ((hoption [] orderBy <* closeParen)
                 <**> (hoptional afilter <**> pure (flip5 AggregateApp))))
     -- separate cases with no all or distinct which must have at
     -- least one scalar expr
    ,commaSep1 scalarExprOrStar
     <**> choice
          [closeParen *> hidden (choice
                         [window
                         ,withinGroup
                         ,(Just <$> afilter) <**> pure (flip3 aggAppWithoutDupeOrd)
                         ,pure (flip App)])
          ,hidden orderBy <* closeParen
           <**> (hoptional afilter <**> pure (flip4 aggAppWithoutDupe))]
     -- no scalarExprs: duplicates and order by not allowed
    ,([] <$ closeParen) <**> choice
                             [window
                             ,withinGroup
                             ,pure $ flip App]
    ]
  where
    aggAppWithoutDupeOrd n es f = AggregateApp n SQDefault es [] f
    aggAppWithoutDupe n = AggregateApp n SQDefault

afilter :: Parser ScalarExpr
afilter = keyword_ "filter" *> parens (keyword_ "where" *> scalarExpr)

withinGroup :: Parser ([ScalarExpr] -> [Name] -> ScalarExpr)
withinGroup =
    (keywords_ ["within", "group"] *> parens orderBy) <**> pure (flip3 AggregateAppGroup)

{-
==== window

parse a window call as a suffix of a regular function call
this looks like this:
functionname(args) over ([partition by ids] [order by orderitems])

No support for explicit frames yet.

TODO: add window support for other aggregate variations, needs some
changes to the syntax also
-}

window :: Parser ([ScalarExpr] -> [Name] -> ScalarExpr)
window =
  keyword_ "over" *> openParen *> option [] partitionBy
  <**> (option [] orderBy
        <**> ((optional frameClause <* closeParen) <**> pure (flip5 WindowApp)))
  where
    partitionBy =
        label "partition by" $
        keywords_ ["partition","by"] *> commaSep1 scalarExpr
    frameClause =
        label "frame clause" $
        frameRowsRange -- TODO: this 'and' could be an issue
        <**> choice [(keyword_ "between" *> frameLimit True)
                      <**> ((keyword_ "and" *> frameLimit True)
                            <**> pure (flip3 FrameBetween))
                      -- maybe this should still use a b expression
                      -- for consistency
                     ,frameLimit False <**> pure (flip FrameFrom)]
    frameRowsRange = FrameRows <$ keyword_ "rows"
                     <|> FrameRange <$ keyword_ "range"
    frameLimit useB =
        choice
        [Current <$ keywords_ ["current", "row"]
         -- todo: create an automatic left factor for stuff like this
        ,keyword_ "unbounded" *>
         choice [UnboundedPreceding <$ keyword_ "preceding"
                ,UnboundedFollowing <$ keyword_ "following"]
        ,(if useB then scalarExprB else scalarExpr)
         <**> (Preceding <$ keyword_ "preceding"
               <|> Following <$ keyword_ "following")
        ]

{-
== suffixes

These are all generic suffixes on any scalar expr

=== in

in: two variations:
a in (expr0, expr1, ...)
a in (queryexpr)
-}

inSuffix :: Parser (ScalarExpr -> ScalarExpr)
inSuffix =
    mkIn <$> inty
         <*> parens (choice
                     [InQueryExpr <$> queryExpr
                     ,InList <$> commaSep1 scalarExpr])
  where
    inty = choice [True <$ keyword_ "in"
                  ,False <$ keywords_ ["not","in"]]
    mkIn i v e = In i e v

{-
=== between

between:
expr between expr and expr

There is a complication when parsing between - when parsing the second
expression it is ambiguous when you hit an 'and' whether it is a
binary operator or part of the between. This code follows what
postgres does, which might be standard across SQL implementations,
which is that you can't have a binary and operator in the middle
expression in a between unless it is wrapped in parens. The 'bExpr
parsing' is used to create alternative scalar expression parser which
is identical to the normal one expect it doesn't recognise the binary
and operator. This is the call to scalarExprB.
-}

betweenSuffix :: Parser (ScalarExpr -> ScalarExpr)
betweenSuffix =
    makeOp . Name Nothing
           <$> opName
           <*> scalarExprB
           <*> (keyword_ "and" *> scalarExprB)
  where
    opName = choice
             ["between" <$ keyword_ "between"
             ,"not between" <$ try (keywords_ ["not","between"])]
    makeOp n b c a = SpecialOp [n] [a,b,c]

{-
=== quantified comparison

a = any (select * from t)
-}

quantifiedComparisonSuffix :: Parser (ScalarExpr -> ScalarExpr)
quantifiedComparisonSuffix = do
    c <- comp
    cq <- compQuan
    q <- parens queryExpr
    pure $ \v -> QuantifiedComparison v [c] cq q
  where
    comp = Name Nothing <$> choice (map symbol
           ["=", "<>", "<=", "<", ">", ">="])
    compQuan = choice
               [CPAny <$ keyword_ "any"
               ,CPSome <$ keyword_ "some"
               ,CPAll <$ keyword_ "all"]

{-
=== match

a match (select a from t)
-}

matchPredicateSuffix :: Parser (ScalarExpr -> ScalarExpr)
matchPredicateSuffix = do
    keyword_ "match"
    u <- option False (True <$ keyword_ "unique")
    q <- parens queryExpr
    pure $ \v -> Match v u q

-- === array subscript

arraySuffix :: Parser (ScalarExpr -> ScalarExpr)
arraySuffix = do
    es <- brackets (commaSep scalarExpr)
    pure $ \v -> Array v es

{-
=== escape

It is going to be really difficult to support an arbitrary character
for the escape now there is a separate lexer ...

TODO: this needs fixing. Escape is only part of other nodes, and not a
separate suffix.
-}

{-escapeSuffix :: Parser (ScalarExpr -> ScalarExpr)
escapeSuffix = do
    ctor <- choice
            [Escape <$ keyword_ "escape"
            ,UEscape <$ keyword_ "uescape"]
    c <- escapeChar
    pure $ \v -> ctor v c
  where
    escapeChar :: Parser Char
    escapeChar = (identifierTok [] Nothing <|> symbolTok Nothing) >>= oneOnly
    oneOnly :: String -> Parser Char
    oneOnly c = case c of
                   [c'] -> pure c'
                   _ -> fail "escape char must be single char"
-}

-- === collate

collateSuffix:: Parser (ScalarExpr -> ScalarExpr)
collateSuffix = do
    keyword_ "collate"
    i <- names "collation name"
    pure $ \v -> Collate v i

{-
== odbc syntax

the parser supports three kinds of odbc syntax, two of which are
scalar expressions (the other is a variation on joins)
-}


odbcExpr :: Parser ScalarExpr
odbcExpr =
    braces (odbcTimeLit <|> odbcFunc)
  where
    odbcTimeLit =
        OdbcLiteral <$> choice [OLDate <$ keyword "d"
                               ,OLTime <$ keyword "t"
                               ,OLTimestamp <$ keyword "ts"]
                    <*> singleQuotesOnlyStringTok
    -- todo: this parser is too general, the expr part
    -- should be only a function call (from a whitelist of functions)
    -- or the extract operator
    odbcFunc = OdbcFunc <$> (keyword "fn" *> scalarExpr)

{-
==  operators

The 'regular' operators in this parsing and in the abstract syntax are
unary prefix, unary postfix and binary infix operators. The operators
can be symbols (a + b), single keywords (a and b) or multiple keywords
(a is similar to b).

TODO: carefully review the precedences and associativities.

TODO: to fix the parsing completely, I think will need to parse
without precedence and associativity and fix up afterwards, since SQL
syntax is way too messy. It might be possible to avoid this if we
wanted to avoid extensibility and to not be concerned with parse error
messages, but both of these are too important.
-}

opTable :: Bool -> [[E.Operator Parser ScalarExpr]]
opTable bExpr =
        [-- parse match and quantified comparisons as postfix ops
          -- todo: left factor the quantified comparison with regular
          -- binary comparison, somehow
         [postfix $ try quantifiedComparisonSuffix
         ,postfix matchPredicateSuffix]

        ,[binarySymL "."]

        ,[postfix arraySuffix
         ,postfix collateSuffix]

        ,[prefixSym "+", prefixSym "-"]

        ,[binarySymL "^"]

        ,[binarySymL "*"
         ,binarySymL "/"
         ,binarySymL "%"]

        ,[binarySymL "+"
         ,binarySymL "-"]

        ,[binarySymR "||"
         ,prefixSym "~"
         ,binarySymR "&"
         ,binarySymR "|"]

        ,[binaryKeywordN "overlaps"]

        ,[binaryKeywordN "like"
         -- have to use try with inSuffix because of a conflict
         -- with 'in' in position function, and not between
         -- between also has a try in it to deal with 'not'
         -- ambiguity
         ,postfix $ try inSuffix
         ,postfix betweenSuffix]
         -- todo: figure out where to put the try?
         ++ [binaryKeywordsN $ makeKeywordTree
             ["not like"
             ,"is similar to"
             ,"is not similar to"]]
          ++ [multisetBinOp]

        ,[binarySymN "<"
         ,binarySymN ">"
         ,binarySymN ">="
         ,binarySymN "<="
         ,binarySymR "!="
         ,binarySymR "<>"
         ,binarySymR "="]

        ,[postfixKeywords $ makeKeywordTree
             ["is null"
             ,"is not null"
             ,"is true"
             ,"is not true"
             ,"is false"
             ,"is not false"
             ,"is unknown"
             ,"is not unknown"]]
         ++ [binaryKeywordsN $ makeKeywordTree
             ["is distinct from"
             ,"is not distinct from"]]

        ,[prefixKeyword "not"]

        ,[binaryKeywordL "and" | not bExpr]

        ,[binaryKeywordL "or"]

       ]
  where
    binarySymL nm = E.InfixL (hidden $ mkBinOp nm <$ symbol_ nm)
    binarySymR nm = E.InfixR (hidden $ mkBinOp nm <$ symbol_ nm)
    binarySymN nm = E.InfixN (hidden $ mkBinOp nm <$ symbol_ nm)
    binaryKeywordN nm = E.InfixN (hidden $ mkBinOp nm <$ keyword_ nm)
    binaryKeywordL nm = E.InfixL (hidden $ mkBinOp nm <$ keyword_ nm)
    mkBinOp nm a b = BinOp a (mkNm nm) b
    prefixSym  nm = prefix (hidden $ PrefixOp (mkNm nm) <$ symbol_ nm)
    prefixKeyword  nm = prefix (hidden $ PrefixOp (mkNm nm) <$ keyword_ nm)
    mkNm nm = [Name Nothing nm]
    binaryKeywordsN p =
        E.InfixN (hidden $ do
                 o <- try p
                 pure (\a b -> BinOp a [Name Nothing $ T.unwords o] b))
    multisetBinOp = E.InfixL (hidden $ do
        keyword_ "multiset"
        o <- choice [Union <$ keyword_ "union"
                    ,Intersect <$ keyword_ "intersect"
                    ,Except <$ keyword_ "except"]
        d <- hoption SQDefault duplicates
        pure (\a b -> MultisetBinOp a o d b))
    postfixKeywords p =
      postfix $ hidden $ do
          o <- try p
          pure $ PostfixOp [Name Nothing $ T.unwords o]
    -- parse repeated prefix or postfix operators
    postfix p = E.Postfix $ foldr1 (flip (.)) <$> some (hidden p)
    prefix p = E.Prefix $ foldr1 (.) <$> some (hidden p)

{-
== scalar expression top level

This parses most of the scalar exprs.The order of the parsers and use
of try is carefully done to make everything work. It is a little
fragile and could at least do with some heavy explanation. Update: the
'try's have migrated into the individual parsers, they still need
documenting/fixing.
-}

scalarExpr :: Parser ScalarExpr
scalarExpr = expressionLabel $ E.makeExprParser term (opTable False)

-- used when parsing contexts where a * or x.* is allowed
-- currently at the top level of a select item or top level of
-- argument passed to an app-like. This list may need to be extended.

scalarExprOrStar :: Parser ScalarExpr
scalarExprOrStar = star <|> scalarExpr

-- use this to get a nice unexpected keyword error which doesn't also
-- mangle other errors
expressionLabel :: Parser a -> Parser a
expressionLabel p = label "expression" p <|> failOnKeyword

term :: Parser ScalarExpr
term = expressionLabel $
    choice
    [simpleLiteral
    ,parameter
    ,positionalArg
    ,parensExpr
    ,caseExpr
    ,cast
    ,convertSqlServer
    ,arrayCtor
    ,multisetCtor
    ,nextValueFor
    ,subquery
    ,intervalLit
    ,specialOpKs
    ,idenExpr
    ,odbcExpr]

-- expose the b expression for window frame clause range between

scalarExprB :: Parser ScalarExpr
scalarExprB = label "expression" $ E.makeExprParser term (opTable True)

{-
== helper parsers

This is used in interval literals and in interval type names.
-}

intervalQualifier :: Parser (IntervalTypeField,Maybe IntervalTypeField)
intervalQualifier =
    (,) <$> intervalField
        <*> optional (keyword_ "to" *> intervalField)
  where
    intervalField =
        Itf
        <$> datetimeField
        <*> optional
            (parens ((,) <$> unsignedInteger
                         <*> optional (comma *> unsignedInteger)))

{-
TODO: use datetime field in extract also
use a data type for the datetime field?
-}

datetimeField :: Parser Text
datetimeField =
    choice (map keyword ["year","month","day"
                        ,"hour","minute","second"])
    <?> "datetime field"

{-
This is used in multiset operations (scalar expr), selects (query expr)
and set operations (query expr).
-}

duplicates :: Parser SetQuantifier
duplicates =
    choice [All <$ keyword_ "all"
           ,Distinct <$ keyword "distinct"]

{-
-------------------------------------------------

= query expressions

== select lists
-}

selectItem :: Parser (ScalarExpr,Maybe Name)
selectItem =
    label "select item" $ choice
    [(,Nothing) <$> star
    ,(,) <$> scalarExpr <*> optional als]
  where
    als = label "alias" $ optional (keyword_ "as") *> name "alias"

selectList :: Parser [(ScalarExpr,Maybe Name)]
selectList = commaSep1 selectItem

{-
== from

Here is the rough grammar for joins

tref
(cross | [natural] ([inner] | (left | right | full) [outer])) join
tref
[on expr | using (...)]

TODO: either use explicit 'operator precedence' parsers or build
expression parser for the 'tref operators' such as joins, lateral,
aliases.
-}

from :: Parser [TableRef]
from = label "from" (keyword_ "from" *> commaSep1 tref)
  where
    tref = nonJoinTref <**> (hidden (chainl tjoin) <|> pure id)
    nonJoinTref =
        label "table ref" $ choice
        [hidden $ parens $ choice
             [TRQueryExpr <$> queryExpr
             ,TRParens <$> tref]
        ,TRLateral <$> (hidden (keyword_ "lateral") *> nonJoinTref)
        ,do
         n <- names "table name"
         choice [TRFunction n <$> hidden (parens (commaSep scalarExpr))
                ,pure $ TRSimple n]
         -- todo: I think you can only have outer joins inside the oj,
         -- not sure.
        ,TROdbc <$> (hidden (braces (keyword_ "oj" *> tref)))
        ] <**> (talias <|> pure id)
    talias = fromAlias <**> pure (flip TRAlias)
    tjoin =
        (\jn jt tr1 jc tr0 -> TRJoin tr0 jn jt tr1 jc)
        <$> option False (True <$ keyword_ "natural")
        <*> joinType
        <*> nonJoinTref
        <*> hoptional joinCondition
    chainl p = foldr (.) id . reverse <$> some p


{-
TODO: factor the join stuff to produce better error messages (and make
it more readable)
-}

joinType :: Parser JoinType
joinType = choice
    [JCross <$ keyword_ "cross" <* keyword_ "join"
    ,JInner <$ keyword_ "inner" <* keyword_ "join"
    ,JLeft <$ keyword_ "left"
           <* optional (keyword_ "outer")
           <* keyword_ "join"
    ,JRight <$ keyword_ "right"
            <* optional (keyword_ "outer")
            <* keyword_ "join"
    ,JFull <$ keyword_ "full"
           <* optional (keyword_ "outer")
           <* keyword_ "join"
    ,JInner <$ keyword_ "join"]

joinCondition :: Parser JoinCondition
joinCondition = choice
    [keyword_ "on" >> JoinOn <$> scalarExpr
    ,keyword_ "using" >> JoinUsing <$> parens (commaSep1 (name "column name"))]

fromAlias :: Parser Alias
fromAlias = Alias <$> tableAlias <*> columnAliases
  where
    tableAlias = hoptional (keyword_ "as") *> name "alias"
    columnAliases = hoptional $ parens $ commaSep1 (name "column name")

{-
== simple other parts

Parsers for where, group by, having, order by and limit, which are
pretty trivial.
-}

whereClause :: Parser ScalarExpr
whereClause = label "where" (keyword_ "where" *> scalarExpr)

groupByClause :: Parser [GroupingExpr]
groupByClause =
    label "group by" (keywords_ ["group","by"] *> commaSep1 groupingExpression)
  where
    groupingExpression =
      label "grouping expression" $
      choice
      [keyword_ "cube" >>
       Cube <$> parens (commaSep groupingExpression)
      ,keyword_ "rollup" >>
       Rollup <$> parens (commaSep groupingExpression)
      ,GroupingParens <$> parens (commaSep groupingExpression)
      ,keywords_ ["grouping", "sets"] >>
       GroupingSets <$> parens (commaSep groupingExpression)
      ,SimpleGroup <$> scalarExpr
      ]

having :: Parser ScalarExpr
having = label "having" (keyword_ "having" *> scalarExpr)

orderBy :: Parser [SortSpec]
orderBy = label "order by" (keywords_ ["order","by"] *> commaSep1 ob)
  where
    ob = SortSpec
         <$> scalarExpr
         <*> hoption DirDefault (choice [Asc <$ keyword_ "asc"
                                       ,Desc <$ keyword_ "desc"])
         <*> hoption NullsOrderDefault
             -- todo: left factor better
             (keyword_ "nulls" >>
                    choice [NullsFirst <$ keyword "first"
                           ,NullsLast <$ keyword "last"])

{-
allows offset and fetch in either order
+ postgresql offset without row(s) and limit instead of fetch also
-}

offsetFetch :: Parser (Maybe ScalarExpr, Maybe ScalarExpr)
offsetFetch =
    P.runPermutation $ (,) <$> maybePermutation offset <*> maybePermutation fetch
  where
    maybePermutation p = P.toPermutationWithDefault Nothing (Just <$> p)

offset :: Parser ScalarExpr
offset = label "offset" (keyword_ "offset" *> scalarExpr
         <* option () (choice [keyword_ "rows"
                              ,keyword_ "row"]))

fetch :: Parser ScalarExpr
fetch = fetchFirst <|> limit
  where
    fetchFirst = guardDialect diFetchFirst
                 *> fs *> scalarExpr <* ro
    fs = makeKeywordTree ["fetch first", "fetch next"]
    ro = makeKeywordTree ["rows only", "row only"]
    -- todo: not in ansi sql dialect
    limit = guardDialect diLimit *>
            keyword_ "limit" *> scalarExpr

-- == common table expressions

with :: Parser QueryExpr
with = keyword_ "with" >>
    With <$> hoption False (True <$ keyword_ "recursive")
         <*> commaSep1 withQuery <*> queryExpr
  where
    withQuery = (,) <$> (withAlias <* keyword_ "as")
                    <*> parens queryExpr
    withAlias = Alias <$> name "alias" <*> columnAliases
    columnAliases = hoptional $ parens $ commaSep1 (name "column alias")


{-
== query expression

This parser parses any query expression variant: normal select, cte,
and union, etc..
-}

queryExpr :: Parser QueryExpr
queryExpr = label "query expr" $ E.makeExprParser qeterm qeOpTable
  where
    qeterm = label "query expr" (with <|> select <|> table <|> values)

    select = keyword_ "select" >>
        mkSelect
        <$> hoption SQDefault duplicates
        <*> selectList
        <*> optional tableExpression
    mkSelect d sl Nothing =
        toQueryExpr $ makeSelect {msSetQuantifier = d, msSelectList = sl}
    mkSelect d sl (Just (TableExpression f w g h od ofs fe)) =
        Select d sl f w g h od ofs fe
    values = keyword_ "values"
             >> Values <$> commaSep (parens (commaSep scalarExpr))
    table = keyword_ "table" >> Table <$> names "table name"

    qeOpTable =
        [[E.InfixL $ setOp Intersect "intersect"]
        ,[E.InfixL $ setOp Except "except"
         ,E.InfixL $ setOp Union "union"]]
    setOp :: SetOperatorName -> Text -> Parser (QueryExpr -> QueryExpr -> QueryExpr)
    setOp ctor opName = hidden (cq
        <$> (ctor <$ keyword_ opName)
        <*> hoption SQDefault duplicates
        <*> corr)
    cq o d c q0 q1 = QueryExprSetOp q0 o d c q1
    corr = hoption Respectively (Corresponding <$ keyword_ "corresponding")


{-
local data type to help with parsing the bit after the select list,
called 'table expression' in the ansi sql grammar. Maybe this should
be in the public syntax?
-}

data TableExpression
    = TableExpression
      {_teFrom :: [TableRef]
      ,_teWhere :: Maybe ScalarExpr
      ,_teGroupBy :: [GroupingExpr]
      ,_teHaving :: Maybe ScalarExpr
      ,_teOrderBy :: [SortSpec]
      ,_teOffset :: Maybe ScalarExpr
      ,_teFetchFirst :: Maybe ScalarExpr}

tableExpression :: Parser TableExpression
tableExpression =
    label "from" $
    mkTe
    <$> from
    <*> optional whereClause
    <*> option [] groupByClause
    <*> optional having
    <*> option [] orderBy
    <*> (hidden offsetFetch)
 where
    mkTe f w g h od (ofs,fe) =
        TableExpression f w g h od ofs fe

{-
wrapper for query expr which ignores optional trailing semicolon.

TODO: change style
-}

topLevelQueryExpr :: Parser QueryExpr
topLevelQueryExpr = queryExpr <??> (id <$ semi)

topLevelStatement :: Parser Statement
topLevelStatement = statement

{-
-------------------------

= Statements
-}

statementWithoutSemicolon :: Parser Statement
statementWithoutSemicolon =
    label "statement" $ choice
    [keyword_ "create" *> choice [createSchema
                                 ,createTable
                                 ,createIndex
                                 ,createView
                                 ,createDomain
                                 ,createSequence
                                 ,createRole
                                 ,createAssertion]
    ,keyword_ "alter" *> choice [alterTable
                                ,alterDomain
                                ,alterSequence]
    ,keyword_ "drop" *> choice [dropSchema
                               ,dropTable
                               ,dropView
                               ,dropDomain
                               ,dropSequence
                               ,dropRole
                               ,dropAssertion]
    ,delete
    ,truncateSt
    ,insert
    ,update
    ,startTransaction
    ,savepoint
    ,releaseSavepoint
    ,commit
    ,rollback
    ,grant
    ,revoke
    ,SelectStatement <$> queryExpr
    ]

statement :: Parser Statement
statement = statementWithoutSemicolon <* optional semi <|> EmptyStatement <$ hidden semi

createSchema :: Parser Statement
createSchema = keyword_ "schema" >>
    CreateSchema <$> names "schema name"

createTable :: Parser Statement
createTable = do
  d <- askDialect id
  let
    parseColumnDef = TableColumnDef <$> columnDef
    parseConstraintDef = uncurry TableConstraintDef <$> tableConstraintDef
    separator = if diNonCommaSeparatedConstraints d
      then optional comma
      else Just <$> comma
    constraints = sepBy parseConstraintDef (hidden separator)
    entries = ((:) <$> parseColumnDef <*> ((comma >> entries) <|> pure [])) <|> constraints
    withoutRowid = if diWithoutRowidTables d
      then fromMaybe False <$> optional (keywords_ ["without", "rowid"] >> pure True)
      else pure False

  keyword_ "table" >>
    CreateTable
    <$> names  "table name"
    <*> parens entries
    <*> withoutRowid

createIndex :: Parser Statement
createIndex =
    CreateIndex
    <$> ((keyword_ "index" >> pure False) <|>
         (keywords_ ["unique", "index"] >> pure True))
    <*> names "index name"
    <*> (keyword_ "on" >> names "table name")
    <*> parens (commaSep1 (name "column name"))

columnDef :: Parser ColumnDef
columnDef = do
  optionalType <- askDialect diOptionalColumnTypes
  ColumnDef <$> name "column name"
    <*> (if optionalType then optional typeName else Just <$> typeName)
    <*> optional defaultClause
    <*> option [] (some colConstraintDef)
  where
    defaultClause = label "column default clause" $ choice [
        keyword_ "default" >>
        DefaultClause <$> scalarExpr
        -- todo: left factor
       ,try (keywords_ ["generated","always","as"] >>
             GenerationClause <$> parens scalarExpr)
       ,keyword_ "generated" >>
        IdentityColumnSpec
        <$> (GeneratedAlways <$ keyword_ "always"
             <|> GeneratedByDefault <$ keywords_ ["by", "default"])
        <*> (keywords_ ["as", "identity"] *>
             option [] (parens sequenceGeneratorOptions))
       ]

tableConstraintDef :: Parser (Maybe [Name], TableConstraint)
tableConstraintDef =
    label "table constraint" $
    (,)
    <$> optional (keyword_ "constraint" *> names "constraint name")
    <*> (unique <|> primaryKey <|> check <|> references)
  where
    unique = keyword_ "unique" >>
        TableUniqueConstraint <$> parens (commaSep1 $ name "column name")
    primaryKey = keywords_ ["primary", "key"] >>
        TablePrimaryKeyConstraint <$> parens (commaSep1 $ name "column name")
    check = keyword_ "check" >> TableCheckConstraint <$> parens scalarExpr
    references = keywords_ ["foreign", "key"] >>
        (\cs ft ftcs m (u,d) -> TableReferencesConstraint cs ft ftcs m u d)
        <$> parens (commaSep1 $ name "column name")
        <*> (keyword_ "references" *> names "table name")
        <*> hoptional (parens $ commaSep1 $ name "column name")
        <*> refMatch
        <*> refActions

refMatch :: Parser ReferenceMatch
refMatch = hoption DefaultReferenceMatch
            (keyword_ "match" *>
             choice [MatchFull <$ keyword_ "full"
                    ,MatchPartial <$ keyword_ "partial"
                    ,MatchSimple <$ keyword_ "simple"])
refActions :: Parser (ReferentialAction,ReferentialAction)
refActions =
    P.runPermutation $ (,)
    <$> P.toPermutationWithDefault DefaultReferentialAction onUpdate
    <*> P.toPermutationWithDefault DefaultReferentialAction onDelete
  where
    -- todo: left factor?
    onUpdate = try (keywords_ ["on", "update"]) *> referentialAction
    onDelete = try (keywords_ ["on", "delete"]) *> referentialAction
    referentialAction = choice [
         RefCascade <$ keyword_ "cascade"
         -- todo: left factor?
        ,RefSetNull <$ try (keywords_ ["set", "null"])
        ,RefSetDefault <$ try (keywords_ ["set", "default"])
        ,RefRestrict <$ keyword_ "restrict"
        ,RefNoAction <$ keywords_ ["no", "action"]]

colConstraintDef :: Parser ColConstraintDef
colConstraintDef =
    ColConstraintDef
    <$> optional (keyword_ "constraint" *> names "constraint name")
    <*> (nullable <|> notNull <|> unique <|> primaryKey <|> check <|> references)
  where
    nullable = ColNullableConstraint <$ keyword "null"
    notNull = ColNotNullConstraint <$ keywords_ ["not", "null"]
    unique = ColUniqueConstraint <$ keyword_ "unique"
    primaryKey = do
      keywords_ ["primary", "key"]
      d <- askDialect id
      autoincrement <- if diAutoincrement d
        then optional (keyword_ "autoincrement")
        else pure Nothing
      pure $ ColPrimaryKeyConstraint $ isJust autoincrement
    check = keyword_ "check" >> ColCheckConstraint <$> parens scalarExpr
    references = keyword_ "references" >>
        (\t c m (ou,od) -> ColReferencesConstraint t c m ou od)
        <$> names "table name"
        <*> optional (parens $ name "column name")
        <*> refMatch
        <*> refActions

-- slightly hacky parser for signed integers

signedInteger :: Parser Integer
signedInteger =
    (*) <$> option 1 (1 <$ symbol "+" <|> (-1) <$ symbol "-")
    <*> unsignedInteger

sequenceGeneratorOptions :: Parser [SequenceGeneratorOption]
sequenceGeneratorOptions =
         -- todo: could try to combine exclusive options
         -- such as cycle and nocycle
         -- sort out options which are sometimes not allowed
         -- as datatype, and restart with
    P.runPermutation ((\a b c d e f g h j k -> catMaybes [a,b,c,d,e,f,g,h,j,k])
                  <$> maybePermutation startWith
                  <*> maybePermutation dataType
                  <*> maybePermutation restart
                  <*> maybePermutation incrementBy
                  <*> maybePermutation maxValue
                  <*> maybePermutation noMaxValue
                  <*> maybePermutation minValue
                  <*> maybePermutation noMinValue
                  <*> maybePermutation scycle
                  <*> maybePermutation noCycle
                 )
  where
    maybePermutation p = P.toPermutationWithDefault Nothing (Just <$> p)
    startWith = keywords_ ["start", "with"] >>
                SGOStartWith <$> signedInteger
    dataType = keyword_ "as" >>
               SGODataType <$> typeName
    restart = keyword_ "restart" >>
              SGORestart <$> optional (keyword_ "with" *> signedInteger)
    incrementBy = keywords_ ["increment", "by"] >>
                SGOIncrementBy <$> signedInteger
    maxValue = keyword_ "maxvalue" >>
                SGOMaxValue <$> signedInteger
    noMaxValue = SGONoMaxValue <$ try (keywords_ ["no","maxvalue"])
    minValue = keyword_ "minvalue" >>
                SGOMinValue <$> signedInteger
    noMinValue = SGONoMinValue <$ try (keywords_ ["no","minvalue"])
    scycle = SGOCycle <$ keyword_ "cycle"
    noCycle = SGONoCycle <$ try (keywords_ ["no","cycle"])


alterTable :: Parser Statement
alterTable = keyword_ "table" >>
    -- the choices have been ordered so that it works
    AlterTable <$> names "table name"
    <*> choice [addConstraint
               ,dropConstraint
               ,addColumnDef
               ,alterColumn
               ,dropColumn
               ]
  where
    addColumnDef = try (keyword_ "add"
                        *> optional (keyword_ "column")) >>
                   AddColumnDef <$> columnDef
    alterColumn = keyword_ "alter" >> optional (keyword_ "column") >>
                  name "column name"
                  <**> choice [setDefault
                              ,dropDefault
                              ,setNotNull
                              ,dropNotNull
                              ,setDataType]
    setDefault :: Parser (Name -> AlterTableAction)
    -- todo: left factor
    setDefault = try (keywords_ ["set","default"]) >>
                 scalarExpr <**> pure (flip AlterColumnSetDefault)
    dropDefault = AlterColumnDropDefault <$ try (keywords_ ["drop","default"])
    setNotNull = AlterColumnSetNotNull <$ try (keywords_ ["set","not","null"])
    dropNotNull = AlterColumnDropNotNull <$ try (keywords_ ["drop","not","null"])
    setDataType = try (keywords_ ["set","data","type"]) >>
                  typeName <**> pure (flip AlterColumnSetDataType)
    dropColumn = try (keyword_ "drop" *> optional (keyword_ "column")) >>
                 DropColumn <$> name "column name" <*> dropBehaviour
    -- todo: left factor, this try is especially bad
    addConstraint = try (keyword_ "add" >>
        uncurry AddTableConstraintDef <$> tableConstraintDef)
    dropConstraint = try (keywords_ ["drop","constraint"]) >>
        DropTableConstraintDef <$> names "constraint name" <*> dropBehaviour


dropSchema :: Parser Statement
dropSchema = keyword_ "schema" >>
    DropSchema <$> names "schema name" <*> dropBehaviour

dropTable :: Parser Statement
dropTable = keyword_ "table" >>
    DropTable <$> names "table name" <*> dropBehaviour

createView :: Parser Statement
createView =
    CreateView
    <$> (hoption False (True <$ keyword_ "recursive") <* keyword_ "view")
    <*> names "view name"
    <*> optional (parens (commaSep1 $ name "column name"))
    <*> (keyword_ "as" *> queryExpr)
    <*> hoptional (choice [
            -- todo: left factor
            DefaultCheckOption <$ try (keywords_ ["with", "check", "option"])
           ,CascadedCheckOption <$ try (keywords_ ["with", "cascaded", "check", "option"])
           ,LocalCheckOption <$ try (keywords_ ["with", "local", "check", "option"])
            ])

dropView :: Parser Statement
dropView = keyword_ "view" >>
    DropView <$> names "view name" <*> dropBehaviour

createDomain :: Parser Statement
createDomain = keyword_ "domain" >>
    CreateDomain
    <$> names "domain name"
    <*> ((optional (keyword_ "as") *> typeName) <?> "alias")
    <*> optional (keyword_ "default" *> scalarExpr)
    <*> many con
  where
    con = (,) <$> optional (keyword_ "constraint" *> names "constraint name")
          <*> (keyword_ "check" *> parens scalarExpr)

alterDomain :: Parser Statement
alterDomain = keyword_ "domain" >>
    AlterDomain
    <$> names "domain name"
    <*> (setDefault <|> constraint
         <|> (keyword_ "drop" *> (dropDefault <|> dropConstraint)))
  where
    setDefault = keywords_ ["set", "default"] >> ADSetDefault <$> scalarExpr
    constraint = keyword_ "add" >>
       ADAddConstraint
       <$> optional (keyword_ "constraint" *> names "constraint name")
       <*> (keyword_ "check" *> parens scalarExpr)
    dropDefault = ADDropDefault <$ keyword_ "default"
    dropConstraint = keyword_ "constraint" >> ADDropConstraint <$> names "constraint name"

dropDomain :: Parser Statement
dropDomain = keyword_ "domain" >>
    DropDomain <$> names "domain name" <*> dropBehaviour

createSequence :: Parser Statement
createSequence = keyword_ "sequence" >>
    CreateSequence
    <$> names "sequence name"
    <*> sequenceGeneratorOptions

alterSequence :: Parser Statement
alterSequence = keyword_ "sequence" >>
    AlterSequence
    <$> names "sequence name"
    <*> sequenceGeneratorOptions

dropSequence :: Parser Statement
dropSequence = keyword_ "sequence" >>
    DropSequence <$> names "sequence name" <*> dropBehaviour

createAssertion :: Parser Statement
createAssertion = keyword_ "assertion" >>
    CreateAssertion
    <$> names "assertion name"
    <*> (keyword_ "check" *> parens scalarExpr)


dropAssertion :: Parser Statement
dropAssertion = keyword_ "assertion" >>
    DropAssertion <$> names "assertion name" <*> dropBehaviour

{-
-----------------

= dml
-}

delete :: Parser Statement
delete = keywords_ ["delete","from"] >>
    Delete
    <$> names "table name"
    <*> optional (hoptional (keyword_ "as") *> name "alias")
    <*> optional (keyword_ "where" *> scalarExpr)

truncateSt :: Parser Statement
truncateSt = keywords_ ["truncate", "table"] >>
    Truncate
    <$> names "table name"
    <*> hoption DefaultIdentityRestart
        (ContinueIdentity <$ keywords_ ["continue","identity"]
         <|> RestartIdentity <$ keywords_ ["restart","identity"])

insert :: Parser Statement
insert = keywords_ ["insert", "into"] >>
    Insert
    <$> names "table name"
    <*> (hoptional (parens $ commaSep1 $ name "column name"))
    <*>
        -- slight hack
        (DefaultInsertValues <$ label "values" (keywords_ ["default", "values"])
         <|> InsertQuery <$> queryExpr)

update :: Parser Statement
update = keywords_ ["update"] >>
    Update
    <$> names "table name"
    <*> label "alias" (optional (optional (keyword_ "as") *> name "alias"))
    <*> (keyword_ "set" *> commaSep1 setClause)
    <*> optional (keyword_ "where" *> scalarExpr)
  where
    setClause = label "set clause" (multipleSet <|> singleSet)
    multipleSet = SetMultiple
                  <$> parens (commaSep1 $ names "column name")
                  <*> (symbol "=" *> parens (commaSep1 scalarExpr))
    singleSet = Set
                <$> names "column name"
                <*> (symbol "=" *> scalarExpr)

dropBehaviour :: Parser DropBehaviour
dropBehaviour =
    option DefaultDropBehaviour
    (Restrict <$ keyword_ "restrict"
    <|> Cascade <$ keyword_ "cascade")

{-
-----------------------------

= transaction management
-}

startTransaction :: Parser Statement
startTransaction = StartTransaction <$ keywords_ ["start","transaction"]

savepoint :: Parser Statement
savepoint = keyword_ "savepoint" >>
    Savepoint <$> name "savepoint name"

releaseSavepoint :: Parser Statement
releaseSavepoint = keywords_ ["release","savepoint"] >>
    ReleaseSavepoint <$> name "savepoint name"

commit :: Parser Statement
commit = Commit <$ keyword_ "commit" <* hoptional (keyword_ "work")

rollback :: Parser Statement
rollback = keyword_ "rollback" >> hoptional (keyword_ "work") >>
    Rollback <$> optional (keywords_ ["to", "savepoint"] *> name "savepoint name")


{-
------------------------------

= Access control

TODO: fix try at the 'on'
-}

grant :: Parser Statement
grant = keyword_ "grant" >> (try priv <|> role)
  where
    priv = GrantPrivilege
           <$> commaSep privilegeAction
           <*> (keyword_ "on" *> privilegeObject)
           <*> (keyword_ "to" *> commaSep (name "role name"))
           <*> option WithoutGrantOption
               (WithGrantOption <$ keywords_ ["with","grant","option"])
    role = GrantRole
           <$> commaSep (name "role name")
           <*> (keyword_ "to" *> commaSep (name "role name"))
           <*> option WithoutAdminOption
               (WithAdminOption <$ keywords_ ["with","admin","option"])

createRole :: Parser Statement
createRole = keyword_ "role" >>
    CreateRole <$> name "role name"

dropRole :: Parser Statement
dropRole = keyword_ "role" >>
    DropRole <$> name "role name"

-- TODO: fix try at the 'on'

revoke :: Parser Statement
revoke = keyword_ "revoke" >> (try priv <|> role)
  where
    priv = RevokePrivilege
           <$> option NoGrantOptionFor
               (GrantOptionFor <$ keywords_ ["grant","option","for"])
           <*> commaSep privilegeAction
           <*> (keyword_ "on" *> privilegeObject)
           <*> (keyword_ "from" *> commaSep (name "role name"))
           <*> dropBehaviour
    role = RevokeRole
           <$> option NoAdminOptionFor
               (AdminOptionFor <$ keywords_ ["admin","option", "for"])
           <*> commaSep (name "role name")
           <*> (keyword_ "from" *> commaSep (name "role name"))
           <*> dropBehaviour

privilegeAction :: Parser PrivilegeAction
privilegeAction = choice
    [PrivAll <$ keywords_ ["all","privileges"]
    ,keyword_ "select" >>
     PrivSelect <$> option [] (parens $ commaSep $ name "column name")
    ,PrivDelete <$ keyword_ "delete"
    ,PrivUsage <$ keyword_ "usage"
    ,PrivTrigger <$ keyword_ "trigger"
    ,PrivExecute <$ keyword_ "execute"
    ,keyword_ "insert" >>
     PrivInsert <$> option [] (parens $ commaSep $ name "column name")
    ,keyword_ "update" >>
     PrivUpdate <$> option [] (parens $ commaSep $ name "column name")
    ,keyword_ "references" >>
     PrivReferences <$> option [] (parens $ commaSep $ name "column name")
    ]

privilegeObject :: Parser PrivilegeObject
privilegeObject = choice
    [keyword_ "domain" >> PrivDomain <$> names "domain name"
    ,keyword_ "type" >> PrivType <$> names "type name"
    ,keyword_ "sequence" >> PrivSequence <$> names "sequence name"
    ,keywords_ ["specific","function"] >> PrivFunction <$> names "function name"
    ,optional (keyword_ "table") >> PrivTable <$> names "table name"
    ]


{-
----------------------------

wrapper to parse a series of statements. They must be separated by
semicolon, but for the last statement, the trailing semicolon is
optional.
-}

statements :: Parser [Statement]
statements = many statement

{-
----------------------------------------------

= multi keyword helper

This helper is to help parsing multiple options of multiple keywords
with similar prefixes, e.g. parsing 'is null' and 'is not null'.

use to left factor/ improve:
typed literal and general identifiers
not like, not in, not between operators
help with factoring keyword functions and other app-likes
the join keyword sequences
fetch first/next
row/rows only

There is probably a simpler way of doing this but I am a bit
thick.
-}

makeKeywordTree :: [Text] -> Parser [Text]
makeKeywordTree sets =
    label (T.intercalate ", " sets) $
    parseTrees (sort $ map T.words sets)
  where
    parseTrees :: [[Text]] -> Parser [Text]
    parseTrees ws = do
      let gs :: [[[Text]]]
          gs = groupBy ((==) `on` safeHead) ws
      choice $ map parseGroup gs
    parseGroup :: [[Text]] -> Parser [Text]
    parseGroup l@((k:_):_) = do
        keyword_ k
        let tls = mapMaybe safeTail l
            pr = (k:) <$> parseTrees tls
        if any null tls
          then pr <|> pure [k]
          else pr
    parseGroup _ = guard False >> fail "impossible"
    safeHead (x:_) = Just x
    safeHead [] = Nothing
    safeTail (_:x) = Just x
    safeTail [] = Nothing

------------------------------------------------------------------------------

-- parser helpers

{-
parses an optional postfix element and applies its result to its left
hand result, taken from uu-parsinglib

TODO: make sure the precedence higher than <|> and lower than the
other operators so it can be used nicely

TODO: this name is not so good because it's similar to <?> which does
something completely different
-}

(<??>) :: Parser a -> Parser (a -> a) -> Parser a
p <??> q = p <**> hoption id q

-- 0 to many repeated applications of suffix parser

chainrSuffix :: Parser a -> Parser (a -> a) -> Parser a
chainrSuffix p q = foldr ($) <$> p <*> (reverse <$> many (hidden q))

{-
These are to help with left factored parsers:

a <**> (b <**> (c <**> pure (flip3 ctor)))

Not sure the names are correct, but they follow a pattern with flip
a <**> (b <**> pure (flip ctor))
-}

flip3 :: (a -> b -> c -> t) -> c -> b -> a -> t
flip3 f a b c = f c b a

flip4 :: (a -> b -> c -> d -> t) -> d -> c -> b -> a -> t
flip4 f a b c d = f d c b a

flip5 :: (a -> b -> c -> d -> e -> t) -> e -> d -> c -> b -> a -> t
flip5 f a b c d e = f e d c b a

--------------------------------------

unsignedInteger :: Parser Integer
unsignedInteger = read . T.unpack <$> sqlNumberTok True <?> "natural number"

-- todo: work out the symbol parsing better

symbol :: Text -> Parser Text
symbol s = symbolTok (Just s) <?> s

singleCharSymbol :: Char -> Parser Char
singleCharSymbol c = c <$ symbol (T.singleton c)

questionMark :: Parser Char
questionMark = singleCharSymbol '?' <?> "question mark"

openParen :: Parser ()
openParen = void $ singleCharSymbol '('

closeParen :: Parser ()
closeParen = void $ singleCharSymbol ')'

comma :: Parser Char
comma = singleCharSymbol ','

semi :: Parser Char
semi = singleCharSymbol ';'

-- = helper functions

keyword :: Text -> Parser Text
keyword k = keywordTok [k] <?> k

-- helper function to improve error messages

keywords_ :: [Text] -> Parser ()
keywords_ ks = label (T.unwords ks) $ mapM_ keyword_ ks

parens :: Parser a -> Parser a
parens = between openParen closeParen

brackets :: Parser a -> Parser a
brackets = between (singleCharSymbol '[') (singleCharSymbol ']')

braces :: Parser a -> Parser a
braces = between (singleCharSymbol '{') (singleCharSymbol '}')

commaSep :: Parser a -> Parser [a]
commaSep = (`sepBy` hidden comma)

keyword_ :: Text -> Parser ()
keyword_ = void . keyword

symbol_ :: Text -> Parser ()
symbol_ = void . symbol

commaSep1 :: Parser a -> Parser [a]
commaSep1 = (`sepBy1` hidden comma)

hoptional :: Parser a -> Parser (Maybe a)
hoptional = hidden . optional

hoption :: a -> Parser a -> Parser a
hoption a p = hidden $ option a p

label :: Text -> Parser a -> Parser a
label x = M.label (T.unpack x)

(<?>) :: Parser a -> Text -> Parser a
(<?>) p a = (M.<?>) p (T.unpack a)

------------------------------------------------------------------------------

-- interfacing with the lexing
{-
TODO: push checks into here:
keyword blacklists
unsigned integer match
symbol matching
keyword matching

-}
stringTok :: Parser (Text,Text,Text)
stringTok = token test Set.empty <?> "string literal"
  where
    test (L.WithPos _ _ _ (L.SqlString s e t)) = Just (s,e,t)
    test _ = Nothing

singleQuotesOnlyStringTok :: Parser Text
singleQuotesOnlyStringTok = token test Set.empty <?> "string literal"
  where
    test (L.WithPos _ _ _ (L.SqlString "'" "'" t)) = Just t
    test _ = Nothing

{-
This is to support SQL strings where you can write
'part of a string' ' another part'
and it will parse as a single string

It is only allowed when all the strings are quoted with ' atm.

TODO: move this to the lexer?
-}

stringTokExtend :: Parser (Text,Text,Text)
stringTokExtend = do
    (s,e,x) <- stringTok
    choice [
         do
         guard (s == "'" && e == "'")
         (s',e',y) <- stringTokExtend
         guard (s' == "'" && e' == "'")
         pure (s,e,x <> y)
        ,pure (s,e,x)
        ]

hostParamTok :: Parser Text
hostParamTok = token test Set.empty <?> "host param"
  where
    test (L.WithPos _ _ _ (L.PrefixedVariable c p)) = Just $ T.cons c p
    test _ = Nothing

positionalArgTok :: Parser Int
positionalArgTok = token test Set.empty <?> "positional arg"
  where
    test (L.WithPos _ _ _ (L.PositionalArg p)) = Just p
    test _ = Nothing

sqlNumberTok :: Bool -> Parser Text
sqlNumberTok intOnly = token test Set.empty <?> "number"
  where
    test (L.WithPos _ _ _ (L.SqlNumber p)) | not intOnly || T.all isDigit p = Just p
    test _ = Nothing

symbolTok :: Maybe Text -> Parser Text
symbolTok sym = token test Set.empty <?> lbl
  where
    test (L.WithPos _ _ _ (L.Symbol p)) =
        case sym of
            Nothing -> Just p
            Just sym' | sym' == p -> Just p
            _ -> Nothing
    test _ = Nothing
    lbl = case sym of
              Nothing -> "symbol"
              Just p -> p

{-
The blacklisted names are mostly needed when we parse something with
an optional alias, e.g. select a a from t. If we write select a from
t, we have to make sure the from isn't parsed as an alias. I'm not
sure what other places strictly need the blacklist, and in theory it
could be tuned differently for each place the identifierString/
identifier parsers are used to only blacklist the bare
minimum. Something like this might be needed for dialect support, even
if it is pretty silly to use a keyword as an unquoted identifier when
there is a quoting syntax as well.

The standard has a weird mix of reserved keywords and unreserved
keywords (I'm not sure what exactly being an unreserved keyword
means).

The current approach tries to have everything which is a keyword only
in the keyword list - so it can only be used in some other context if
quoted. If something is a 'ansi keyword', but appears only as an
identifier or function name for instance in the syntax (or something
that looks identical to this), then it isn't treated as a keyword at
all. When there is some overlap (e.g. 'set'), then there is either
special case parsing code to handle this (in the case of set), or it
is not treated as a keyword (not perfect, but if it more or less
works, ok for now).

An exception to this is the standard type names are considered as
keywords at the moment, with a special case in the type parser to
make this work. Maybe this isn't necessary or is a bad idea.

It is possible to have a problem if you remove something which is a
keyword from this list, and still want to parse statements using it
as a keyword - for instance, removing things like 'from' or 'as',
will likely mean many things don't parse anymore.

-}

identifierTok :: [Text] -> Parser (Maybe (Text,Text), Text)
identifierTok blackList = do
    token test Set.empty <?> "identifier"
  where
    test (L.WithPos _ _ _ (L.Identifier q@(Just {}) p)) = Just (q,p)
    test (L.WithPos _ _ _ (L.Identifier q@Nothing p))
        | T.toLower p `notElem` blackList = Just (q,p)
    test _ = Nothing

keywordTok :: [Text] -> Parser Text
keywordTok allowed = do
    token test Set.empty  where
    test (L.WithPos _ _ _ (L.Identifier Nothing p))
        | T.toLower p `elem` allowed = Just p
    test _ = Nothing


unexpectedKeywordError :: Text -> Parser a
unexpectedKeywordError kw =
    failure (Just $ Label (NE.fromList $ T.unpack $ "keyword " <> kw)) Set.empty

failOnKeyword :: Parser a
failOnKeyword = do
    kws <- asks diKeywords
    i <- lookAhead $ keywordTok kws
    unexpectedKeywordError i

------------------------------------------------------------------------------

-- dialect

guardDialect :: (Dialect -> Bool) -> Parser ()
guardDialect p = guard . p =<< ask

askDialect :: (Dialect -> a) -> Parser a
askDialect = asks

