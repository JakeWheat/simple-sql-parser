
= TOC:

notes
Public api
Names - parsing identifiers
Typenames
Value expressions
  simple literals
  star, param
  parens expression, row constructor and scalar subquery
  case, cast, exists, unique, array/ multiset constructor
  typed literal, app, special function, aggregate, window function
  suffixes: in, between, quantified comparison, match predicate, array
    subscript, escape, collate
  operators
  value expression top level
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

== Parser rrror messages

A lot of care has been given to generating good parser error messages
for invalid syntax. There are a few utils below which partially help
in this area.

There is a set of crafted bad expressions in ErrorMessages.lhs, these
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
* value expressions which can start with an identifier
* infix and suffix operators

=== typenames

There are a number of variations of typename syntax. The standard
deals with this by switching on the name of the type which is parsed
first. This code doesn't do this currently, but might in the
future. Taking the approach in the standard grammar will limit the
extensibility of the parser and might affect the ease of adapting to
support other sql dialects.

=== identifier value expressions

There are a lot of value expression nodes which start with
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
parens comma separated value expressions or something similar, and it
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

value expressions: every variation on value expressions uses the same
parser/syntax. This means we don't try to stop non boolean valued
expressions in boolean valued contexts in the parser. Another area
this affects is that we allow general value expressions in group by,
whereas the standard only allows column names with optional collation.

These are all areas which are specified (roughly speaking) in the
syntax rather than the semantics in the standard, and we are not
fixing them in the syntax but leaving them till the semantic checking
(which doesn't exist in this code at this time).

> {-# LANGUAGE TupleSections #-}
> -- | This is the module with the parser functions.
> module Language.SQL.SimpleSQL.Parser
>     (parseQueryExpr
>     ,parseValueExpr
>     ,parseQueryExprs
>     ,ParseError(..)) where

> import Control.Monad.Identity (Identity)
> import Control.Monad (guard, void, when)
> import Control.Applicative ((<$), (<$>), (<*>) ,(<*), (*>))
> import Data.Maybe (fromMaybe,catMaybes)
> import Data.Char (toLower)
> import Text.Parsec (errorPos,sourceLine,sourceColumn,sourceName
>                    ,setPosition,setSourceColumn,setSourceLine,getPosition
>                    ,option,between,sepBy,sepBy1,string,manyTill,anyChar
>                    ,try,string,many1,oneOf,digit,(<|>),choice,char,eof
>                    ,optionMaybe,optional,many,letter,parse
>                    ,chainl1, (<?>),notFollowedBy,alphaNum, lookAhead)
> import Text.Parsec.String (Parser)
> import qualified Text.Parsec as P (ParseError)
> import Text.Parsec.Perm (permute,(<$?>), (<|?>))
> import qualified Text.Parsec.Expr as E
> import Data.List (intercalate,sort,groupBy)
> import Data.Function (on)
> import Language.SQL.SimpleSQL.Syntax

= Public API

> -- | Parses a query expr, trailing semicolon optional.
> parseQueryExpr :: FilePath
>                   -- ^ filename to use in errors
>                -> Maybe (Int,Int)
>                   -- ^ line number and column number of the first character
>                   -- in the source (to use in errors)
>                -> String
>                   -- ^ the SQL source to parse
>                -> Either ParseError QueryExpr
> parseQueryExpr = wrapParse topLevelQueryExpr

> -- | Parses a list of query expressions, with semi colons between
> -- them. The final semicolon is optional.
> parseQueryExprs :: FilePath
>                    -- ^ filename to use in errors
>                 -> Maybe (Int,Int)
>                    -- ^ line number and column number of the first character
>                    -- in the source (to use in errors)
>                 -> String
>                    -- ^ the SQL source to parse
>                 -> Either ParseError [QueryExpr]
> parseQueryExprs = wrapParse queryExprs

> -- | Parses a value expression.
> parseValueExpr :: FilePath
>                    -- ^ filename to use in errors
>                 -> Maybe (Int,Int)
>                    -- ^ line number and column number of the first character
>                    -- in the source (to use in errors)
>                 -> String
>                    -- ^ the SQL source to parse
>                 -> Either ParseError ValueExpr
> parseValueExpr = wrapParse valueExpr

This helper function takes the parser given and:

sets the position when parsing
automatically skips leading whitespace
checks the parser parses all the input using eof
converts the error return to the nice wrapper

> wrapParse :: Parser a
>           -> FilePath
>           -> Maybe (Int,Int)
>           -> String
>           -> Either ParseError a
> wrapParse parser f p src =
>     either (Left . convParseError src) Right
>     $ parse (setPos p *> whitespace *> parser <* eof) f src

> -- | Type to represent parse errors.
> data ParseError = ParseError
>                   {peErrorString :: String
>                    -- ^ contains the error message
>                   ,peFilename :: FilePath
>                    -- ^ filename location for the error
>                   ,pePosition :: (Int,Int)
>                    -- ^ line number and column number location for the error
>                   ,peFormattedError :: String
>                    -- ^ formatted error with the position, error
>                    -- message and source context
>                   } deriving (Eq,Show)

------------------------------------------------

= Names

Names represent identifiers and a few other things. The parser here
handles regular identifiers, dotten chain identifiers, quoted
identifiers and unicode quoted identifiers.

Dots: dots in identifier chains are parsed here and represented in the
Iden constructor usually. If parts of the chains are non identifier
value expressions, then this is represented by a BinOp "."
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

> name :: Parser Name
> name = choice [QName <$> quotedIdentifier
>               ,UQName <$> uquotedIdentifier
>               ,Name <$> identifierBlacklist blacklist]

> names :: Parser [Name]
> names = ((:[]) <$> name) >>= optionSuffix another
>   where
>     another n =
>         (((n++) . (:[])) <$> try (symbol "." *> name)) >>= optionSuffix another

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

TODO: this code needs heavy refactoring

> typeName :: Parser TypeName
> typeName =
>     (rowTypeName <|> intervalTypeName <|> ref <|> otherTypeName)
>     >>= tnSuffix
>     <?> "typename"
>   where
>     -- row type names - a little like create table
>     rowTypeName =
>         RowTypeName <$> (keyword_ "row" *> parens (commaSep1 rowField))
>     rowField = (,) <$> name <*> typeName
>     -- interval type names: interval a [to b]
>     intervalTypeName =
>         keyword_ "interval" >>
>         uncurry IntervalTypeName <$> intervalQualifier
>     ref =
>         keyword_ "ref" >>
>         RefTypeName
>         <$> parens (names)
>         <*> optionMaybe (keyword_ "scope" *> names)
>     -- other type names, which includes:
>     -- precision, scale, lob scale and units, timezone, character
>     -- set and collations
>     otherTypeName = do
>         tn <- (try reservedTypeNames <|> names)
>         choice [try $ timezone tn
>                ,try (precscale tn) >>= optionSuffix charSuffix
>                ,try $ lob tn
>                ,optionSuffix charSuffix $ TypeName tn]
>     timezone tn = do
>         TimeTypeName tn
>         <$> optionMaybe prec
>         <*> choice [True <$ keywords_ ["with", "time","zone"]
>                    ,False <$ keywords_ ["without", "time","zone"]]
>     charSuffix (PrecTypeName t p) = chars t (Just p)
>     charSuffix (TypeName t) = chars t Nothing
>     charSuffix _ = fail ""
>     chars tn p =
>         ((,) <$> option [] charSet
>              <*> option [] tcollate)
>           >>= uncurry mkit
>         where
>           mkit [] [] = fail ""
>           mkit a b = return $ CharTypeName tn p a b
>     lob tn = parens $ do
>         (x,y) <- lobPrecToken
>         z <- optionMaybe lobUnits
>         return $ LobTypeName tn x y z
>     precscale tn = parens (commaSep unsignedInteger) >>= makeWrap
>                    where
>                      makeWrap [a] = return $ PrecTypeName tn a
>                      makeWrap [a,b] = return $ PrecScaleTypeName tn a b
>                      makeWrap _ = fail "there must be one or two precision components"
>     prec = parens unsignedInteger
>     charSet = keywords_ ["character", "set"] *> names
>     tcollate = keyword_ "collate" *> names
>     lobPrecToken = lexeme $ do
>         p <- read <$> many1 digit <?> "unsigned integer"
>         x <- choice [Just LobK <$ keyword_ "k"
>                     ,Just LobM <$ keyword_ "m"
>                     ,Just LobG <$ keyword_ "g"
>                     ,return Nothing]
>         return (p,x)
>     lobUnits = choice [LobCharacters <$ keyword_ "characters"
>                       ,LobCodeUnits <$ keyword_ "code_units"
>                       ,LobOctets <$ keyword_ "octets"]
>     -- deal with multiset and array suffixes
>     tnSuffix x =
>         multisetSuffix x <|> arrayTNSuffix x <|> return x
>     multisetSuffix x =
>         (MultisetTypeName x <$ keyword_ "multiset") >>= tnSuffix
>     arrayTNSuffix x =
>         (keyword_ "array" >> ArrayTypeName x
>                              <$> optionMaybe (brackets unsignedInteger)
>         ) >>= tnSuffix
>     -- this parser handles the fixed set of multi word
>     -- type names, plus all the type names which are
>     -- reserved words
>     reservedTypeNames = (:[]) . Name . unwords <$> makeKeywordTree
>         ["double precision"
>         ,"character varying"
>         ,"char varying"
>         ,"character large object"
>         ,"char large object"
>         ,"national character"
>         ,"national char"
>         ,"national character varying"
>         ,"national char varying"
>         ,"national character large object"
>         ,"nchar large object"
>         ,"nchar varying"
>         ,"bit varying"
>         ,"binary large object"
>         -- reserved keyword typenames:
>         ,"array"
>         ,"bigint"
>         ,"binary"
>         ,"blob"
>         ,"boolean"
>         ,"char"
>         ,"character"
>         ,"clob"
>         ,"date"
>         ,"dec"
>         ,"decimal"
>         ,"double"
>         ,"float"
>         ,"int"
>         ,"integer"
>         ,"nchar"
>         ,"nclob"
>         ,"numeric"
>         ,"real"
>         ,"smallint"
>         ,"time"
>         ,"timestamp"
>         ,"varchar"
>         ]

= Value expressions

== simple literals

See the stringToken lexer below for notes on string literal syntax.

> stringLit :: Parser ValueExpr
> stringLit = StringLit <$> stringToken

> numberLit :: Parser ValueExpr
> numberLit = NumLit <$> numberLiteral

> characterSetLit :: Parser ValueExpr
> characterSetLit =
>     CSStringLit <$> shortCSPrefix <*> stringToken
>   where
>     shortCSPrefix = try $ choice
>         [(:[]) <$> oneOf "nNbBxX"
>         ,string "u&"
>         ,string "U&"
>         ] <* lookAhead quote

> simpleLiteral :: Parser ValueExpr
> simpleLiteral = numberLit <|> stringLit <|> characterSetLit

== star, param, host param

=== star

used in select *, select x.*, and agg(*) variations, and some other
places as well. The parser doesn't attempt to check that the star is
in a valid context, it parses it OK in any value expression context.

> star :: Parser ValueExpr
> star = Star <$ symbol "*"

== parameter

unnamed parameter or named parameter
use in e.g. select * from t where a = ?
select x from t where x > :param

> parameter :: Parser ValueExpr
> parameter = choice
>     [Parameter <$ questionMark
>     ,HostParameter
>      <$> hostParameterToken
>      <*> optionMaybe (keyword "indicator" *> hostParameterToken)]

== parens

value expression parens, row ctor and scalar subquery

> parensExpr :: Parser ValueExpr
> parensExpr = parens $ choice
>     [SubQueryExpr SqSq <$> queryExpr
>     ,ctor <$> commaSep1 valueExpr]
>   where
>     ctor [a] = Parens a
>     ctor as = SpecialOp [Name "rowctor"] as

== case, cast, exists, unique, array/multiset constructor, interval

All of these start with a fixed keyword which is reserved, so no other
syntax can start with the same keyword.

=== case expression

> caseExpr :: Parser ValueExpr
> caseExpr =
>     Case <$> (keyword_ "case" *> optionMaybe valueExpr)
>          <*> many1 whenClause
>          <*> optionMaybe elseClause
>          <* keyword_ "end"
>   where
>    whenClause = (,) <$> (keyword_ "when" *> commaSep1 valueExpr)
>                     <*> (keyword_ "then" *> valueExpr)
>    elseClause = keyword_ "else" *> valueExpr

=== cast

cast: cast(expr as type)

> cast :: Parser ValueExpr
> cast = keyword_ "cast" >>
>        parens (Cast <$> valueExpr
>                     <*> (keyword_ "as" *> typeName))

=== exists, unique

subquery expression:
[exists|unique] (queryexpr)

> subquery :: Parser ValueExpr
> subquery = SubQueryExpr <$> sqkw <*> parens queryExpr
>   where
>     sqkw = choice
>            [SqExists <$ keyword_ "exists"
>            ,SqUnique <$ keyword_ "unique"]

=== array/multiset constructor

> arrayCtor :: Parser ValueExpr
> arrayCtor = keyword_ "array" >>
>     choice
>     [ArrayCtor <$> parens queryExpr
>     ,Array (Iden [Name "array"]) <$> brackets (commaSep valueExpr)]

As far as I can tell, table(query expr) is just syntax sugar for
multiset(query expr). It must be there for compatibility or something.

> multisetCtor :: Parser ValueExpr
> multisetCtor =
>     choice
>     [keyword_ "multiset" >>
>      choice
>      [MultisetQueryCtor <$> parens queryExpr
>      ,MultisetCtor <$> brackets (commaSep valueExpr)]
>     ,keyword_ "table" >>
>      MultisetQueryCtor <$> parens queryExpr]

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
this

> intervalLit :: Parser ValueExpr
> intervalLit = try (keyword_ "interval" >> do
>     s <- optionMaybe $ choice [True <$ symbol_ "+"
>                               ,False <$ symbol_ "-"]
>     lit <- stringToken
>     q <- optionMaybe intervalQualifier
>     mkIt s lit q)
>   where
>     mkIt Nothing val Nothing = return $ TypedLit (TypeName [Name "interval"]) val
>     mkIt s val (Just (a,b)) = return $ IntervalLit s val a b
>     mkIt (Just {}) _val Nothing = fail "cannot use sign without interval qualifier"

== typed literal, app, special, aggregate, window, iden

All of these start with identifiers (some of the special functions
start with reserved keywords).

they are all variations on suffixes on the basic identifier parser

The windows is a suffix on the app parser

=== iden prefix term

all the value expressions which start with an identifier

(todo: really put all of them here instead of just some of them)

> idenExpr :: Parser ValueExpr
> idenExpr =
>     -- todo: work out how to left factor this
>     try (TypedLit <$> typeName <*> stringToken)
>     <|> (names >>= iden)
>   where
>     iden n = app n <|> return (Iden n)

=== special

These are keyword operators which don't look like normal prefix,
postfix or infix binary operators. They mostly look like function
application but with keywords in the argument list instead of commas
to separate the arguments.

the special op keywords
parse an operator which is
operatorname(firstArg keyword0 arg0 keyword1 arg1 etc.)

> data SpecialOpKFirstArg = SOKNone
>                         | SOKOptional
>                         | SOKMandatory

> specialOpK :: String -- name of the operator
>            -> SpecialOpKFirstArg -- has a first arg without a keyword
>            -> [(String,Bool)] -- the other args with their keywords
>                               -- and whether they are optional
>            -> Parser ValueExpr
> specialOpK opName firstArg kws =
>     keyword_ opName >> do
>     void openParen
>     let pfa = do
>               e <- valueExpr
>               -- check we haven't parsed the first
>               -- keyword as an identifier
>               guard (case (e,kws) of
>                   (Iden [Name i], (k,_):_) | map toLower i == k -> False
>                   _ -> True)
>               return e
>     fa <- case firstArg of
>          SOKNone -> return Nothing
>          SOKOptional -> optionMaybe (try pfa)
>          SOKMandatory -> Just <$> pfa
>     as <- mapM parseArg kws
>     void closeParen
>     return $ SpecialOpK [Name opName] fa $ catMaybes as
>   where
>     parseArg (nm,mand) =
>         let p = keyword_ nm >> valueExpr
>         in fmap (nm,) <$> if mand
>                           then Just <$> p
>                           else optionMaybe (try p)

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

> specialOpKs :: Parser ValueExpr
> specialOpKs = choice $ map try
>     [extract, position, substring, convert, translate, overlay, trim]

> extract :: Parser ValueExpr
> extract = specialOpK "extract" SOKMandatory [("from", True)]

> position :: Parser ValueExpr
> position = specialOpK "position" SOKMandatory [("in", True)]

strictly speaking, the substring must have at least one of from and
for, but the parser doens't enforce this

> substring :: Parser ValueExpr
> substring = specialOpK "substring" SOKMandatory
>                 [("from", False),("for", False)]

> convert :: Parser ValueExpr
> convert = specialOpK "convert" SOKMandatory [("using", True)]


> translate :: Parser ValueExpr
> translate = specialOpK "translate" SOKMandatory [("using", True)]

> overlay :: Parser ValueExpr
> overlay = specialOpK "overlay" SOKMandatory
>                 [("placing", True),("from", True),("for", False)]

trim is too different because of the optional char, so a custom parser
the both ' ' is filled in as the default if either parts are missing
in the source

> trim :: Parser ValueExpr
> trim =
>     keyword "trim" >>
>     parens (mkTrim
>             <$> option "both" sides
>             <*> option " " stringToken
>             <*> (keyword_ "from" *> valueExpr))
>   where
>     sides = choice ["leading" <$ keyword_ "leading"
>                    ,"trailing" <$ keyword_ "trailing"
>                    ,"both" <$ keyword_ "both"]
>     mkTrim fa ch fr =
>       SpecialOpK [Name "trim"] Nothing
>           $ catMaybes [Just (fa,StringLit ch)
>                       ,Just ("from", fr)]


=== app, aggregate, window

this represents anything which syntactically looks like regular C
function application: an identifier, parens with comma sep value
expression arguments.

The parsing for the aggregate extensions is here as well:

aggregate([all|distinct] args [order by orderitems])

TODO: try to refactor the parser to not allow distinct/all or order by
if there are no value exprs

> aggOrApp :: [Name] -> Parser ValueExpr
> aggOrApp n =
>     makeApp n
>     <$> parens ((,,) <$> (fromMaybe SQDefault <$> duplicates)
>                      <*> choice [commaSep valueExpr]
>                      <*> (optionMaybe orderBy))
>   where
>     makeApp i (SQDefault,es,Nothing) = App i es
>     makeApp i (d,es,od) = AggregateApp i d es (fromMaybe [] od) Nothing

> app :: [Name] -> Parser ValueExpr
> app n = aggOrApp n >>= \a -> choice
>         [windowSuffix a
>         ,filterSuffix a
>         ,withinGroupSuffix a
>         ,return a]

> filterSuffix :: ValueExpr -> Parser ValueExpr
> filterSuffix (App nm es) =
>     filterSuffix (AggregateApp nm SQDefault es [] Nothing)
> filterSuffix agg@(AggregateApp {}) =
>     filterSuffix' agg
> filterSuffix _ = fail ""

> filterSuffix' :: ValueExpr -> Parser ValueExpr
> filterSuffix' agg =
>     keyword_ "filter" >>
>     rep <$> parens(keyword_ "where" *> (Just <$> valueExpr))
>   where
>     rep f = agg {aggFilter = f}



> withinGroupSuffix :: ValueExpr -> Parser ValueExpr
> withinGroupSuffix (App nm es) = keywords_ ["within", "group"] >>
>     AggregateAppGroup nm es <$> parens orderBy
> withinGroupSuffix _ = fail ""

==== window suffix

parse a window call as a suffix of a regular function call
this looks like this:
functionname(args) over ([partition by ids] [order by orderitems])

No support for explicit frames yet.

The convention in this file is that the 'Suffix', erm, suffix on
parser names means that they have been left factored. These are almost
always used with the optionSuffix combinator.

> windowSuffix :: ValueExpr -> Parser ValueExpr
> windowSuffix (App f es) =
>     keyword_ "over"
>     *> parens (WindowApp f es
>                <$> option [] partitionBy
>                <*> option [] orderBy
>                <*> optionMaybe frameClause)
>   where
>     partitionBy = keywords_ ["partition","by"] >> commaSep1 valueExpr
>     frameClause =
>         mkFrame <$> choice [FrameRows <$ keyword_ "rows"
>                            ,FrameRange <$ keyword_ "range"]
>                 <*> frameStartEnd
>     frameStartEnd =
>         choice
>         [keyword_ "between" >>
>          mkFrameBetween <$> frameLimit True
>                         <*> (keyword_ "and" *> frameLimit True)
>         ,mkFrameFrom <$> frameLimit False]
>     -- use the bexpression style from the between parsing for frame between
>     frameLimit useB =
>         choice
>         [Current <$ keywords_ ["current", "row"]
>          -- todo: create an automatic left factor for stuff like
>          -- this
>         ,keyword_ "unbounded" >>
>          choice [UnboundedPreceding <$ keyword_ "preceding"
>                 ,UnboundedFollowing <$ keyword_ "following"]
>         ,do
>          e <- if useB then valueExprB else valueExpr
>          choice [Preceding e <$ keyword_ "preceding"
>                 ,Following e <$ keyword_ "following"]
>         ]
>     mkFrameBetween s e rs = FrameBetween rs s e
>     mkFrameFrom s rs = FrameFrom rs s
>     mkFrame rs c = c rs
> windowSuffix _ = fail ""



== suffixes

These are all generic suffixes on any value expr

=== in

in: two variations:
a in (expr0, expr1, ...)
a in (queryexpr)

this is parsed as a postfix operator which is why it is in this form

> inSuffix :: Parser (ValueExpr -> ValueExpr)
> inSuffix =
>     mkIn <$> inty
>          <*> parens (choice
>                      [InQueryExpr <$> queryExpr
>                      ,InList <$> commaSep1 valueExpr])
>   where
>     inty = choice [True <$ keyword_ "in"
>                   ,False <$ keywords_ ["not","in"]]
>     mkIn i v = \e -> In i e v

=== between

between:
expr between expr and expr

There is a complication when parsing between - when parsing the second
expression it is ambiguous when you hit an 'and' whether it is a
binary operator or part of the between. This code follows what
postgres does, which might be standard across SQL implementations,
which is that you can't have a binary and operator in the middle
expression in a between unless it is wrapped in parens. The 'bExpr
parsing' is used to create alternative value expression parser which
is identical to the normal one expect it doesn't recognise the binary
and operator. This is the call to valueExprB.

> betweenSuffix :: Parser (ValueExpr -> ValueExpr)
> betweenSuffix =
>     makeOp <$> Name <$> opName
>            <*> valueExprB
>            <*> (keyword_ "and" *> valueExprB)
>   where
>     opName = choice
>              ["between" <$ keyword_ "between"
>              ,"not between" <$ try (keywords_ ["not","between"])]
>     makeOp n b c = \a -> SpecialOp [n] [a,b,c]

=== quantified comparison

a = any (select * from t)

> quantifiedComparisonSuffix :: Parser (ValueExpr -> ValueExpr)
> quantifiedComparisonSuffix = do
>     c <- comp
>     cq <- compQuan
>     q <- parens queryExpr
>     return $ \v -> QuantifiedComparison v [c] cq q
>   where
>     comp = Name <$> choice (map symbol
>            ["=", "<>", "<=", "<", ">", ">="])
>     compQuan = choice
>                [CPAny <$ keyword_ "any"
>                ,CPSome <$ keyword_ "some"
>                ,CPAll <$ keyword_ "all"]

=== match

a match (select a from t)

> matchPredicateSuffix :: Parser (ValueExpr -> ValueExpr)
> matchPredicateSuffix = do
>     keyword_ "match"
>     u <- option False (True <$ keyword_ "unique")
>     q <- parens queryExpr
>     return $ \v -> Match v u q

=== array subscript

> arraySuffix :: Parser (ValueExpr -> ValueExpr)
> arraySuffix = do
>     es <- brackets (commaSep valueExpr)
>     return $ \v -> Array v es

=== escape

> escapeSuffix :: Parser (ValueExpr -> ValueExpr)
> escapeSuffix = do
>     ctor <- choice
>             [Escape <$ keyword_ "escape"
>             ,UEscape <$ keyword_ "uescape"]
>     c <- anyChar
>     return $ \v -> ctor v c

=== collate

> collateSuffix:: Parser (ValueExpr -> ValueExpr)
> collateSuffix = do
>     keyword_ "collate"
>     i <- names
>     return $ \v -> Collate v i


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

> opTable :: Bool -> [[E.Operator String () Identity ValueExpr]]
> opTable bExpr =
>         [-- parse match and quantified comparisons as postfix ops
>           -- todo: left factor the quantified comparison with regular
>           -- binary comparison, somehow
>          [E.Postfix $ try quantifiedComparisonSuffix
>          ,E.Postfix matchPredicateSuffix
>          ]
>         ,[binarySym "." E.AssocLeft]
>         ,[postfix' arraySuffix
>          ,postfix' escapeSuffix
>          ,postfix' collateSuffix]
>         ,[prefixSym "+", prefixSym "-"]
>         ,[binarySym "^" E.AssocLeft]
>         ,[binarySym "*" E.AssocLeft
>          ,binarySym "/" E.AssocLeft
>          ,binarySym "%" E.AssocLeft]
>         ,[binarySym "+" E.AssocLeft
>          ,binarySym "-" E.AssocLeft]
>         ,[binarySym ">=" E.AssocNone
>          ,binarySym "<=" E.AssocNone
>          ,binarySym "!=" E.AssocRight
>          ,binarySym "<>" E.AssocRight
>          ,binarySym "||" E.AssocRight
>          ,prefixSym "~"
>          ,binarySym "&" E.AssocRight
>          ,binarySym "|" E.AssocRight
>          ,binaryKeyword "like" E.AssocNone
>          ,binaryKeyword "overlaps" E.AssocNone]
>          ++ [binaryKeywords $ makeKeywordTree
>              ["not like"
>              ,"is similar to"
>              ,"is not similar to"
>              ,"is distinct from"
>              ,"is not distinct from"]
>             ,postfixKeywords $ makeKeywordTree
>              ["is null"
>              ,"is not null"
>              ,"is true"
>              ,"is not true"
>              ,"is false"
>              ,"is not false"
>              ,"is unknown"
>              ,"is not unknown"]
>             ]
>          ++ [multisetBinOp]
>          -- have to use try with inSuffix because of a conflict
>          -- with 'in' in position function, and not between
>          -- between also has a try in it to deal with 'not'
>          -- ambiguity
>           ++ [E.Postfix $ try inSuffix,E.Postfix betweenSuffix]
>         ]
>         ++
>         [[binarySym "<" E.AssocNone
>          ,binarySym ">" E.AssocNone]
>         ,[binarySym "=" E.AssocRight]
>         ,[prefixKeyword "not"]]
>         ++
>         if bExpr then [] else [[binaryKeyword "and" E.AssocLeft]]
>         ++
>         [[binaryKeyword "or" E.AssocLeft]]
>   where
>     binarySym nm assoc = binary (symbol_ nm) nm assoc
>     binaryKeyword nm assoc = binary (keyword_ nm) nm assoc
>     binaryKeywords p =
>         E.Infix (do
>             o <- try p
>             return (\a b -> BinOp a [Name $ unwords o] b))
>             E.AssocNone
>     postfixKeywords p =
>       postfix' $ do
>           o <- try p
>           return $ PostfixOp [Name $ unwords o]
>     binary p nm assoc =
>       E.Infix (p >> return (\a b -> BinOp a [Name nm] b)) assoc
>     multisetBinOp = E.Infix (do
>         keyword_ "multiset"
>         o <- choice [Union <$ keyword_ "union"
>                     ,Intersect <$ keyword_ "intersect"
>                     ,Except <$ keyword_ "except"]
>         d <- fromMaybe SQDefault <$> duplicates
>         return (\a b -> MultisetBinOp a o d b))
>           E.AssocLeft
>     prefixKeyword nm = prefix (keyword_ nm) nm
>     prefixSym nm = prefix (symbol_ nm) nm
>     prefix p nm = prefix' (p >> return (PrefixOp [Name nm]))
>     -- hack from here
>     -- http://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported
>     -- not implemented properly yet
>     -- I don't think this will be enough for all cases
>     -- at least it works for 'not not a'
>     -- ok: "x is not true is not true"
>     -- no work: "x is not true is not null"
>     prefix'  p = E.Prefix  . chainl1 p $ return       (.)
>     postfix' p = E.Postfix . chainl1 p $ return (flip (.))

== value expression top level

This parses most of the value exprs.The order of the parsers and use
of try is carefully done to make everything work. It is a little
fragile and could at least do with some heavy explanation.

> valueExpr :: Parser ValueExpr
> valueExpr = E.buildExpressionParser (opTable False) term

> term :: Parser ValueExpr
> term = choice [simpleLiteral
>               ,parameter
>               ,star
>               ,parensExpr
>               ,caseExpr
>               ,cast
>               ,arrayCtor
>               ,multisetCtor
>               ,subquery
>               ,intervalLit
>               ,specialOpKs
>               ,idenExpr]
>        <?> "value expression"

expose the b expression for window frame clause range between

> valueExprB :: Parser ValueExpr
> valueExprB = E.buildExpressionParser (opTable True) term

== helper parsers

This is used in interval literals and in interval type names.

> intervalQualifier :: Parser (IntervalTypeField,Maybe IntervalTypeField)
> intervalQualifier =
>     (,) <$> intervalField
>         <*> optionMaybe (keyword_ "to" *> intervalField)
>   where
>     intervalField =
>         Itf
>         <$> datetimeField
>         <*> optionMaybe
>             (parens ((,) <$> unsignedInteger
>                          <*> optionMaybe (comma *> unsignedInteger)))

TODO: use datetime field in extract also
use a data type for the datetime field?

> datetimeField :: Parser String
> datetimeField = choice (map keyword ["year","month","day"
>                                     ,"hour","minute","second"])
>                 <?> "datetime field"

This is used in multiset operations (value expr), selects (query expr)
and set operations (query expr).

> duplicates :: Parser (Maybe SetQuantifier)
> duplicates = optionMaybe $
>     choice [All <$ keyword_ "all"
>            ,Distinct <$ keyword "distinct"]

-------------------------------------------------

= query expressions

== select lists

> selectItem :: Parser (ValueExpr,Maybe Name)
> selectItem = (,) <$> valueExpr <*> optionMaybe als
>   where als = optional (keyword_ "as") *> name

> selectList :: Parser [(ValueExpr,Maybe Name)]
> selectList = commaSep1 selectItem

== from

Here is the rough grammar for joins

tref
(cross | [natural] ([inner] | (left | right | full) [outer])) join
tref
[on expr | using (...)]

> from :: Parser [TableRef]
> from = keyword_ "from" *> commaSep1 tref
>   where
>     tref = nonJoinTref >>= optionSuffix joinTrefSuffix
>     nonJoinTref = choice
>         [parens $ choice
>              [TRQueryExpr <$> queryExpr
>              ,TRParens <$> tref]
>         ,TRLateral <$> (keyword_ "lateral"
>                         *> nonJoinTref)
>         ,do
>          n <- names
>          choice [TRFunction n
>                  <$> parens (commaSep valueExpr)
>                 ,return $ TRSimple n]]
>         >>= optionSuffix aliasSuffix
>     aliasSuffix j = option j (TRAlias j <$> fromAlias)
>     joinTrefSuffix t =
>         (TRJoin t <$> option False (True <$ keyword_ "natural")
>                   <*> joinType
>                   <*> nonJoinTref
>                   <*> optionMaybe joinCondition)
>         >>= optionSuffix joinTrefSuffix

TODO: factor the join stuff to produce better error messages (and make
it more readable)

> joinType :: Parser JoinType
> joinType = choice
>     [JCross <$ keyword_ "cross" <* keyword_ "join"
>     ,JInner <$ keyword_ "inner" <* keyword_ "join"
>     ,JLeft <$ keyword_ "left"
>            <* optional (keyword_ "outer")
>            <* keyword_ "join"
>     ,JRight <$ keyword_ "right"
>             <* optional (keyword_ "outer")
>             <* keyword_ "join"
>     ,JFull <$ keyword_ "full"
>            <* optional (keyword_ "outer")
>            <* keyword_ "join"
>     ,JInner <$ keyword_ "join"]

> joinCondition :: Parser JoinCondition
> joinCondition = choice
>     [keyword_ "on" >> JoinOn <$> valueExpr
>     ,keyword_ "using" >> JoinUsing <$> parens (commaSep1 name)]

> fromAlias :: Parser Alias
> fromAlias = Alias <$> tableAlias <*> columnAliases
>   where
>     tableAlias = optional (keyword_ "as") *> name
>     columnAliases = optionMaybe $ parens $ commaSep1 name

== simple other parts

Parsers for where, group by, having, order by and limit, which are
pretty trivial.

> whereClause :: Parser ValueExpr
> whereClause = keyword_ "where" *> valueExpr

> groupByClause :: Parser [GroupingExpr]
> groupByClause = keywords_ ["group","by"] *> commaSep1 groupingExpression
>   where
>     groupingExpression = choice
>       [keyword_ "cube" >>
>        Cube <$> parens (commaSep groupingExpression)
>       ,keyword_ "rollup" >>
>        Rollup <$> parens (commaSep groupingExpression)
>       ,GroupingParens <$> parens (commaSep groupingExpression)
>       ,keywords_ ["grouping", "sets"] >>
>        GroupingSets <$> parens (commaSep groupingExpression)
>       ,SimpleGroup <$> valueExpr
>       ]

> having :: Parser ValueExpr
> having = keyword_ "having" *> valueExpr

> orderBy :: Parser [SortSpec]
> orderBy = keywords_ ["order","by"] *> commaSep1 ob
>   where
>     ob = SortSpec
>          <$> valueExpr
>          <*> option DirDefault (choice [Asc <$ keyword_ "asc"
>                                        ,Desc <$ keyword_ "desc"])
>          <*> option NullsOrderDefault
>              -- todo: left factor better
>              (keyword_ "nulls" >>
>                     choice [NullsFirst <$ keyword "first"
>                            ,NullsLast <$ keyword "last"])

allows offset and fetch in either order
+ postgresql offset without row(s) and limit instead of fetch also

> offsetFetch :: Parser (Maybe ValueExpr, Maybe ValueExpr)
> offsetFetch = permute ((,) <$?> (Nothing, Just <$> offset)
>                            <|?> (Nothing, Just <$> fetch))

> offset :: Parser ValueExpr
> offset = keyword_ "offset" *> valueExpr
>          <* option () (choice [keyword_ "rows"
>                               ,keyword_ "row"])

> fetch :: Parser ValueExpr
> fetch = choice [ansiFetch, limit]
>   where
>     fs = makeKeywordTree ["fetch first", "fetch next"]
>     ro = makeKeywordTree ["rows only", "row only"]
>     ansiFetch = fs *> valueExpr <* ro
>     limit = keyword_ "limit" *> valueExpr

== common table expressions

> with :: Parser QueryExpr
> with = keyword_ "with" >>
>     With <$> option False (True <$ keyword_ "recursive")
>          <*> commaSep1 withQuery <*> queryExpr
>   where
>     withQuery = (,) <$> (fromAlias <* keyword_ "as")
>                     <*> parens queryExpr

== query expression

This parser parses any query expression variant: normal select, cte,
and union, etc..

> queryExpr :: Parser QueryExpr
> queryExpr = choice
>     [with
>     ,choice [values,table, select] >>= optionSuffix queryExprSuffix]
>   where
>     select = keyword_ "select" >>
>         mkSelect
>         <$> (fromMaybe SQDefault <$> duplicates)
>         <*> selectList
>         <*> optionMaybe tableExpression
>     mkSelect d sl Nothing =
>         makeSelect{qeSetQuantifier = d, qeSelectList = sl}
>     mkSelect d sl (Just (TableExpression f w g h od ofs fe)) =
>         Select d sl f w g h od ofs fe
>     values = keyword_ "values"
>              >> Values <$> commaSep (parens (commaSep valueExpr))
>     table = keyword_ "table" >> Table <$> names

local data type to help with parsing the bit after the select list,
called 'table expression' in the ansi sql grammar. Maybe this should
be in the public syntax?

> data TableExpression
>     = TableExpression
>       {_teFrom :: [TableRef]
>       ,_teWhere :: Maybe ValueExpr
>       ,_teGroupBy :: [GroupingExpr]
>       ,_teHaving :: Maybe ValueExpr
>       ,_teOrderBy :: [SortSpec]
>       ,_teOffset :: Maybe ValueExpr
>       ,_teFetchFirst :: Maybe ValueExpr}

> tableExpression :: Parser TableExpression
> tableExpression = mkTe <$> from
>                        <*> optionMaybe whereClause
>                        <*> option [] groupByClause
>                        <*> optionMaybe having
>                        <*> option [] orderBy
>                        <*> offsetFetch
>  where
>     mkTe f w g h od (ofs,fe) =
>         TableExpression f w g h od ofs fe

> queryExprSuffix :: QueryExpr -> Parser QueryExpr
> queryExprSuffix qe = cqSuffix >>= optionSuffix queryExprSuffix
>   where
>     cqSuffix = CombineQueryExpr qe
>                <$> setOp
>                <*> (fromMaybe SQDefault <$> duplicates)
>                <*> corr
>                <*> queryExpr
>     setOp = choice [Union <$ keyword_ "union"
>                    ,Intersect <$ keyword_ "intersect"
>                    ,Except <$ keyword_ "except"]
>             <?> "set operator"
>     corr = option Respectively (Corresponding <$ keyword_ "corresponding")


wrapper for query expr which ignores optional trailing semicolon.

> topLevelQueryExpr :: Parser QueryExpr
> topLevelQueryExpr = queryExpr >>= optionSuffix ((semi *>) . return)

wrapper to parse a series of query exprs from a single source. They
must be separated by semicolon, but for the last expression, the
trailing semicolon is optional.

> queryExprs :: Parser [QueryExpr]
> queryExprs = (:[]) <$> queryExpr
>              >>= optionSuffix ((semi *>) . return)
>              >>= optionSuffix (\p -> (p++) <$> queryExprs)

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

> makeKeywordTree :: [String] -> Parser [String]
> makeKeywordTree sets =
>     parseTrees (sort $ map words sets)
>     --  ?? <?> intercalate "," sets
>   where
>     parseTrees :: [[String]] -> Parser [String]
>     parseTrees ws = do
>       let gs :: [[[String]]]
>           gs = groupBy ((==) `on` safeHead) ws
>       choice $ map parseGroup gs
>     parseGroup :: [[String]] -> Parser [String]
>     parseGroup l@((k:_):_) = do
>         keyword_ k
>         let tls = catMaybes $ map safeTail l
>             pr = (k:) <$> parseTrees tls
>         if (or $ map null tls)
>           then pr <|> return [k]
>           else pr
>     parseGroup _ = guard False >> error "impossible"
>     safeHead (x:_) = Just x
>     safeHead [] = Nothing
>     safeTail (_:x) = Just x
>     safeTail [] = Nothing

------------------------------------------------

= lexing parsers

whitespace parser which skips comments also

> whitespace :: Parser ()
> whitespace =
>     choice [simpleWhitespace *> whitespace
>            ,lineComment *> whitespace
>            ,blockComment *> whitespace
>            ,return ()] <?> "whitespace"
>   where
>     lineComment = try (string "--")
>                   *> manyTill anyChar (void (char '\n') <|> eof)
>     blockComment = -- no nesting of block comments in SQL
>                    try (string "/*")
>                    -- try used here so it doesn't fail when we see a
>                    -- '*' which isn't followed by a '/'
>                    *> manyTill anyChar (try $ string "*/")
>     -- use many1 so we can more easily avoid non terminating loops
>     simpleWhitespace = void $ many1 (oneOf " \t\n")

> lexeme :: Parser a -> Parser a
> lexeme p = p <* whitespace

> unsignedInteger :: Parser Integer
> unsignedInteger = read <$> lexeme (many1 digit) <?> "integer"


number literals

here is the rough grammar target:

digits
digits.[digits][e[+-]digits]
[digits].digits[e[+-]digits]
digitse[+-]digits

numbers are parsed to strings, not to a numeric type. This is to avoid
making a decision on how to represent numbers, the client code can
make this choice.

> numberLiteral :: Parser String
> numberLiteral =
>     lexeme (numToken <* notFollowedBy (alphaNum <|> char '.'))
>     <?> "number literal"
>   where
>     numToken = choice [int
>                        >>= optionSuffix dot
>                        >>= optionSuffix fracts
>                        >>= optionSuffix expon
>                       ,fract "" >>= optionSuffix expon]
>     int = many1 digit
>     fract p = dot p >>= fracts
>     dot p = (p++) <$> string "."
>     fracts p = (p++) <$> int
>     expon p = concat <$> sequence
>               [return p
>               ,(:[]) <$> oneOf "eE"
>               ,option "" (string "+" <|> string "-")
>               ,int]


> identifier :: Parser String
> identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
>              <?> "identifier"
>   where
>     firstChar = letter <|> char '_' <?> "identifier"
>     nonFirstChar = digit <|> firstChar <?> ""

> quotedIdentifier :: Parser String
> quotedIdentifier = quotedIdenHelper

> quotedIdenHelper :: Parser String
> quotedIdenHelper =
>     lexeme (dq *> manyTill anyChar dq >>= optionSuffix moreIden)
>     <?> "identifier"
>   where
>     moreIden s0 = do
>          void dq
>          s <- manyTill anyChar dq
>          optionSuffix moreIden (s0 ++ "\"" ++ s)
>     dq = char '"' <?> "double quote"

> uquotedIdentifier :: Parser String
> uquotedIdentifier =
>   try (string "u&" <|> string "U&") *> quotedIdenHelper
>   <?> "identifier"

parses an identifier with a : prefix. The : isn't included in the
return value

> hostParameterToken :: Parser String
> hostParameterToken = lexeme $ char ':' *> identifier

todo: work out the symbol parsing better

> symbol :: String -> Parser String
> symbol s = try (lexeme $ do
>     u <- choice (many1 (char '.') :
>                  map (try . string) [">=","<=","!=","<>","||"]
>                  ++ map (string . (:[])) "+-^*/%~&|<>=")
>     guard (s == u)
>     return s)
>     <?> s

> questionMark :: Parser Char
> questionMark = lexeme (char '?') <?> "question mark"

> openParen :: Parser Char
> openParen = lexeme $ char '('

> closeParen :: Parser Char
> closeParen = lexeme $ char ')'

> openBracket :: Parser Char
> openBracket = lexeme $ char '['

> closeBracket :: Parser Char
> closeBracket = lexeme $ char ']'


> comma :: Parser Char
> comma = lexeme (char ',') <?> "comma"

> semi :: Parser Char
> semi = lexeme (char ';') <?> "semicolon"

> quote :: Parser Char
> quote = lexeme (char '\'') <?> "single quote"

> --stringToken :: Parser String
> --stringToken = lexeme (char '\'' *> manyTill anyChar (char '\''))
> -- todo: tidy this up, add the prefixes stuff, and add the multiple
> -- string stuff
> stringToken :: Parser String
> stringToken =
>     lexeme (nlquote *> manyTill anyChar nlquote
>     >>= optionSuffix moreString)
>     <?> "string"
>   where
>     moreString s0 = choice
>         [-- handle two adjacent quotes
>          do
>          void nlquote
>          s <- manyTill anyChar nlquote
>          optionSuffix moreString (s0 ++ "'" ++ s)
>         ,-- handle string in separate parts
>          -- e.g. 'part 1' 'part 2'
>          do --can this whitespace be factored out?
>             -- since it will be parsed twice when there is no more literal
>             -- yes: split the adjacent quote and multiline literal
>             -- into two different suffixes
>             -- won't need to call lexeme at the top level anymore after this
>          try (whitespace <* nlquote)
>          s <- manyTill anyChar nlquote
>          optionSuffix moreString (s0 ++ s)
>         ]
>     -- non lexeme quote
>     nlquote = char '\'' <?> "single quote"

= helper functions

> keyword :: String -> Parser String
> keyword k = try (do
>     i <- identifier
>     guard (map toLower i == k)
>     return k) <?> k

helper function to improve error messages

> keywords_ :: [String] -> Parser ()
> keywords_ ks = mapM_ keyword_ ks <?> intercalate " " ks


> parens :: Parser a -> Parser a
> parens = between openParen closeParen

> brackets :: Parser a -> Parser a
> brackets = between openBracket closeBracket

> commaSep :: Parser a -> Parser [a]
> commaSep = (`sepBy` comma)

> keyword_ :: String -> Parser ()
> keyword_ = void . keyword

> symbol_ :: String -> Parser ()
> symbol_ = void . symbol

> commaSep1 :: Parser a -> Parser [a]
> commaSep1 = (`sepBy1` comma)

a possible issue with the option suffix is that it enforces left
associativity when chaining it recursively. Have to review
all these uses and figure out if any should be right associative
instead, and create an alternative suffix parser

> optionSuffix :: (a -> Parser a) -> a -> Parser a
> optionSuffix p a = option a (p a)

> identifierBlacklist :: [String] -> Parser String
> identifierBlacklist bl = try (do
>     i <- identifier
>     when (map toLower i `elem` bl) $
>         fail $ "keyword not allowed here: " ++ i
>     return i)
>     <?> "identifier"

> blacklist :: [String]
> blacklist = reservedWord {-
>     [-- case
>      "case", "when", "then", "else", "end"
>     ,--join
>      "natural","inner","outer","cross","left","right","full","join"
>     ,"on","using","lateral"
>     ,"from","where","group","having","order","limit", "offset", "fetch"
>     ,"as","in"
>     ,"except", "intersect", "union"
>     ] -}

These blacklisted names are mostly needed when we parse something with
an optional alias, e.g. select a a from t. If we write select a from
t, we have to make sure the from isn't parsed as an alias. I'm not
sure what other places strictly need the blacklist, and in theory it
could be tuned differently for each place the identifierString/
identifier parsers are used to only blacklist the bare
minimum. Something like this might be needed for dialect support, even
if it is pretty silly to use a keyword as an unquoted identifier when
there is a effing quoting syntax as well.

The standard has a weird mix of reserved keywords and unreserved
keywords (I'm not sure what exactly being an unreserved keyword
means).

> _nonReservedWord :: [String]
> _nonReservedWord =
>     ["a"
>     ,"abs"
>     ,"absolute"
>     ,"action"
>     ,"ada"
>     ,"admin"
>     ,"after"
>     ,"always"
>     ,"asc"
>     ,"assertion"
>     ,"assignment"
>     ,"attribute"
>     ,"attributes"
>     ,"avg"
>     ,"before"
>     ,"bernoulli"
>     ,"breadth"
>     ,"c"
>     ,"cardinality"
>     ,"cascade"
>     ,"catalog"
>     ,"catalog_name"
>     ,"ceil"
>     ,"ceiling"
>     ,"chain"
>     ,"characteristics"
>     ,"characters"
>     ,"character_length"
>     ,"character_set_catalog"
>     ,"character_set_name"
>     ,"character_set_schema"
>     ,"char_length"
>     ,"checked"
>     ,"class_origin"
>     ,"coalesce"
>     ,"cobol"
>     ,"code_units"
>     ,"collation"
>     ,"collation_catalog"
>     ,"collation_name"
>     ,"collation_schema"
>     ,"collect"
>     ,"column_name"
>     ,"command_function"
>     ,"command_function_code"
>     ,"committed"
>     ,"condition"
>     ,"condition_number"
>     ,"connection_name"
>     ,"constraints"
>     ,"constraint_catalog"
>     ,"constraint_name"
>     ,"constraint_schema"
>     ,"constructors"
>     ,"contains"
>     ,"convert"
>     ,"corr"
>     ,"count"
>     ,"covar_pop"
>     ,"covar_samp"
>     ,"cume_dist"
>     ,"current_collation"
>     ,"cursor_name"
>     ,"data"
>     ,"datetime_interval_code"
>     ,"datetime_interval_precision"
>     ,"defaults"
>     ,"deferrable"
>     ,"deferred"
>     ,"defined"
>     ,"definer"
>     ,"degree"
>     ,"dense_rank"
>     ,"depth"
>     ,"derived"
>     ,"desc"
>     ,"descriptor"
>     ,"diagnostics"
>     ,"dispatch"
>     ,"domain"
>     ,"dynamic_function"
>     ,"dynamic_function_code"
>     ,"equals"
>     ,"every"
>     ,"exception"
>     ,"exclude"
>     ,"excluding"
>     ,"exp"
>     ,"extract"
>     ,"final"
>     ,"first"
>     ,"floor"
>     ,"following"
>     ,"fortran"
>     ,"found"
>     ,"fusion"
>     ,"g"
>     ,"general"
>     ,"go"
>     ,"goto"
>     ,"granted"
>     ,"hierarchy"
>     ,"implementation"
>     ,"including"
>     ,"increment"
>     ,"initially"
>     ,"instance"
>     ,"instantiable"
>     ,"intersection"
>     ,"invoker"
>     ,"isolation"
>     ,"k"
>     ,"key"
>     ,"key_member"
>     ,"key_type"
>     ,"last"
>     ,"length"
>     ,"level"
>     ,"ln"
>     ,"locator"
>     ,"lower"
>     ,"m"
>     ,"map"
>     ,"matched"
>     ,"max"
>     ,"maxvalue"
>     ,"message_length"
>     ,"message_octet_length"
>     ,"message_text"
>     ,"min"
>     ,"minvalue"
>     ,"mod"
>     ,"more"
>     ,"mumps"
>     ,"name"
>     ,"names"
>     ,"nesting"
>     ,"next"
>     ,"normalize"
>     ,"normalized"
>     ,"nullable"
>     ,"nullif"
>     ,"nulls"
>     ,"number"
>     ,"object"
>     ,"octets"
>     ,"octet_length"
>     ,"option"
>     ,"options"
>     ,"ordering"
>     ,"ordinality"
>     ,"others"
>     ,"overlay"
>     ,"overriding"
>     ,"pad"
>     ,"parameter_mode"
>     ,"parameter_name"
>     ,"parameter_ordinal_position"
>     ,"parameter_specific_catalog"
>     ,"parameter_specific_name"
>     ,"parameter_specific_schema"
>     ,"partial"
>     ,"pascal"
>     ,"path"
>     ,"percentile_cont"
>     ,"percentile_disc"
>     ,"percent_rank"
>     ,"placing"
>     ,"pli"
>     ,"position"
>     ,"power"
>     ,"preceding"
>     ,"preserve"
>     ,"prior"
>     ,"privileges"
>     ,"public"
>     ,"rank"
>     ,"read"
>     ,"relative"
>     ,"repeatable"
>     ,"restart"
>     ,"returned_cardinality"
>     ,"returned_length"
>     ,"returned_octet_length"
>     ,"returned_sqlstate"
>     ,"role"
>     ,"routine"
>     ,"routine_catalog"
>     ,"routine_name"
>     ,"routine_schema"
>     ,"row_count"
>     ,"row_number"
>     ,"scale"
>     ,"schema"
>     ,"schema_name"
>     ,"scope_catalog"
>     ,"scope_name"
>     ,"scope_schema"
>     ,"section"
>     ,"security"
>     ,"self"
>     ,"sequence"
>     ,"serializable"
>     ,"server_name"
>     ,"session"
>     ,"sets"
>     ,"simple"
>     ,"size"
>     ,"source"
>     ,"space"
>     ,"specific_name"
>     ,"sqrt"
>     ,"state"
>     ,"statement"
>     ,"stddev_pop"
>     ,"stddev_samp"
>     ,"structure"
>     ,"style"
>     ,"subclass_origin"
>     ,"substring"
>     ,"sum"
>     ,"tablesample"
>     ,"table_name"
>     ,"temporary"
>     ,"ties"
>     ,"top_level_count"
>     ,"transaction"
>     ,"transactions_committed"
>     ,"transactions_rolled_back"
>     ,"transaction_active"
>     ,"transform"
>     ,"transforms"
>     ,"translate"
>     ,"trigger_catalog"
>     ,"trigger_name"
>     ,"trigger_schema"
>     ,"trim"
>     ,"type"
>     ,"unbounded"
>     ,"uncommitted"
>     ,"under"
>     ,"unnamed"
>     ,"usage"
>     ,"user_defined_type_catalog"
>     ,"user_defined_type_code"
>     ,"user_defined_type_name"
>     ,"user_defined_type_schema"
>     ,"view"
>     ,"work"
>     ,"write"
>     ,"zone"]

> reservedWord :: [String]
> reservedWord =
>     ["add"
>     --,"all"
>     ,"allocate"
>     ,"alter"
>     ,"and"
>     --,"any"
>     ,"are"
>     ,"array"
>     ,"as"
>     ,"asensitive"
>     ,"asymmetric"
>     ,"at"
>     ,"atomic"
>     ,"authorization"
>     ,"begin"
>     ,"between"
>     ,"bigint"
>     ,"binary"
>     ,"blob"
>     ,"boolean"
>     ,"both"
>     ,"by"
>     ,"call"
>     ,"called"
>     ,"cascaded"
>     ,"case"
>     ,"cast"
>     ,"char"
>     ,"character"
>     ,"check"
>     ,"clob"
>     ,"close"
>     ,"collate"
>     ,"column"
>     ,"commit"
>     ,"connect"
>     ,"constraint"
>     ,"continue"
>     ,"corresponding"
>     ,"create"
>     ,"cross"
>     ,"cube"
>     ,"current"
>     --,"current_date"
>     ,"current_default_transform_group"
>     ,"current_path"
>     ,"current_role"
>     ,"current_time"
>     ,"current_timestamp"
>     ,"current_transform_group_for_type"
>     ,"current_user"
>     ,"cursor"
>     ,"cycle"
>     ,"date"
>     --,"day"
>     ,"deallocate"
>     ,"dec"
>     ,"decimal"
>     ,"declare"
>     --,"default"
>     ,"delete"
>     ,"deref"
>     ,"describe"
>     ,"deterministic"
>     ,"disconnect"
>     ,"distinct"
>     ,"double"
>     ,"drop"
>     ,"dynamic"
>     ,"each"
>     --,"element"
>     ,"else"
>     ,"end"
>     ,"end-exec"
>     ,"escape"
>     ,"except"
>     ,"exec"
>     ,"execute"
>     ,"exists"
>     ,"external"
>     --,"false"
>     ,"fetch"
>     ,"filter"
>     ,"float"
>     ,"for"
>     ,"foreign"
>     ,"free"
>     ,"from"
>     ,"full"
>     ,"function"
>     ,"get"
>     ,"global"
>     ,"grant"
>     ,"group"
>     ,"grouping"
>     ,"having"
>     ,"hold"
>     --,"hour"
>     ,"identity"
>     ,"immediate"
>     ,"in"
>     ,"indicator"
>     ,"inner"
>     ,"inout"
>     ,"input"
>     ,"insensitive"
>     ,"insert"
>     ,"int"
>     ,"integer"
>     ,"intersect"
>     ,"interval"
>     ,"into"
>     ,"is"
>     ,"isolation"
>     ,"join"
>     ,"language"
>     ,"large"
>     ,"lateral"
>     ,"leading"
>     ,"left"
>     ,"like"
>     ,"local"
>     ,"localtime"
>     ,"localtimestamp"
>     ,"match"
>     ,"member"
>     ,"merge"
>     ,"method"
>     --,"minute"
>     ,"modifies"
>     ,"module"
>     --,"month"
>     ,"multiset"
>     ,"national"
>     ,"natural"
>     ,"nchar"
>     ,"nclob"
>     ,"new"
>     ,"no"
>     ,"none"
>     ,"not"
>     --,"null"
>     ,"numeric"
>     ,"of"
>     ,"old"
>     ,"on"
>     ,"only"
>     ,"open"
>     ,"or"
>     ,"order"
>     ,"out"
>     ,"outer"
>     ,"output"
>     ,"over"
>     ,"overlaps"
>     ,"parameter"
>     ,"partition"
>     ,"precision"
>     ,"prepare"
>     ,"primary"
>     ,"procedure"
>     ,"range"
>     ,"reads"
>     ,"real"
>     ,"recursive"
>     ,"ref"
>     ,"references"
>     ,"referencing"
>     --,"regr_avgx"
>     --,"regr_avgy"
>     --,"regr_count"
>     --,"regr_intercept"
>     --,"regr_r2"
>     --,"regr_slope"
>     --,"regr_sxx"
>     --,"regr_sxy"
>     --,"regr_syy"
>     ,"release"
>     ,"result"
>     ,"return"
>     ,"returns"
>     ,"revoke"
>     ,"right"
>     ,"rollback"
>     ,"rollup"
>     --,"row"
>     ,"rows"
>     ,"savepoint"
>     ,"scroll"
>     ,"search"
>     --,"second"
>     ,"select"
>     ,"sensitive"
>     ,"session_user"
>     --,"set"
>     ,"similar"
>     ,"smallint"
>     --,"some"
>     ,"specific"
>     ,"specifictype"
>     ,"sql"
>     ,"sqlexception"
>     ,"sqlstate"
>     ,"sqlwarning"
>     --,"start"
>     ,"static"
>     ,"submultiset"
>     ,"symmetric"
>     ,"system"
>     ,"system_user"
>     ,"table"
>     ,"then"
>     ,"time"
>     ,"timestamp"
>     ,"timezone_hour"
>     ,"timezone_minute"
>     ,"to"
>     ,"trailing"
>     ,"translation"
>     ,"treat"
>     ,"trigger"
>     --,"true"
>     ,"uescape"
>     ,"union"
>     ,"unique"
>     --,"unknown"
>     ,"unnest"
>     ,"update"
>     ,"upper"
>     ,"user"
>     ,"using"
>     --,"value"
>     ,"values"
>     --,"var_pop"
>     --,"var_samp"
>     ,"varchar"
>     ,"varying"
>     ,"when"
>     ,"whenever"
>     ,"where"
>     ,"width_bucket"
>     ,"window"
>     ,"with"
>     ,"within"
>     ,"without"
>     --,"year"

>     -- added for this parser
>     ,"limit"
>     ,"offset"
>     ]

--------------------------------------------

= helper functions

> setPos :: Maybe (Int,Int) -> Parser ()
> setPos Nothing = return ()
> setPos (Just (l,c)) = fmap f getPosition >>= setPosition
>   where f = flip setSourceColumn c
>             . flip setSourceLine l

> convParseError :: String -> P.ParseError -> ParseError
> convParseError src e =
>     ParseError
>     {peErrorString = show e
>     ,peFilename = sourceName p
>     ,pePosition = (sourceLine p, sourceColumn p)
>     ,peFormattedError = formatError src e}
>   where
>     p = errorPos e

format the error more nicely: emacs format for positioning, plus
context

> formatError :: String -> P.ParseError -> String
> formatError src e =
>     sourceName p ++ ":" ++ show (sourceLine p)
>     ++ ":" ++ show (sourceColumn p) ++ ":"
>     ++ context
>     ++ show e
>   where
>     context =
>         let lns = take 1 $ drop (sourceLine p - 1) $ lines src
>         in case lns of
>              [x] -> "\n" ++ x ++ "\n"
>                     ++ replicate (sourceColumn p - 1) ' ' ++ "^\n"
>              _ -> ""
>     p = errorPos e
