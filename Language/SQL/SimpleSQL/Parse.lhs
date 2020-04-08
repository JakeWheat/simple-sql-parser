
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

> {-# LANGUAGE TupleSections #-}
> -- | This is the module with the parser functions.
> module Language.SQL.SimpleSQL.Parse
>     (parseQueryExpr
>     ,parseScalarExpr
>     ,parseStatement
>     ,parseStatements
>     ,ParseError(..)) where

> import Control.Monad.Identity (Identity)
> import Control.Monad (guard, void)
> import Control.Applicative ((<**>))
> import Data.Char (toLower, isDigit)
> import Text.Parsec (setPosition,setSourceColumn,setSourceLine,getPosition
>                    ,option,between,sepBy,sepBy1
>                    ,try,many,many1,(<|>),choice,eof
>                    ,optionMaybe,optional,runParser
>                    ,chainl1, chainr1,(<?>))
> import Text.Parsec.Perm (permute,(<$?>), (<|?>))
> import Text.Parsec.Prim (getState, token)
> import Text.Parsec.Pos (newPos)
> import qualified Text.Parsec.Expr as E
> import Data.List (intercalate,sort,groupBy)
> import Data.Function (on)
> import Data.Maybe
> import Text.Parsec.String (GenParser)

> import Language.SQL.SimpleSQL.Syntax
> import Language.SQL.SimpleSQL.Combinators
> import Language.SQL.SimpleSQL.Errors
> import Language.SQL.SimpleSQL.Dialect
> import qualified Language.SQL.SimpleSQL.Lex as L


= Public API

> -- | Parses a query expr, trailing semicolon optional.
> parseQueryExpr :: Dialect
>                   -- ^ dialect of SQL to use
>                -> FilePath
>                   -- ^ filename to use in error messages
>                -> Maybe (Int,Int)
>                   -- ^ line number and column number of the first character
>                   -- in the source to use in error messages
>                -> String
>                   -- ^ the SQL source to parse
>                -> Either ParseError QueryExpr
> parseQueryExpr = wrapParse topLevelQueryExpr

> -- | Parses a statement, trailing semicolon optional.
> parseStatement :: Dialect
>                   -- ^ dialect of SQL to use
>                -> FilePath
>                   -- ^ filename to use in error messages
>                -> Maybe (Int,Int)
>                   -- ^ line number and column number of the first character
>                   -- in the source to use in error messages
>                -> String
>                   -- ^ the SQL source to parse
>                -> Either ParseError Statement
> parseStatement = wrapParse topLevelStatement


> -- | Parses a list of statements, with semi colons between
> -- them. The final semicolon is optional.
> parseStatements :: Dialect
>                   -- ^ dialect of SQL to use
>                 -> FilePath
>                    -- ^ filename to use in error messages
>                 -> Maybe (Int,Int)
>                    -- ^ line number and column number of the first character
>                    -- in the source to use in error messages
>                 -> String
>                    -- ^ the SQL source to parse
>                 -> Either ParseError [Statement]
> parseStatements = wrapParse statements

> -- | Parses a scalar expression.
> parseScalarExpr :: Dialect
>                    -- ^ dialect of SQL to use
>                 -> FilePath
>                    -- ^ filename to use in error messages
>                 -> Maybe (Int,Int)
>                    -- ^ line number and column number of the first character
>                    -- in the source to use in error messages
>                 -> String
>                    -- ^ the SQL source to parse
>                 -> Either ParseError ScalarExpr
> parseScalarExpr = wrapParse scalarExpr

This helper function takes the parser given and:

sets the position when parsing
automatically skips leading whitespace
checks the parser parses all the input using eof
converts the error return to the nice wrapper

> wrapParse :: Parser a
>           -> Dialect
>           -> FilePath
>           -> Maybe (Int,Int)
>           -> String
>           -> Either ParseError a
> wrapParse parser d f p src = do
>     let (l,c) = fromMaybe (1,1) p
>     lx <- L.lexSQL d f (Just (l,c)) src
>     either (Left . convParseError src) Right
>       $ runParser (setPos p *> parser <* eof)
>                   d f $ filter keep lx
>   where
>     setPos Nothing = pure ()
>     setPos (Just (l,c)) = fmap up getPosition >>= setPosition
>       where up = flip setSourceColumn c . flip setSourceLine l
>     keep (_,L.Whitespace {}) = False
>     keep (_,L.LineComment {}) = False
>     keep (_,L.BlockComment {}) = False
>     keep _ = True


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

> name :: Parser Name
> name = do
>     d <- getState
>     uncurry Name <$> identifierTok (blacklist d)

todo: replace (:[]) with a named function all over

> names :: Parser [Name]
> names = reverse <$> (((:[]) <$> name) <??*> anotherName)
>   -- can't use a simple chain here since we
>   -- want to wrap the . + name in a try
>   -- this will change when this is left factored
>   where
>     anotherName :: Parser ([Name] -> [Name])
>     anotherName = try ((:) <$> (symbol "." *> name))

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

> typeName :: Parser TypeName
> typeName =
>     (rowTypeName <|> intervalTypeName <|> otherTypeName)
>     <??*> tnSuffix
>   where
>     rowTypeName =
>         RowTypeName <$> (keyword_ "row" *> parens (commaSep1 rowField))
>     rowField = (,) <$> name <*> typeName
>     ----------------------------
>     intervalTypeName =
>         keyword_ "interval" *>
>         (uncurry IntervalTypeName <$> intervalQualifier)
>     ----------------------------
>     otherTypeName =
>         nameOfType <**>
>             (typeNameWithParens
>              <|> pure Nothing <**> (timeTypeName <|> charTypeName)
>              <|> pure TypeName)
>     nameOfType = reservedTypeNames <|> names
>     charTypeName = charSet <**> (option [] tcollate <$$$$> CharTypeName)
>                    <|> pure [] <**> (tcollate <$$$$> CharTypeName)
>     typeNameWithParens =
>         (openParen *> unsignedInteger)
>         <**> (closeParen *> precMaybeSuffix
>               <|> (precScaleTypeName <|> precLengthTypeName) <* closeParen)
>     precMaybeSuffix = (. Just) <$> (timeTypeName <|> charTypeName)
>                       <|> pure (flip PrecTypeName)
>     precScaleTypeName = (comma *> unsignedInteger) <$$$> PrecScaleTypeName
>     precLengthTypeName =
>         Just <$> lobPrecSuffix
>         <**> (optionMaybe lobUnits <$$$$> PrecLengthTypeName)
>         <|> pure Nothing <**> ((Just <$> lobUnits) <$$$$> PrecLengthTypeName)
>     timeTypeName = tz <$$$> TimeTypeName
>     ----------------------------
>     lobPrecSuffix = PrecK <$ keyword_ "k"
>                     <|> PrecM <$ keyword_ "m"
>                     <|> PrecG <$ keyword_ "g"
>                     <|> PrecT <$ keyword_ "t"
>                     <|> PrecP <$ keyword_ "p"
>     lobUnits = PrecCharacters <$ keyword_ "characters"
>                -- char and byte are the oracle spelling
>                -- todo: move these to oracle dialect
>                <|> PrecCharacters <$ keyword_ "char"
>                <|> PrecOctets <$ keyword_ "octets"
>                <|> PrecOctets <$ keyword_ "byte"
>     tz = True <$ keywords_ ["with", "time","zone"]
>          <|> False <$ keywords_ ["without", "time","zone"]
>     charSet = keywords_ ["character", "set"] *> names
>     tcollate = keyword_ "collate" *> names
>     ----------------------------
>     tnSuffix = multiset <|> array
>     multiset = MultisetTypeName <$ keyword_ "multiset"
>     array = keyword_ "array" *>
>         (optionMaybe (brackets unsignedInteger) <$$> ArrayTypeName)
>     ----------------------------
>     -- this parser handles the fixed set of multi word
>     -- type names, plus all the type names which are
>     -- reserved words
>     reservedTypeNames = do
>         d <- getState
>         (:[]) . Name Nothing . unwords <$> makeKeywordTree (diSpecialTypeNames d)
>         

= Scalar expressions

== simple literals

See the stringToken lexer below for notes on string literal syntax.

> stringLit :: Parser ScalarExpr
> stringLit = (\(s,e,t) -> StringLit s e t) <$> stringTokExtend

> numberLit :: Parser ScalarExpr
> numberLit = NumLit <$> sqlNumberTok False

> simpleLiteral :: Parser ScalarExpr
> simpleLiteral = numberLit <|> stringLit

== star, param, host param

=== star

used in select *, select x.*, and agg(*) variations, and some other
places as well. The parser doesn't attempt to check that the star is
in a valid context, it parses it OK in any scalar expression context.

> star :: Parser ScalarExpr
> star = Star <$ symbol "*"

== parameter

unnamed parameter or named parameter
use in e.g. select * from t where a = ?
select x from t where x > :param

> parameter :: Parser ScalarExpr
> parameter = choice
>     [Parameter <$ questionMark
>     ,HostParameter
>      <$> hostParamTok
>      <*> optionMaybe (keyword "indicator" *> hostParamTok)]

== positional arg

> positionalArg :: Parser ScalarExpr
> positionalArg = PositionalArg <$> positionalArgTok

== parens

scalar expression parens, row ctor and scalar subquery

> parensExpr :: Parser ScalarExpr
> parensExpr = parens $ choice
>     [SubQueryExpr SqSq <$> queryExpr
>     ,ctor <$> commaSep1 scalarExpr]
>   where
>     ctor [a] = Parens a
>     ctor as = SpecialOp [Name Nothing "rowctor"] as

== case, cast, exists, unique, array/multiset constructor, interval

All of these start with a fixed keyword which is reserved, so no other
syntax can start with the same keyword.

=== case expression

> caseExpr :: Parser ScalarExpr
> caseExpr =
>     Case <$> (keyword_ "case" *> optionMaybe scalarExpr)
>          <*> many1 whenClause
>          <*> optionMaybe elseClause
>          <* keyword_ "end"
>   where
>    whenClause = (,) <$> (keyword_ "when" *> commaSep1 scalarExpr)
>                     <*> (keyword_ "then" *> scalarExpr)
>    elseClause = keyword_ "else" *> scalarExpr

=== cast

cast: cast(expr as type)

> cast :: Parser ScalarExpr
> cast = keyword_ "cast" *>
>        parens (Cast <$> scalarExpr
>                     <*> (keyword_ "as" *> typeName))

=== exists, unique

subquery expression:
[exists|unique] (queryexpr)

> subquery :: Parser ScalarExpr
> subquery = SubQueryExpr <$> sqkw <*> parens queryExpr
>   where
>     sqkw = SqExists <$ keyword_ "exists" <|> SqUnique <$ keyword_ "unique"

=== array/multiset constructor

> arrayCtor :: Parser ScalarExpr
> arrayCtor = keyword_ "array" >>
>     choice
>     [ArrayCtor <$> parens queryExpr
>     ,Array (Iden [Name Nothing "array"]) <$> brackets (commaSep scalarExpr)]

As far as I can tell, table(query expr) is just syntax sugar for
multiset(query expr). It must be there for compatibility or something.

> multisetCtor :: Parser ScalarExpr
> multisetCtor =
>     choice
>     [keyword_ "multiset" >>
>      choice
>      [MultisetQueryCtor <$> parens queryExpr
>      ,MultisetCtor <$> brackets (commaSep scalarExpr)]
>     ,keyword_ "table" >>
>      MultisetQueryCtor <$> parens queryExpr]

> nextValueFor :: Parser ScalarExpr
> nextValueFor = keywords_ ["next","value","for"] >>
>     NextValueFor <$> names

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

> intervalLit :: Parser ScalarExpr
> intervalLit = try (keyword_ "interval" >> do
>     s <- optionMaybe $ choice [Plus <$ symbol_ "+"
>                               ,Minus <$ symbol_ "-"]
>     lit <- singleQuotesOnlyStringTok
>     q <- optionMaybe intervalQualifier
>     mkIt s lit q)
>   where
>     mkIt Nothing val Nothing = pure $ TypedLit (TypeName [Name Nothing "interval"]) val
>     mkIt s val (Just (a,b)) = pure $ IntervalLit s val a b
>     mkIt (Just {}) _val Nothing = fail "cannot use sign without interval qualifier"

== typed literal, app, special, aggregate, window, iden

All of these start with identifiers (some of the special functions
start with reserved keywords).

they are all variations on suffixes on the basic identifier parser

The windows is a suffix on the app parser

=== iden prefix term

all the scalar expressions which start with an identifier

(todo: really put all of them here instead of just some of them)

> idenExpr :: Parser ScalarExpr
> idenExpr =
>     -- todo: work out how to left factor this
>     try (TypedLit <$> typeName <*> singleQuotesOnlyStringTok)
>     <|> (names <**> option Iden app)
>     <|> keywordFunctionOrIden
>   where
>     -- special cases for keywords that can be parsed as an iden or app
>     keywordFunctionOrIden = try $ do
>         x <- unquotedIdentifierTok [] Nothing
>         d <- getState
>         let i = map toLower x `elem` diIdentifierKeywords d
>             a = map toLower x `elem` diAppKeywords d
>         case () of
>             _  | i && a -> pure [Name Nothing x] <**> option Iden app
>                | i -> pure (Iden [Name Nothing x])
>                | a -> pure [Name Nothing x] <**> app
>                | otherwise -> fail ""


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
>            -> Parser ScalarExpr
> specialOpK opName firstArg kws =
>     keyword_ opName >> do
>     void openParen
>     let pfa = do
>               e <- scalarExpr
>               -- check we haven't parsed the first
>               -- keyword as an identifier
>               case (e,kws) of
>                   (Iden [Name Nothing i], (k,_):_)
>                       | map toLower i == k ->
>                           fail $ "cannot use keyword here: " ++ i
>                   _ -> return ()
>               pure e
>     fa <- case firstArg of
>          SOKNone -> pure Nothing
>          SOKOptional -> optionMaybe (try pfa)
>          SOKMandatory -> Just <$> pfa
>     as <- mapM parseArg kws
>     void closeParen
>     pure $ SpecialOpK [Name Nothing opName] fa $ catMaybes as
>   where
>     parseArg (nm,mand) =
>         let p = keyword_ nm >> scalarExpr
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

> specialOpKs :: Parser ScalarExpr
> specialOpKs = choice $ map try
>     [extract, position, substring, convert, translate, overlay, trim]

> extract :: Parser ScalarExpr
> extract = specialOpK "extract" SOKMandatory [("from", True)]

> position :: Parser ScalarExpr
> position = specialOpK "position" SOKMandatory [("in", True)]

strictly speaking, the substring must have at least one of from and
for, but the parser doens't enforce this

> substring :: Parser ScalarExpr
> substring = specialOpK "substring" SOKMandatory
>                 [("from", False),("for", False)]

> convert :: Parser ScalarExpr
> convert = specialOpK "convert" SOKMandatory [("using", True)]


> translate :: Parser ScalarExpr
> translate = specialOpK "translate" SOKMandatory [("using", True)]

> overlay :: Parser ScalarExpr
> overlay = specialOpK "overlay" SOKMandatory
>                 [("placing", True),("from", True),("for", False)]

trim is too different because of the optional char, so a custom parser
the both ' ' is filled in as the default if either parts are missing
in the source

> trim :: Parser ScalarExpr
> trim =
>     keyword "trim" >>
>     parens (mkTrim
>             <$> option "both" sides
>             <*> option " " singleQuotesOnlyStringTok
>             <*> (keyword_ "from" *> scalarExpr))
>   where
>     sides = choice ["leading" <$ keyword_ "leading"
>                    ,"trailing" <$ keyword_ "trailing"
>                    ,"both" <$ keyword_ "both"]
>     mkTrim fa ch fr =
>       SpecialOpK [Name Nothing "trim"] Nothing
>           $ catMaybes [Just (fa,StringLit "'" "'" ch)
>                       ,Just ("from", fr)]

=== app, aggregate, window

This parses all these variations:
normal function application with just a csv of scalar exprs
aggregate variations (distinct, order by in parens, filter and where
  suffixes)
window apps (fn/agg followed by over)

This code is also a little dense like the typename code because of
left factoring, later they will even have to be partially combined
together.

> app :: Parser ([Name] -> ScalarExpr)
> app =
>     openParen *> choice
>     [duplicates
>      <**> (commaSep1 scalarExpr
>            <**> (((option [] orderBy) <* closeParen)
>                  <**> (optionMaybe afilter <$$$$$> AggregateApp)))
>      -- separate cases with no all or distinct which must have at
>      -- least one scalar expr
>     ,commaSep1 scalarExpr
>      <**> choice
>           [closeParen *> choice
>                          [window
>                          ,withinGroup
>                          ,(Just <$> afilter) <$$$> aggAppWithoutDupeOrd
>                          ,pure (flip App)]
>           ,orderBy <* closeParen
>            <**> (optionMaybe afilter <$$$$> aggAppWithoutDupe)]
>      -- no scalarExprs: duplicates and order by not allowed
>     ,([] <$ closeParen) <**> option (flip App) (window <|> withinGroup)
>     ]
>   where
>     aggAppWithoutDupeOrd n es f = AggregateApp n SQDefault es [] f
>     aggAppWithoutDupe n = AggregateApp n SQDefault

> afilter :: Parser ScalarExpr
> afilter = keyword_ "filter" *> parens (keyword_ "where" *> scalarExpr)

> withinGroup :: Parser ([ScalarExpr] -> [Name] -> ScalarExpr)
> withinGroup =
>     (keywords_ ["within", "group"] *> parens orderBy) <$$$> AggregateAppGroup

==== window

parse a window call as a suffix of a regular function call
this looks like this:
functionname(args) over ([partition by ids] [order by orderitems])

No support for explicit frames yet.

TODO: add window support for other aggregate variations, needs some
changes to the syntax also

> window :: Parser ([ScalarExpr] -> [Name] -> ScalarExpr)
> window =
>   keyword_ "over" *> openParen *> option [] partitionBy
>   <**> (option [] orderBy
>         <**> (((optionMaybe frameClause) <* closeParen) <$$$$$> WindowApp))
>   where
>     partitionBy = keywords_ ["partition","by"] *> commaSep1 scalarExpr
>     frameClause =
>         frameRowsRange -- TODO: this 'and' could be an issue
>         <**> (choice [(keyword_ "between" *> frameLimit True)
>                       <**> ((keyword_ "and" *> frameLimit True)
>                             <$$$> FrameBetween)
>                       -- maybe this should still use a b expression
>                       -- for consistency
>                      ,frameLimit False <**> pure (flip FrameFrom)])
>     frameRowsRange = FrameRows <$ keyword_ "rows"
>                      <|> FrameRange <$ keyword_ "range"
>     frameLimit useB =
>         choice
>         [Current <$ keywords_ ["current", "row"]
>          -- todo: create an automatic left factor for stuff like this
>         ,keyword_ "unbounded" *>
>          choice [UnboundedPreceding <$ keyword_ "preceding"
>                 ,UnboundedFollowing <$ keyword_ "following"]
>         ,(if useB then scalarExprB else scalarExpr)
>          <**> (Preceding <$ keyword_ "preceding"
>                <|> Following <$ keyword_ "following")
>         ]

== suffixes

These are all generic suffixes on any scalar expr

=== in

in: two variations:
a in (expr0, expr1, ...)
a in (queryexpr)

> inSuffix :: Parser (ScalarExpr -> ScalarExpr)
> inSuffix =
>     mkIn <$> inty
>          <*> parens (choice
>                      [InQueryExpr <$> queryExpr
>                      ,InList <$> commaSep1 scalarExpr])
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
parsing' is used to create alternative scalar expression parser which
is identical to the normal one expect it doesn't recognise the binary
and operator. This is the call to scalarExprB.

> betweenSuffix :: Parser (ScalarExpr -> ScalarExpr)
> betweenSuffix =
>     makeOp <$> Name Nothing <$> opName
>            <*> scalarExprB
>            <*> (keyword_ "and" *> scalarExprB)
>   where
>     opName = choice
>              ["between" <$ keyword_ "between"
>              ,"not between" <$ try (keywords_ ["not","between"])]
>     makeOp n b c = \a -> SpecialOp [n] [a,b,c]

=== quantified comparison

a = any (select * from t)

> quantifiedComparisonSuffix :: Parser (ScalarExpr -> ScalarExpr)
> quantifiedComparisonSuffix = do
>     c <- comp
>     cq <- compQuan
>     q <- parens queryExpr
>     pure $ \v -> QuantifiedComparison v [c] cq q
>   where
>     comp = Name Nothing <$> choice (map symbol
>            ["=", "<>", "<=", "<", ">", ">="])
>     compQuan = choice
>                [CPAny <$ keyword_ "any"
>                ,CPSome <$ keyword_ "some"
>                ,CPAll <$ keyword_ "all"]

=== match

a match (select a from t)

> matchPredicateSuffix :: Parser (ScalarExpr -> ScalarExpr)
> matchPredicateSuffix = do
>     keyword_ "match"
>     u <- option False (True <$ keyword_ "unique")
>     q <- parens queryExpr
>     pure $ \v -> Match v u q

=== array subscript

> arraySuffix :: Parser (ScalarExpr -> ScalarExpr)
> arraySuffix = do
>     es <- brackets (commaSep scalarExpr)
>     pure $ \v -> Array v es

=== escape

It is going to be really difficult to support an arbitrary character
for the escape now there is a separate lexer ...

TODO: this needs fixing. Escape is only part of other nodes, and not a
separate suffix.

> {-escapeSuffix :: Parser (ScalarExpr -> ScalarExpr)
> escapeSuffix = do
>     ctor <- choice
>             [Escape <$ keyword_ "escape"
>             ,UEscape <$ keyword_ "uescape"]
>     c <- escapeChar
>     pure $ \v -> ctor v c
>   where
>     escapeChar :: Parser Char
>     escapeChar = (identifierTok [] Nothing <|> symbolTok Nothing) >>= oneOnly
>     oneOnly :: String -> Parser Char
>     oneOnly c = case c of
>                    [c'] -> return c'
>                    _ -> fail "escape char must be single char"
> -}

=== collate

> collateSuffix:: Parser (ScalarExpr -> ScalarExpr)
> collateSuffix = do
>     keyword_ "collate"
>     i <- names
>     pure $ \v -> Collate v i

== odbc syntax

the parser supports three kinds of odbc syntax, two of which are
scalar expressions (the other is a variation on joins)


> odbcExpr :: Parser ScalarExpr
> odbcExpr = between (symbol "{") (symbol "}")
>            (odbcTimeLit <|> odbcFunc)
>   where
>     odbcTimeLit =
>         OdbcLiteral <$> choice [OLDate <$ keyword "d"
>                                ,OLTime <$ keyword "t"
>                                ,OLTimestamp <$ keyword "ts"]
>                     <*> singleQuotesOnlyStringTok
>     -- todo: this parser is too general, the expr part
>     -- should be only a function call (from a whitelist of functions)
>     -- or the extract operator
>     odbcFunc = OdbcFunc <$> (keyword "fn" *> scalarExpr)

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

> opTable :: Bool -> [[E.Operator [Token] ParseState Identity ScalarExpr]]
> opTable bExpr =
>         [-- parse match and quantified comparisons as postfix ops
>           -- todo: left factor the quantified comparison with regular
>           -- binary comparison, somehow
>          [E.Postfix $ try quantifiedComparisonSuffix
>          ,E.Postfix matchPredicateSuffix
>          ]

>         ,[binarySym "." E.AssocLeft]

>         ,[postfix' arraySuffix
>          ,postfix' collateSuffix]

>         ,[prefixSym "+", prefixSym "-"]

>         ,[binarySym "^" E.AssocLeft]

>         ,[binarySym "*" E.AssocLeft
>          ,binarySym "/" E.AssocLeft
>          ,binarySym "%" E.AssocLeft]

>         ,[binarySym "+" E.AssocLeft
>          ,binarySym "-" E.AssocLeft]

>         ,[binarySym "||" E.AssocRight
>          ,prefixSym "~"
>          ,binarySym "&" E.AssocRight
>          ,binarySym "|" E.AssocRight]

>         ,[binaryKeyword "overlaps" E.AssocNone]

>         ,[binaryKeyword "like" E.AssocNone
>          -- have to use try with inSuffix because of a conflict
>          -- with 'in' in position function, and not between
>          -- between also has a try in it to deal with 'not'
>          -- ambiguity
>          ,E.Postfix $ try inSuffix
>          ,E.Postfix betweenSuffix]
>          -- todo: figure out where to put the try?
>          ++ [binaryKeywords $ makeKeywordTree
>              ["not like"
>              ,"is similar to"
>              ,"is not similar to"]]
>          ++ [multisetBinOp]

>         ,[binarySym "<" E.AssocNone
>          ,binarySym ">" E.AssocNone
>          ,binarySym ">=" E.AssocNone
>          ,binarySym "<=" E.AssocNone
>          ,binarySym "!=" E.AssocRight
>          ,binarySym "<>" E.AssocRight
>          ,binarySym "=" E.AssocRight]

>         ,[postfixKeywords $ makeKeywordTree
>              ["is null"
>              ,"is not null"
>              ,"is true"
>              ,"is not true"
>              ,"is false"
>              ,"is not false"
>              ,"is unknown"
>              ,"is not unknown"]]
>          ++ [binaryKeywords $ makeKeywordTree
>              ["is distinct from"
>              ,"is not distinct from"]]

>         ,[prefixKeyword "not"]

>         ,if bExpr then [] else [binaryKeyword "and" E.AssocLeft]

>         ,[binaryKeyword "or" E.AssocLeft]

>        ]
>   where
>     binarySym nm assoc = binary (symbol_ nm) nm assoc
>     binaryKeyword nm assoc = binary (keyword_ nm) nm assoc
>     binaryKeywords p =
>         E.Infix (do
>                  o <- try p
>                  pure (\a b -> BinOp a [Name Nothing $ unwords o] b))
>             E.AssocNone
>     postfixKeywords p =
>       postfix' $ do
>           o <- try p
>           pure $ PostfixOp [Name Nothing $ unwords o]
>     binary p nm assoc =
>       E.Infix (p >> pure (\a b -> BinOp a [Name Nothing nm] b)) assoc
>     multisetBinOp = E.Infix (do
>         keyword_ "multiset"
>         o <- choice [Union <$ keyword_ "union"
>                     ,Intersect <$ keyword_ "intersect"
>                     ,Except <$ keyword_ "except"]
>         d <- option SQDefault duplicates
>         pure (\a b -> MultisetBinOp a o d b))
>           E.AssocLeft
>     prefixKeyword nm = prefix (keyword_ nm) nm
>     prefixSym nm = prefix (symbol_ nm) nm
>     prefix p nm = prefix' (p >> pure (PrefixOp [Name Nothing nm]))
>     -- hack from here
>     -- http://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported
>     -- not implemented properly yet
>     -- I don't think this will be enough for all cases
>     -- at least it works for 'not not a'
>     -- ok: "x is not true is not true"
>     -- no work: "x is not true is not null"
>     prefix'  p = E.Prefix  . chainl1 p $ pure       (.)
>     postfix' p = E.Postfix . chainl1 p $ pure (flip (.))

== scalar expression top level

This parses most of the scalar exprs.The order of the parsers and use
of try is carefully done to make everything work. It is a little
fragile and could at least do with some heavy explanation. Update: the
'try's have migrated into the individual parsers, they still need
documenting/fixing.

> scalarExpr :: Parser ScalarExpr
> scalarExpr = E.buildExpressionParser (opTable False) term

> term :: Parser ScalarExpr
> term = choice [simpleLiteral
>               ,parameter
>               ,positionalArg
>               ,star
>               ,parensExpr
>               ,caseExpr
>               ,cast
>               ,arrayCtor
>               ,multisetCtor
>               ,nextValueFor
>               ,subquery
>               ,intervalLit
>               ,specialOpKs
>               ,idenExpr
>               ,odbcExpr]
>        <?> "scalar expression"

expose the b expression for window frame clause range between

> scalarExprB :: Parser ScalarExpr
> scalarExprB = E.buildExpressionParser (opTable True) term

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

This is used in multiset operations (scalar expr), selects (query expr)
and set operations (query expr).

> duplicates :: Parser SetQuantifier
> duplicates =
>     choice [All <$ keyword_ "all"
>            ,Distinct <$ keyword "distinct"]

-------------------------------------------------

= query expressions

== select lists

> selectItem :: Parser (ScalarExpr,Maybe Name)
> selectItem = (,) <$> scalarExpr <*> optionMaybe als
>   where als = optional (keyword_ "as") *> name

> selectList :: Parser [(ScalarExpr,Maybe Name)]
> selectList = commaSep1 selectItem

== from

Here is the rough grammar for joins

tref
(cross | [natural] ([inner] | (left | right | full) [outer])) join
tref
[on expr | using (...)]

TODO: either use explicit 'operator precedence' parsers or build
expression parser for the 'tref operators' such as joins, lateral,
aliases.

> from :: Parser [TableRef]
> from = keyword_ "from" *> commaSep1 tref
>   where
>     -- TODO: use P (a->) for the join tref suffix
>     -- chainl or buildexpressionparser
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
>                  <$> parens (commaSep scalarExpr)
>                 ,pure $ TRSimple n]
>          -- todo: I think you can only have outer joins inside the oj,
>          -- not sure.
>         ,TROdbc <$> (symbol "{" *> keyword_ "oj" *> tref <* symbol "}")
>         ] <??> aliasSuffix
>     aliasSuffix = fromAlias <$$> TRAlias
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
>     [keyword_ "on" >> JoinOn <$> scalarExpr
>     ,keyword_ "using" >> JoinUsing <$> parens (commaSep1 name)]

> fromAlias :: Parser Alias
> fromAlias = Alias <$> tableAlias <*> columnAliases
>   where
>     tableAlias = optional (keyword_ "as") *> name
>     columnAliases = optionMaybe $ parens $ commaSep1 name

== simple other parts

Parsers for where, group by, having, order by and limit, which are
pretty trivial.

> whereClause :: Parser ScalarExpr
> whereClause = keyword_ "where" *> scalarExpr

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
>       ,SimpleGroup <$> scalarExpr
>       ]

> having :: Parser ScalarExpr
> having = keyword_ "having" *> scalarExpr

> orderBy :: Parser [SortSpec]
> orderBy = keywords_ ["order","by"] *> commaSep1 ob
>   where
>     ob = SortSpec
>          <$> scalarExpr
>          <*> option DirDefault (choice [Asc <$ keyword_ "asc"
>                                        ,Desc <$ keyword_ "desc"])
>          <*> option NullsOrderDefault
>              -- todo: left factor better
>              (keyword_ "nulls" >>
>                     choice [NullsFirst <$ keyword "first"
>                            ,NullsLast <$ keyword "last"])

allows offset and fetch in either order
+ postgresql offset without row(s) and limit instead of fetch also

> offsetFetch :: Parser (Maybe ScalarExpr, Maybe ScalarExpr)
> offsetFetch = permute ((,) <$?> (Nothing, Just <$> offset)
>                            <|?> (Nothing, Just <$> fetch))

> offset :: Parser ScalarExpr
> offset = keyword_ "offset" *> scalarExpr
>          <* option () (choice [keyword_ "rows"
>                               ,keyword_ "row"])

> fetch :: Parser ScalarExpr
> fetch = fetchFirst <|> limit
>   where
>     fetchFirst = guardDialect diFetchFirst
>                  *> fs *> scalarExpr <* ro
>     fs = makeKeywordTree ["fetch first", "fetch next"]
>     ro = makeKeywordTree ["rows only", "row only"]
>     -- todo: not in ansi sql dialect
>     limit = guardDialect diLimit *>
>             keyword_ "limit" *> scalarExpr

== common table expressions

> with :: Parser QueryExpr
> with = keyword_ "with" >>
>     With <$> option False (True <$ keyword_ "recursive")
>          <*> commaSep1 withQuery <*> queryExpr
>   where
>     withQuery = (,) <$> (withAlias <* keyword_ "as")
>                     <*> parens queryExpr
>     withAlias = Alias <$> name <*> columnAliases
>     columnAliases = optionMaybe $ parens $ commaSep1 name


== query expression

This parser parses any query expression variant: normal select, cte,
and union, etc..

> queryExpr :: Parser QueryExpr
> queryExpr = choice
>     [with
>     ,chainr1 (choice [values,table, select]) setOp]
>   where
>     select = keyword_ "select" >>
>         mkSelect
>         <$> option SQDefault duplicates
>         <*> selectList
>         <*> optionMaybe tableExpression
>     mkSelect d sl Nothing =
>         makeSelect{qeSetQuantifier = d, qeSelectList = sl}
>     mkSelect d sl (Just (TableExpression f w g h od ofs fe)) =
>         Select d sl f w g h od ofs fe
>     values = keyword_ "values"
>              >> Values <$> commaSep (parens (commaSep scalarExpr))
>     table = keyword_ "table" >> Table <$> names

local data type to help with parsing the bit after the select list,
called 'table expression' in the ansi sql grammar. Maybe this should
be in the public syntax?

> data TableExpression
>     = TableExpression
>       {_teFrom :: [TableRef]
>       ,_teWhere :: Maybe ScalarExpr
>       ,_teGroupBy :: [GroupingExpr]
>       ,_teHaving :: Maybe ScalarExpr
>       ,_teOrderBy :: [SortSpec]
>       ,_teOffset :: Maybe ScalarExpr
>       ,_teFetchFirst :: Maybe ScalarExpr}

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

> setOp :: Parser (QueryExpr -> QueryExpr -> QueryExpr)
> setOp = cq
>         <$> setOpK
>         <*> option SQDefault duplicates
>         <*> corr
>   where
>     cq o d c q0 q1 = QueryExprSetOp q0 o d c q1
>     setOpK = choice [Union <$ keyword_ "union"
>                     ,Intersect <$ keyword_ "intersect"
>                     ,Except <$ keyword_ "except"]
>             <?> "set operator"
>     corr = option Respectively (Corresponding <$ keyword_ "corresponding")


wrapper for query expr which ignores optional trailing semicolon.

TODO: change style

> topLevelQueryExpr :: Parser QueryExpr
> topLevelQueryExpr = queryExpr <??> (id <$ semi)

> topLevelStatement :: Parser Statement
> topLevelStatement = statement <??> (id <$ semi)

-------------------------

= Statements

> statement :: Parser Statement
> statement = choice
>     [keyword_ "create" *> choice [createSchema
>                                  ,createTable
>                                  ,createIndex
>                                  ,createView
>                                  ,createDomain
>                                  ,createSequence
>                                  ,createRole
>                                  ,createAssertion]
>     ,keyword_ "alter" *> choice [alterTable
>                                 ,alterDomain
>                                 ,alterSequence]
>     ,keyword_ "drop" *> choice [dropSchema
>                                ,dropTable
>                                ,dropView
>                                ,dropDomain
>                                ,dropSequence
>                                ,dropRole
>                                ,dropAssertion]
>     ,delete
>     ,truncateSt
>     ,insert
>     ,update
>     ,startTransaction
>     ,savepoint
>     ,releaseSavepoint
>     ,commit
>     ,rollback
>     ,grant
>     ,revoke
>     ,SelectStatement <$> queryExpr
>     ]

> createSchema :: Parser Statement
> createSchema = keyword_ "schema" >>
>     CreateSchema <$> names

> createTable :: Parser Statement
> createTable = keyword_ "table" >>
>     CreateTable
>     <$> names
>     -- todo: is this order mandatory or is it a perm?
>     <*> parens (commaSep1 (uncurry TableConstraintDef <$> tableConstraintDef
>                            <|> TableColumnDef <$> columnDef))

> createIndex :: Parser Statement
> createIndex =
>     CreateIndex
>     <$> ((keyword_ "index" >> pure False) <|>
>          (keywords_ ["unique", "index"] >> pure True))
>     <*> names
>     <*> (keyword_ "on" >> name)
>     <*> parens (commaSep1 name)

> columnDef :: Parser ColumnDef
> columnDef = ColumnDef <$> name <*> typeName
>             <*> optionMaybe defaultClause
>             <*> option [] (many1 colConstraintDef)
>   where
>     defaultClause = choice [
>         keyword_ "default" >>
>         DefaultClause <$> scalarExpr
>         -- todo: left factor
>        ,try (keywords_ ["generated","always","as"] >>
>              GenerationClause <$> parens scalarExpr)
>        ,keyword_ "generated" >>
>         IdentityColumnSpec
>         <$> (GeneratedAlways <$ keyword_ "always"
>              <|> GeneratedByDefault <$ keywords_ ["by", "default"])
>         <*> (keywords_ ["as", "identity"] *>
>              option [] (parens sequenceGeneratorOptions))
>        ]

> tableConstraintDef :: Parser (Maybe [Name], TableConstraint)
> tableConstraintDef =
>     (,)
>     <$> (optionMaybe (keyword_ "constraint" *> names))
>     <*> (unique <|> primaryKey <|> check <|> references)
>   where
>     unique = keyword_ "unique" >>
>         TableUniqueConstraint <$> parens (commaSep1 name)
>     primaryKey = keywords_ ["primary", "key"] >>
>         TablePrimaryKeyConstraint <$> parens (commaSep1 name)
>     check = keyword_ "check" >> TableCheckConstraint <$> parens scalarExpr
>     references = keywords_ ["foreign", "key"] >>
>         (\cs ft ftcs m (u,d) -> TableReferencesConstraint cs ft ftcs m u d)
>         <$> parens (commaSep1 name)
>         <*> (keyword_ "references" *> names)
>         <*> optionMaybe (parens $ commaSep1 name)
>         <*> refMatch
>         <*> refActions

> refMatch :: Parser ReferenceMatch
> refMatch = option DefaultReferenceMatch
>             (keyword_ "match" *>
>              choice [MatchFull <$ keyword_ "full"
>                     ,MatchPartial <$ keyword_ "partial"
>                     ,MatchSimple <$ keyword_ "simple"])
> refActions :: Parser (ReferentialAction,ReferentialAction)
> refActions = permute ((,) <$?> (DefaultReferentialAction, onUpdate)
>                           <|?> (DefaultReferentialAction, onDelete))
>   where
>     -- todo: left factor?
>     onUpdate = try (keywords_ ["on", "update"]) *> referentialAction
>     onDelete = try (keywords_ ["on", "delete"]) *> referentialAction
>     referentialAction = choice [
>          RefCascade <$ keyword_ "cascade"
>          -- todo: left factor?
>         ,RefSetNull <$ try (keywords_ ["set", "null"])
>         ,RefSetDefault <$ try (keywords_ ["set", "default"])
>         ,RefRestrict <$ keyword_ "restrict"
>         ,RefNoAction <$ keywords_ ["no", "action"]]

> colConstraintDef :: Parser ColConstraintDef
> colConstraintDef =
>     ColConstraintDef
>     <$> (optionMaybe (keyword_ "constraint" *> names))
>     <*> (nullable <|> notNull <|> unique <|> primaryKey <|> check <|> references)
>   where
>     nullable = ColNullableConstraint <$ keyword "null"
>     notNull = ColNotNullConstraint <$ keywords_ ["not", "null"]
>     unique = ColUniqueConstraint <$ keyword_ "unique"
>     primaryKey = ColPrimaryKeyConstraint <$ keywords_ ["primary", "key"]
>     check = keyword_ "check" >> ColCheckConstraint <$> parens scalarExpr
>     references = keyword_ "references" >>
>         (\t c m (ou,od) -> ColReferencesConstraint t c m ou od)
>         <$> names
>         <*> optionMaybe (parens name)
>         <*> refMatch
>         <*> refActions

slightly hacky parser for signed integers

> signedInteger :: Parser Integer
> signedInteger =
>     (*) <$> option 1 (1 <$ symbol "+" <|> (-1) <$ symbol "-")
>     <*> unsignedInteger

> sequenceGeneratorOptions :: Parser [SequenceGeneratorOption]
> sequenceGeneratorOptions =
>          -- todo: could try to combine exclusive options
>          -- such as cycle and nocycle
>          -- sort out options which are sometimes not allowed
>          -- as datatype, and restart with
>     permute ((\a b c d e f g h j k -> catMaybes [a,b,c,d,e,f,g,h,j,k])
>                   <$?> nj startWith
>                   <|?> nj dataType
>                   <|?> nj restart
>                   <|?> nj incrementBy
>                   <|?> nj maxValue
>                   <|?> nj noMaxValue
>                   <|?> nj minValue
>                   <|?> nj noMinValue
>                   <|?> nj scycle
>                   <|?> nj noCycle
>                  )
>   where
>     nj p = (Nothing,Just <$> p)
>     startWith = keywords_ ["start", "with"] >>
>                 SGOStartWith <$> signedInteger
>     dataType = keyword_ "as" >>
>                SGODataType <$> typeName
>     restart = keyword_ "restart" >>
>               SGORestart <$> optionMaybe (keyword_ "with" *> signedInteger)
>     incrementBy = keywords_ ["increment", "by"] >>
>                 SGOIncrementBy <$> signedInteger
>     maxValue = keyword_ "maxvalue" >>
>                 SGOMaxValue <$> signedInteger
>     noMaxValue = SGONoMaxValue <$ try (keywords_ ["no","maxvalue"])
>     minValue = keyword_ "minvalue" >>
>                 SGOMinValue <$> signedInteger
>     noMinValue = SGONoMinValue <$ try (keywords_ ["no","minvalue"])
>     scycle = SGOCycle <$ keyword_ "cycle"
>     noCycle = SGONoCycle <$ try (keywords_ ["no","cycle"])


> alterTable :: Parser Statement
> alterTable = keyword_ "table" >>
>     -- the choices have been ordered so that it works
>     AlterTable <$> names <*> choice [addConstraint
>                                     ,dropConstraint
>                                     ,addColumnDef
>                                     ,alterColumn
>                                     ,dropColumn
>                                     ]
>   where
>     addColumnDef = try (keyword_ "add"
>                         *> optional (keyword_ "column")) >>
>                    AddColumnDef <$> columnDef
>     alterColumn = keyword_ "alter" >> optional (keyword_ "column") >>
>                   name <**> choice [setDefault
>                                    ,dropDefault
>                                    ,setNotNull
>                                    ,dropNotNull
>                                    ,setDataType]
>     setDefault :: Parser (Name -> AlterTableAction)
>     -- todo: left factor
>     setDefault = try (keywords_ ["set","default"]) >>
>                  scalarExpr <$$> AlterColumnSetDefault
>     dropDefault = AlterColumnDropDefault <$ try (keywords_ ["drop","default"])
>     setNotNull = AlterColumnSetNotNull <$ try (keywords_ ["set","not","null"])
>     dropNotNull = AlterColumnDropNotNull <$ try (keywords_ ["drop","not","null"])
>     setDataType = try (keywords_ ["set","data","type"]) >>
>                   typeName <$$> AlterColumnSetDataType
>     dropColumn = try (keyword_ "drop" *> optional (keyword_ "column")) >>
>                  DropColumn <$> name <*> dropBehaviour
>     -- todo: left factor, this try is especially bad
>     addConstraint = try (keyword_ "add" >>
>         uncurry AddTableConstraintDef <$> tableConstraintDef)
>     dropConstraint = try (keywords_ ["drop","constraint"]) >>
>         DropTableConstraintDef <$> names <*> dropBehaviour


> dropSchema :: Parser Statement
> dropSchema = keyword_ "schema" >>
>     DropSchema <$> names <*> dropBehaviour

> dropTable :: Parser Statement
> dropTable = keyword_ "table" >>
>     DropTable <$> names <*> dropBehaviour

> createView :: Parser Statement
> createView =
>     CreateView
>     <$> (option False (True <$ keyword_ "recursive") <* keyword_ "view")
>     <*> names
>     <*> optionMaybe (parens (commaSep1 name))
>     <*> (keyword_ "as" *> queryExpr)
>     <*> optionMaybe (choice [
>             -- todo: left factor
>             DefaultCheckOption <$ try (keywords_ ["with", "check", "option"])
>            ,CascadedCheckOption <$ try (keywords_ ["with", "cascaded", "check", "option"])
>            ,LocalCheckOption <$ try (keywords_ ["with", "local", "check", "option"])
>             ])

> dropView :: Parser Statement
> dropView = keyword_ "view" >>
>     DropView <$> names <*> dropBehaviour

> createDomain :: Parser Statement
> createDomain = keyword_ "domain" >>
>     CreateDomain
>     <$> names
>     <*> (optional (keyword_ "as") *> typeName)
>     <*> optionMaybe (keyword_ "default" *> scalarExpr)
>     <*> many con
>   where
>     con = (,) <$> optionMaybe (keyword_ "constraint" *> names)
>           <*> (keyword_ "check" *> parens scalarExpr)

> alterDomain :: Parser Statement
> alterDomain = keyword_ "domain" >>
>     AlterDomain
>     <$> names
>     <*> (setDefault <|> constraint
>          <|> (keyword_ "drop" *> (dropDefault <|> dropConstraint)))
>   where
>     setDefault = keywords_ ["set", "default"] >> ADSetDefault <$> scalarExpr
>     constraint = keyword_ "add" >>
>        ADAddConstraint
>        <$> optionMaybe (keyword_ "constraint" *> names)
>        <*> (keyword_ "check" *> parens scalarExpr)
>     dropDefault = ADDropDefault <$ keyword_ "default"
>     dropConstraint = keyword_ "constraint" >> ADDropConstraint <$> names

> dropDomain :: Parser Statement
> dropDomain = keyword_ "domain" >>
>     DropDomain <$> names <*> dropBehaviour

> createSequence :: Parser Statement
> createSequence = keyword_ "sequence" >>
>     CreateSequence
>     <$> names
>     <*> sequenceGeneratorOptions

> alterSequence :: Parser Statement
> alterSequence = keyword_ "sequence" >>
>     AlterSequence
>     <$> names
>     <*> sequenceGeneratorOptions

> dropSequence :: Parser Statement
> dropSequence = keyword_ "sequence" >>
>     DropSequence <$> names <*> dropBehaviour

> createAssertion :: Parser Statement
> createAssertion = keyword_ "assertion" >>
>     CreateAssertion
>     <$> names
>     <*> (keyword_ "check" *> parens scalarExpr)


> dropAssertion :: Parser Statement
> dropAssertion = keyword_ "assertion" >>
>     DropAssertion <$> names <*> dropBehaviour

-----------------

= dml

> delete :: Parser Statement
> delete = keywords_ ["delete","from"] >>
>     Delete
>     <$> names
>     <*> optionMaybe (optional (keyword_ "as") *> name)
>     <*> optionMaybe (keyword_ "where" *> scalarExpr)

> truncateSt :: Parser Statement
> truncateSt = keywords_ ["truncate", "table"] >>
>     Truncate
>     <$> names
>     <*> option DefaultIdentityRestart
>         (ContinueIdentity <$ keywords_ ["continue","identity"]
>          <|> RestartIdentity <$ keywords_ ["restart","identity"])

> insert :: Parser Statement
> insert = keywords_ ["insert", "into"] >>
>     Insert
>     <$> names
>     <*> optionMaybe (parens $ commaSep1 name)
>     <*> (DefaultInsertValues <$ keywords_ ["default", "values"]
>          <|> InsertQuery <$> queryExpr)

> update :: Parser Statement
> update = keywords_ ["update"] >>
>     Update
>     <$> names
>     <*> optionMaybe (optional (keyword_ "as") *> name)
>     <*> (keyword_ "set" *> commaSep1 setClause)
>     <*> optionMaybe (keyword_ "where" *> scalarExpr)
>   where
>     setClause = multipleSet <|> singleSet
>     multipleSet = SetMultiple
>                   <$> parens (commaSep1 names)
>                   <*> (symbol "=" *> parens (commaSep1 scalarExpr))
>     singleSet = Set
>                 <$> names
>                 <*> (symbol "=" *> scalarExpr)

> dropBehaviour :: Parser DropBehaviour
> dropBehaviour =
>     option DefaultDropBehaviour
>     (Restrict <$ keyword_ "restrict"
>     <|> Cascade <$ keyword_ "cascade")

-----------------------------

= transaction management

> startTransaction :: Parser Statement
> startTransaction = StartTransaction <$ keywords_ ["start","transaction"]

> savepoint :: Parser Statement
> savepoint = keyword_ "savepoint" >>
>     Savepoint <$> name

> releaseSavepoint :: Parser Statement
> releaseSavepoint = keywords_ ["release","savepoint"] >>
>     ReleaseSavepoint <$> name

> commit :: Parser Statement
> commit = Commit <$ keyword_ "commit" <* optional (keyword_ "work")

> rollback :: Parser Statement
> rollback = keyword_ "rollback" >> optional (keyword_ "work") >>
>     Rollback <$> optionMaybe (keywords_ ["to", "savepoint"] *> name)


------------------------------

= Access control

TODO: fix try at the 'on'

> grant :: Parser Statement
> grant = keyword_ "grant" >> (try priv <|> role)
>   where
>     priv = GrantPrivilege
>            <$> commaSep privilegeAction
>            <*> (keyword_ "on" *> privilegeObject)
>            <*> (keyword_ "to" *> commaSep name)
>            <*> option WithoutGrantOption
>                (WithGrantOption <$ keywords_ ["with","grant","option"])
>     role = GrantRole
>            <$> commaSep name
>            <*> (keyword_ "to" *> commaSep name)
>            <*> option WithoutAdminOption
>                (WithAdminOption <$ keywords_ ["with","admin","option"])

> createRole :: Parser Statement
> createRole = keyword_ "role" >>
>     CreateRole <$> name

> dropRole :: Parser Statement
> dropRole = keyword_ "role" >>
>     DropRole <$> name

TODO: fix try at the 'on'

> revoke :: Parser Statement
> revoke = keyword_ "revoke" >> (try priv <|> role)
>   where
>     priv = RevokePrivilege
>            <$> option NoGrantOptionFor
>                (GrantOptionFor <$ keywords_ ["grant","option","for"])
>            <*> commaSep privilegeAction
>            <*> (keyword_ "on" *> privilegeObject)
>            <*> (keyword_ "from" *> commaSep name)
>            <*> dropBehaviour
>     role = RevokeRole
>            <$> option NoAdminOptionFor
>                (AdminOptionFor <$ keywords_ ["admin","option", "for"])
>            <*> commaSep name
>            <*> (keyword_ "from" *> commaSep name)
>            <*> dropBehaviour

> privilegeAction :: Parser PrivilegeAction
> privilegeAction = choice
>     [PrivAll <$ keywords_ ["all","privileges"]
>     ,keyword_ "select" >>
>      PrivSelect <$> option [] (parens $ commaSep name)
>     ,PrivDelete <$ keyword_ "delete"
>     ,PrivUsage <$ keyword_ "usage"
>     ,PrivTrigger <$ keyword_ "trigger"
>     ,PrivExecute <$ keyword_ "execute"
>     ,keyword_ "insert" >>
>      PrivInsert <$> option [] (parens $ commaSep name)
>     ,keyword_ "update" >>
>      PrivUpdate <$> option [] (parens $ commaSep name)
>     ,keyword_ "references" >>
>      PrivReferences <$> option [] (parens $ commaSep name)
>     ]

> privilegeObject :: Parser PrivilegeObject
> privilegeObject = choice
>     [keyword_ "domain" >> PrivDomain <$> names
>     ,keyword_ "type" >> PrivType <$> names
>     ,keyword_ "sequence" >> PrivSequence <$> names
>     ,keywords_ ["specific","function"] >> PrivFunction <$> names
>     ,optional (keyword_ "table") >> PrivTable <$> names
>     ]


----------------------------

wrapper to parse a series of statements. They must be separated by
semicolon, but for the last statement, the trailing semicolon is
optional.

TODO: change style

> statements :: Parser [Statement]
> statements = (:[]) <$> statement
>              >>= optionSuffix ((semi *>) . pure)
>              >>= optionSuffix (\p -> (p++) <$> statements)

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
>           then pr <|> pure [k]
>           else pr
>     parseGroup _ = guard False >> error "impossible"
>     safeHead (x:_) = Just x
>     safeHead [] = Nothing
>     safeTail (_:x) = Just x
>     safeTail [] = Nothing

------------------------------------------------

= lexing

TODO: push checks into here:
keyword blacklists
unsigned integer match
symbol matching
keyword matching

> stringTok :: Parser (String,String,String)
> stringTok = mytoken (\tok ->
>     case tok of
>       L.SqlString s e t -> Just (s,e,t)
>       _ -> Nothing)

> singleQuotesOnlyStringTok :: Parser String
> singleQuotesOnlyStringTok = mytoken (\tok ->
>     case tok of
>       L.SqlString "'" "'" t -> Just t
>       _ -> Nothing)

This is to support SQL strings where you can write
'part of a string' ' another part'
and it will parse as a single string

It is only allowed when all the strings are quoted with ' atm.

> stringTokExtend :: Parser (String,String,String)
> stringTokExtend = do
>     (s,e,x) <- stringTok
>     choice [
>          do
>          guard (s == "'" && e == "'")
>          (s',e',y) <- stringTokExtend
>          guard (s' == "'" && e' == "'")
>          return $ (s,e,x ++ y)
>         ,return (s,e,x)
>         ]

> hostParamTok :: Parser String
> hostParamTok = mytoken (\tok ->
>     case tok of
>       L.PrefixedVariable c p -> Just (c:p)
>       _ -> Nothing)

> positionalArgTok :: Parser Int
> positionalArgTok = mytoken (\tok ->
>     case tok of
>       L.PositionalArg p -> Just p
>       _ -> Nothing)


> sqlNumberTok :: Bool -> Parser String
> sqlNumberTok intOnly = mytoken (\tok ->
>     case tok of
>       L.SqlNumber p | not intOnly || all isDigit p -> Just p
>       _ -> Nothing)


> symbolTok :: Maybe String -> Parser String
> symbolTok sym = mytoken (\tok ->
>     case (sym,tok) of
>       (Nothing, L.Symbol p) -> Just p
>       (Just s, L.Symbol p) | s == p -> Just p
>       _ -> Nothing)

> identifierTok :: [String] -> Parser (Maybe (String,String), String)
> identifierTok blackList = mytoken (\tok ->
>     case tok of
>       L.Identifier q@(Just {}) p -> Just (q,p)
>       L.Identifier q p | map toLower p `notElem` blackList -> Just (q,p)
>       _ -> Nothing)

> unquotedIdentifierTok :: [String] -> Maybe String -> Parser String
> unquotedIdentifierTok blackList kw = mytoken (\tok ->
>     case (kw,tok) of
>       (Nothing, L.Identifier Nothing p) | map toLower p `notElem` blackList -> Just p
>       (Just k, L.Identifier Nothing p) | k == map toLower p -> Just p
>       _ -> Nothing)

> mytoken :: (L.Token -> Maybe a) -> Parser a
> mytoken test = token showToken posToken testToken
>   where
>     showToken (_,tok)   = show tok
>     posToken  ((a,b,c),_)  = newPos a b c
>     testToken (_,tok)   = test tok

> unsignedInteger :: Parser Integer
> unsignedInteger = read <$> sqlNumberTok True <?> "natural number"

todo: work out the symbol parsing better

> symbol :: String -> Parser String
> symbol s = symbolTok (Just s) <?> s

> singleCharSymbol :: Char -> Parser Char
> singleCharSymbol c = c <$ symbol [c]

> questionMark :: Parser Char
> questionMark = singleCharSymbol '?' <?> "question mark"

> openParen :: Parser Char
> openParen = singleCharSymbol '('

> closeParen :: Parser Char
> closeParen = singleCharSymbol ')'

> openBracket :: Parser Char
> openBracket = singleCharSymbol '['

> closeBracket :: Parser Char
> closeBracket = singleCharSymbol ']'


> comma :: Parser Char
> comma = singleCharSymbol ','

> semi :: Parser Char
> semi = singleCharSymbol ';'

= helper functions

> keyword :: String -> Parser String
> keyword k = unquotedIdentifierTok [] (Just k) <?> k

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

> blacklist :: Dialect -> [String]
> blacklist d = diKeywords d

These blacklisted names are mostly needed when we parse something with
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



-----------

Used to make the dialect available during parsing so different parsers
can be used for different dialects. Not sure if this is the best way
to do it, but it's convenient

> type ParseState = Dialect

> type Token = ((String,Int,Int),L.Token)

> type Parser = GenParser Token ParseState

> guardDialect :: (Dialect -> Bool) -> Parser ()
> guardDialect f = do
>     d <- getState
>     guard (f d)

The dialect stuff could also be used for custom options: e.g. to only
parse dml for instance.
