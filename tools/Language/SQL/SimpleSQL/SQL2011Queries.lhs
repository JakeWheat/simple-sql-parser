
This file goes through the grammar for SQL 2011 (using the draft standard).

We are only looking at the query syntax, and no other parts.

There are other files which cover some of the other sections.
Possible sections not covered yet:
13 modules
16 control statements
20 dynamic
22 direct
23 diagnostics



The goal is to create some example tests for each bit of grammar, with
some areas getting more comprehensive coverage tests, and also to note
which parts aren't currently supported.

> module Language.SQL.SimpleSQL.SQL2011Queries (sql2011QueryTests) where
> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax

> sql2011QueryTests :: TestItem
> sql2011QueryTests = Group "sql 2011 query tests"
>     [literals
>     ,identifiers
>     ,typeNameTests
>     ,fieldDefinition
>     ,valueExpressions
>     ,queryExpressions
>     ,scalarSubquery
>     ,predicates
>     ,intervalQualifier
>     ,collateClause
>     ,aggregateFunction
>     ,sortSpecificationList
>     ]

= 5 Lexical elements

The tests don't make direct use of these definitions.

== 5.1 <SQL terminal character>

Function

Define the terminal symbols of the SQL language and the elements of
strings.

<SQL terminal character> ::= <SQL language character>

<SQL language character> ::=
    <simple Latin letter>
  | <digit>
  | <SQL special character>

<simple Latin letter> ::=
    <simple Latin upper case letter>
  | <simple Latin lower case letter>

<simple Latin upper case letter> ::=
    A | B | C | D | E | F | G | H | I | J | K | L | M | N | O
  | P | Q | R | S | T | U | V | W | X | Y | Z

<simple Latin lower case letter> ::=
    a | b | c | d | e | f | g | h | i | j | k | l | m | n | o
  | p | q | r | s | t | u | v | w | x | y | z

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

<SQL special character> ::=
    <space>
  | <double quote>
  | <percent>
  | <ampersand>
  | <quote>
  | <left paren>
  | <right paren>
  | <asterisk>
  | <plus sign>
  | <comma>
  | <minus sign>
  | <period>
  | <solidus>
  | <colon>
  | <semicolon>
  | <less than operator>
  | <equals operator>
  | <greater than operator>
  | <question mark>
  | <left bracket>
  | <right bracket>
  | <circumflex>
  | <underscore>
  | <vertical bar>
  | <left brace>
  | <right brace>

<space> ::= !! See the Syntax Rules.

<double quote> ::= "

<percent> ::= %

<ampersand> ::= &

<quote> ::= '

<left paren> ::= (

<right paren> ::= )

<asterisk> ::= *

<plus sign> ::= +

<comma> ::= ,

<minus sign> ::= -

<period> ::= .

<solidus> ::= /

<reverse solidus> ::= \

<colon> ::= :

<semicolon> ::= ;

<less than operator> ::= <

<equals operator> ::= =

<greater than operator> ::= >

<question mark> ::= ?

<left bracket or trigraph> ::= <left bracket> | <left bracket trigraph>

<right bracket or trigraph> ::= <right bracket> | <right bracket trigraph>

<left bracket> ::= [

<left bracket trigraph> ::= ??(

<right bracket> ::= ]

<right bracket trigraph> ::= ??)

<circumflex> ::= ^

<underscore> ::= _

<vertical bar> ::= |

<left brace> ::= {

<right brace> ::= }

== 5.2 <token> and <separator>

Function

Specify lexical units (tokens and separators) that participate in SQL
language.

<token> ::= <nondelimiter token> | <delimiter token>

<nondelimiter token> ::=
    <regular identifier>
  | <key word>
  | <unsigned numeric literal>
  | <national character string literal>
  | <binary string literal>
  | <large object length token>
  | <Unicode delimited identifier>
  | <Unicode character string literal>
  | <SQL language identifier>

<regular identifier> ::= <identifier body>

<identifier body> ::= <identifier start> [ <identifier part>... ]

<identifier part> ::= <identifier start> | <identifier extend>

<identifier start> ::= !! See the Syntax Rules.

<identifier extend> ::= !! See the Syntax Rules.

<large object length token> ::= <digit>... <multiplier>

<multiplier> ::= K | M | G | T | P

<delimited identifier> ::=
  <double quote> <delimited identifier body> <double quote>

<delimited identifier body> ::= <delimited identifier part>...

<delimited identifier part> ::=
    <nondoublequote character>
  | <doublequote symbol>

<Unicode delimited identifier> ::=
  U <ampersand> <double quote> <Unicode delimiter body> <double quote>
      <Unicode escape specifier>

<Unicode escape specifier> ::=
  [ UESCAPE <quote> <Unicode escape character> <quote> ]

<Unicode delimiter body> ::= <Unicode identifier part>...

<Unicode identifier part> ::=
    <delimited identifier part>
  | <Unicode escape value>

<Unicode escape value> ::=
    <Unicode 4 digit escape value>
  | <Unicode 6 digit escape value>
  | <Unicode character escape value>

<Unicode 4 digit escape value> ::=
  <Unicode escape character> <hexit> <hexit> <hexit> <hexit>

<Unicode 6 digit escape value> ::=
  <Unicode escape character> <plus sign>
      <hexit> <hexit> <hexit> <hexit> <hexit> <hexit>

<Unicode character escape value> ::=
  <Unicode escape character> <Unicode escape character>

<Unicode escape character> ::= !! See the Syntax Rules.

<nondoublequote character> ::= !! See the Syntax Rules.

<doublequote symbol> ::= ""!! two consecutive double quote characters

<delimiter token> ::=
    <character string literal>
  | <date string>
  | <time string>
  | <timestamp string>
  | <interval string>
  | <delimited identifier>
  | <SQL special character>
  | <not equals operator>
  | <greater than or equals operator>
  | <less than or equals operator>
  | <concatenation operator>
  | <right arrow>
  | <left bracket trigraph>
  | <right bracket trigraph>
  | <double colon>
  | <double period>
  | <named argument assignment token>

<not equals operator> ::= <>

<greater than or equals operator> ::= >=

<less than or equals operator> ::= <=

<concatenation operator> ::= ||

<right arrow> ::= ->

<double colon> ::= ::

<double period> ::= ..

<named argument assignment token> ::= =>

<separator> ::= { <comment> | <white space> }...

<white space> ::= !! See the Syntax Rules.

<comment> ::= <simple comment> | <bracketed comment>

<simple comment> ::=
  <simple comment introducer> [ <comment character>... ] <newline>

<simple comment introducer> ::= <minus sign> <minus sign>

<bracketed comment> ::=
  <bracketed comment introducer>
      <bracketed comment contents>
      <bracketed comment terminator>

<bracketed comment introducer> ::= /*

<bracketed comment terminator> ::= */

<bracketed comment contents> ::=
  [ { <comment character> | <separator> }... ]!! See the Syntax Rules.

<comment character> ::= <nonquote character> | <quote>

<newline> ::= !! See the Syntax Rules.

<key word> ::= <reserved word> | <non-reserved word>

<non-reserved word> ::=
    A | ABSOLUTE | ACTION | ADA | ADD | ADMIN | AFTER | ALWAYS | ASC
  | ASSERTION | ASSIGNMENT | ATTRIBUTE | ATTRIBUTES

  | BEFORE | BERNOULLI | BREADTH

  | C | CASCADE | CATALOG | CATALOG_NAME | CHAIN | CHARACTER_SET_CATALOG
  | CHARACTER_SET_NAME | CHARACTER_SET_SCHEMA | CHARACTERISTICS | CHARACTERS
  | CLASS_ORIGIN | COBOL | COLLATION | COLLATION_CATALOG | COLLATION_NAME | COLLATION_SCHEMA
  | COLUMN_NAME | COMMAND_FUNCTION | COMMAND_FUNCTION_CODE | COMMITTED
  | CONDITION_NUMBER | CONNECTION | CONNECTION_NAME | CONSTRAINT_CATALOG | CONSTRAINT_NAME
  | CONSTRAINT_SCHEMA | CONSTRAINTS | CONSTRUCTOR | CONTINUE | CURSOR_NAME

  | DATA | DATETIME_INTERVAL_CODE | DATETIME_INTERVAL_PRECISION | DEFAULTS | DEFERRABLE
  | DEFERRED | DEFINED | DEFINER | DEGREE | DEPTH | DERIVED | DESC | DESCRIPTOR
  | DIAGNOSTICS | DISPATCH | DOMAIN | DYNAMIC_FUNCTION | DYNAMIC_FUNCTION_CODE

  | ENFORCED | EXCLUDE | EXCLUDING | EXPRESSION

  | FINAL | FIRST | FLAG | FOLLOWING | FORTRAN | FOUND

  | G | GENERAL | GENERATED | GO | GOTO | GRANTED

  | HIERARCHY

  | IGNORE | IMMEDIATE | IMMEDIATELY | IMPLEMENTATION | INCLUDING | INCREMENT | INITIALLY
  | INPUT | INSTANCE | INSTANTIABLE | INSTEAD | INVOKER | ISOLATION

  | K | KEY | KEY_MEMBER | KEY_TYPE

  | LAST | LENGTH | LEVEL | LOCATOR

  | M | MAP | MATCHED | MAXVALUE | MESSAGE_LENGTH | MESSAGE_OCTET_LENGTH
  | MESSAGE_TEXT | MINVALUE | MORE | MUMPS

  | NAME | NAMES | NESTING | NEXT | NFC | NFD | NFKC | NFKD
  | NORMALIZED | NULLABLE | NULLS | NUMBER

  | OBJECT | OCTETS | OPTION | OPTIONS | ORDERING | ORDINALITY | OTHERS
  | OUTPUT | OVERRIDING

  | P | PAD | PARAMETER_MODE | PARAMETER_NAME | PARAMETER_ORDINAL_POSITION
  | PARAMETER_SPECIFIC_CATALOG | PARAMETER_SPECIFIC_NAME | PARAMETER_SPECIFIC_SCHEMA
  | PARTIAL | PASCAL | PATH | PLACING | PLI | PRECEDING | PRESERVE | PRIOR
  | PRIVILEGES | PUBLIC

  | READ | RELATIVE | REPEATABLE | RESPECT | RESTART | RESTRICT | RETURNED_CARDINALITY
  | RETURNED_LENGTH | RETURNED_OCTET_LENGTH | RETURNED_SQLSTATE | ROLE
  | ROUTINE | ROUTINE_CATALOG | ROUTINE_NAME | ROUTINE_SCHEMA | ROW_COUNT

  | SCALE | SCHEMA | SCHEMA_NAME | SCOPE_CATALOG | SCOPE_NAME | SCOPE_SCHEMA
  | SECTION | SECURITY | SELF | SEQUENCE | SERIALIZABLE | SERVER_NAME | SESSION
  | SETS | SIMPLE | SIZE | SOURCE | SPACE | SPECIFIC_NAME | STATE | STATEMENT
  | STRUCTURE | STYLE | SUBCLASS_ORIGIN

  | T | TABLE_NAME | TEMPORARY | TIES | TOP_LEVEL_COUNT | TRANSACTION
  | TRANSACTION_ACTIVE | TRANSACTIONS_COMMITTED | TRANSACTIONS_ROLLED_BACK
  | TRANSFORM | TRANSFORMS | TRIGGER_CATALOG | TRIGGER_NAME | TRIGGER_SCHEMA | TYPE

  | UNBOUNDED | UNCOMMITTED | UNDER | UNNAMED | USAGE | USER_DEFINED_TYPE_CATALOG
  | USER_DEFINED_TYPE_CODE | USER_DEFINED_TYPE_NAME | USER_DEFINED_TYPE_SCHEMA

  | VIEW

  | WORK | WRITE

  | ZONE

<reserved word> ::=
    ABS | ALL | ALLOCATE | ALTER | AND | ANY | ARE | ARRAY | ARRAY_AGG
  | ARRAY_MAX_CARDINALITY | AS | ASENSITIVE | ASYMMETRIC | AT | ATOMIC | AUTHORIZATION
  | AVG

  | BEGIN | BEGIN_FRAME | BEGIN_PARTITION | BETWEEN | BIGINT | BINARY
  | BLOB | BOOLEAN | BOTH | BY

  | CALL | CALLED | CARDINALITY | CASCADED | CASE | CAST | CEIL | CEILING
  | CHAR | CHAR_LENGTH | CHARACTER | CHARACTER_LENGTH | CHECK | CLOB | CLOSE
  | COALESCE | COLLATE | COLLECT | COLUMN | COMMIT | CONDITION | CONNECT
  | CONSTRAINT | CONTAINS | CONVERT | CORR | CORRESPONDING | COUNT | COVAR_POP
  | COVAR_SAMP | CREATE | CROSS | CUBE | CUME_DIST | CURRENT | CURRENT_CATALOG
  | CURRENT_DATE | CURRENT_DEFAULT_TRANSFORM_GROUP | CURRENT_PATH | CURRENT_ROLE
  | CURRENT_ROW | CURRENT_SCHEMA | CURRENT_TIME | CURRENT_TIMESTAMP
  | CURRENT_TRANSFORM_GROUP_FOR_TYPE | CURRENT_USER | CURSOR | CYCLE

  | DATE | DAY | DEALLOCATE | DEC | DECIMAL | DECLARE | DEFAULT | DELETE
  | DENSE_RANK | DEREF | DESCRIBE | DETERMINISTIC | DISCONNECT | DISTINCT
  | DOUBLE | DROP | DYNAMIC

  | EACH | ELEMENT | ELSE | END | END_FRAME | END_PARTITION | END-EXEC
  | EQUALS | ESCAPE | EVERY | EXCEPT | EXEC | EXECUTE | EXISTS | EXP
  | EXTERNAL | EXTRACT

  | FALSE | FETCH | FILTER | FIRST_VALUE | FLOAT | FLOOR | FOR | FOREIGN
  | FRAME_ROW | FREE | FROM | FULL | FUNCTION | FUSION

  | GET | GLOBAL | GRANT | GROUP | GROUPING | GROUPS

  | HAVING | HOLD | HOUR

  | IDENTITY | IN | INDICATOR | INNER | INOUT | INSENSITIVE | INSERT
  | INT | INTEGER | INTERSECT | INTERSECTION | INTERVAL | INTO | IS

  | JOIN

  | LAG | LANGUAGE | LARGE | LAST_VALUE | LATERAL | LEAD | LEADING | LEFT
  | LIKE | LIKE_REGEX | LN | LOCAL | LOCALTIME | LOCALTIMESTAMP | LOWER

  | MATCH | MAX | MEMBER | MERGE | METHOD | MIN | MINUTE
  | MOD | MODIFIES | MODULE | MONTH | MULTISET

  | NATIONAL | NATURAL | NCHAR | NCLOB | NEW | NO | NONE | NORMALIZE | NOT
  | NTH_VALUE | NTILE | NULL | NULLIF | NUMERIC

  | OCTET_LENGTH | OCCURRENCES_REGEX | OF | OFFSET | OLD | ON | ONLY | OPEN
  | OR | ORDER | OUT | OUTER | OVER | OVERLAPS | OVERLAY

  | PARAMETER | PARTITION | PERCENT | PERCENT_RANK | PERCENTILE_CONT
  | PERCENTILE_DISC | PERIOD | PORTION | POSITION | POSITION_REGEX | POWER | PRECEDES
  | PRECISION | PREPARE | PRIMARY | PROCEDURE

  | RANGE | RANK | READS | REAL | RECURSIVE | REF | REFERENCES | REFERENCING
  | REGR_AVGX | REGR_AVGY | REGR_COUNT | REGR_INTERCEPT | REGR_R2 | REGR_SLOPE
  | REGR_SXX | REGR_SXY | REGR_SYY | RELEASE | RESULT | RETURN | RETURNS
  | REVOKE | RIGHT | ROLLBACK | ROLLUP | ROW | ROW_NUMBER | ROWS

  | SAVEPOINT | SCOPE | SCROLL | SEARCH | SECOND | SELECT
  | SENSITIVE | SESSION_USER | SET | SIMILAR | SMALLINT | SOME | SPECIFIC
  | SPECIFICTYPE | SQL | SQLEXCEPTION | SQLSTATE | SQLWARNING | SQRT | START
  | STATIC | STDDEV_POP | STDDEV_SAMP | SUBMULTISET | SUBSTRING | SUBSTRING_REGEX
  | SUCCEEDS | SUM | SYMMETRIC | SYSTEM | SYSTEM_TIME | SYSTEM_USER

  | TABLE | TABLESAMPLE | THEN | TIME | TIMESTAMP | TIMEZONE_HOUR | TIMEZONE_MINUTE
  | TO | TRAILING | TRANSLATE | TRANSLATE_REGEX | TRANSLATION | TREAT
  | TRIGGER | TRUNCATE | TRIM | TRIM_ARRAY | TRUE

  | UESCAPE | UNION | UNIQUE | UNKNOWN | UNNEST | UPDATE | UPPER | USER | USING

  | VALUE | VALUES | VALUE_OF | VAR_POP | VAR_SAMP | VARBINARY
  | VARCHAR | VARYING | VERSIONING

  | WHEN | WHENEVER | WHERE | WIDTH_BUCKET | WINDOW | WITH | WITHIN | WITHOUT

  | YEAR

== 5.3 <literal>

Function
Specify a non-null value.

> literals :: TestItem
> literals = Group "literals"
>     [numericLiterals,generalLiterals]

<literal> ::= <signed numeric literal> | <general literal>

<unsigned literal> ::= <unsigned numeric literal> | <general literal>

<general literal> ::=
    <character string literal>
  | <national character string literal>
  | <Unicode character string literal>
  | <binary string literal>
  | <datetime literal>
  | <interval literal>
  | <boolean literal>

> generalLiterals :: TestItem
> generalLiterals = Group "general literals"
>     [characterStringLiterals
>     ,nationalCharacterStringLiterals
>     ,unicodeCharacterStringLiterals
>     ,binaryStringLiterals
>     ,dateTimeLiterals
>     ,intervalLiterals
>     ,booleanLiterals]

<character string literal> ::=
  [ <introducer> <character set specification> ]
      <quote> [ <character representation>... ] <quote>
      [ { <separator> <quote> [ <character representation>... ] <quote> }... ]

<introducer> ::= <underscore>

<character representation> ::= <nonquote character> | <quote symbol>

<nonquote character> ::= !! See the Syntax Rules.

<quote symbol> ::= <quote> <quote>

> characterStringLiterals :: TestItem
> characterStringLiterals = Group "character string literals"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("'a regular string literal'"
>      ,StringLit "a regular string literal")
>     ,("'something' ' some more' 'and more'"
>      ,StringLit "something some moreand more")
>     ,("'something' \n ' some more' \t 'and more'"
>      ,StringLit "something some moreand more")
>     ,("'something' -- a comment\n ' some more' /*another comment*/ 'and more'"
>      ,StringLit "something some moreand more")
>     ,("'a quote: '', stuff'"
>      ,StringLit "a quote: ', stuff")
>     ,("''"
>      ,StringLit "")

I'm not sure how this should work. Maybe the parser should reject non
ascii characters in strings and identifiers unless the current SQL
character set allows them.

>     ,("_francais 'français'"
>      ,TypedLit (TypeName [Name "_francais"]) "français")
>     ]

<national character string literal> ::=
  N <quote> [ <character representation>... ]
      <quote> [ { <separator> <quote> [ <character representation>... ] <quote> }... ]

> nationalCharacterStringLiterals :: TestItem
> nationalCharacterStringLiterals = Group "national character string literals"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("N'something'", CSStringLit "N" "something")
>     ,("n'something'", CSStringLit "n" "something")
>     ]

<Unicode character string literal> ::=
  [ <introducer> <character set specification> ]
      U <ampersand> <quote> [ <Unicode representation>... ] <quote>
      [ { <separator> <quote> [ <Unicode representation>... ] <quote> }... ]
      <Unicode escape specifier>

<Unicode representation> ::=
    <character representation>
  | <Unicode escape value>

> unicodeCharacterStringLiterals :: TestItem
> unicodeCharacterStringLiterals = Group "unicode character string literals"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("U&'something'", CSStringLit "U&" "something")
>     ,("u&'something' escape ="
>      ,Escape (CSStringLit "u&" "something") '=')
>     ,("u&'something' uescape ="
>      ,UEscape (CSStringLit "u&" "something") '=')
>     ]

TODO: unicode escape

<binary string literal> ::=
  X <quote> [ <space>... ] [ { <hexit> [ <space>... ] <hexit> [ <space>... ] }... ] <quote>
      [ { <separator> <quote> [ <space>... ] [ { <hexit> [ <space>... ]
      <hexit> [ <space>... ] }... ] <quote> }... ]

<hexit> ::= <digit> | A | B | C | D | E | F | a | b | c | d | e | f

> binaryStringLiterals :: TestItem
> binaryStringLiterals = Group "binary string literals"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [--("B'101010'", CSStringLit "B" "101010")
>      ("X'7f7f7f'", CSStringLit "X" "7f7f7f")
>     ,("X'7f7f7f' escape z", Escape (CSStringLit "X" "7f7f7f") 'z')
>     ]

<signed numeric literal> ::= [ <sign> ] <unsigned numeric literal>

<unsigned numeric literal> ::=
    <exact numeric literal>
  | <approximate numeric literal>

<exact numeric literal> ::=
    <unsigned integer> [ <period> [ <unsigned integer> ] ]
  | <period> <unsigned integer>

<sign> ::= <plus sign> | <minus sign>

<approximate numeric literal> ::= <mantissa> E <exponent>

<mantissa> ::= <exact numeric literal>

<exponent> ::= <signed integer>

<signed integer> ::= [ <sign> ] <unsigned integer>

<unsigned integer> ::= <digit>...

> numericLiterals :: TestItem
> numericLiterals = Group "numeric literals"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("11", NumLit "11")
>     ,("11.11", NumLit "11.11")

>     ,("11E23", NumLit "11E23")
>     ,("11E+23", NumLit "11E+23")
>     ,("11E-23", NumLit "11E-23")

>     ,("11.11E23", NumLit "11.11E23")
>     ,("11.11E+23", NumLit "11.11E+23")
>     ,("11.11E-23", NumLit "11.11E-23")

>     ,("+11E23", PrefixOp [Name "+"] $ NumLit "11E23")
>     ,("+11E+23", PrefixOp [Name "+"] $ NumLit "11E+23")
>     ,("+11E-23", PrefixOp [Name "+"] $ NumLit "11E-23")
>     ,("+11.11E23", PrefixOp [Name "+"] $ NumLit "11.11E23")
>     ,("+11.11E+23", PrefixOp [Name "+"] $ NumLit "11.11E+23")
>     ,("+11.11E-23", PrefixOp [Name "+"] $ NumLit "11.11E-23")

>     ,("-11E23", PrefixOp [Name "-"] $ NumLit "11E23")
>     ,("-11E+23", PrefixOp [Name "-"] $ NumLit "11E+23")
>     ,("-11E-23", PrefixOp [Name "-"] $ NumLit "11E-23")
>     ,("-11.11E23", PrefixOp [Name "-"] $ NumLit "11.11E23")
>     ,("-11.11E+23", PrefixOp [Name "-"] $ NumLit "11.11E+23")
>     ,("-11.11E-23", PrefixOp [Name "-"] $ NumLit "11.11E-23")

>     ,("11.11e23", NumLit "11.11e23")

>     ]

<datetime literal> ::= <date literal> | <time literal> | <timestamp literal>

<date literal> ::= DATE <date string>

<time literal> ::= TIME <time string>

<timestamp literal> ::= TIMESTAMP <timestamp string>

<date string> ::= <quote> <unquoted date string> <quote>

<time string> ::= <quote> <unquoted time string> <quote>

<timestamp string> ::= <quote> <unquoted timestamp string> <quote>

<time zone interval> ::= <sign> <hours value> <colon> <minutes value>

<date value> ::=
  <years value> <minus sign> <months value> <minus sign> <days value>

<time value> ::= <hours value> <colon> <minutes value> <colon> <seconds value>

> dateTimeLiterals :: TestItem
> dateTimeLiterals = Group "datetime literals"
>     [-- TODO: datetime literals
>     ]

<interval literal> ::=
  INTERVAL [ <sign> ] <interval string> <interval qualifier>

<interval string> ::= <quote> <unquoted interval string> <quote>

<unquoted date string> ::= <date value>

<unquoted time string> ::= <time value> [ <time zone interval> ]

<unquoted timestamp string> ::=
  <unquoted date string> <space> <unquoted time string>

<unquoted interval string> ::=
  [ <sign> ] { <year-month literal> | <day-time literal> }

<year-month literal> ::=
    <years value> [ <minus sign> <months value> ]
  | <months value>

<day-time literal> ::= <day-time interval> | <time interval>

<day-time interval> ::=
  <days value> [ <space> <hours value> [ <colon> <minutes value>
      [ <colon> <seconds value> ] ] ]

<time interval> ::=
    <hours value> [ <colon> <minutes value> [ <colon> <seconds value> ] ]
  | <minutes value> [ <colon> <seconds value> ]
  | <seconds value>

<years value> ::= <datetime value>

<months value> ::= <datetime value>

<days value> ::= <datetime value>

<hours value> ::= <datetime value>

<minutes value> ::= <datetime value>

<seconds value> ::= <seconds integer value> [ <period> [ <seconds fraction> ] ]

<seconds integer value> ::= <unsigned integer>

<seconds fraction> ::= <unsigned integer>

<datetime value> ::= <unsigned integer>

> intervalLiterals :: TestItem
> intervalLiterals = Group "intervalLiterals literals"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("interval '1'", TypedLit (TypeName [Name "interval"]) "1")
>     ,("interval '1' day"
>      ,IntervalLit Nothing "1" (Itf "day" Nothing) Nothing)
>     ,("interval '1' day(3)"
>      ,IntervalLit Nothing "1" (Itf "day" $ Just (3,Nothing)) Nothing)
>     ,("interval + '1' day(3)"
>      ,IntervalLit (Just True) "1" (Itf "day" $ Just (3,Nothing)) Nothing)
>     ,("interval - '1' second(2,2)"
>      ,IntervalLit (Just False) "1" (Itf "second" $ Just (2,Just 2)) Nothing)
>     ,("interval '1' year to month"
>      ,IntervalLit Nothing "1" (Itf "year" Nothing)
>                                   (Just $ Itf "month" Nothing))

>     ,("interval '1' year(4) to second(2,3) "
>      ,IntervalLit Nothing "1" (Itf "year" $ Just (4,Nothing))
>                             (Just $ Itf "second" $ Just (2, Just 3)))
>     ]

<boolean literal> ::= TRUE | FALSE | UNKNOWN

> booleanLiterals :: TestItem
> booleanLiterals = Group "boolean literals"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("true", Iden [Name "true"])
>     ,("false", Iden [Name "false"])
>     ,("unknown", Iden [Name "unknown"])
>     ]

== 5.4 Names and identifiers

Function
Specify names.

<identifier> ::= <actual identifier>

<actual identifier> ::=
    <regular identifier>
  | <delimited identifier>
  | <Unicode delimited identifier>

> identifiers :: TestItem
> identifiers = Group "identifiers"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("test",Iden [Name "test"])
>     ,("_test",Iden [Name "_test"])
>     ,("t1",Iden [Name "t1"])
>     ,("a.b",Iden [Name "a", Name "b"])
>     ,("a.b.c",Iden [Name "a", Name "b", Name "c"])
>     ,("\"quoted iden\"", Iden [QName "quoted iden"])
>     ,("\"quoted \"\" iden\"", Iden [QName "quoted \" iden"])
>     ,("U&\"quoted iden\"", Iden [UQName "quoted iden"])
>     ,("U&\"quoted \"\" iden\"", Iden [UQName "quoted \" iden"])
>     ]

TODO: more identifiers, e.g. unicode escapes?, mixed quoted/unquoted
chains

TODO: review below stuff for exact rules

<SQL language identifier> ::=
  <SQL language identifier start> [ <SQL language identifier part>... ]

<SQL language identifier start> ::= <simple Latin letter>

<SQL language identifier part> ::=
    <simple Latin letter>
  | <digit>
  | <underscore>

<authorization identifier> ::= <role name> | <user identifier>

<table name> ::= <local or schema qualified name>

<domain name> ::= <schema qualified name>

<schema name> ::= [ <catalog name> <period> ] <unqualified schema name>

<unqualified schema name> ::= <identifier>

<catalog name> ::= <identifier>

<schema qualified name> ::= [ <schema name> <period> ] <qualified identifier>

<local or schema qualified name> ::=
  [ <local or schema qualifier> <period> ] <qualified identifier>

<local or schema qualifier> ::= <schema name> | <local qualifier>

<qualified identifier> ::= <identifier>

<column name> ::= <identifier>

<correlation name> ::= <identifier>

<query name> ::= <identifier>

<SQL-client module name> ::= <identifier>

<procedure name> ::= <identifier>

<schema qualified routine name> ::= <schema qualified name>

<method name> ::= <identifier>

<specific name> ::= <schema qualified name>

<cursor name> ::= <local qualified name>

<local qualified name> ::=
  [ <local qualifier> <period> ] <qualified identifier>

<local qualifier> ::= MODULE

<host parameter name> ::= <colon> <identifier>

<SQL parameter name> ::= <identifier>

<constraint name> ::= <schema qualified name>

<external routine name> ::= <identifier> | <character string literal>

<trigger name> ::= <schema qualified name>

<collation name> ::= <schema qualified name>

<character set name> ::= [ <schema name> <period> ] <SQL language identifier>

<transliteration name> ::= <schema qualified name>

<transcoding name> ::= <schema qualified name>

<schema-resolved user-defined type name> ::= <user-defined type name>

<user-defined type name> ::= [ <schema name> <period> ] <qualified identifier>

<attribute name> ::= <identifier>

<field name> ::= <identifier>

<savepoint name> ::= <identifier>

<sequence generator name> ::= <schema qualified name>

<role name> ::= <identifier>

<user identifier> ::= <identifier>

<connection name> ::= <simple value specification>

<SQL-server name> ::= <simple value specification>

<connection user name> ::= <simple value specification>

<SQL statement name> ::= <statement name> | <extended statement name>

<statement name> ::= <identifier>

<extended statement name> ::= [ <scope option> ] <simple value specification>

<dynamic cursor name> ::= <cursor name> | <extended cursor name>

<extended cursor name> ::= [ <scope option> ] <simple value specification>

<descriptor name> ::=
    <non-extended descriptor name>
  | <extended descriptor name>

<non-extended descriptor name> ::= <identifier>

<extended descriptor name> ::= [ <scope option> ] <simple value specification>

<scope option> ::= GLOBAL | LOCAL

<window name> ::= <identifier>

= 6 Scalar expressions

== 6.1 <data type>

Function
Specify a data type.

<data type> ::=
    <predefined type>
  | <row type>
  | <path-resolved user-defined type name>
  | <reference type>
  | <collection type>

<predefined type> ::=
    <character string type> [ CHARACTER SET <character set specification> ]
          [ <collate clause> ]
  | <national character string type> [ <collate clause> ]
  | <binary string type>
  | <numeric type>
  | <boolean type>
  | <datetime type>
  | <interval type>

<character string type> ::=
    CHARACTER [ <left paren> <character length> <right paren> ]
  | CHAR [ <left paren> <character length> <right paren> ]
  | CHARACTER VARYING <left paren> <character length> <right paren>
  | CHAR VARYING <left paren> <character length> <right paren>
  | VARCHAR <left paren> <character length> <right paren>
  | <character large object type>

<character large object type> ::=
    CHARACTER LARGE OBJECT [ <left paren> <character large object length> <right paren> ]
  | CHAR LARGE OBJECT [ <left paren> <character large object length> <right paren> ]
  | CLOB [ <left paren> <character large object length> <right paren> ]

<national character string type> ::=
    NATIONAL CHARACTER [ <left paren> <character length> <right paren> ]
  | NATIONAL CHAR [ <left paren> <character length> <right paren> ]
  | NCHAR [ <left paren> <character length> <right paren> ]
  | NATIONAL CHARACTER VARYING <left paren> <character length> <right paren>
  | NATIONAL CHAR VARYING <left paren> <character length> <right paren>
  | NCHAR VARYING <left paren> <character length> <right paren>
  | <national character large object type>

<national character large object type> ::=
    NATIONAL CHARACTER LARGE OBJECT [ <left paren> <character large object length> <right
    paren> ]
  | NCHAR LARGE OBJECT [ <left paren> <character large object length> <right paren> ]
  | NCLOB [ <left paren> <character large object length> <right paren> ]

<binary string type> ::=
    BINARY [ <left paren> <length> <right paren> ]
  | BINARY VARYING <left paren> <length> <right paren>
  | VARBINARY <left paren> <length> <right paren>
  | <binary large object string type>

<binary large object string type> ::=
    BINARY LARGE OBJECT [ <left paren> <large object length> <right paren> ]
  | BLOB [ <left paren> <large object length> <right paren> ]

<numeric type> ::= <exact numeric type> | <approximate numeric type>

<exact numeric type> ::=
    NUMERIC [ <left paren> <precision> [ <comma> <scale> ] <right paren> ]
  | DECIMAL [ <left paren> <precision> [ <comma> <scale> ] <right paren> ]
  | DEC [ <left paren> <precision> [ <comma> <scale> ] <right paren> ]
  | SMALLINT
  | INTEGER
  | INT
  | BIGINT

<approximate numeric type> ::=
    FLOAT [ <left paren> <precision> <right paren> ]
  | REAL
  | DOUBLE PRECISION

<length> ::= <unsigned integer>

<character length> ::= <length> [ <char length units> ]

<large object length> ::=
    <length> [ <multiplier> ]
  | <large object length token>

<character large object length> ::=
  <large object length> [ <char length units> ]

<char length units> ::= CHARACTERS | OCTETS

<precision> ::= <unsigned integer>

<scale> ::= <unsigned integer>

<boolean type> ::= BOOLEAN

<datetime type> ::=
    DATE
  | TIME [ <left paren> <time precision> <right paren> ] [ <with or without time zone> ]
  | TIMESTAMP [ <left paren> <timestamp precision> <right paren> ]
      [ <with or without time zone> ]

<with or without time zone> ::= WITH TIME ZONE | WITHOUT TIME ZONE

<time precision> ::= <time fractional seconds precision>

<timestamp precision> ::= <time fractional seconds precision>

<time fractional seconds precision> ::= <unsigned integer>

<interval type> ::= INTERVAL <interval qualifier>

<row type> ::= ROW <row type body>

<row type body> ::=
  <left paren> <field definition> [ { <comma> <field definition> }... ] <right paren>

<reference type> ::=
  REF <left paren> <referenced type> <right paren> [ <scope clause> ]

<scope clause> ::= SCOPE <table name>

<referenced type> ::= <path-resolved user-defined type name>

<path-resolved user-defined type name> ::= <user-defined type name>

<collection type> ::= <array type> | <multiset type>

<array type> ::=
  <data type> ARRAY
      [ <left bracket or trigraph> <maximum cardinality> <right bracket or trigraph> ]

<maximum cardinality> ::= <unsigned integer>

<multiset type> ::= <data type> MULTISET

TODO: below, add new stuff:
review the length syntaxes
binary, binary varying/varbinary
new multipliers

create a list of type name variations:

> typeNames :: [(String,TypeName)]
> typeNames =
>     basicTypes
>     ++ concatMap makeArray basicTypes
>     ++ map makeMultiset basicTypes
>   where
>     makeArray (s,t) = [(s ++ " array", ArrayTypeName t Nothing)
>                       ,(s ++ " array[5]", ArrayTypeName t (Just 5))]
>     makeMultiset (s,t) = (s ++ " multiset", MultisetTypeName t)
>     basicTypes =
>         -- example of every standard type name
>         map (\t -> (t,TypeName [Name t]))
>         ["binary"
>         ,"binary varying"
>         ,"character"
>         ,"char"
>         ,"character varying"
>         ,"char varying"
>         ,"varbinary"
>         ,"varchar"
>         ,"character large object"
>         ,"char large object"
>         ,"clob"
>         ,"national character"
>         ,"national char"
>         ,"nchar"
>         ,"national character varying"
>         ,"national char varying"
>         ,"nchar varying"
>         ,"national character large object"
>         ,"nchar large object"
>         ,"nclob"
>         ,"binary large object"
>         ,"blob"
>         ,"numeric"
>         ,"decimal"
>         ,"dec"
>         ,"smallint"
>         ,"integer"
>         ,"int"
>         ,"bigint"
>         ,"float"
>         ,"real"
>         ,"double precision"
>         ,"boolean"
>         ,"date"
>         ,"time"
>         ,"timestamp"]
>         --interval -- not allowed without interval qualifier
>         --row -- not allowed without row type body
>         -- array -- not allowed on own
>         -- multiset -- not allowed on own

>          ++
>          [-- 1 single prec + 1 with multiname
>           ("char(5)", PrecTypeName [Name "char"] 5)
>          ,("char varying(5)", PrecTypeName [Name "char varying"] 5)
>          -- 1 scale
>          ,("decimal(15,2)", PrecScaleTypeName [Name "decimal"] 15 2)
>          ,("char(3 octets)"
>           ,PrecLengthTypeName [Name "char"] 3 Nothing (Just PrecOctets))
>          ,("varchar(50 characters)"
>           ,PrecLengthTypeName [Name "varchar"] 50 Nothing (Just PrecCharacters))
>          -- lob prec + with multiname
>          ,("blob(3M)", PrecLengthTypeName [Name "blob"] 3 (Just PrecM) Nothing)
>          ,("blob(3T)", PrecLengthTypeName [Name "blob"] 3 (Just PrecT) Nothing)
>          ,("blob(3P)", PrecLengthTypeName [Name "blob"] 3 (Just PrecP) Nothing)
>          ,("blob(4M characters) "
>           ,PrecLengthTypeName [Name "blob"] 4 (Just PrecM) (Just PrecCharacters))
>          ,("blob(6G octets) "
>           ,PrecLengthTypeName [Name "blob"] 6 (Just PrecG) (Just PrecOctets))
>          ,("national character large object(7K) "
>           ,PrecLengthTypeName [Name "national character large object"]
>                7 (Just PrecK) Nothing)
>          -- 1 with and without tz
>          ,("time with time zone"
>           ,TimeTypeName [Name "time"] Nothing True)
>          ,("datetime(3) without time zone"
>           ,TimeTypeName [Name "datetime"] (Just 3) False)
>          -- chars: (single/multiname) x prec x charset x collate
>          -- 1111
>          ,("char varying(5) character set something collate something_insensitive"
>           ,CharTypeName [Name "char varying"] (Just 5)
>            [Name "something"] [Name "something_insensitive"])
>           -- 0111
>          ,("char(5) character set something collate something_insensitive"
>           ,CharTypeName [Name "char"] (Just 5)
>            [Name "something"] [Name "something_insensitive"])

>           -- 1011
>          ,("char varying character set something collate something_insensitive"
>           ,CharTypeName [Name "char varying"] Nothing
>            [Name "something"] [Name "something_insensitive"])
>           -- 0011
>          ,("char character set something collate something_insensitive"
>           ,CharTypeName [Name "char"] Nothing
>            [Name "something"] [Name "something_insensitive"])

>           -- 1101
>          ,("char varying(5) collate something_insensitive"
>           ,CharTypeName [Name "char varying"] (Just 5)
>            [] [Name "something_insensitive"])
>           -- 0101
>          ,("char(5) collate something_insensitive"
>           ,CharTypeName [Name "char"] (Just 5)
>            [] [Name "something_insensitive"])
>           -- 1001
>          ,("char varying collate something_insensitive"
>           ,CharTypeName [Name "char varying"] Nothing
>            [] [Name "something_insensitive"])
>           -- 0001
>          ,("char collate something_insensitive"
>           ,CharTypeName [Name "char"] Nothing
>            [] [Name "something_insensitive"])

>          -- 1110
>          ,("char varying(5) character set something"
>           ,CharTypeName [Name "char varying"] (Just 5)
>            [Name "something"] [])
>           -- 0110
>          ,("char(5) character set something"
>           ,CharTypeName [Name "char"] (Just 5)
>            [Name "something"] [])
>           -- 1010
>          ,("char varying character set something"
>           ,CharTypeName [Name "char varying"] Nothing
>            [Name "something"] [])
>           -- 0010
>          ,("char character set something"
>           ,CharTypeName [Name "char"] Nothing
>            [Name "something"] [])
>           -- 1100
>          ,("char varying character set something"
>           ,CharTypeName [Name "char varying"] Nothing
>            [Name "something"] [])

>          -- single row field, two row field
>          ,("row(a int)", RowTypeName [(Name "a", TypeName [Name "int"])])
>          ,("row(a int,b char)"
>           ,RowTypeName [(Name "a", TypeName [Name "int"])
>                        ,(Name "b", TypeName [Name "char"])])
>          -- interval each type raw
>          ,("interval year"
>           ,IntervalTypeName (Itf "year" Nothing) Nothing)
>          -- one type with single suffix
>          -- one type with double suffix
>          ,("interval year(2)"
>           ,IntervalTypeName (Itf "year" $ Just (2,Nothing)) Nothing)
>          ,("interval second(2,5)"
>           ,IntervalTypeName (Itf "second" $ Just (2,Just 5)) Nothing)
>          -- a to b with raw
>          -- a to b with single suffix
>          ,("interval year to month"
>           ,IntervalTypeName (Itf "year" Nothing)
>                             (Just $ Itf "month" Nothing))
>          ,("interval year(4) to second(2,3)"
>           ,IntervalTypeName (Itf "year" $ Just (4,Nothing))
>                             (Just $ Itf "second" $ Just (2, Just 3)))
>          ]

Now test each variation in both cast expression and typed literal
expression

> typeNameTests :: TestItem
> typeNameTests = Group "type names" $ map (uncurry (TestValueExpr SQL2011))
>     $ concatMap makeTests typeNames
>   where
>     makeTests (ctn, stn) =
>         [("cast('test' as " ++ ctn ++ ")", Cast (StringLit "test") stn)
>         ,(ctn ++ " 'test'", TypedLit stn "test")
>         ]


== 6.2 <field definition>

Function
Define a field of a row type.

<field definition> ::= <field name> <data type>

> fieldDefinition :: TestItem
> fieldDefinition = Group "field definition"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("cast('(1,2)' as row(a int,b char))"
>      ,Cast (StringLit "(1,2)")
>      $ RowTypeName [(Name "a", TypeName [Name "int"])
>                    ,(Name "b", TypeName [Name "char"])])]

== 6.3 <value expression primary>

Function
Specify a value that is syntactically self-delimited.

<value expression primary> ::=
    <parenthesized value expression>
  | <nonparenthesized value expression primary>

<parenthesized value expression> ::=
  <left paren> <value expression> <right paren>

<nonparenthesized value expression primary> ::=
       <unsigned value specification>
     | <column reference>
     | <set function specification>
     | <window function>
     | <nested window function>
     | <scalar subquery>
     | <case expression>
     | <cast specification>
     | <field reference>
     | <subtype treatment>
     | <method invocation>
     | <static method invocation>
     | <new specification>
     | <attribute or method reference>
     | <reference resolution>
     | <collection value constructor>
     | <array element reference>
     | <multiset element reference>
     | <next value expression>
     | <routine invocation>

<collection value constructor> ::=
    <array value constructor>
  | <multiset value constructor>

> valueExpressions :: TestItem
> valueExpressions = Group "value expressions"
>     [generalValueSpecification
>     ,parameterSpecification
>     ,contextuallyTypedValueSpecification
>     ,identifierChain
>     ,columnReference
>     ,setFunctionSpecification
>     ,windowFunction
>     ,nestedWindowFunction
>     ,caseExpression
>     ,castSpecification
>     ,nextValueExpression
>     ,fieldReference
>     ,arrayElementReference
>     ,multisetElementReference
>     ,numericValueExpression
>     ,numericValueFunction
>     ,stringValueExpression
>     ,stringValueFunction
>     ,datetimeValueExpression
>     ,datetimeValueFunction
>     ,intervalValueExpression
>     ,intervalValueFunction
>     ,booleanValueExpression
>     ,arrayValueExpression
>     ,arrayValueFunction
>     ,arrayValueConstructor
>     ,multisetValueExpression
>     ,multisetValueFunction
>     ,multisetValueConstructor
>     ,parenthesizedValueExpression
>     ]

> parenthesizedValueExpression :: TestItem
> parenthesizedValueExpression = Group "parenthesized value expression"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("(3)", Parens (NumLit "3"))
>     ,("((3))", Parens $ Parens (NumLit "3"))
>     ]

== 6.4 <value specification> and <target specification>

Function
Specify one or more values, host parameters, SQL parameters, dynamic parameters, or host variables.

<value specification> ::= <literal> | <general value specification>

<unsigned value specification> ::=
    <unsigned literal>
  | <general value specification>

 <general value specification> ::=
    <host parameter specification>
  | <SQL parameter reference>
  | <dynamic parameter specification>
  | <embedded variable specification>
  | <current collation specification>
  | CURRENT_CATALOG
  | CURRENT_DEFAULT_TRANSFORM_GROUP
  | CURRENT_PATH
  | CURRENT_ROLE
  | CURRENT_SCHEMA
  | CURRENT_TRANSFORM_GROUP_FOR_TYPE <path-resolved user-defined type name>
  | CURRENT_USER
  | SESSION_USER
  | SYSTEM_USER
  | USER
  | VALUE

> generalValueSpecification :: TestItem
> generalValueSpecification = Group "general value specification"
>     $ map (uncurry (TestValueExpr SQL2011)) $
>     map mkIden ["CURRENT_DEFAULT_TRANSFORM_GROUP"
>                ,"CURRENT_PATH"
>                ,"CURRENT_ROLE"
>                ,"CURRENT_USER"
>                ,"SESSION_USER"
>                ,"SYSTEM_USER"
>                ,"USER"
>                ,"VALUE"]
>   where
>     mkIden nm = (nm,Iden [Name nm])

TODO: add the missing bits

<simple value specification> ::=
    <literal>
  | <host parameter name>
  | <SQL parameter reference>
  | <embedded variable name>

<target specification> ::=
    <host parameter specification>
  | <SQL parameter reference>
  | <column reference>
  | <target array element specification>
  | <dynamic parameter specification>
  | <embedded variable specification>

<simple target specification> ::=
    <host parameter name>
  | <SQL parameter reference>
  | <column reference>
  | <embedded variable name>

<host parameter specification> ::=
  <host parameter name> [ <indicator parameter> ]

<dynamic parameter specification> ::= <question mark>

<embedded variable specification> ::=
  <embedded variable name> [ <indicator variable> ]

<indicator variable> ::= [ INDICATOR ] <embedded variable name>

<indicator parameter> ::= [ INDICATOR ] <host parameter name>

<target array element specification> ::=
  <target array reference>
      <left bracket or trigraph> <simple value specification> <right bracket or trigraph>

<target array reference> ::= <SQL parameter reference> | <column reference>

> parameterSpecification :: TestItem
> parameterSpecification = Group "parameter specification"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [(":hostparam", HostParameter "hostparam" Nothing)
>     ,(":hostparam indicator :another_host_param"
>      ,HostParameter "hostparam" $ Just "another_host_param")
>     ,("?", Parameter)
>     ,(":h[3]", Array (HostParameter "h" Nothing) [NumLit "3"])
>     ]

<current collation specification> ::=
  COLLATION FOR <left paren> <string value expression> <right paren>

TODO: review the modules stuff

== 6.5 <contextually typed value specification>

Function
Specify a value whose data type is to be inferred from its context.

<contextually typed value specification> ::=
    <implicitly typed value specification>
  | <default specification>

<implicitly typed value specification> ::=
    <null specification>
  | <empty specification>

<null specification> ::= NULL

<empty specification> ::=
    ARRAY <left bracket or trigraph> <right bracket or trigraph>
  | MULTISET <left bracket or trigraph> <right bracket or trigraph>

<default specification> ::= DEFAULT

> contextuallyTypedValueSpecification :: TestItem
> contextuallyTypedValueSpecification =
>     Group "contextually typed value specification"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("null", Iden [Name "null"])
>     ,("array[]", Array (Iden [Name "array"]) [])
>     ,("multiset[]", MultisetCtor [])
>     ,("default", Iden [Name "default"])
>     ]

== 6.6 <identifier chain>

Function
Disambiguate a <period>-separated chain of identifiers.

<identifier chain> ::= <identifier> [ { <period> <identifier> }... ]

<basic identifier chain> ::= <identifier chain>

> identifierChain :: TestItem
> identifierChain = Group "identifier chain"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("a.b", Iden [Name "a",Name "b"])]

== 6.7 <column reference>

Function
Reference a column.

<column reference> ::=
    <basic identifier chain>
  | MODULE <period> <qualified identifier> <period> <column name>

> columnReference :: TestItem
> columnReference = Group "column reference"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("module.a.b", Iden [Name "module",Name "a",Name "b"])]

== 6.8 <SQL parameter reference>

Function
Reference an SQL parameter.

<SQL parameter reference> ::= <basic identifier chain>

== 6.9 <set function specification>

Function
Specify a value derived by the application of a function to an argument.

<set function specification> ::= <aggregate function> | <grouping operation>

<grouping operation> ::=
  GROUPING <left paren> <column reference>
      [ { <comma> <column reference> }... ] <right paren>

> setFunctionSpecification :: TestItem
> setFunctionSpecification = Group "set function specification"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     [("SELECT SalesQuota, SUM(SalesYTD) TotalSalesYTD,\n\
>       \   GROUPING(SalesQuota) AS Grouping\n\
>       \FROM Sales.SalesPerson\n\
>       \GROUP BY ROLLUP(SalesQuota);"
>      ,makeSelect
>       {qeSelectList = [(Iden [Name "SalesQuota"],Nothing)
>                       ,(App [Name "SUM"] [Iden [Name "SalesYTD"]]
>                        ,Just (Name "TotalSalesYTD"))
>                       ,(App [Name "GROUPING"] [Iden [Name "SalesQuota"]]
>                        ,Just (Name "Grouping"))]
>       ,qeFrom = [TRSimple [Name "Sales",Name "SalesPerson"]]
>       ,qeGroupBy = [Rollup [SimpleGroup (Iden [Name "SalesQuota"])]]})
>     ]

== 6.10 <window function>

Function
Specify a window function.

<window function> ::=
  <window function type> OVER <window name or specification>

<window function type> ::=
    <rank function type> <left paren> <right paren>
  | ROW_NUMBER <left paren> <right paren>
  | <aggregate function>
  | <ntile function>
  | <lead or lag function>
  | <first or last value function>
  | <nth value function>

<rank function type> ::= RANK | DENSE_RANK | PERCENT_RANK | CUME_DIST

<ntile function> ::= NTILE <left paren> <number of tiles> <right paren>

<number of tiles> ::=
    <simple value specification>
  | <dynamic parameter specification>

<lead or lag function> ::=
  <lead or lag> <left paren> <lead or lag extent>
      [ <comma> <offset> [ <comma> <default expression> ] ] <right paren>
      [ <null treatment> ]

<lead or lag> ::= LEAD | LAG

<lead or lag extent> ::= <value expression>

<offset> ::= <exact numeric literal>

<default expression> ::= <value expression>

<null treatment> ::= RESPECT NULLS | IGNORE NULLS

<first or last value function> ::=
  <first or last value> <left paren> <value expression> <right paren> [ <null treatment>
  ]

<first or last value> ::= FIRST_VALUE | LAST_VALUE

<nth value function> ::=
  NTH_VALUE <left paren> <value expression> <comma> <nth row> <right paren>
      [ <from first or last> ] [ <null treatment> ]

<nth row> ::= <simple value specification> | <dynamic parameter specification>

<from first or last> ::= FROM FIRST | FROM LAST

<window name or specification> ::=
    <window name>
  | <in-line window specification>

<in-line window specification> ::= <window specification>

> windowFunction :: TestItem
> windowFunction = Group "window function"
>     [-- todo: window function
>     ]

== 6.11 <nested window function>

Function

Specify a function nested in an aggregated argument of an
<aggregate function> simply contained in a <window function>.

<nested window function> ::=
    <nested row number function>
  | <value_of expression at row>

<nested row number function> ::=
  ROW_NUMBER <left paren> <row marker> <right paren>

<value_of expression at row> ::=
  VALUE_OF <left paren> <value expression> AT <row marker expression>
      [ <comma> <value_of default value> ] <right paren>

<row marker> ::=
    BEGIN_PARTITION
  | BEGIN_FRAME
  | CURRENT_ROW
  | FRAME_ROW
  | END_FRAME
  | END_PARTITION

<row marker expression> ::= <row marker> [ <row marker delta> ]

<row marker delta> ::=
    <plus sign> <row marker offset>
  | <minus sign> <row marker offset>

<row marker offset> ::=
    <simple value specification>
  | <dynamic parameter specification>

<value_of default value> ::= <value expression>

> nestedWindowFunction :: TestItem
> nestedWindowFunction = Group "nested window function"
>     [-- todo: nested window function
>     ]


== 6.12 <case expression>

Function
Specify a conditional value.

<case expression> ::= <case abbreviation> | <case specification>

<case abbreviation> ::=
    NULLIF <left paren> <value expression> <comma> <value expression> <right paren>
  | COALESCE <left paren> <value expression>
      { <comma> <value expression> }... <right paren>

<case specification> ::= <simple case> | <searched case>

<simple case> ::=
  CASE <case operand> <simple when clause>... [ <else clause> ] END

<searched case> ::= CASE <searched when clause>... [ <else clause> ] END

<simple when clause> ::= WHEN <when operand list> THEN <result>

<searched when clause> ::= WHEN <search condition> THEN <result>

<else clause> ::= ELSE <result>

<case operand> ::= <row value predicand> | <overlaps predicate part 1>

<when operand list> ::= <when operand> [ { <comma> <when operand> }... ]

<when operand> ::=
    <row value predicand>
  | <comparison predicate part 2>
  | <between predicate part 2>
  | <in predicate part 2>
  | <character like predicate part 2>
  | <octet like predicate part 2>
  | <similar predicate part 2>
  | <regex like predicate part 2>
  | <null predicate part 2>
  | <quantified comparison predicate part 2>
  | <normalized predicate part 2>
  | <match predicate part 2>
  | <overlaps predicate part 2>
  | <distinct predicate part 2>
  | <member predicate part 2>
  | <submultiset predicate part 2>
  | <set predicate part 2>
  | <type predicate part 2>

I haven't seen these part 2 style when operands in the wild. It
doesn't even allow all the binary operators here. We will allow them
all, and parser and represent these expressions by considering all the
binary ops as unary prefix ops.

<result> ::= <result expression> | NULL

<result expression> ::= <value expression>

> caseExpression :: TestItem
> caseExpression = Group "case expression"
>     [-- todo: case expression
>     ]

== 6.13 <cast specification>

Function
Specify a data conversion.

<cast specification> ::=
     CAST <left paren> <cast operand> AS <cast target> <right paren>

<cast operand> ::= <value expression> | <implicitly typed value specification>

<cast target> ::= <domain name> | <data type>

> castSpecification :: TestItem
> castSpecification = Group "cast specification"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("cast(a as int)"
>      ,Cast (Iden [Name "a"]) (TypeName [Name "int"]))
>     ]

== 6.14 <next value expression>

Function
Return the next value of a sequence generator.

<next value expression> ::= NEXT VALUE FOR <sequence generator name>

> nextValueExpression :: TestItem
> nextValueExpression = Group "next value expression"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("next value for a.b", NextValueFor [Name "a", Name "b"])
>     ]

== 6.15 <field reference>

Function
Reference a field of a row value.

<field reference> ::= <value expression primary> <period> <field name>

> fieldReference :: TestItem
> fieldReference = Group "field reference"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("f(something).a"
>       ,BinOp (App [Name "f"] [Iden [Name "something"]])
>        [Name "."]
>        (Iden [Name "a"]))
>     ]

TODO: try all possible value expression syntax variations followed by
field reference

== 6.16 <subtype treatment>

Function
Modify the declared type of an expression.

<subtype treatment> ::=
  TREAT <left paren> <subtype operand> AS <target subtype> <right paren>

<subtype operand> ::= <value expression>

<target subtype> ::= <path-resolved user-defined type name> | <reference type>

todo: subtype treatment

== 6.17 <method invocation>

Function
Reference an SQL-invoked method of a user-defined type value.

<method invocation> ::= <direct invocation> | <generalized invocation>

<direct invocation> ::=
  <value expression primary> <period> <method name> [ <SQL argument list> ]

<generalized invocation> ::=
  <left paren> <value expression primary> AS <data type> <right paren>
      <period> <method name> [ <SQL argument list> ]

<method selection> ::= <routine invocation>

<constructor method selection> ::= <routine invocation>

todo: method invocation

== 6.18 <static method invocation>

Function
Invoke a static method.

<static method invocation> ::=
  <path-resolved user-defined type name> <double colon> <method name>
      [ <SQL argument list> ]

<static method selection> ::= <routine invocation>

todo: static method invocation

== 6.19 <new specification>

Function
Invoke a method on a newly-constructed value of a structured type.

<new specification> ::=
  NEW <path-resolved user-defined type name> <SQL argument list>

<new invocation> ::= <method invocation> | <routine invocation>

todo: new specification

== 6.20 <attribute or method reference>

Function
Return a value acquired by accessing a column of the row identified by
a value of a reference type or by invoking an SQL-invoked method.

<attribute or method reference> ::=
  <value expression primary> <dereference operator> <qualified identifier>
      [ <SQL argument list> ]

<dereference operator> ::= <right arrow>

todo: attribute of method reference

== 6.21 <dereference operation>

Function
Access a column of the row identified by a value of a reference type.

<dereference operation> ::=
  <reference value expression> <dereference operator> <attribute name>

todo: deference operation

== 6.22 <method reference>

Function
Return a value acquired from invoking an SQL-invoked routine that is a method.

<method reference> ::=
  <value expression primary> <dereference operator> <method name> <SQL argument list>

todo: method reference

== 6.23 <reference resolution>

Function
Obtain the value referenced by a reference value.

<reference resolution> ::=
  DEREF <left paren> <reference value expression> <right paren>

todo: reference resolution

== 6.24 <array element reference>

Function
Return an element of an array.

<array element reference> ::=
  <array value expression>
      <left bracket or trigraph> <numeric value expression> <right bracket or trigraph>

> arrayElementReference :: TestItem
> arrayElementReference = Group "array element reference"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("something[3]"
>      ,Array (Iden [Name "something"]) [NumLit "3"])
>     ,("(something(a))[x]"
>       ,Array (Parens (App [Name "something"] [Iden [Name "a"]]))
>         [Iden [Name "x"]])
>     ,("(something(a))[x][y] "
>       ,Array (
>         Array (Parens (App [Name "something"] [Iden [Name "a"]]))
>         [Iden [Name "x"]])
>         [Iden [Name "y"]])
>     ]

== 6.25 <multiset element reference>

Function
Return the sole element of a multiset of one element.

<multiset element reference> ::=
  ELEMENT <left paren> <multiset value expression> <right paren>

> multisetElementReference :: TestItem
> multisetElementReference = Group "multisetElementReference"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("element(something)"
>      ,App [Name "element"] [Iden [Name "something"]])
>     ]

== 6.26 <value expression>

Function
Specify a value.

<value expression> ::=
    <common value expression>
  | <boolean value expression>
  | <row value expression>

<common value expression> ::=
       <numeric value expression>
     | <string value expression>
     | <datetime value expression>
     | <interval value expression>
     | <user-defined type value expression>
     | <reference value expression>
     | <collection value expression>

<user-defined type value expression> ::= <value expression primary>

<reference value expression> ::= <value expression primary>

<collection value expression> ::=
    <array value expression>
  | <multiset value expression>

== 6.27 <numeric value expression>

Function
Specify a numeric value.

<numeric value expression> ::=
    <term>
  | <numeric value expression> <plus sign> <term>
  | <numeric value expression> <minus sign> <term>

<term> ::= <factor> | <term> <asterisk> <factor> | <term> <solidus> <factor>

<factor> ::= [ <sign> ] <numeric primary>

<numeric primary> ::= <value expression primary> | <numeric value function>

> numericValueExpression :: TestItem
> numericValueExpression = Group "numeric value expression"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("a + b", binOp "+")
>     ,("a - b", binOp "-")
>     ,("a * b", binOp "*")
>     ,("a / b", binOp "/")
>     ,("+a", prefOp "+")
>     ,("-a", prefOp "-")
>     ]
>   where
>     binOp o = BinOp (Iden [Name "a"]) [Name o] (Iden [Name "b"])
>     prefOp o = PrefixOp [Name o] (Iden [Name "a"])

TODO: precedence and associativity tests (need to review all operators
for what precendence and associativity tests to write)

== 6.28 <numeric value function>

Function
Specify a function yielding a value of type numeric.

<numeric value function> ::=
    <position expression>
  | <regex occurrences function>
  | <regex position expression>
  | <extract expression>
  | <length expression>
  | <cardinality expression>
  | <max cardinality expression>
  | <absolute value expression>
  | <modulus expression>
  | <natural logarithm>
  | <exponential function>
  | <power function>
  | <square root>
  | <floor function>
  | <ceiling function>
  | <width bucket function>


> numericValueFunction :: TestItem
> numericValueFunction = Group "numeric value function"
>     [-- todo: numeric value function
>     ]

<position expression> ::=
    <character position expression>
  | <binary position expression>

<regex occurrences function> ::=
  OCCURRENCES_REGEX <left paren>
      <XQuery pattern> [ FLAG <XQuery option flag> ]
      IN <regex subject string>
      [ FROM <start position> ]
      [ USING <char length units> ]
      <right paren>

<XQuery pattern> ::= <character value expression>

<XQuery option flag> ::= <character value expression>

<regex subject string> ::= <character value expression>

<regex position expression> ::=
  POSITION_REGEX <left paren>
      [ <regex position start or after> ]
      <XQuery pattern> [ FLAG <XQuery option flag> ]
      IN <regex subject string>
      [ FROM <start position> ]
      [ USING <char length units> ]
      [ OCCURRENCE <regex occurrence> ]
      [ GROUP <regex capture group> ]
      <right paren>

<regex position start or after> ::= START | AFTER

<regex occurrence> ::= <numeric value expression>

<regex capture group> ::= <numeric value expression>

<character position expression> ::=
  POSITION <left paren> <character value expression 1> IN <character value expression 2>
      [ USING <char length units> ] <right paren>

<character value expression 1> ::= <character value expression>

<character value expression 2> ::= <character value expression>

<binary position expression> ::=
  POSITION <left paren> <binary value expression> IN <binary value expression> <right paren>

<length expression> ::= <char length expression> | <octet length expression>

<char length expression> ::=
  { CHAR_LENGTH | CHARACTER_LENGTH } <left paren> <character value expression>
      [ USING <char length units> ] <right paren>

<octet length expression> ::=
  OCTET_LENGTH <left paren> <string value expression> <right paren>

<extract expression> ::=
  EXTRACT <left paren> <extract field> FROM <extract source> <right paren>

<extract field> ::= <primary datetime field> | <time zone field>

<time zone field> ::= TIMEZONE_HOUR | TIMEZONE_MINUTE

<extract source> ::= <datetime value expression> | <interval value expression>

<cardinality expression> ::=
  CARDINALITY <left paren> <collection value expression> <right paren>

<max cardinality expression> ::=
  ARRAY_MAX_CARDINALITY <left paren> <array value expression> <right paren>

<absolute value expression> ::=
  ABS <left paren> <numeric value expression> <right paren>

<modulus expression> ::=
  MOD <left paren> <numeric value expression dividend> <comma>
      <numeric value expression divisor> <right paren>

<numeric value expression dividend> ::= <numeric value expression>

<numeric value expression divisor> ::= <numeric value expression>

<natural logarithm> ::=
  LN <left paren> <numeric value expression> <right paren>

<exponential function> ::=
  EXP <left paren> <numeric value expression> <right paren>

<power function> ::=
  POWER <left paren> <numeric value expression base> <comma>
      <numeric value expression exponent> <right paren>

<numeric value expression base> ::= <numeric value expression>

<numeric value expression exponent> ::= <numeric value expression>

<square root> ::= SQRT <left paren> <numeric value expression> <right paren>

<floor function> ::=
  FLOOR <left paren> <numeric value expression> <right paren>

<ceiling function> ::=
  { CEIL | CEILING } <left paren> <numeric value expression> <right paren>

<width bucket function> ::=
  WIDTH_BUCKET <left paren> <width bucket operand> <comma> <width bucket bound 1> <comma>
      <width bucket bound 2> <comma> <width bucket count> <right paren>

<width bucket operand> ::= <numeric value expression>

<width bucket bound 1> ::= <numeric value expression>

<width bucket bound 2> ::= <numeric value expression>

<width bucket count> ::= <numeric value expression>

== 6.29 <string value expression>

Function
Specify a character string value or a binary string value.

<string value expression> ::=
    <character value expression>
  | <binary value expression>

<character value expression> ::= <concatenation> | <character factor>

<concatenation> ::=
  <character value expression> <concatenation operator> <character factor>

<character factor> ::= <character primary> [ <collate clause> ]

<character primary> ::= <value expression primary> | <string value function>

<binary value expression> ::= <binary concatenation> | <binary factor>

<binary factor> ::= <binary primary>

<binary primary> ::= <value expression primary> | <string value function>

<binary concatenation> ::=
  <binary value expression> <concatenation operator> <binary factor>

> stringValueExpression :: TestItem
> stringValueExpression = Group "string value expression"
>     [-- todo: string value expression
>     ]

== 6.30 <string value function>

Function
Specify a function yielding a value of type character string or binary string.

<string value function> ::=
    <character value function>
  | <binary value function>

<character value function> ::=
    <character substring function>
  | <regular expression substring function>
  | <regex substring function>
  | <fold>
  | <transcoding>
  | <character transliteration>
  | <regex transliteration>
  | <trim function>
  | <character overlay function>
  | <normalize function>
  | <specific type method>

> stringValueFunction :: TestItem
> stringValueFunction = Group "string value function"
>     [-- todo: string value function
>     ]

<character substring function> ::=
  SUBSTRING <left paren> <character value expression> FROM <start position>
      [ FOR <string length> ] [ USING <char length units> ] <right paren>

<regular expression substring function> ::=
  SUBSTRING <left paren> <character value expression> SIMILAR <character value expression>
      ESCAPE <escape character> <right paren>

<regex substring function> ::=
  SUBSTRING_REGEX <left paren>
      <XQuery pattern> [ FLAG <XQuery option flag> ]
      IN <regex subject string>
      [ FROM <start position> ]
      [ USING <char length units> ]
      [ OCCURRENCE <regex occurrence> ]
      [ GROUP <regex capture group> ]
      <right paren>

<fold> ::=
  { UPPER | LOWER } <left paren> <character value expression> <right paren>

<transcoding> ::=
  CONVERT <left paren> <character value expression>
      USING <transcoding name> <right paren>

<character transliteration> ::=
  TRANSLATE <left paren> <character value expression>
      USING <transliteration name> <right paren>

<regex transliteration> ::=
  TRANSLATE_REGEX <left paren>
      <XQuery pattern> [ FLAG <XQuery option flag> ]
      IN <regex subject string>
      [ WITH <XQuery replacement string> ]
      [ FROM <start position> ]
      [ USING <char length units> ]
      [ OCCURRENCE <regex transliteration occurrence> ]
      <right paren>

<XQuery replacement string> ::= <character value expression>

<regex transliteration occurrence> ::= <regex occurrence> | ALL

<trim function> ::= TRIM <left paren> <trim operands> <right paren>

<trim operands> ::=
  [ [ <trim specification> ] [ <trim character> ] FROM ] <trim source>

<trim source> ::= <character value expression>

<trim specification> ::= LEADING | TRAILING | BOTH

<trim character> ::= <character value expression>

<character overlay function> ::=
  OVERLAY <left paren> <character value expression> PLACING <character value expression>
      FROM <start position> [ FOR <string length> ]
      [ USING <char length units> ] <right paren>

<normalize function> ::=
  NORMALIZE <left paren> <character value expression>
      [ <comma> <normal form> [ <comma> <normalize function result length> ] ] <right paren>

<normal form> ::= NFC | NFD | NFKC | NFKD

<normalize function result length> ::=
    <character length>
  | <character large object length>

<specific type method> ::=
  <user-defined type value expression> <period> SPECIFICTYPE
      [ <left paren> <right paren> ]

<binary value function> ::=
       <binary substring function>
     | <binary trim function>
     | <binary overlay function>

<binary substring function> ::=
  SUBSTRING <left paren> <binary value expression> FROM <start position>
      [ FOR <string length> ] <right paren>

<binary trim function> ::=
  TRIM <left paren> <binary trim operands> <right paren>

<binary trim operands> ::=
  [ [ <trim specification> ] [ <trim octet> ] FROM ] <binary trim source>

<binary trim source> ::= <binary value expression>

<trim octet> ::= <binary value expression>

<binary overlay function> ::=
  OVERLAY <left paren> <binary value expression> PLACING <binary value expression>
      FROM <start position> [ FOR <string length> ] <right paren>

<start position> ::= <numeric value expression>

<string length> ::= <numeric value expression>

== 6.31 <datetime value expression>

Function
Specify a datetime value.

<datetime value expression> ::=
    <datetime term>
  | <interval value expression> <plus sign> <datetime term>
  | <datetime value expression> <plus sign> <interval term>
  | <datetime value expression> <minus sign> <interval term>

> datetimeValueExpression :: TestItem
> datetimeValueExpression = Group "datetime value expression"
>     [-- todo: datetime value expression
>      datetimeValueFunction 
>     ]

<datetime term> ::= <datetime factor>

<datetime factor> ::= <datetime primary> [ <time zone> ]

<datetime primary> ::= <value expression primary> | <datetime value function>

<time zone> ::= AT <time zone specifier>

<time zone specifier> ::= LOCAL | TIME ZONE <interval primary>

== 6.32 <datetime value function>

Function
Specify a function yielding a value of type datetime.

<datetime value function> ::=
    <current date value function>
  | <current time value function>
  | <current timestamp value function>
  | <current local time value function>
  | <current local timestamp value function>

> datetimeValueFunction :: TestItem
> datetimeValueFunction = Group "datetime value function"
>     [-- todo: datetime value function
>     ]

<current date value function> ::= CURRENT_DATE

<current time value function> ::=
  CURRENT_TIME [ <left paren> <time precision> <right paren> ]

<current local time value function> ::=
  LOCALTIME [ <left paren> <time precision> <right paren> ]

<current timestamp value function> ::=
  CURRENT_TIMESTAMP [ <left paren> <timestamp precision> <right paren> ]

<current local timestamp value function> ::=
  LOCALTIMESTAMP [ <left paren> <timestamp precision> <right paren> ]

== 6.33 <interval value expression>

Function
Specify an interval value.

<interval value expression> ::=
    <interval term>
  | <interval value expression 1> <plus sign> <interval term 1>
  | <interval value expression 1> <minus sign> <interval term 1>
  | <left paren> <datetime value expression> <minus sign> <datetime term> <right paren>
      <interval qualifier>

> intervalValueExpression :: TestItem
> intervalValueExpression = Group "interval value expression"
>     [-- todo: interval value expression
>     ]


<interval term> ::=
    <interval factor>
  | <interval term 2> <asterisk> <factor>
  | <interval term 2> <solidus> <factor>
  | <term> <asterisk> <interval factor>

<interval factor> ::= [ <sign> ] <interval primary>

<interval primary> ::=
    <value expression primary> [ <interval qualifier> ]
  | <interval value function>

<interval value expression 1> ::= <interval value expression>

<interval term 1> ::= <interval term>

<interval term 2> ::= <interval term>

== 6.34 <interval value function>

Function
Specify a function yielding a value of type interval.

<interval value function> ::= <interval absolute value function>

<interval absolute value function> ::=
  ABS <left paren> <interval value expression> <right paren>

> intervalValueFunction :: TestItem
> intervalValueFunction = Group "interval value function"
>     [-- todo: interval value function
>     ]


== 6.35 <boolean value expression>

Function
Specify a boolean value.

<boolean value expression> ::=
    <boolean term>
  | <boolean value expression> OR <boolean term>

<boolean term> ::= <boolean factor> | <boolean term> AND <boolean factor>

<boolean factor> ::= [ NOT ] <boolean test>

<boolean test> ::= <boolean primary> [ IS [ NOT ] <truth value> ]

<truth value> ::= TRUE | FALSE | UNKNOWN

<boolean primary> ::= <predicate> | <boolean predicand>

<boolean predicand> ::=
    <parenthesized boolean value expression>
  | <nonparenthesized value expression primary>

<parenthesized boolean value expression> ::=
  <left paren> <boolean value expression> <right paren>


> booleanValueExpression :: TestItem
> booleanValueExpression = Group "booleab value expression"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("a or b", BinOp a [Name "or"] b)
>     ,("a and b", BinOp a [Name "and"] b)
>     ,("not a", PrefixOp [Name "not"] a)
>     ,("a is true", postfixOp "is true")
>     ,("a is false", postfixOp "is false")
>     ,("a is unknown", postfixOp "is unknown")
>     ,("a is not true", postfixOp "is not true")
>     ,("a is not false", postfixOp "is not false")
>     ,("a is not unknown", postfixOp "is not unknown")
>     ,("(a or b)", Parens $ BinOp a [Name "or"] b)
>     ]
>   where
>     a = Iden [Name "a"]
>     b = Iden [Name "b"]
>     postfixOp nm = PostfixOp [Name nm] a

TODO: review if more tests are needed. Should at least have
precendence tests for mixed and, or and not without parens.

== 6.36 <array value expression>

Function
Specify an array value.

<array value expression> ::= <array concatenation> | <array primary>

<array concatenation> ::=
  <array value expression 1> <concatenation operator> <array primary>

<array value expression 1> ::= <array value expression>

<array primary> ::= <array value function> | <value expression primary>

> arrayValueExpression :: TestItem
> arrayValueExpression = Group "array value expression"
>     [-- todo: array value expression
>     ]

== 6.37 <array value function>

Function
Specify a function yielding a value of an array type.

<array value function> ::= <trim array function>

<trim array function> ::=
  TRIM_ARRAY <left paren> <array value expression> <comma> <numeric value expression>
    <right paren>

> arrayValueFunction :: TestItem
> arrayValueFunction = Group "array value function"
>     [-- todo: array value function
>     ]

== 6.38 <array value constructor>

Function
Specify construction of an array.

<array value constructor> ::=
    <array value constructor by enumeration>
  | <array value constructor by query>

<array value constructor by enumeration> ::=
  ARRAY <left bracket or trigraph> <array element list> <right bracket or trigraph>

<array element list> ::= <array element> [ { <comma> <array element> }... ]

<array element> ::= <value expression>

<array value constructor by query> ::= ARRAY <table subquery>

> arrayValueConstructor :: TestItem
> arrayValueConstructor = Group "array value constructor"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("array[1,2,3]"
>      ,Array (Iden [Name "array"])
>       [NumLit "1", NumLit "2", NumLit "3"])
>     ,("array[a,b,c]"
>      ,Array (Iden [Name "array"])
>       [Iden [Name "a"], Iden [Name "b"], Iden [Name "c"]])
>     ,("array(select * from t)"
>       ,ArrayCtor (makeSelect
>                   {qeSelectList = [(Star,Nothing)]
>                   ,qeFrom = [TRSimple [Name "t"]]}))
>     ,("array(select * from t order by a)"
>       ,ArrayCtor (makeSelect
>                   {qeSelectList = [(Star,Nothing)]
>                   ,qeFrom = [TRSimple [Name "t"]]
>                   ,qeOrderBy = [SortSpec (Iden [Name "a"])
>                                     DirDefault NullsOrderDefault]}))
>     ]


== 6.39 <multiset value expression>

Function
Specify a multiset value.

<multiset value expression> ::=
    <multiset term>
  | <multiset value expression> MULTISET UNION [ ALL | DISTINCT ] <multiset term>
  | <multiset value expression> MULTISET EXCEPT [ ALL | DISTINCT ] <multiset term>

<multiset term> ::=
    <multiset primary>
  | <multiset term> MULTISET INTERSECT [ ALL | DISTINCT ] <multiset primary>

<multiset primary> ::= <multiset value function> | <value expression primary>

> multisetValueExpression :: TestItem
> multisetValueExpression = Group "multiset value expression"
>    $ map (uncurry (TestValueExpr SQL2011))
>    [("a multiset union b"
>     ,MultisetBinOp (Iden [Name "a"]) Union SQDefault (Iden [Name "b"]))
>    ,("a multiset union all b"
>     ,MultisetBinOp (Iden [Name "a"]) Union All (Iden [Name "b"]))
>    ,("a multiset union distinct b"
>     ,MultisetBinOp (Iden [Name "a"]) Union Distinct (Iden [Name "b"]))
>    ,("a multiset except b"
>     ,MultisetBinOp (Iden [Name "a"]) Except SQDefault (Iden [Name "b"]))
>    ,("a multiset intersect b"
>     ,MultisetBinOp (Iden [Name "a"]) Intersect SQDefault (Iden [Name "b"]))
>    ]

TODO: check precedence and associativity

== 6.40 <multiset value function>

Function
Specify a function yielding a value of a multiset type.

<multiset value function> ::= <multiset set function>

<multiset set function> ::=
  SET <left paren> <multiset value expression> <right paren>

> multisetValueFunction :: TestItem
> multisetValueFunction = Group "multiset value function"
>    $ map (uncurry (TestValueExpr SQL2011))
>    [("set(a)", App [Name "set"] [Iden [Name "a"]])
>    ]

== 6.41 <multiset value constructor>

Function
Specify construction of a multiset.

<multiset value constructor> ::=
    <multiset value constructor by enumeration>
  | <multiset value constructor by query>
  | <table value constructor by query>

<multiset value constructor by enumeration> ::=
  MULTISET <left bracket or trigraph> <multiset element list> <right bracket or trigraph>

<multiset element list> ::=
  <multiset element> [ { <comma> <multiset element> }... ]

<multiset element> ::= <value expression>

<multiset value constructor by query> ::= MULTISET <table subquery>

<table value constructor by query> ::= TABLE <table subquery>

> multisetValueConstructor :: TestItem
> multisetValueConstructor = Group "multiset value constructor"
>    $ map (uncurry (TestValueExpr SQL2011))
>    [("multiset[a,b,c]", MultisetCtor[Iden [Name "a"]
>                                     ,Iden [Name "b"], Iden [Name "c"]])
>    ,("multiset(select * from t)", MultisetQueryCtor qe)
>    ,("table(select * from t)", MultisetQueryCtor qe)
>    ]
>   where
>     qe = makeSelect {qeSelectList = [(Star,Nothing)]
>                     ,qeFrom = [TRSimple [Name "t"]]}


= 7 Query expressions

> queryExpressions :: TestItem
> queryExpressions = Group "query expressions"
>     [rowValueConstructor
>     ,tableValueConstructor
>     ,fromClause
>     ,tableReference
>     ,joinedTable
>     ,whereClause
>     ,groupByClause
>     ,havingClause
>     ,windowClause
>     ,querySpecification
>     ,withQueryExpression
>     ,setOpQueryExpression
>     ,explicitTableQueryExpression
>     ,orderOffsetFetchQueryExpression
>     ,searchOrCycleClause
>     ]


== 7.1 <row value constructor>

Function
Specify a value or list of values to be constructed into a row.

<row value constructor> ::=
    <common value expression>
  | <boolean value expression>
  | <explicit row value constructor>

<explicit row value constructor> ::=
    <left paren> <row value constructor element> <comma>
        <row value constructor element list> <right paren>
  | ROW <left paren> <row value constructor element list> <right paren>
  | <row subquery>

<row value constructor element list> ::=
  <row value constructor element> [ { <comma> <row value constructor element> }... ]

<row value constructor element> ::= <value expression>

<contextually typed row value constructor> ::=
    <common value expression>
  | <boolean value expression>
  | <contextually typed value specification>
  | <left paren> <contextually typed value specification> <right paren>
  | <left paren> <contextually typed row value constructor element> <comma>
      <contextually typed row value constructor element list> <right paren>
  | ROW <left paren> <contextually typed row value constructor element list> <right paren>

<contextually typed row value constructor element list> ::=
  <contextually typed row value constructor element>
      [ { <comma> <contextually typed row value constructor element> }... ]

<contextually typed row value constructor element> ::=
    <value expression>
  | <contextually typed value specification>

<row value constructor predicand> ::=
    <common value expression>
  | <boolean predicand>
  | <explicit row value constructor>

> rowValueConstructor :: TestItem
> rowValueConstructor = Group "row value constructor"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("(a,b)"
>      ,SpecialOp [Name "rowctor"] [Iden [Name "a"], Iden [Name "b"]])
>     ,("row(1)",App [Name "row"] [NumLit "1"])
>     ,("row(1,2)",App [Name "row"] [NumLit "1",NumLit "2"])
>     ]

== 7.2 <row value expression>

Function
Specify a row value.

<row value expression> ::=
    <row value special case>
  | <explicit row value constructor>

<table row value expression> ::=
    <row value special case>
  | <row value constructor>

<contextually typed row value expression> ::=
    <row value special case>
  | <contextually typed row value constructor>

<row value predicand> ::=
    <row value special case>
  | <row value constructor predicand>

<row value special case> ::= <nonparenthesized value expression primary>

There is nothing new here.

== 7.3 <table value constructor>

Function
Specify a set of <row value expression>s to be constructed into a table.

<table value constructor> ::= VALUES <row value expression list>

<row value expression list> ::=
  <table row value expression> [ { <comma> <table row value expression> }... ]

<contextually typed table value constructor> ::=
  VALUES <contextually typed row value expression list>

<contextually typed row value expression list> ::=
  <contextually typed row value expression>
      [ { <comma> <contextually typed row value expression> }... ]

> tableValueConstructor :: TestItem
> tableValueConstructor = Group "table value constructor"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     [("values (1,2), (a+b,(select count(*) from t));"
>      ,Values [[NumLit "1", NumLit "2"]
>              ,[BinOp (Iden [Name "a"]) [Name "+"]
>                      (Iden [Name "b"])
>               ,SubQueryExpr SqSq
>                (makeSelect
>                 {qeSelectList = [(App [Name "count"] [Star],Nothing)]
>                 ,qeFrom = [TRSimple [Name "t"]]})]])
>     ]

== 7.4 <table expression>

Function
Specify a table or a grouped table.

<table expression> ::=
  <from clause>
      [ <where clause> ]
      [ <group by clause> ]
      [ <having clause> ]
      [ <window clause> ]

== 7.5 <from clause>

Function
Specify a table derived from one or more tables.

<from clause> ::= FROM <table reference list>

<table reference list> ::=
  <table reference> [ { <comma> <table reference> }... ]

> fromClause :: TestItem
> fromClause = Group "fromClause"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     [("select * from tbl1,tbl2"
>      ,makeSelect
>       {qeSelectList = [(Star, Nothing)]
>       ,qeFrom = [TRSimple [Name "tbl1"], TRSimple [Name "tbl2"]]
>       })]


== 7.6 <table reference>

Function
Reference a table.

> tableReference :: TestItem
> tableReference = Group "table reference"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     [("select * from t", sel)

<table reference> ::= <table factor> | <joined table>

<table factor> ::= <table primary> [ <sample clause> ]

<sample clause> ::=
  TABLESAMPLE <sample method> <left paren> <sample percentage> <right paren>
      [ <repeatable clause> ]

<sample method> ::= BERNOULLI | SYSTEM

<repeatable clause> ::= REPEATABLE <left paren> <repeat argument> <right paren>

<sample percentage> ::= <numeric value expression>

<repeat argument> ::= <numeric value expression>

<table primary> ::=
    <table or query name> [ <query system time period specification> ]
          [ [ AS ] <correlation name>
            [ <left paren> <derived column list> <right paren> ] ]
  | <derived table> [ AS ] <correlation name>
        [ <left paren> <derived column list> <right paren> ]
  | <lateral derived table> [ AS ] <correlation name>
        [ <left paren> <derived column list> <right paren> ]
  | <collection derived table> [ AS ] <correlation name>
        [ <left paren> <derived column list> <right paren> ]
  | <table function derived table> [ AS ] <correlation name>
        [ <left paren> <derived column list> <right paren> ]
  | <only spec> [ [ AS ] <correlation name>
        [ <left paren> <derived column list> <right paren> ] ]
  | <data change delta table> [ [ AS ] <correlation name>
        [ <left paren> <derived column list> <right paren> ] ]
  | <parenthesized joined table>

<query system time period specification> ::=
    FOR SYSTEM_TIME AS OF <point in time 1>
  | FOR SYSTEM_TIME BETWEEN [ ASYMMETRIC | SYMMETRIC ]
      <point in time 1> AND <point in time 2>
  | FOR SYSTEM_TIME FROM <point in time 1> TO <point in time 2>

TODO: query system time period spec

<point in time 1> ::= <point in time>

<point in time 2> ::= <point in time>

<point in time> ::= <datetime value expression>

<only spec> ::= ONLY <left paren> <table or query name> <right paren>

TODO: only

<lateral derived table> ::= LATERAL <table subquery>

<collection derived table> ::=
  UNNEST <left paren> <collection value expression>
      [ { <comma> <collection value expression> }... ] <right paren>
      [ WITH ORDINALITY ]

<table function derived table> ::=
  TABLE <left paren> <collection value expression> <right paren>

<derived table> ::= <table subquery>

<table or query name> ::= <table name> | <transition table name> | <query name>

<derived column list> ::= <column name list>

<column name list> ::= <column name> [ { <comma> <column name> }... ]

<data change delta table> ::=
  <result option> TABLE <left paren> <data change statement> <right paren>

<data change statement> ::=
    <delete statement: searched>
  | <insert statement>
  | <merge statement>
  | <update statement: searched>

<result option> ::= FINAL | NEW | OLD

<parenthesized joined table> ::=
    <left paren> <parenthesized joined table> <right paren>
  | <left paren> <joined table> <right paren>


>     -- table or query name
>     ,("select * from t u", a sel)
>     ,("select * from t as u", a sel)
>     ,("select * from t u(a,b)", sel1 )
>     ,("select * from t as u(a,b)", sel1)
>     -- derived table TODO: realistic example
>     ,("select * from (select * from t) u"
>      ,a $ sel {qeFrom = [TRQueryExpr sel]})
>     -- lateral TODO: realistic example
>     ,("select * from lateral t"
>      ,af TRLateral sel)
>     -- TODO: bug, lateral should bind more tightly than the alias
>     --,("select * from lateral t u"
>     -- ,a $ af sel TRLateral)
>     -- collection TODO: realistic example
>      -- TODO: make it work
>     --,("select * from unnest(a)", undefined)
>     --,("select * from unnest(a,b)", undefined)
>     --,("select * from unnest(a,b) with ordinality", undefined)
>     --,("select * from unnest(a,b) with ordinality u", undefined)
>     --,("select * from unnest(a,b) with ordinality as u", undefined)
>     -- table fn TODO: realistic example
>      -- TODO: make it work
>     --,("select * from table(a)", undefined)
>     -- parens
>     ,("select * from (a join b)", jsel)
>     ,("select * from (a join b) u", a jsel)
>     ,("select * from ((a join b)) u", a $ af TRParens jsel)
>     ,("select * from ((a join b) u) u", a $ af TRParens $ a jsel)
>     ]
>   where
>     sel = makeSelect
>           {qeSelectList = [(Star, Nothing)]
>           ,qeFrom = [TRSimple [Name "t"]]}
>     af f s = s {qeFrom = map f (qeFrom s)}
>     a s = af (\x -> TRAlias x $ Alias (Name "u") Nothing) s
>     sel1 = makeSelect
>           {qeSelectList = [(Star, Nothing)]
>           ,qeFrom = [TRAlias (TRSimple [Name "t"])
>                      $ Alias (Name "u") $ Just [Name "a", Name "b"]]}
>     jsel = sel {qeFrom =
>                 [TRParens $ TRJoin (TRSimple [Name "a"])
>                                    False
>                                    JInner
>                                    (TRSimple [Name "b"])
>                                    Nothing]}

== 7.7 <joined table>

Function
Specify a table derived from a Cartesian product, inner join, or outer join.

<joined table> ::= <cross join> | <qualified join> | <natural join>

<cross join> ::= <table reference> CROSS JOIN <table factor>

<qualified join> ::=
  { <table reference> | <partitioned join table> }
      [ <join type> ] JOIN
      { <table reference> | <partitioned join table> }
      <join specification>

<partitioned join table> ::=
  <table factor> PARTITION BY
      <partitioned join column reference list>

<partitioned join column reference list> ::=
  <left paren> <partitioned join column reference>
      [ { <comma> <partitioned join column reference> }... ]
      <right paren>

<partitioned join column reference> ::= <column reference>

<natural join> ::=
  { <table reference> | <partitioned join table> }
      NATURAL [ <join type> ] JOIN
      { <table factor> | <partitioned join table> }

<join specification> ::= <join condition> | <named columns join>

<join condition> ::= ON <search condition>

<named columns join> ::= USING <left paren> <join column list> <right paren>

<join type> ::= INNER | <outer join type> [ OUTER ]

<outer join type> ::= LEFT | RIGHT | FULL

<join column list> ::= <column name list>

> joinedTable :: TestItem
> joinedTable = Group "joined table"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     [("select * from a cross join b"
>      ,sel $ TRJoin a False JCross b Nothing)
>     ,("select * from a join b on true"
>      ,sel $ TRJoin a False JInner b
>       (Just $ JoinOn $ Iden [Name "true"]))
>     ,("select * from a join b using (c)"
>      ,sel $ TRJoin a False JInner b
>       (Just $ JoinUsing [Name "c"]))
>     ,("select * from a inner join b on true"
>      ,sel $ TRJoin a False JInner b
>       (Just $ JoinOn $ Iden [Name "true"]))
>     ,("select * from a left join b on true"
>      ,sel $ TRJoin a False JLeft b
>       (Just $ JoinOn $ Iden [Name "true"]))
>     ,("select * from a left outer join b on true"
>      ,sel $ TRJoin a False JLeft b
>       (Just $ JoinOn $ Iden [Name "true"]))
>     ,("select * from a right join b on true"
>      ,sel $ TRJoin a False JRight b
>       (Just $ JoinOn $ Iden [Name "true"]))
>     ,("select * from a full join b on true"
>      ,sel $ TRJoin a False JFull b
>       (Just $ JoinOn $ Iden [Name "true"]))
>     ,("select * from a natural join b"
>      ,sel $ TRJoin a True JInner b Nothing)
>     ,("select * from a natural inner join b"
>      ,sel $ TRJoin a True JInner b Nothing)
>     ,("select * from a natural left join b"
>      ,sel $ TRJoin a True JLeft b Nothing)
>     ,("select * from a natural left outer join b"
>      ,sel $ TRJoin a True JLeft b Nothing)
>     ,("select * from a natural right join b"
>      ,sel $ TRJoin a True JRight b Nothing)
>     ,("select * from a natural full join b"
>      ,sel $ TRJoin a True JFull b Nothing)
>     ]
>   where
>     sel t = makeSelect
>             {qeSelectList = [(Star, Nothing)]
>             ,qeFrom = [t]}
>     a = TRSimple [Name "a"]
>     b = TRSimple [Name "b"]

TODO: partitioned joins

== 7.8 <where clause>

Function

Specify a table derived by the application of a <search condition> to
the result of the preceding <from clause>.

<where clause> ::= WHERE <search condition>

> whereClause :: TestItem
> whereClause = Group "where clause"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     [("select * from t where a = 5"
>     ,makeSelect
>      {qeSelectList = [(Star,Nothing)]
>      ,qeFrom = [TRSimple [Name "t"]]
>      ,qeWhere = Just $ BinOp (Iden [Name "a"]) [Name "="] (NumLit "5")})]

== 7.9 <group by clause>

Function

Specify a grouped table derived by the application of the <group by
clause> to the result of the previously specified clause.

<group by clause> ::= GROUP BY [ <set quantifier> ] <grouping element list>

<grouping element list> ::=
  <grouping element> [ { <comma> <grouping element> }... ]

<grouping element> ::=
    <ordinary grouping set>
  | <rollup list>
  | <cube list>
  | <grouping sets specification>
  | <empty grouping set>

<ordinary grouping set> ::=
    <grouping column reference>
  | <left paren> <grouping column reference list> <right paren>

<grouping column reference> ::= <column reference> [ <collate clause> ]

<grouping column reference list> ::=
  <grouping column reference> [ { <comma> <grouping column reference> }... ]

<rollup list> ::=
  ROLLUP <left paren> <ordinary grouping set list> <right paren>

<ordinary grouping set list> ::=
  <ordinary grouping set> [ { <comma> <ordinary grouping set> }... ]

<cube list> ::= CUBE <left paren> <ordinary grouping set list> <right paren>

<grouping sets specification> ::=
  GROUPING SETS <left paren> <grouping set list> <right paren>

<grouping set list> ::= <grouping set> [ { <comma> <grouping set> }... ]

<grouping set> ::=
    <ordinary grouping set>
  | <rollup list>
  | <cube list>
  | <grouping sets specification>
  | <empty grouping set>

<empty grouping set> ::= <left paren> <right paren>


> groupByClause :: TestItem
> groupByClause = Group "group by clause"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     [("select a,sum(x) from t group by a"
>      ,qe [SimpleGroup $ Iden [Name "a"]])
>      ,("select a,sum(x) from t group by a collate c"
>      ,qe [SimpleGroup $ Collate (Iden [Name "a"]) [Name "c"]])
>     ,("select a,b,sum(x) from t group by a,b"
>      ,qex [SimpleGroup $ Iden [Name "a"]
>           ,SimpleGroup $ Iden [Name "b"]])
>     -- todo: group by set quantifier
>     --,("select a,sum(x) from t group by distinct a"
>     --,undefined)
>     --,("select a,sum(x) from t group by all a"
>     -- ,undefined)
>     ,("select a,b,sum(x) from t group by rollup(a,b)"
>      ,qex [Rollup [SimpleGroup $ Iden [Name "a"]
>                   ,SimpleGroup $ Iden [Name "b"]]])
>     ,("select a,b,sum(x) from t group by cube(a,b)"
>      ,qex [Cube [SimpleGroup $ Iden [Name "a"]
>                 ,SimpleGroup $ Iden [Name "b"]]])
>     ,("select a,b,sum(x) from t group by grouping sets((),(a,b))"
>      ,qex [GroupingSets [GroupingParens []
>                         ,GroupingParens [SimpleGroup $ Iden [Name "a"]
>                                         ,SimpleGroup $ Iden [Name "b"]]]])
>     ,("select sum(x) from t group by ()"
>      ,let x = qe [GroupingParens []]
>       in x {qeSelectList = tail $ qeSelectList x})
>     ]
>   where
>     qe g = makeSelect
>            {qeSelectList = [(Iden [Name "a"], Nothing)
>                            ,(App [Name "sum"] [Iden [Name "x"]], Nothing)]
>            ,qeFrom = [TRSimple [Name "t"]]
>            ,qeGroupBy = g}
>     qex g = let x = qe g
>             in x {qeSelectList = let [a,b] = qeSelectList x
>                                  in [a,(Iden [Name "b"],Nothing),b]}

== 7.10 <having clause>

Function

Specify a grouped table derived by the elimination of groups that do
not satisfy a <search condition>.

<having clause> ::= HAVING <search condition>

> havingClause :: TestItem
> havingClause = Group "having clause"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     [("select a,sum(x) from t group by a having sum(x) > 1000"
>      ,makeSelect
>       {qeSelectList = [(Iden [Name "a"], Nothing)
>                       ,(App [Name "sum"] [Iden [Name "x"]], Nothing)]
>       ,qeFrom = [TRSimple [Name "t"]]
>       ,qeGroupBy = [SimpleGroup $ Iden [Name "a"]]
>       ,qeHaving = Just $ BinOp (App [Name "sum"] [Iden [Name "x"]])
>                                [Name ">"]
>                                (NumLit "1000")})
>     ]

== 7.11 <window clause>

Function
Specify one or more window definitions.

<window clause> ::= WINDOW <window definition list>

<window definition list> ::=
  <window definition> [ { <comma> <window definition> }... ]

<window definition> ::= <new window name> AS <window specification>

<new window name> ::= <window name>

<window specification> ::=
  <left paren> <window specification details> <right paren>

<window specification details> ::=
  [ <existing window name> ]
      [ <window partition clause> ]
      [ <window order clause> ]
      [ <window frame clause> ]

<existing window name> ::= <window name>

<window partition clause> ::=
  PARTITION BY <window partition column reference list>

<window partition column reference list> ::=
  <window partition column reference>
      [ { <comma> <window partition column reference> }... ]

<window partition column reference> ::= <column reference> [ <collate clause> ]

<window order clause> ::= ORDER BY <sort specification list>

<window frame clause> ::=
  <window frame units> <window frame extent>
      [ <window frame exclusion> ]

<window frame units> ::= ROWS | RANGE | GROUPS

<window frame extent> ::= <window frame start> | <window frame between>

<window frame start> ::=
    UNBOUNDED PRECEDING
  | <window frame preceding>
  | CURRENT ROW

<window frame preceding> ::= <unsigned value specification> PRECEDING

<window frame between> ::=
  BETWEEN <window frame bound 1> AND <window frame bound 2>

<window frame bound 1> ::= <window frame bound>

<window frame bound 2> ::= <window frame bound>

<window frame bound> ::=
    <window frame start>
  | UNBOUNDED FOLLOWING
  | <window frame following>

<window frame following> ::= <unsigned value specification> FOLLOWING

<window frame exclusion> ::=
    EXCLUDE CURRENT ROW
  | EXCLUDE GROUP
  | EXCLUDE TIES
  | EXCLUDE NO OTHERS

> windowClause :: TestItem
> windowClause = Group "window clause"
>     [-- todo: window clause
>     ]

== 7.12 <query specification>

Function
Specify a table derived from the result of a <table expression>.

<query specification> ::=
  SELECT [ <set quantifier> ] <select list> <table expression>

<select list> ::=
    <asterisk>
  | <select sublist> [ { <comma> <select sublist> }... ]

<select sublist> ::= <derived column> | <qualified asterisk>

<qualified asterisk> ::=
    <asterisked identifier chain> <period> <asterisk>
  | <all fields reference>

<asterisked identifier chain> ::=
  <asterisked identifier> [ { <period> <asterisked identifier> }... ]

<asterisked identifier> ::= <identifier>

<derived column> ::= <value expression> [ <as clause> ]

<as clause> ::= [ AS ] <column name>

<all fields reference> ::=
  <value expression primary> <period> <asterisk>
      [ AS <left paren> <all fields column name list> <right paren> ]

<all fields column name list> ::= <column name list>

> querySpecification :: TestItem
> querySpecification = Group "query specification"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     [("select a from t",qe)
>     ,("select all a from t",qe {qeSetQuantifier = All})
>     ,("select distinct a from t",qe {qeSetQuantifier = Distinct})
>     ,("select * from t", qe {qeSelectList = [(Star,Nothing)]})
>     ,("select a.* from t"
>      ,qe {qeSelectList = [(BinOp (Iden [Name "a"]) [Name "."] Star
>                            ,Nothing)]})
>     ,("select a b from t"
>      ,qe {qeSelectList = [(Iden [Name "a"], Just $ Name "b")]})
>     ,("select a as b from t"
>      ,qe {qeSelectList = [(Iden [Name "a"], Just $ Name "b")]})
>     ,("select a,b from t"
>      ,qe {qeSelectList = [(Iden [Name "a"], Nothing)
>                          ,(Iden [Name "b"], Nothing)]})
>     -- todo: all field reference alias
>     --,("select * as (a,b) from t",undefined)
>     ]
>   where
>     qe = makeSelect
>          {qeSelectList = [(Iden [Name "a"], Nothing)]
>          ,qeFrom = [TRSimple [Name "t"]]
>          }

== 7.13 <query expression>

Function
Specify a table.

<query expression> ::=
  [ <with clause> ] <query expression body>
      [ <order by clause> ] [ <result offset clause> ] [ <fetch first clause> ]

<with clause> ::= WITH [ RECURSIVE ] <with list>

<with list> ::= <with list element> [ { <comma> <with list element> }... ]

<with list element> ::=
  <query name> [ <left paren> <with column list> <right paren> ]
      AS <table subquery> [ <search or cycle clause> ]

<with column list> ::= <column name list>

> withQueryExpression :: TestItem
> withQueryExpression= Group "with query expression"
>     [-- todo: with query expression
>     ]

<query expression body> ::=
    <query term>
  | <query expression body> UNION [ ALL | DISTINCT ]
      [ <corresponding spec> ] <query term>
  | <query expression body> EXCEPT [ ALL | DISTINCT ]
      [ <corresponding spec> ] <query term>

<query term> ::=
    <query primary>
  | <query term> INTERSECT [ ALL | DISTINCT ]
      [ <corresponding spec> ] <query primary>

<query primary> ::=
    <simple table>
  | <left paren> <query expression body>
      [ <order by clause> ] [ <result offset clause> ] [ <fetch first clause> ]
      <right paren>

> setOpQueryExpression :: TestItem
> setOpQueryExpression= Group "set operation query expression"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     -- todo: complete setop query expression tests
>     [{-("select * from t union select * from t"
>      ,undefined)
>     ,("select * from t union all select * from t"
>      ,undefined)
>     ,("select * from t union distinct select * from t"
>      ,undefined)
>     ,("select * from t union corresponding select * from t"
>      ,undefined)
>     ,("select * from t union corresponding by (a,b) select * from t"
>      ,undefined)
>     ,("select * from t except select * from t"
>      ,undefined)
>     ,("select * from t in intersect select * from t"
>      ,undefined)-}
>     ]

TODO: tests for the associativity and precendence

TODO: not sure exactly where parens are allowed, we will allow them
everywhere

<simple table> ::=
    <query specification>
  | <table value constructor>
  | <explicit table>

<explicit table> ::= TABLE <table or query name>

<corresponding spec> ::=
  CORRESPONDING [ BY <left paren> <corresponding column list> <right paren> ]

<corresponding column list> ::= <column name list>

> explicitTableQueryExpression :: TestItem
> explicitTableQueryExpression= Group "explicit table query expression"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     [("table t", Table [Name "t"])
>     ]


<order by clause> ::= ORDER BY <sort specification list>

<result offset clause> ::= OFFSET <offset row count> { ROW | ROWS }

<fetch first clause> ::=
  FETCH { FIRST | NEXT } [ <fetch first quantity> ] { ROW | ROWS } { ONLY | WITH TIES }

<fetch first quantity> ::= <fetch first row count> | <fetch first percentage>

<offset row count> ::= <simple value specification>

<fetch first row count> ::= <simple value specification>

<fetch first percentage> ::= <simple value specification> PERCENT

> orderOffsetFetchQueryExpression :: TestItem
> orderOffsetFetchQueryExpression = Group "order, offset, fetch query expression"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     [-- todo: finish tests for order offset and fetch
>      ("select a from t order by a"
>      ,qe {qeOrderBy = [SortSpec (Iden [Name "a"])
>                            DirDefault NullsOrderDefault]})
>     ,("select a from t offset 5 row"
>      ,qe {qeOffset = Just $ NumLit "5"})
>     ,("select a from t offset 5 rows"
>      ,qe {qeOffset = Just $ NumLit "5"})
>     ,("select a from t fetch first 5 row only"
>      ,qe {qeFetchFirst = Just $ NumLit "5"})
>     -- todo: support with ties and percent in fetch
>     --,("select a from t fetch next 5 rows with ties"
>     --,("select a from t fetch first 5 percent rows only"
>     ]
>  where
>     qe = makeSelect
>          {qeSelectList = [(Iden [Name "a"], Nothing)]
>          ,qeFrom = [TRSimple [Name "t"]]
>          }


== 7.14 <search or cycle clause>

Function

Specify the generation of ordering and cycle detection information in
the result of recursive query expressions.

<search or cycle clause> ::=
    <search clause>
  | <cycle clause>
  | <search clause> <cycle clause>

<search clause> ::= SEARCH <recursive search order> SET <sequence column>

<recursive search order> ::=
    DEPTH FIRST BY <column name list>
  | BREADTH FIRST BY <column name list>

<sequence column> ::= <column name>

<cycle clause> ::=
  CYCLE <cycle column list> SET <cycle mark column> TO <cycle mark value>
      DEFAULT <non-cycle mark value> USING <path column>

<cycle column list> ::= <cycle column> [ { <comma> <cycle column> }... ]

<cycle column> ::= <column name>

<cycle mark column> ::= <column name>

<path column> ::= <column name>

<cycle mark value> ::= <value expression>

<non-cycle mark value> ::= <value expression>

> searchOrCycleClause :: TestItem
> searchOrCycleClause = Group "search or cycle clause"
>     [-- todo: search or cycle clause
>     ]

== 7.15 <subquery>

Function

Specify a scalar value, a row, or a table derived from a <query
expression>.

<scalar subquery> ::= <subquery>

<row subquery> ::= <subquery>

<table subquery> ::= <subquery>

<subquery> ::= <left paren> <query expression> <right paren>

> scalarSubquery :: TestItem
> scalarSubquery = Group "scalar subquery"
>     [-- todo: scalar subquery
>     ]

= 8 Predicates

== 8.1 <predicate>

Function
Specify a condition that can be evaluated to give a boolean value.

<predicate> ::=
    <comparison predicate>
  | <between predicate>
  | <in predicate>
  | <like predicate>
  | <similar predicate>
  | <regex like predicate>
  | <null predicate>
  | <quantified comparison predicate>
  | <exists predicate>
  | <unique predicate>
  | <normalized predicate>
  | <match predicate>
  | <overlaps predicate>
  | <distinct predicate>
  | <member predicate>
  | <submultiset predicate>
  | <set predicate>
  | <type predicate>
  | <period predicate>

> predicates :: TestItem
> predicates = Group "predicates"
>     [comparisonPredicates
>     ,betweenPredicate
>     ,inPredicate
>     ,likePredicate
>     ,similarPredicate
>     ,regexLikePredicate
>     ,nullPredicate
>     ,quantifiedComparisonPredicate
>     ,existsPredicate
>     ,uniquePredicate
>     ,normalizedPredicate
>     ,matchPredicate
>     ,overlapsPredicate
>     ,distinctPredicate
>     ,memberPredicate
>     ,submultisetPredicate
>     ,setPredicate
>     ,periodPredicate
>     ]


== 8.1 <predicate>

No grammar

== 8.2 <comparison predicate>

Function
Specify a comparison of two row values.

<comparison predicate> ::= <row value predicand> <comparison predicate part 2>

<comparison predicate part 2> ::= <comp op> <row value predicand>

<comp op> ::=
    <equals operator>
  | <not equals operator>
  | <less than operator>
  | <greater than operator>
  | <less than or equals operator>
  | <greater than or equals operator>

> comparisonPredicates :: TestItem
> comparisonPredicates = Group "comparison predicates"
>     $ map (uncurry (TestValueExpr SQL2011))
>     $ map mkOp ["=", "<>", "<", ">", "<=", ">="]
>     ++ [("ROW(a) = ROW(b)"
>         ,BinOp (App [Name "ROW"] [a])
>                [Name "="]
>                (App [Name "ROW"] [b]))
>        ,("(a,b) = (c,d)"
>         ,BinOp (SpecialOp [Name "rowctor"] [a,b])
>            [Name "="]
>            (SpecialOp [Name "rowctor"] [Iden [Name "c"], Iden [Name "d"]]))
>     ]
>   where
>     mkOp nm = ("a " ++ nm ++ " b"
>               ,BinOp a [Name nm] b)
>     a = Iden [Name "a"]
>     b = Iden [Name "b"]

TODO: what other tests, more complex expressions with comparisons?

== 8.3 <between predicate>

Function
Specify a range comparison.

<between predicate> ::= <row value predicand> <between predicate part 2>

<between predicate part 2> ::=
  [ NOT ] BETWEEN [ ASYMMETRIC | SYMMETRIC ]
      <row value predicand> AND <row value predicand>

> betweenPredicate :: TestItem
> betweenPredicate = Group "between predicate"
>     [-- todo: between predicate
>     ]

== 8.4 <in predicate>

Function
Specify a quantified comparison.

<in predicate> ::= <row value predicand> <in predicate part 2>

<in predicate part 2> ::= [ NOT ] IN <in predicate value>

<in predicate value> ::=
    <table subquery>
  | <left paren> <in value list> <right paren>

<in value list> ::=
  <row value expression> [ { <comma> <row value expression> }... ]

> inPredicate :: TestItem
> inPredicate = Group "in predicate"
>     [-- todo: in predicate
>     ]

== 8.5 <like predicate>

Function
Specify a pattern-match comparison.

<like predicate> ::= <character like predicate> | <octet like predicate>

<character like predicate> ::=
  <row value predicand> <character like predicate part 2>

<character like predicate part 2> ::=
  [ NOT ] LIKE <character pattern> [ ESCAPE <escape character> ]

<character pattern> ::= <character value expression>

<escape character> ::= <character value expression>

<octet like predicate> ::= <row value predicand> <octet like predicate part 2>

<octet like predicate part 2> ::=
  [ NOT ] LIKE <octet pattern> [ ESCAPE <escape octet> ]

<octet pattern> ::= <binary value expression>

<escape octet> ::= <binary value expression>

> likePredicate :: TestItem
> likePredicate = Group "like predicate"
>     [-- todo: like predicate
>     ]

== 8.6 <similar predicate>

Function
Specify a character string similarity by means of a regular expression.

<similar predicate> ::= <row value predicand> <similar predicate part 2>

<similar predicate part 2> ::=
  [ NOT ] SIMILAR TO <similar pattern> [ ESCAPE <escape character> ]

<similar pattern> ::= <character value expression>

<regular expression> ::=
    <regular term>
  | <regular expression> <vertical bar> <regular term>

<regular term> ::= <regular factor> | <regular term> <regular factor>

<regular factor> ::=
    <regular primary>
  | <regular primary> <asterisk>
  | <regular primary> <plus sign>
  | <regular primary> <question mark>
  | <regular primary> <repeat factor>

<repeat factor> ::= <left brace> <low value> [ <upper limit> ] <right brace>

<upper limit> ::= <comma> [ <high value> ]

<low value> ::= <unsigned integer>

<high value> ::= <unsigned integer>

<regular primary> ::=
    <character specifier>
  | <percent>
  | <regular character set>
  | <left paren> <regular expression> <right paren>

<character specifier> ::= <non-escaped character> | <escaped character>

<non-escaped character> ::= !! See the Syntax Rules.

<escaped character> ::= !! See the Syntax Rules.

<regular character set> ::=
    <underscore>
  | <left bracket> <character enumeration>... <right bracket>
  | <left bracket> <circumflex> <character enumeration>... <right bracket>
  | <left bracket> <character enumeration include>...
      <circumflex> <character enumeration exclude>... <right bracket>

<character enumeration include> ::= <character enumeration>

<character enumeration exclude> ::= <character enumeration>

<character enumeration> ::=
    <character specifier>
  | <character specifier> <minus sign> <character specifier>
  | <left bracket> <colon> <regular character set identifier> <colon> <right bracket>

<regular character set identifier> ::= <identifier>

> similarPredicate :: TestItem
> similarPredicate = Group "similar predicate"
>     [-- todo: similar predicate
>     ]


== 8.7 <regex like predicate>

Function
Specify a pattern-match comparison using an XQuery regular expression.

<regex like predicate> ::= <row value predicand> <regex like predicate part 2>

<regex like predicate part 2> ::=
  [ NOT ] LIKE_REGEX <XQuery pattern> [ FLAG <XQuery option flag> ]

> regexLikePredicate :: TestItem
> regexLikePredicate = Group "regex like predicate"
>     [-- todo: regex like predicate
>     ]

== 8.8 <null predicate>

Function
Specify a test for a null value.

<null predicate> ::= <row value predicand> <null predicate part 2>

<null predicate part 2> ::= IS [ NOT ] NULL

> nullPredicate :: TestItem
> nullPredicate = Group "null predicate"
>     [-- todo: null predicate
>     ]

== 8.9 <quantified comparison predicate>

Function
Specify a quantified comparison.

<quantified comparison predicate> ::=
  <row value predicand> <quantified comparison predicate part 2>

<quantified comparison predicate part 2> ::=
  <comp op> <quantifier> <table subquery>

<quantifier> ::= <all> | <some>

<all> ::= ALL

<some> ::= SOME | ANY

> quantifiedComparisonPredicate :: TestItem
> quantifiedComparisonPredicate = Group "quantified comparison predicate"
>     $ map (uncurry (TestValueExpr SQL2011))

>     [("a = any (select * from t)"
>      ,QuantifiedComparison (Iden [Name "a"]) [Name "="] CPAny qe)
>     ,("a <= some (select * from t)"
>      ,QuantifiedComparison (Iden [Name "a"]) [Name "<="] CPSome qe)
>     ,("a > all (select * from t)"
>      ,QuantifiedComparison (Iden [Name "a"]) [Name ">"] CPAll qe)
>     ,("(a,b) <> all (select * from t)"
>      ,QuantifiedComparison
>       (SpecialOp [Name "rowctor"] [Iden [Name "a"]
>                                   ,Iden [Name "b"]]) [Name "<>"] CPAll qe)
>     ]
>   where
>     qe = makeSelect
>          {qeSelectList = [(Star,Nothing)]
>          ,qeFrom = [TRSimple [Name "t"]]}

== 8.10 <exists predicate>

Function
Specify a test for a non-empty set.

<exists predicate> ::= EXISTS <table subquery>

> existsPredicate :: TestItem
> existsPredicate = Group "exists predicate"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("exists(select * from t where a = 4)"
>      ,SubQueryExpr SqExists
>       $ makeSelect
>         {qeSelectList = [(Star,Nothing)]
>         ,qeFrom = [TRSimple [Name "t"]]
>         ,qeWhere = Just (BinOp (Iden [Name "a"]) [Name "="] (NumLit "4"))
>         }
>      )]

== 8.11 <unique predicate>

Function
Specify a test for the absence of duplicate rows.

<unique predicate> ::= UNIQUE <table subquery>

> uniquePredicate :: TestItem
> uniquePredicate = Group "unique predicate"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("unique(select * from t where a = 4)"
>      ,SubQueryExpr SqUnique
>       $ makeSelect
>         {qeSelectList = [(Star,Nothing)]
>         ,qeFrom = [TRSimple [Name "t"]]
>         ,qeWhere = Just (BinOp (Iden [Name "a"]) [Name "="] (NumLit "4"))
>         }
>      )]

== 8.12 <normalized predicate>

Function
Determine whether a character string value is normalized.

<normalized predicate> ::= <row value predicand> <normalized predicate part 2>

<normalized predicate part 2> ::= IS [ NOT ] [ <normal form> ] NORMALIZED

> normalizedPredicate :: TestItem
> normalizedPredicate = Group "normalized predicate"
>     [-- todo: normalized predicate
>     ]

== 8.13 <match predicate>

Function
Specify a test for matching rows.

<match predicate> ::= <row value predicand> <match predicate part 2>

<match predicate part 2> ::=
  MATCH [ UNIQUE ] [ SIMPLE | PARTIAL | FULL ] <table subquery>

> matchPredicate :: TestItem
> matchPredicate = Group "match predicate"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("a match (select a from t)"
>      ,Match (Iden [Name "a"]) False qe)
>     ,("(a,b) match (select a,b from t)"
>      ,Match (SpecialOp [Name "rowctor"]
>              [Iden [Name "a"], Iden [Name "b"]]) False qea)
>     ,("(a,b) match unique (select a,b from t)"
>      ,Match (SpecialOp [Name "rowctor"]
>              [Iden [Name "a"], Iden [Name "b"]]) True qea)
>     ]
>   where
>     qe = makeSelect
>          {qeSelectList = [(Iden [Name "a"],Nothing)]
>          ,qeFrom = [TRSimple [Name "t"]]}
>     qea = qe {qeSelectList = qeSelectList qe
>                              ++ [(Iden [Name "b"],Nothing)]}

TODO: simple, partial and full

== 8.14 <overlaps predicate>

Function
Specify a test for an overlap between two datetime periods.

<overlaps predicate> ::=
  <overlaps predicate part 1> <overlaps predicate part 2>

<overlaps predicate part 1> ::= <row value predicand 1>

<overlaps predicate part 2> ::= OVERLAPS <row value predicand 2>

<row value predicand 1> ::= <row value predicand>

<row value predicand 2> ::= <row value predicand>

> overlapsPredicate :: TestItem
> overlapsPredicate = Group "overlaps predicate"
>     [-- todo: overlaps predicate
>     ]

== 8.15 <distinct predicate>

Function
Specify a test of whether two row values are distinct

<distinct predicate> ::= <row value predicand 3> <distinct predicate part 2>

<distinct predicate part 2> ::=
  IS [ NOT ] DISTINCT FROM <row value predicand 4>

<row value predicand 3> ::= <row value predicand>

<row value predicand 4> ::= <row value predicand>

> distinctPredicate :: TestItem
> distinctPredicate = Group "distinct predicate"
>     [-- todo: distinct predicate
>     ]

== 8.16 <member predicate>

Function
Specify a test of whether a value is a member of a multiset.

<member predicate> ::= <row value predicand> <member predicate part 2>

<member predicate part 2> ::= [ NOT ] MEMBER [ OF ] <multiset value expression>

> memberPredicate :: TestItem
> memberPredicate = Group "member predicate"
>     [-- todo: member predicate
>     ]

== 8.17 <submultiset predicate>

Function
Specify a test of whether a multiset is a submultiset of another multiset.

<submultiset predicate> ::=
  <row value predicand> <submultiset predicate part 2>

<submultiset predicate part 2> ::=
  [ NOT ] SUBMULTISET [ OF ] <multiset value expression>

> submultisetPredicate :: TestItem
> submultisetPredicate = Group "submultiset predicate"
>     [-- todo: submultiset predicate
>     ]

== 8.18 <set predicate>

Function

Specify a test of whether a multiset is a set (that is, does not
contain any duplicates).

<set predicate> ::= <row value predicand> <set predicate part 2>

<set predicate part 2> ::= IS [ NOT ] A SET

> setPredicate :: TestItem
> setPredicate = Group "set predicate"
>     [-- todo: set predicate
>     ]

== 8.19 <type predicate>

Function
Specify a type test.

<type predicate> ::= <row value predicand> <type predicate part 2>

<type predicate part 2> ::=
  IS [ NOT ] OF <left paren> <type list> <right paren>

<type list> ::=
  <user-defined type specification>
      [ { <comma> <user-defined type specification> }... ]

<user-defined type specification> ::=
    <inclusive user-defined type specification>
  | <exclusive user-defined type specification>

<inclusive user-defined type specification> ::=
  <path-resolved user-defined type name>

<exclusive user-defined type specification> ::=
  ONLY <path-resolved user-defined type name>

TODO: type predicate

== 8.20 <period predicate>

Function
Specify a test to determine the relationship between periods.

<period predicate> ::=
    <period overlaps predicate>
  | <period equals predicate>
  | <period contains predicate>
  | <period precedes predicate>
  | <period succeeds predicate>
  | <period immediately precedes predicate>
  | <period immediately succeeds predicate>

<period overlaps predicate> ::=
  <period predicand 1> <period overlaps predicate part 2>

<period overlaps predicate part 2> ::= OVERLAPS <period predicand 2>

<period predicand 1> ::= <period predicand>

<period predicand 2> ::= <period predicand>

<period predicand> ::=
    <period reference>
  | PERIOD <left paren> <period start value> <comma> <period end value> <right paren>

<period reference> ::= <basic identifier chain>

<period start value> ::= <datetime value expression>

<period end value> ::= <datetime value expression>

<period equals predicate> ::=
  <period predicand 1> <period equals predicate part 2>

<period equals predicate part 2> ::= EQUALS <period predicand 2>

<period contains predicate> ::=
  <period predicand 1> <period contains predicate part 2>

<period contains predicate part 2> ::=
  CONTAINS <period or point-in-time predicand>

<period or point-in-time predicand> ::=
    <period predicand>
  | <datetime value expression>

<period precedes predicate> ::=
  <period predicand 1> <period precedes predicate part 2>

<period precedes predicate part 2> ::= PRECEDES <period predicand 2>

<period succeeds predicate> ::=
  <period predicand 1> <period succeeds predicate part 2>

<period succeeds predicate part 2> ::= SUCCEEDS <period predicand 2>

<period immediately precedes predicate> ::=
  <period predicand 1> <period immediately precedes predicate part 2>

<period immediately precedes predicate part 2> ::=
  IMMEDIATELY PRECEDES <period predicand 2>

<period immediately succeeds predicate> ::=
  <period predicand 1> <period immediately succeeds predicate part 2>

<period immediately succeeds predicate part 2> ::=
  IMMEDIATELY SUCCEEDS <period predicand 2>

> periodPredicate :: TestItem
> periodPredicate = Group "period predicate"
>     [-- todo: period predicate
>     ]

== 8.21 <search condition>

Function

Specify a condition that is True, False, or Unknown, depending on the
value of a <boolean value expression>.

<search condition> ::= <boolean value expression>

= 10 Additional common elements

== 10.1 <interval qualifier>

Function
Specify the precision of an interval data type.

<interval qualifier> ::= <start field> TO <end field> | <single datetime field>

<start field> ::=
  <non-second primary datetime field>
      [ <left paren> <interval leading field precision> <right paren> ]

<end field> ::=
    <non-second primary datetime field>
  | SECOND [ <left paren> <interval fractional seconds precision> <right paren> ]

<single datetime field> ::=
    <non-second primary datetime field>
        [ <left paren> <interval leading field precision> <right paren> ]
  | SECOND [ <left paren> <interval leading field precision>
      [ <comma> <interval fractional seconds precision> ] <right paren> ]

<primary datetime field> ::= <non-second primary datetime field> | SECOND

<non-second primary datetime field> ::= YEAR | MONTH | DAY | HOUR | MINUTE

<interval fractional seconds precision> ::= <unsigned integer>

<interval leading field precision> ::= <unsigned integer>

> intervalQualifier :: TestItem
> intervalQualifier = Group "interval qualifier"
>     [-- todo: interval qualifier
>     ]

todo: also test all of these in the typenames and in the interval
literal tests

== 10.2 <language clause>

Function
Specify a programming language.

<language clause> ::= LANGUAGE <language name>

<language name> ::= ADA | C | COBOL | FORTRAN | M | MUMPS | PASCAL | PLI | SQL

== 10.3 <path specification>

Function
Specify an order for searching for an SQL-invoked routine.

<path specification> ::= PATH <schema name list>

<schema name list> ::= <schema name> [ { <comma> <schema name> }... ]

== 10.4 <routine invocation>

Function
Invoke an SQL-invoked routine.

<routine invocation> ::= <routine name> <SQL argument list>

<routine name> ::= [ <schema name> <period> ] <qualified identifier>

<SQL argument list> ::=
  <left paren> [ <SQL argument> [ { <comma> <SQL argument> }... ] ] <right paren>

<SQL argument> ::=
    <value expression>
  | <generalized expression>
  | <target specification>
  | <contextually typed value specification>
  | <named argument specification>

<generalized expression> ::=
  <value expression> AS <path-resolved user-defined type name>

<named argument specification> ::=
  <SQL parameter name> <named argument assignment token>
      <named argument SQL argument>

<named argument SQL argument> ::=
    <value expression>
  | <target specification>
     | <contextually typed value specification>

== 10.5 <character set specification>

Function
Identify a character set.

<character set specification> ::=
    <standard character set name>
  | <implementation-defined character set name>
  | <user-defined character set name>

<standard character set name> ::= <character set name>

<implementation-defined character set name> ::= <character set name>

<user-defined character set name> ::= <character set name>

tested in the type names

== 10.6 <specific routine designator>

Function
Specify an SQL-invoked routine.

<specific routine designator> ::=
    SPECIFIC <routine type> <specific name>
  | <routine type> <member name> [ FOR <schema-resolved user-defined type name> ]

<routine type> ::=
    ROUTINE
  | FUNCTION
  | PROCEDURE
  | [ INSTANCE | STATIC | CONSTRUCTOR ] METHOD

<member name> ::= <member name alternatives> [ <data type list> ]

<member name alternatives> ::= <schema qualified routine name> | <method name>

<data type list> ::=
  <left paren> [ <data type> [ { <comma> <data type> }... ] ] <right paren>

== 10.7 <collate clause>

Function
Specify a default collation.

<collate clause> ::= COLLATE <collation name>

> collateClause :: TestItem
> collateClause = Group "collate clause"
>     $ map (uncurry (TestValueExpr SQL2011))
>     [("a collate my_collation"
>      ,Collate (Iden [Name "a"]) [Name "my_collation"])]

== 10.8 <constraint name definition> and <constraint characteristics>

Function
Specify the name of a constraint and its characteristics.

<constraint name definition> ::= CONSTRAINT <constraint name>

<constraint characteristics> ::=
    <constraint check time> [ [ NOT ] DEFERRABLE ] [ <constraint enforcement> ]
  | [ NOT ] DEFERRABLE [ <constraint check time> ] [ <constraint enforcement> ]
  | <constraint enforcement>

<constraint check time> ::= INITIALLY DEFERRED | INITIALLY IMMEDIATE

<constraint enforcement> ::= [ NOT ] ENFORCED

== 10.9 <aggregate function>

Function
Specify a value computed from a collection of rows.

<aggregate function> ::=
    COUNT <left paren> <asterisk> <right paren> [ <filter clause> ]
  | <general set function> [ <filter clause> ]
  | <binary set function> [ <filter clause> ]
  | <ordered set function> [ <filter clause> ]
  | <array aggregate function> [ <filter clause> ]

<general set function> ::=
  <set function type> <left paren> [ <set quantifier> ]
      <value expression> <right paren>

<set function type> ::= <computational operation>

<computational operation> ::=
    AVG
  | MAX
  | MIN
  | SUM
  | EVERY
  | ANY
  | SOME
  | COUNT
  | STDDEV_POP
  | STDDEV_SAMP
  | VAR_SAMP
  | VAR_POP
  | COLLECT
  | FUSION
  | INTERSECTION

<set quantifier> ::= DISTINCT | ALL

<filter clause> ::= FILTER <left paren> WHERE <search condition> <right paren>

<binary set function> ::=
  <binary set function type> <left paren> <dependent variable expression> <comma>
      <independent variable expression> <right paren>

<binary set function type> ::=
    COVAR_POP
  | COVAR_SAMP
  | CORR
  | REGR_SLOPE
  | REGR_INTERCEPT
  | REGR_COUNT
  | REGR_R2
  | REGR_AVGX
  | REGR_AVGY
  | REGR_SXX
  | REGR_SYY
  | REGR_SXY

<dependent variable expression> ::= <numeric value expression>

<independent variable expression> ::= <numeric value expression>

<ordered set function> ::=
    <hypothetical set function>
  | <inverse distribution function>

<hypothetical set function> ::=
  <rank function type> <left paren>
      <hypothetical set function value expression list> <right paren>
      <within group specification>

<within group specification> ::=
  WITHIN GROUP <left paren> ORDER BY <sort specification list> <right paren>

<hypothetical set function value expression list> ::=
  <value expression> [ { <comma> <value expression> }... ]

<inverse distribution function> ::=
  <inverse distribution function type> <left paren>
      <inverse distribution function argument> <right paren>
      <within group specification>

<inverse distribution function argument> ::= <numeric value expression>

<inverse distribution function type> ::= PERCENTILE_CONT | PERCENTILE_DISC

<array aggregate function> ::=
  ARRAY_AGG
      <left paren> <value expression> [ ORDER BY <sort specification list> ] <right paren>

> aggregateFunction :: TestItem
> aggregateFunction = Group "aggregate function"
>     $ map (uncurry (TestValueExpr SQL2011)) $
>     [("count(*)",App [Name "count"] [Star])
>     ,("count(*) filter (where something > 5)"
>      ,AggregateApp [Name "count"] SQDefault [Star] [] fil)

gsf

>     ,("count(a)",App [Name "count"] [Iden [Name "a"]])
>     ,("count(distinct a)"
>      ,AggregateApp [Name "count"]
>                    Distinct
>                    [Iden [Name "a"]] [] Nothing)
>     ,("count(all a)"
>      ,AggregateApp [Name "count"]
>                    All
>                    [Iden [Name "a"]] [] Nothing)
>     ,("count(all a) filter (where something > 5)"
>      ,AggregateApp [Name "count"]
>                    All
>                    [Iden [Name "a"]] [] fil)
>     ] ++ concatMap mkSimpleAgg
>          ["avg","max","min","sum"
>          ,"every", "any", "some"
>          ,"stddev_pop","stddev_samp","var_samp","var_pop"
>          ,"collect","fusion","intersection"]

bsf

>     ++ concatMap mkBsf
>          ["COVAR_POP","COVAR_SAMP","CORR","REGR_SLOPE"
>           ,"REGR_INTERCEPT","REGR_COUNT","REGR_R2"
>           ,"REGR_AVGX","REGR_AVGY"
>           ,"REGR_SXX","REGR_SYY","REGR_SXY"]

osf

>     ++
>     [("rank(a,c) within group (order by b)"
>      ,AggregateAppGroup [Name "rank"]
>           [Iden [Name "a"], Iden [Name "c"]]
>           ob)]
>     ++ map mkGp ["dense_rank","percent_rank"
>                 ,"cume_dist", "percentile_cont"
>                 ,"percentile_disc"]
>     ++ [("array_agg(a)", App [Name "array_agg"] [Iden [Name "a"]])
>        ,("array_agg(a order by z)"
>         ,AggregateApp [Name "array_agg"]
>                        SQDefault
>                        [Iden [Name "a"]]
>                        [SortSpec (Iden [Name "z"])
>                             DirDefault NullsOrderDefault]
>                        Nothing)]

>   where
>     fil = Just $ BinOp (Iden [Name "something"]) [Name ">"] (NumLit "5")
>     ob = [SortSpec (Iden [Name "b"]) DirDefault NullsOrderDefault]
>     mkGp nm = (nm ++ "(a) within group (order by b)"
>               ,AggregateAppGroup [Name nm]
>                [Iden [Name "a"]]
>                ob)

>     mkSimpleAgg nm =
>         [(nm ++ "(a)",App [Name nm] [Iden [Name "a"]])
>         ,(nm ++ "(distinct a)"
>          ,AggregateApp [Name nm]
>                        Distinct
>                        [Iden [Name "a"]] [] Nothing)]
>     mkBsf nm =
>         [(nm ++ "(a,b)",App [Name nm] [Iden [Name "a"],Iden [Name "b"]])
>         ,(nm ++"(a,b) filter (where something > 5)"
>           ,AggregateApp [Name nm]
>                         SQDefault
>                         [Iden [Name "a"],Iden [Name "b"]] [] fil)]

== 10.10 <sort specification list>

Function
Specify a sort order.

<sort specification list> ::=
  <sort specification> [ { <comma> <sort specification> }... ]

<sort specification> ::=
  <sort key> [ <ordering specification> ] [ <null ordering> ]

<sort key> ::= <value expression>

<ordering specification> ::= ASC | DESC

<null ordering> ::=
  | NULLS LAST
    NULLS FIRST

> sortSpecificationList :: TestItem
> sortSpecificationList = Group "sort specification list"
>     $ map (uncurry (TestQueryExpr SQL2011))
>     [("select * from t order by a"
>      ,qe {qeOrderBy = [SortSpec (Iden [Name "a"])
>                            DirDefault NullsOrderDefault]})
>     ,("select * from t order by a,b"
>      ,qe {qeOrderBy = [SortSpec (Iden [Name "a"])
>                            DirDefault NullsOrderDefault
>                       ,SortSpec (Iden [Name "b"])
>                            DirDefault NullsOrderDefault]})
>     ,("select * from t order by a asc,b"
>      ,qe {qeOrderBy = [SortSpec (Iden [Name "a"])
>                            Asc NullsOrderDefault
>                       ,SortSpec (Iden [Name "b"])
>                            DirDefault NullsOrderDefault]})
>     ,("select * from t order by a desc,b"
>      ,qe {qeOrderBy = [SortSpec (Iden [Name "a"])
>                            Desc NullsOrderDefault
>                       ,SortSpec (Iden [Name "b"])
>                            DirDefault NullsOrderDefault]})
>     ,("select * from t order by a collate x desc,b"
>      ,qe {qeOrderBy = [SortSpec
>                            (Collate (Iden [Name "a"]) [Name "x"])
>                            Desc NullsOrderDefault
>                       ,SortSpec (Iden [Name "b"])
>                            DirDefault NullsOrderDefault]})
>     ,("select * from t order by 1,2"
>      ,qe {qeOrderBy = [SortSpec (NumLit "1")
>                            DirDefault NullsOrderDefault
>                       ,SortSpec (NumLit "2")
>                            DirDefault NullsOrderDefault]})
>     ]
>   where
>     qe = makeSelect
>          {qeSelectList = [(Star,Nothing)]
>          ,qeFrom = [TRSimple [Name "t"]]}
