
This file goes through the grammar from SQL 2003. The grammar is taken
from this site: http://savage.net.au/SQL/.

We are only interested in the query syntax, goes through sections 5-10
(section 9 has no syntax so isn't covered here).

The goal is to create some coverage tests to get close to supporting a
large amount of the SQL.

> module Language.SQL.SimpleSQL.SQL2003 where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax



= 5 Lexical Elements

Basic definitions of characters used, tokens, symbols, etc. Most of this section would normally be handled within the lexical analyzer rather than in the grammar proper. Further, the original document does not quote the various single characters, which makes it hard to process automatically.

[There seems to be a lot of unused stuff here, so skip this section and only do bits which 

5.1 <SQL terminal character> (p151)

<SQL terminal character>    ::=   <SQL language character>

<SQL language character>    ::=   <simple Latin letter> | <digit> | <SQL special character>

<simple Latin letter>    ::=   <simple Latin upper case letter> | <simple Latin lower case letter>

<simple Latin upper case letter>    ::= 
         A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

<simple Latin lower case letter>    ::= 
         a | b | c | d | e | f | g | h | i | j | k | l | m | n | o | p | q | r | s | t | u | v | w | x | y | z

<digit>    ::=   0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

<SQL special character>    ::= 
         <space>
     |     <double quote>
     |     <percent>
     |     <ampersand>
     |     <quote>
     |     <left paren>
     |     <right paren>
     |     <asterisk>
     |     <plus sign>
     |     <comma>
     |     <minus sign>
     |     <period>
     |     <solidus>
     |     <colon>
     |     <semicolon>
     |     <less than operator>
     |     <equals operator>
     |     <greater than operator>
     |     <question mark>
     |     <left bracket>
     |     <right bracket>
     |     <circumflex>
     |     <underscore>
     |     <vertical bar>
     |     <left brace>
     |     <right brace>

<space>    ::=   !! See the Syntax Rules.

<double quote>    ::=   "

<percent>    ::=   %

<ampersand>    ::=   &

<quote>    ::=   '

<left paren>    ::=   (

<right paren>    ::=   )

<asterisk>    ::=   *

<plus sign>    ::=   +

<comma>    ::=   ,

<minus sign>    ::=   -

<period>    ::=   .

<solidus>    ::=   /

<colon>    ::=   :

<semicolon>    ::=   ;

<less than operator>    ::=   <

<equals operator>    ::=   =

<greater than operator>    ::=   >

<question mark>    ::=   ?

The trigraphs are new in SQL-2003.

<left bracket or trigraph>    ::=   <left bracket> | <left bracket trigraph>

<right bracket or trigraph>    ::=   <right bracket> | <right bracket trigraph>

<left bracket>    ::=   [

<left bracket trigraph>    ::=   ??(

<right bracket>    ::=   ]

<right bracket trigraph>    ::=   ??)

<circumflex>    ::=   ^

<underscore>    ::=   _

<vertical bar>    ::=  /* Nothing */ |

<left brace>    ::=   {

<right brace>    ::=   }



5.2 <token> and <separator> (p134)

Specifying lexical units (tokens and separators) that participate in SQL language.

<token>    ::=   <nondelimiter token> | <delimiter token>

<nondelimiter token>    ::= 
         <regular identifier>
     |     <key word>
     |     <unsigned numeric literal>
     |     <national character string literal>
     |     <bit string literal>
     |     <hex string literal>
     |     <large object length token>
     |     <multiplier>

<regular identifier>    ::=   <identifier body>

<identifier body>    ::=   <identifier start> [ <identifier part> ... ]

<identifier part>    ::=   <identifier start> | <identifier extend>

Previous standard said: 

<identifier start>    ::=   <initial alphabetic character> | <ideographic character>

<identifier start>    ::=   !! See the Syntax Rules.

<identifier extend>    ::=   !! See the Syntax Rules.

<large object length token>    ::=   <digit> ... <multiplier>

<multiplier>    ::=   K | M | G

<delimited identifier>    ::=   <double quote> <delimited identifier body> <double quote>

<delimited identifier body>    ::=   <delimited identifier part> ...

<delimited identifier part>    ::=   <nondoublequote character> | <doublequote symbol>

The productions for <Unicode delimited identifier> and so on are new in SQL-2003.

<Unicode delimited identifier>    ::= 
         U <ampersand> <double quote> <Unicode delimiter body> <double quote>
         <Unicode escape specifier>

<Unicode escape specifier>    ::=   [ UESCAPE <quote> <Unicode escape character> <quote> ]

<Unicode delimiter body>    ::=   <Unicode identifier part> ...

<Unicode identifier part>    ::=   <delimited identifier part> | <Unicode escape value>

<Unicode escape value>    ::= 
         <Unicode 4 digit escape value>
     |     <Unicode 6 digit escape value>
     |     <Unicode character escape value>

Syntax rule 20: <Unicode 4 digit escape value>'<Unicode escape character>+xyzw' is equivalent to the Unicode code point specified by U+xyzw.

<Unicode 4 digit escape value>    ::=   <Unicode escape character> <hexit> <hexit> <hexit> <hexit>

Syntax rule 21: <Unicode 6 digit escape value>'<Unicode escape character>+xyzwrs' is equivalent to the Unicode code point specified by U+xyzwrs.

NOTE 64: The 6-hexit notation is derived by taking the UCS-4 notation defined by ISO/IEC 10646-1 and removing the leading two hexits, whose values are always 0 (zero).

<Unicode 6 digit escape value>    ::= 
         <Unicode escape character> <plus sign> <hexit> <hexit> <hexit> <hexit> <hexit> <hexit>

Syntax rule 22: <Unicode character escape value> is equivalent to a single instance of <Unicode escape character>.

<Unicode character escape value>    ::=   <Unicode escape character> <Unicode escape character>

Syntax rule 15: <Unicode escape character> shall be a single character from the source language character set other than a <hexit>, <plus sign>, or <white space>.

Syntax rule 16: If the source language character set contains <reverse solidus>, then let DEC be <reverse solidus>; otherwise, let DEC be an implementation-defined character from the source language character set that is not a <hexit>, <plus sign>, <double quote>, or <white space>.

Syntax rule 17: If a <Unicode escape specifier> does not contain <Unicode escape character>, then "UESCAPE <quote>DEC<quote>" is implicit.

Syntax rule 18: In a <Unicode escape value> there shall be no <separator> between the <Unicode escape character> and the first <hexit>, nor between any of the <hexit>s.

<Unicode escape character>    ::=   !! See the Syntax Rules (15-18 above).

Syntax rule 6: A <nondoublequote character> is any character of the source language character set other than a <double quote>.

<nondoublequote character>    ::=   !! See the Syntax Rules.

The rule for <doublequote symbol> in the standard uses two adjacent literal double quotes rather than referencing <double quote>; the reasons are not clear. It is annotated '!! two consecutive double quote characters'.

<doublequote symbol>    ::=   <double quote> <double quote>

<delimiter token>    ::= 
         <character string literal>
     |     <date string>
     |     <time string>
     |     <timestamp string>
     |     <interval string>
     |     <delimited identifier>
     |     <Unicode delimited identifier>
     |     <SQL special character>
     |     <not equals operator>
     |     <greater than or equals operator>
     |     <less than or equals operator>
     |     <concatenation operator>
     |     <right arrow>
     |     <left bracket trigraph>
     |     <right bracket trigraph>
     |     <double colon>
     |     <double period>

The rules for <not equals operator> etc in the standard uses two adjacent literal characters rather than referencing <less than> and <greater than>; the reasons are not clear. Note that two characters must be adjacent with no intervening space, not a pair of characters separated by arbitrary white space.

<not equals operator>    ::=   <less than operator> <greater than operator>

<greater than or equals operator>    ::=   <greater than operator> <equals operator>

<less than or equals operator>    ::=   <less than operator> <equals operator>

<concatenation operator>    ::=   <vertical bar> <vertical bar>

<right arrow>    ::=   <minus sign> <greater than operator>

<double colon>    ::=   <colon> <colon>

<double period>    ::=   <period> <period>

<separator>    ::=   { <comment> | <white space> }...

<comment>    ::=   <simple comment> | <bracketed comment>

<simple comment>    ::=   <simple comment introducer> [ <comment character> ... ] <newline>

<simple comment introducer>    ::=   <minus sign> <minus sign> [ <minus sign> ... ]

The <bracketed comment> rule included '!! See the Syntax Rules'. This probably says something about the <slash> <asterisk> and <asterisk> <slash> needing to be adjacent characters rather than adjacent tokens.

<bracketed comment>    ::= 
         <bracketed comment introducer> <bracketed comment contents> <bracketed comment terminator>

<bracketed comment introducer>    ::=   <slash> <asterisk>

<bracketed comment terminator>    ::=   <asterisk> <slash>

<bracketed comment contents>    ::=   [ { <comment character> | <separator> }... ]

<comment character>    ::=   <nonquote character> | <quote>

<newline>    ::=   !! See the Syntax Rules.

There was a surprising amount of movement of keywords between the reserved and non-reserved word classes between SQL-99 and SQL-2003-2 FCD and again between SQL 2003-2 FCD and SQL 2003-2 IS. There is also room to think that much of the host language support moved out of Part 2 (SQL/Foundation).

<key word>    ::=   <reserved word> | <non-reserved word>

<non-reserved word> ::=
A | ABSOLUTE | ACTION | ADA | ADD | ADMIN | AFTER | ALWAYS | ASC
| ASSERTION | ASSIGNMENT | ATTRIBUTE | ATTRIBUTES
| BEFORE | BERNOULLI | BREADTH
|
 C | CASCADE | CATALOG | CATALOG_NAME | CHAIN | CHARACTER_SET_CATALOG
|
 CHARACTER_SET_NAME | CHARACTER_SET_SCHEMA | CHARACTERISTICS | CHARACTERS
|
 CLASS_ORIGIN | COBOL | COLLATION | COLLATION_CATALOG | COLLATION_NAME | COLLATION_SCHEMA
|
 COLUMN_NAME | COMMAND_FUNCTION | COMMAND_FUNCTION_CODE | COMMITTED
|
 CONDITION_NUMBER | CONNECTION | CONNECTION_NAME | CONSTRAINT_CATALOG | CONSTRAINT_NAME
|
 CONSTRAINT_SCHEMA | CONSTRAINTS | CONSTRUCTOR | CONTAINS | CONTINUE | CURSOR_NAME
| DATA | DATETIME_INTERVAL_CODE | DATETIME_INTERVAL_PRECISION | DEFAULTS | DEFERRABLE
| DEFERRED | DEFINED | DEFINER | DEGREE | DEPTH | DERIVED | DESC | DESCRIPTOR
| DIAGNOSTICS | DISPATCH | DOMAIN | DYNAMIC_FUNCTION | DYNAMIC_FUNCTION_CODE
| EQUALS | EXCEPTION | EXCLUDE | EXCLUDING
| FINAL | FIRST | FOLLOWING | FORTRAN | FOUND
| G | GENERAL | GENERATED | GO | GOTO | GRANTED
| HIERARCHY
| IMMEDIATE | IMPLEMENTATION | INCLUDING | INCREMENT | INITIALLY | INPUT | INSTANCE
| INSTANTIABLE | INVOKER | ISOLATION
| K | KEY | KEY_MEMBER | KEY_TYPE
| LAST | LENGTH | LEVEL | LOCATOR
| M | MAP | MATCHED | MAXVALUE | MESSAGE_LENGTH | MESSAGE_OCTET_LENGTH
| MESSAGE_TEXT | MINVALUE | MORE | MUMPS
| NAME | NAMES | NESTING | NEXT | NORMALIZED | NULLABLE | NULLS | NUMBER
| OBJECT | OCTETS | OPTION | OPTIONS | ORDERING | ORDINALITY | OTHERS
| OUTPUT | OVERRIDING
|
 PAD | PARAMETER_MODE | PARAMETER_NAME | PARAMETER_ORDINAL_POSITION
|
 PARAMETER_SPECIFIC_CATALOG | PARAMETER_SPECIFIC_NAME | PARAMETER_SPECIFIC_SCHEMA
|
 PARTIAL | PASCAL | PATH | PLACING | PLI | PRECEDING | PRESERVE | PRIOR
|
 PRIVILEGES | PUBLIC
| READ | RELATIVE | REPEATABLE | RESTART | RESTRICT | RETURNED_CARDINALITY
| RETURNED_LENGTH | RETURNED_OCTET_LENGTH | RETURNED_SQLSTATE | ROLE
| ROUTINE | ROUTINE_CATALOG | ROUTINE_NAME | ROUTINE_SCHEMA | ROW_COUNT
|
 SCALE | SCHEMA | SCHEMA_NAME | SCOPE_CATALOG | SCOPE_NAME | SCOPE_SCHEMA
|
 SECTION | SECURITY | SELF | SEQUENCE | SERIALIZABLE | SERVER_NAME | SESSION
|
 SETS | SIMPLE | SIZE | SOURCE | SPACE | SPECIFIC_NAME | STATE | STATEMENT
|
 STRUCTURE | STYLE | SUBCLASS_ORIGIN
| TABLE_NAME | TEMPORARY | TIES | TOP_LEVEL_COUNT | TRANSACTION
| TRANSACTION_ACTIVE | TRANSACTIONS_COMMITTED | TRANSACTIONS_ROLLED_BACK
| TRANSFORM | TRANSFORMS | TRIGGER_CATALOG | TRIGGER_NAME | TRIGGER_SCHEMA | TYPE
| UNBOUNDED | UNCOMMITTED | UNDER | UNNAMED | USAGE | USER_DEFINED_TYPE_CATALOG
| USER_DEFINED_TYPE_CODE | USER_DEFINED_TYPE_NAME | USER_DEFINED_TYPE_SCHEMA
| VIEW
| WORK | WRITE
| ZONE
<reserved word> ::=
ABS | ALL | ALLOCATE | ALTER | AND | ANY | ARE | ARRAY | AS | ASENSITIVE
| ASYMMETRIC | AT | ATOMIC | AUTHORIZATION | AVG
| BEGIN | BETWEEN | BIGINT | BINARY | BLOB | BOOLEAN | BOTH | BY
|
 CALL | CALLED | CARDINALITY | CASCADED | CASE | CAST | CEIL | CEILING
|
 CHAR | CHAR_LENGTH | CHARACTER | CHARACTER_LENGTH | CHECK | CLOB | CLOSE
|
 COALESCE | COLLATE | COLLECT | COLUMN | COMMIT | CONDITION | CONNECT
|
 CONSTRAINT | CONVERT | CORR | CORRESPONDING | COUNT | COVAR_POP | COVAR_SAMP
|
 CREATE | CROSS | CUBE | CUME_DIST | CURRENT | CURRENT_DATE
|
 CURRENT_DEFAULT_TRANSFORM_GROUP | CURRENT_PATH | CURRENT_ROLE | CURRENT_TIME
|
 CURRENT_TIMESTAMP | CURRENT_TRANSFORM_GROUP_FOR_TYPE | CURRENT_USER
|
 CURSOR | CYCLE
|DATE | DAY | DEALLOCATE | DEC | DECIMAL | DECLARE | DEFAULT | DELETE
DENSE_RANK | DEREF | DESCRIBE | DETERMINISTIC | DISCONNECT | DISTINCT
DOUBLE | DROP | DYNAMIC
EACH | ELEMENT | ELSE | END | END-EXEC | ESCAPE | EVERY | EXCEPT | EXEC
EXECUTE | EXISTS | EXP | EXTERNAL | EXTRACT
FALSE | FETCH | FILTER | FLOAT | FLOOR | FOR | FOREIGN | FREE | FROM
FULL | FUNCTION | FUSION
GET | GLOBAL | GRANT | GROUP | GROUPING
HAVING | HOLD | HOUR
IDENTITY | IN | INDICATOR | INNER | INOUT | INSENSITIVE | INSERT
INT | INTEGER | INTERSECT | INTERSECTION | INTERVAL | INTO | IS
JOIN
LANGUAGE | LARGE | LATERAL | LEADING | LEFT | LIKE | LN | LOCAL
LOCALTIME | LOCALTIMESTAMP | LOWER
MATCH | MAX | MEMBER | MERGE | METHOD | MIN | MINUTE
MOD | MODIFIES | MODULE | MONTH | MULTISET
NATIONAL | NATURAL | NCHAR | NCLOB | NEW | NO | NONE | NORMALIZE | NOT
NULL | NULLIF | NUMERIC
OCTET_LENGTH | OF | OLD | ON | ONLY | OPEN | OR | ORDER | OUT | OUTER
OVER | OVERLAPS | OVERLAY
PARAMETER | PARTITION | PERCENT_RANK | PERCENTILE_CONT | PERCENTILE_DISC
POSITION | POWER | PRECISION | PREPARE | PRIMARY | PROCEDURE
RANGE | RANK | READS | REAL | RECURSIVE | REF | REFERENCES | REFERENCING
REGR_AVGX | REGR_AVGY | REGR_COUNT | REGR_INTERCEPT | REGR_R2 | REGR_SLOPE
REGR_SXX | REGR_SXY | REGR_SYY | RELEASE | RESULT | RETURN | RETURNS
REVOKE | RIGHT | ROLLBACK | ROLLUP | ROW | ROW_NUMBER | ROWS
SAVEPOINT | SCOPE | SCROLL | SEARCH | SECOND | SELECT | SENSITIVE
SESSION_USER | SET | SIMILAR | SMALLINT | SOME | SPECIFIC | SPECIFICTYPE
SQL | SQLEXCEPTION | SQLSTATE | SQLWARNING | SQRT | START | STATIC
STDDEV_POP | STDDEV_SAMP | SUBMULTISET | SUBSTRING | SUM | SYMMETRIC
SYSTEM | SYSTEM_USER
TABLE | TABLESAMPLE | THEN | TIME | TIMESTAMP | TIMEZONE_HOUR | TIMEZONE_MINUTE
TO | TRAILING | TRANSLATE | TRANSLATION | TREAT | TRIGGER | TRIM | TRUE
UESCAPE | UNION | UNIQUE | UNKNOWN | UNNEST | UPDATE
 | UPPER | USER | USING
VALUE | VALUES | VAR_POP | VAR_SAMP | VARCHAR | VARYING
WHEN | WHENEVER | WHERE | WIDTH_BUCKET | WINDOW | WITH | WITHIN | WITHOUT
YEAR



= 5.3 <literal> (p143)

<literal>    ::=   <signed numeric literal> | <general literal>

<unsigned literal>    ::=   <unsigned numeric literal> | <general literal>

<general literal>    ::= 
         <character string literal>
     |     <national character string literal>
     |     <Unicode character string literal>
     |     <binary string literal>
     |     <datetime literal>
     |     <interval literal>
     |     <boolean literal>

== Character string literals

<character string literal>    ::= 
         [ <introducer> <character set specification> ]
         <quote> [ <character representation> ... ] <quote>
         [ { <separator> <quote> [ <character representation> ... ] <quote> }... ]

<introducer>    ::=   <underscore>

<character representation>    ::=   <nonquote character> | <quote symbol>

<nonquote character>    ::=   !! See the Syntax Rules.

The <quote symbol> rule consists of two immediately adjacent <quote> marks with no spaces. As usual, this would be best handled in the lexical analyzer, not in the grammar.

<quote symbol>    ::=   <quote> <quote>

> stringLiterals :: TestItem
> stringLiterals = Group "string literals" $ map (uncurry TestValueExpr)
>     [("'a regular string literal'", undefined)
>     ,("'something' ' some more' 'and more'", undefined)
>     ,("'something' \n ' some more' \t 'and more'", undefined)
>     ,("'something' -- a comment\n ' some more' /*another comment*/ 'and more'", undefined)
>     ,("'a quote: '', stuff'", undefined)
>     ]

TODO: all the stuff with character set representations.

== other string literals

<national character string literal>    ::= 
         N <quote> [ <character representation> ... ] <quote>
         [ { <separator> <quote> [ <character representation> ... ] <quote> }... ]

> nationalCharacterStringLiterals :: TestItem
> nationalCharacterStringLiterals = Group "national character string literals" $ map (uncurry TestValueExpr)
>     [("N'something'", undefined)
>     ,("n'something'", undefined)
>     ]

<Unicode character string literal>    ::= 
         [ <introducer> <character set specification> ]
         U <ampersand> <quote> [ <Unicode representation> ... ] <quote>
         [ { <separator> <quote> [ <Unicode representation> ... ] <quote> }... ]
         [ ESCAPE <escape character> ]

<Unicode representation>    ::=   <character representation> | <Unicode escape value>

> unicodeStringLiterals :: TestItem
> unicodeStringLiterals = Group "national character string literals" $ map (uncurry TestValueExpr)
>     [("U&'something'", undefined)
>     ,("u&'something'", undefined)
>     ]

TODO: put in some unicode and some unicode escape values plus work out
the ESCAPE thing. I think this is to change the unicode escape value
starting character.

== other string literals

<binary string literal>    ::= 
         X <quote> [ { <hexit> <hexit> }... ] <quote>
         [ { <separator> <quote> [ { <hexit> <hexit> }... ] <quote> }... ]
         [ ESCAPE <escape character> ]

<hexit>    ::=   <digit> | A | B | C | D | E | F | a | b | c | d | e | f

Did binary strings get dropped? Maybe should review all the older
standards to include everything that was dropped also?

TODO: how to escapes work here?

> bitBinaryStringLiterals :: TestItem
> bitBinaryStringLiterals = Group "bit and hex string literals" $ map (uncurry TestValueExpr)
>     [("B'101010'", undefined)
>     ,("X'7f7f7f'", undefined)
>     ]

TODO: separator stuff for all the string literals?

== numeric literals

<signed numeric literal>    ::=   [ <sign> ] <unsigned numeric literal>

<unsigned numeric literal>    ::=   <exact numeric literal> | <approximate numeric literal>

<exact numeric literal>    ::= 
         <unsigned integer> [ <period> [ <unsigned integer> ] ]
     |     <period> <unsigned integer>

<sign>    ::=   <plus sign> | <minus sign>

<approximate numeric literal>    ::=   <mantissa> E <exponent>

<mantissa>    ::=   <exact numeric literal>

<exponent>    ::=   <signed integer>

<signed integer>    ::=   [ <sign> ] <unsigned integer>

> numericLiterals :: TestItem
> numericLiterals = Group "numeric literals" $ map (uncurry TestValueExpr)
>     [("11", undefined)
>     ,("11.11", undefined)

>     ,("11E23", undefined)
>     ,("11E+23", undefined)
>     ,("11E-23", undefined)

>     ,("11.11E23", undefined)
>     ,("11.11E+23", undefined)
>     ,("11.11E-23", undefined)

>     ,("+11E23", undefined)
>     ,("+11E+23", undefined)
>     ,("+11E-23", undefined)
>     ,("+11.11E23", undefined)
>     ,("+11.11E+23", undefined)
>     ,("+11.11E-23", undefined)

>     ,("-11E23", undefined)
>     ,("-11E+23", undefined)
>     ,("-11E-23", undefined)
>     ,("-11.11E23", undefined)
>     ,("-11.11E+23", undefined)
>     ,("-11.11E-23", undefined)

>     ,("11.11e23", undefined)

>     ]

== date and time literals

<datetime literal>    ::=   <date literal> | <time literal> | <timestamp literal>

<date literal>    ::=   DATE <date string>

<time literal>    ::=   TIME <time string>

<timestamp literal>    ::=   TIMESTAMP <timestamp string>

<date string>    ::=   <quote> <unquoted date string> <quote>

<time string>    ::=   <quote> <unquoted time string> <quote>

<timestamp string>    ::=   <quote> <unquoted timestamp string> <quote>

<time zone interval>    ::=   <sign> <hours value> <colon> <minutes value>

<date value>    ::=   <years value> <minus sign> <months value> <minus sign> <days value>

<time value>    ::=   <hours value> <colon> <minutes value> <colon> <seconds value>

<interval literal>    ::=   INTERVAL [ <sign> ] <interval string> <interval qualifier>

<interval string>    ::=   <quote> <unquoted interval string> <quote>

<unquoted date string>    ::=   <date value>

<unquoted time string>    ::=   <time value> [ <time zone interval> ]

<unquoted timestamp string>    ::=   <unquoted date string> <space> <unquoted time string>

<unquoted interval string>    ::=   [ <sign> ] { <year-month literal> | <day-time literal> }

<year-month literal>    ::=   <years value> | [ <years value> <minus sign> ] <months value>

<day-time literal>    ::=   <day-time interval> | <time interval>

<day-time interval>    ::= 
         <days value> [ <space> <hours value> [ <colon> <minutes value> [ <colon> <seconds value> ] ] ]

<time interval>    ::= 
         <hours value> [ <colon> <minutes value> [ <colon> <seconds value> ] ]
     |     <minutes value> [ <colon> <seconds value> ]
     |     <seconds value>

<years value>    ::=   <datetime value>

<months value>    ::=   <datetime value>

<days value>    ::=   <datetime value>

<hours value>    ::=   <datetime value>

<minutes value>    ::=   <datetime value>

<seconds value>    ::=   <seconds integer value> [ <period> [ <seconds fraction> ] ]

<seconds integer value>    ::=   <unsigned integer>

<seconds fraction>    ::=   <unsigned integer>

<datetime value>    ::=   <unsigned integer>

All these date literals are just like a restricted version of string
literals. This parser doesn't check the format inside these literals
at this time.

> dateAndTimeLiterals :: TestItem
> dateAndTimeLiterals = Group "date and time literals" $ map (uncurry TestValueExpr)
>     [("date 'date literal", undefined)
>     ,("time 'time literal", undefined)
>     ,("timestamp 'timestamp literal'", undefined)
>     ]

TODO: intervals + more date and time literals

== boolean literals

<boolean literal>    ::=   TRUE | FALSE | UNKNOWN

> booleanLiterals :: TestItem
> booleanLiterals = Group "boolean literals" $ map (uncurry TestValueExpr)
>     [("true", undefined)
>     ,("false", undefined)
>     ,("unknown", undefined)
>     ]

so is unknown effectively an alias for null with a cast or is it
something different?


= 5.4 Names and identifiers (p151)

<identifier>    ::=   <actual identifier>

<actual identifier>    ::=   <regular identifier> | <delimited identifier>

<SQL language identifier>    ::= 
         <SQL language identifier start> [ { <underscore> | <SQL language identifier part> }... ]

<SQL language identifier start>    ::=   <simple Latin letter>

<SQL language identifier part>    ::=   <simple Latin letter> | <digit>

TODO: language identifiers have different rules to generic identifiers

<authorization identifier>    ::=   <role name> | <user identifier>

<table name>    ::=   <local or schema qualified name>

<domain name>    ::=   <schema qualified name>

<schema name>    ::=   [ <catalog name> <period> ] <unqualified schema name>

<catalog name>    ::=   <identifier>

<schema qualified name>    ::=   [ <schema name> <period> ] <qualified identifier>

<local or schema qualified name>    ::=   [ <local or schema qualifier> <period> ] <qualified identifier>

<local or schema qualifier>    ::=   <schema name> | MODULE

<qualified identifier>    ::=   <identifier>

<column name>    ::=   <identifier>

<correlation name>    ::=   <identifier>

<query name>    ::=   <identifier>

<SQL-client module name>    ::=   <identifier>

<procedure name>    ::=   <identifier>

<schema qualified routine name>    ::=   <schema qualified name>

<method name>    ::=   <identifier>

<specific name>    ::=   <schema qualified name>

<cursor name>    ::=   <local qualified name>

<local qualified name>    ::=   [ <local qualifier> <period> ] <qualified identifier>

<local qualifier>    ::=   MODULE

<host parameter name>    ::=   <colon> <identifier>

<SQL parameter name>    ::=   <identifier>

<constraint name>    ::=   <schema qualified name>

<external routine name>    ::=   <identifier> | <character string literal>

<trigger name>    ::=   <schema qualified name>

<collation name>    ::=   <schema qualified name>

<character set name>    ::=   [ <schema name> <period> ] <SQL language identifier>

<transliteration name>    ::=   <schema qualified name>

<transcoding name>    ::=   <schema qualified name>

<user-defined type name>    ::=   <schema qualified type name>

<schema-resolved user-defined type name>    ::=   <user-defined type name>

<schema qualified type name>    ::=   [ <schema name> <period> ] <qualified identifier>

<attribute name>    ::=   <identifier>

<field name>    ::=   <identifier>

<savepoint name>    ::=   <identifier>

<sequence generator name>    ::=   <schema qualified name>

<role name>    ::=   <identifier>

<user identifier>    ::=   <identifier>

<connection name>    ::=   <simple value specification>

<SQL-server name>    ::=   <simple value specification>

<connection user name>    ::=   <simple value specification>

<SQL statement name>    ::=   <statement name> | <extended statement name>

<statement name>    ::=   <identifier>

<extended statement name>    ::=   [ <scope option> ] <simple value specification>

<dynamic cursor name>    ::=   <cursor name> | <extended cursor name>

<extended cursor name>    ::=   [ <scope option> ] <simple value specification>

<descriptor name>    ::=   [ <scope option> ] <simple value specification>

<scope option>    ::=   GLOBAL | LOCAL

<window name>    ::=   <identifier>

> identifiers :: TestItem
> identifiers = Group "identifiers" $ map (uncurry TestValueExpr)
>     [("test",undefined)
>     ,("_test",undefined)
>     ,("t1",undefined)
>     ,("a.b",undefined)
>     ,("a.b.c",undefined)
>     ]

TODO: module stuff



= 6 Scalar expressions

== 6.1 <data type> (p161)

<data type>    ::= 
         <predefined type>
     |     <row type>
     |     <path-resolved user-defined type name>
     |     <reference type>
     |     <collection type>

<predefined type>    ::= 
         <character string type> [ CHARACTER SET <character set specification> ] [ <collate clause> ]
     |     <national character string type> [ <collate clause> ]
     |     <binary large object string type>
     |     <numeric type>
     |     <boolean type>
     |     <datetime type>
     |     <interval type>

<character string type>    ::= 
         CHARACTER [ <left paren> <length> <right paren> ]
     |     CHAR [ <left paren> <length> <right paren> ]
     |     CHARACTER VARYING <left paren> <length> <right paren>
     |     CHAR VARYING <left paren> <length> <right paren>
     |     VARCHAR <left paren> <length> <right paren>
     |     CHARACTER LARGE OBJECT [ <left paren> <large object length> <right paren> ]
     |     CHAR LARGE OBJECT [ <left paren> <large object length> <right paren> ]
     |     CLOB [ <left paren> <large object length> <right paren> ]

<national character string type>    ::= 
         NATIONAL CHARACTER [ <left paren> <length> <right paren> ]
     |     NATIONAL CHAR [ <left paren> <length> <right paren> ]
     |     NCHAR [ <left paren> <length> <right paren> ]
     |     NATIONAL CHARACTER VARYING <left paren> <length> <right paren>
     |     NATIONAL CHAR VARYING <left paren> <length> <right paren>
     |     NCHAR VARYING <left paren> <length> <right paren>
     |     NATIONAL CHARACTER LARGE OBJECT [ <left paren> <large object length> <right paren> ]
     |     NCHAR LARGE OBJECT [ <left paren> <large object length> <right paren> ]
     |     NCLOB [ <left paren> <large object length> <right paren> ]

<binary large object string type>    ::= 
         BINARY LARGE OBJECT [ <left paren> <large object length> <right paren> ]
     |     BLOB [ <left paren> <large object length> <right paren> ]

<numeric type>    ::=   <exact numeric type> | <approximate numeric type>

<exact numeric type>    ::= 
         NUMERIC [ <left paren> <precision> [ <comma> <scale> ] <right paren> ]
     |     DECIMAL [ <left paren> <precision> [ <comma> <scale> ] <right paren> ]
     |     DEC [ <left paren> <precision> [ <comma> <scale> ] <right paren> ]
     |     SMALLINT 
     |     INTEGER 
     |     INT 
     |     BIGINT

<approximate numeric type>    ::= 
         FLOAT [ <left paren> <precision> <right paren> ]
     |     REAL 
     |     DOUBLE PRECISION

<length>    ::=   <unsigned integer>

<large object length>    ::= 
         <unsigned integer> [ <multiplier> ] [ <char length units> ]
     |     <large object length token> [ <char length units> ]

<char length units>    ::=   CHARACTERS | CODE_UNITS | OCTETS

<precision>    ::=   <unsigned integer>

<scale>    ::=   <unsigned integer>

<boolean type>    ::=   BOOLEAN

<datetime type>    ::= 
         DATE 
     |     TIME [ <left paren> <time precision> <right paren> ] [ <with or without time zone> ]
     |     TIMESTAMP [ <left paren> <timestamp precision> <right paren> ] [ <with or without time zone> ]

<with or without time zone>    ::=   WITH TIME ZONE | WITHOUT TIME ZONE

<time precision>    ::=   <time fractional seconds precision>

<timestamp precision>    ::=   <time fractional seconds precision>

<time fractional seconds precision>    ::=   <unsigned integer>

<interval type>    ::=   INTERVAL <interval qualifier>

<row type>    ::=   ROW <row type body>

<row type body>    ::=   <left paren> <field definition> [ { <comma> <field definition> }... ] <right paren>

<reference type>    ::=   REF <left paren> <referenced type> <right paren> [ <scope clause> ]

<scope clause>    ::=   SCOPE <table name>

<referenced type>    ::=   <path-resolved user-defined type name>

<path-resolved user-defined type name>    ::=   <user-defined type name>

<path-resolved user-defined type name>    ::=   <user-defined type name>

<collection type>    ::=   <array type> | <multiset type>

<array type>    ::=   <data type> ARRAY [ <left bracket or trigraph> <unsigned integer> <right bracket or trigraph> ]

<multiset type>    ::=   <data type> MULTISET

TODO: review this list better and fill in lots of missing bits:

> typeNames :: TestItem
> typeNames = Group "type names" $ map (uncurry TestValueExpr)
>     [("cast('test' as character(5))", undefined)
>     ,("cast('test' as character)", undefined)
>     ,("cast('test' as char(5))", undefined)
>     ,("cast('test' as character varying(5))", undefined)
>     ,("cast('test' as char varying(5))", undefined)
>     ,("cast('test' as varchar(5))", undefined)
>     ,("cast('test' as char(5) character set _xxx)", undefined)

>     ,("cast('test' as national character(5))", undefined)
>     ,("cast('test' as national char)", undefined)
>     ,("cast('test' as nchar)", undefined)
>     ,("cast('test' as national character varying)", undefined)
>     ,("cast('test' as national char varying)", undefined)
>     ,("cast('test' as nchar varying)", undefined)

>     ,("cast('test' as bit(4))", undefined)
>     ,("cast('test' as bit varying(4))", undefined)
>     ,("cast('test' as bit varying)", undefined)

>     ,("cast(5 as numeric)", undefined)
>     ,("cast(5 as numeric(3))", undefined)
>     ,("cast(5 as numeric(5,3))", undefined)
>     ,("cast(5 as decimal(5,3))", undefined)
>     ,("cast(5 as dec(5,3))", undefined)
>     ,("cast(5 as integer)", undefined)
>     ,("cast(5 as int)", undefined)
>     ,("cast(5 as smallint)", undefined)
>     ,("cast(5 as float(23))", undefined)
>     ,("cast(5 as float)", undefined)
>     ,("cast(5 as real)", undefined)
>     ,("cast(5 as double precision)", undefined)

>     ,("cast('01-01-99' as date)", undefined)
>     ,("cast('01-01-99' as time)", undefined)
>     ,("cast('01-01-99' as time(3))", undefined)
>     ,("cast('01-01-99' as timestamp(3))", undefined)
>     ,("cast('01-01-99' as timestamp with time zone)", undefined)
>     ,("cast('01-01-99' as time(3) with time zone)", undefined)
>     ]




== 6.2 <field definition> (p173)

<field definition>    ::=   <field name> <data type> [ <reference scope check> ]

This is used when e.g. casting to a row type. TODO



== 6.3 <value expression primary> (p174)

<value expression primary>    ::= 
         <parenthesized value expression>
     |     <nonparenthesized value expression primary>

<parenthesized value expression>    ::=   <left paren> <value expression> <right paren>

> parenthesizedValueExpression :: TestItem
> parenthesizedValueExpression = Group "parenthesized value expression" $ map (uncurry TestValueExpr)
>     [("(3)", undefined)
>     ,("((3))", undefined)
>     ]

<nonparenthesized value expression primary>    ::= 
         <unsigned value specification>
     |     <column reference>
     |     <set function specification>
     |     <window function>
     |     <scalar subquery>
     |     <case expression>
     |     <cast specification>
     |     <field reference>
     |     <subtype treatment>
     |     <method invocation>
     |     <static method invocation>
     |     <new specification>
     |     <attribute or method reference>
     |     <reference resolution>
     |     <collection value constructor>
     |     <array element reference>
     |     <multiset element reference>
     |     <routine invocation>
     |     <next value expression>



== 6.4 <value specification> and <target specification> (p176)

<value specification>    ::=   <literal> | <general value specification>

<unsigned value specification>    ::=   <unsigned literal> | <general value specification>

<general value specification>    ::= 
         <host parameter specification>
     |     <SQL parameter reference>
     |     <dynamic parameter specification>
     |     <embedded variable specification>
     |     <current collation specification>
     |     CURRENT_DEFAULT_TRANSFORM_GROUP 
     |     CURRENT_PATH 
     |     CURRENT_ROLE 
     |     CURRENT_TRANSFORM_GROUP_FOR_TYPE <path-resolved user-defined type name>
     |     CURRENT_USER 
     |     SESSION_USER 
     |     SYSTEM_USER 
     |     USER 
     |     VALUE

TODO: review how the special keywords are parsed and add tests for these

<simple value specification>    ::= 
         <literal>
     |     <host parameter name>
     |     <SQL parameter reference>
     |     <embedded variable name>

<target specification>    ::= 
         <host parameter specification>
     |     <SQL parameter reference>
     |     <column reference>
     |     <target array element specification>
     |     <dynamic parameter specification>
     |     <embedded variable specification>

<simple target specification>    ::= 
         <host parameter specification>
     |     <SQL parameter reference>
     |     <column reference>
     |     <embedded variable name>

<host parameter specification>    ::=   <host parameter name> [ <indicator parameter> ]

<dynamic parameter specification>    ::=   <question mark>

<embedded variable specification>    ::=   <embedded variable name> [ <indicator variable> ]

<indicator variable>    ::=   [ INDICATOR ] <embedded variable name>

<indicator parameter>    ::=   [ INDICATOR ] <host parameter name>

<target array element specification>    ::= 
         <target array reference> <left bracket or trigraph> <simple value specification> <right bracket or trigraph>

<target array reference>    ::=   <SQL parameter reference> | <column reference>

<current collation specification>    ::=   CURRENT_COLLATION <left paren> <string value expression> <right paren>

> targetSpecification :: TestItem
> targetSpecification = Group "target specification" $ map (uncurry TestValueExpr)
>     [(":hostparam", undefined)
>     ,("?", undefined)
>     ,(":h[3]", undefined)
>     ]

TODO: modules stuff, indicators, not sure what current_collation is
for or how it works




== 6.5 <contextually typed value specification> (p181)

<contextually typed value specification>    ::= 
         <implicitly typed value specification> | <default specification>

<implicitly typed value specification>    ::=   <null specification> | <empty specification>

<null specification>    ::=   NULL

<empty specification>    ::= 
         ARRAY <left bracket or trigraph> <right bracket or trigraph>
     |     MULTISET <left bracket or trigraph> <right bracket or trigraph>

<default specification>    ::=   DEFAULT

> contextuallyTypeValueSpec :: TestItem
> contextuallyTypeValueSpec = Group "ontextually typed value specification" $ map (uncurry TestValueExpr)
>     [("null", undefined)
>     ,("array[]", undefined)
>     ,("multiset[]", undefined)
>     ,("default", undefined)
>     ]

todo: trigraphs?



== 6.6 <identifier chain> (p183)

<identifier chain>    ::=   <identifier> [ { <period> <identifier> }... ]

<basic identifier chain>    ::=   <identifier chain>

already covered above in the identifiers and names section



== 6.7 <column reference> (p187)

<column reference>    ::= 
         <basic identifier chain>
     |     MODULE <period> <qualified identifier> <period> <column name>

TODO: work out the exact syntax and add



== 6.8 <SQL parameter reference> (p190)

<SQL parameter reference>    ::=   <basic identifier chain>

already covered above. Why on earth do they introduce so many aliases?
How does this do anything apart from obfuscate everything?



== 6.9 <set function specification> (p191)

<set function specification>    ::=   <aggregate function> | <grouping operation>

<grouping operation>    ::=   GROUPING <left paren> <column reference> [ { <comma> <column reference> }... ] <right paren>

grouping is used to identify lines in a 'superaggregate' row

example:

SELECT 
   DECODE(GROUPING(department_name), 1, 'All Departments', department_name) AS department,
   DECODE(GROUPING(job_id), 1, 'All Jobs', job_id) AS job,
   COUNT(*) "Total Empl", 
   AVG(salary) * 12 "Average Sal"
FROM employees e, departments d
WHERE d.department_id = e.department_id
GROUP BY ROLLUP (department_name, job_id)
ORDER BY department, job, "Total Empl", "Average Sal";

TODO: de-oracle the syntax and add as test case



== 6.10 <window function> (p193)

<window function>    ::=   <window function type> OVER <window name or specification>

<window function type>    ::= 
         <rank function type> <left paren> <right paren>
     |     ROW_NUMBER <left paren> <right paren>
     |     <aggregate function>

<rank function type>    ::=   RANK | DENSE_RANK | PERCENT_RANK | CUME_DIST

<window name or specification>    ::=   <window name> | <in-line window specification>

<in-line window specification>    ::=   <window specification>

TODO: window functions



== 6.11 <case expression> (p197)

<case expression>    ::=   <case abbreviation> | <case specification>

<case abbreviation>    ::= 
         NULLIF <left paren> <value expression> <comma> <value expression> <right paren>
     |     COALESCE <left paren> <value expression> { <comma> <value expression> }... <right paren>

<case specification>    ::=   <simple case> | <searched case>

<simple case>    ::=   CASE <case operand> <simple when clause> ... [ <else clause> ] END

<searched case>    ::=   CASE <searched when clause> ... [ <else clause> ] END

<simple when clause>    ::=   WHEN <when operand> THEN <result>

<searched when clause>    ::=   WHEN <search condition> THEN <result>

<else clause>    ::=   ELSE <result>

<case operand>    ::=   <row value predicand> | <overlaps predicate part>

<when operand>    ::= 
         <row value predicand>
     |     <comparison predicate part 2>
     |     <between predicate part 2>
     |     <in predicate part 2>
     |     <character like predicate part 2>
     |     <octet like predicate part 2>
     |     <similar predicate part 2>
     |     <null predicate part 2>
     |     <quantified comparison predicate part 2>
     |     <match predicate part 2>
     |     <overlaps predicate part 2>
     |     <distinct predicate part 2>
     |     <member predicate part 2>
     |     <submultiset predicate part 2>
     |     <set predicate part 2>
     |     <type predicate part 2>

<result>    ::=   <result expression> | NULL

<result expression>    ::=   <value expression>

TODO: case expressions plus the 'abbreviations'



== 6.12 <cast specification> (p200)

<cast specification>    ::=   CAST <left paren> <cast operand> AS <cast target> <right paren>

<cast operand>    ::=   <value expression> | <implicitly typed value specification>

<cast target>    ::=   <domain name> | <data type>

This is already covered above



== 6.13 <next value expression> (p216)

<next value expression>    ::=   NEXT VALUE FOR <sequence generator name>

> nextValueExpression :: TestItem
> nextValueExpression = Group "next value expression" $ map (uncurry TestValueExpr)
>     [("next value for a.b", undefined)
>     ]



== 6.14 <field reference> (p218)

<field reference>    ::=   <value expression primary> <period> <field name>

already covered



== 6.15 <subtype treatment> (p219)

<subtype treatment>    ::= 
         TREAT <left paren> <subtype operand> AS <target subtype> <right paren>

<subtype operand>    ::=   <value expression>

<target subtype>    ::= 
         <path-resolved user-defined type name>
     |     <reference type>

TODO: subtype treatments



== 6.16 <method invocation> (p221)

<method invocation>    ::=   <direct invocation> | <generalized invocation>

<direct invocation>    ::= 
         <value expression primary> <period> <method name> [ <SQL argument list> ]

<generalized invocation>    ::= 
         <left paren> <value expression primary> AS <data type> <right paren> <period> <method name>
         [ <SQL argument list> ]

<method selection>    ::=   <routine invocation>

<constructor method selection>    ::=   <routine invocation>

TODO: method invocation



== 6.17 <static method invocation> (p223)

<static method invocation>    ::= 
         <path-resolved user-defined type name> <double colon> <method name> [ <SQL argument list> ]

<static method selection>    ::=   <routine invocation>

TODO: static method invocation



6.18 <new specification> (p225)

<new specification>    ::=   NEW <routine invocation>

<new invocation>    ::=   <method invocation> | <routine invocation>

TODO: new specification

6.19 <attribute or method reference> (p227)

<attribute or method reference>    ::= 
         <value expression primary> <dereference operator> <qualified identifier>
         [ <SQL argument list> ]

<dereference operator>    ::=   <right arrow>

TODO: attribute or method reference

6.20 <dereference operation> (p229)

<dereference operation>    ::=   <reference value expression> <dereference operator> <attribute name>

TODO: dereference operation

6.21 <method reference> (p230)

<method reference>    ::= 
         <value expression primary> <dereference operator> <method name> <SQL argument list>

TODO: method reference

6.22 <reference resolution> (p232)

<reference resolution>    ::=   DEREF <left paren> <reference value expression> <right paren>

TODO: reference resolution

6.23 <array element reference> (p234)

<array element reference>    ::= 
         <array value expression> <left bracket or trigraph> <numeric value expression> <right bracket or trigraph>

> arrayElementReference :: TestItem
> arrayElementReference = Group "array element reference" $ map (uncurry TestValueExpr)
>     [("something[3]", undefined)
>     ,("(something(a))[x][y] ", undefined)
>     ]

TODO: work out the precendence of the array element reference suffix

6.24 <multiset element reference> (p235)

<multiset element reference>    ::= 
         ELEMENT <left paren> <multset value expression> <right paren>

> multisetElementReference :: TestItem
> multisetElementReference = Group "multiset element reference" $ map (uncurry TestValueExpr)
>     [("element(something)", undefined)
>     ]

TODO: work out how to parse this

6.25 <value expression> (p236)

Specify a value.

<value expression>    ::= 
         <common value expression>
     |     <boolean value expression>
     |     <row value expression>

<common value expression>    ::= 
         <numeric value expression>
     |     <string value expression>
     |     <datetime value expression>
     |     <interval value expression>
     |     <user-defined type value expression>
     |     <reference value expression>
     |     <collection value expression>

<user-defined type value expression>    ::=   <value expression primary>

<reference value expression>    ::=   <value expression primary>

<collection value expression>    ::=   <array value expression> | <multiset value expression>

<collection value constructor>    ::=   <array value constructor> | <multiset value constructor>

== 6.26 <numeric value expression> (p240)

Specify a numeric value.

<numeric value expression>    ::= 
         <term>
     |     <numeric value expression> <plus sign> <term>
     |     <numeric value expression> <minus sign> <term>

<term>    ::= 
         <factor>
     |     <term> <asterisk> <factor>
     |     <term> <solidus> <factor>

<factor>    ::=   [ <sign> ] <numeric primary>

<numeric primary>    ::= 
         <value expression primary>
     |     <numeric value function>


> numericValueExpression :: TestItem
> numericValueExpression = Group "numeric value expression" $ map (uncurry TestValueExpr)
>     [("a + b", undefined)
>     ,("a - b", undefined)
>     ,("a * b", undefined)
>     ,("a / b", undefined)
>     ,("+a", undefined)
>     ,("-a", undefined)
>     ]

== 6.27 <numeric value function> (p242)

Specify a function yielding a value of type numeric.

<numeric value function>    ::= 
         <position expression>
     |     <extract expression>
     |     <length expression>
     |     <cardinality expression>
     |     <absolute value expression>
     |     <modulus expression>
     |     <natural logarithm>
     |     <exponential function>
     |     <power function>
     |     <square root>
     |     <floor function>
     |     <ceiling function>
     |     <width bucket function>

<position expression>    ::= 
         <string position expression>
     |     <blob position expression>

<string position expression>    ::= 
         POSITION <left paren> <string value expression> IN <string value expression> [ USING <char length units> ] <right paren>

<blob position expression>    ::= 
         POSITION <left paren> <blob value expression> IN <blob value expression> <right paren>

TODO: position expressions

<length expression>    ::= 
         <char length expression>
     |     <octet length expression>

<char length expression>    ::= 
         { CHAR_LENGTH | CHARACTER_LENGTH } <left paren> <string value expression> [ USING <char length units> ] <right paren>

<octet length expression>    ::=   OCTET_LENGTH <left paren> <string value expression> <right paren>

TODO: length expression

<extract expression>    ::=   EXTRACT <left paren> <extract field> FROM <extract source> <right paren>

<extract field>    ::=   <primary datetime field> | <time zone field>

<time zone field>    ::=   TIMEZONE_HOUR | TIMEZONE_MINUTE

<extract source>    ::=   <datetime value expression> | <interval value expression>

TODO: extract expression

<cardinality expression>    ::=   CARDINALITY <left paren> <collection value expression> <right paren>

<absolute value expression>    ::=   ABS <left paren> <numeric value expression> <right paren>

<modulus expression>    ::=   MOD <left paren> <numeric value expression dividend> <comma> <numeric value expression divisor> <right paren>

<natural logarithm>    ::=   LN <left paren> <numeric value expression> <right paren>

<exponential function>    ::=   EXP <left paren> <numeric value expression> <right paren>

<power function>    ::=   POWER <left paren> <numeric value expression base> <comma> <numeric value expression exponent> <right paren>

<numeric value expression base>    ::=   <numeric value expression>

<numeric value expression exponent>    ::=   <numeric value expression>

<square root>    ::=   SQRT <left paren> <numeric value expression> <right paren>

<floor function>    ::=   FLOOR <left paren> <numeric value expression> <right paren>

<ceiling function>    ::=   { CEIL | CEILING } <left paren> <numeric value expression> <right paren>

<width bucket function>    ::=   WIDTH_BUCKET <left paren> <width bucket operand> <comma> <width bucket bound 1> <comma> <width bucket bound 2> <comma> <width bucket count> <right paren>

<width bucket operand>    ::=   <numeric value expression>

<width bucket bound 1>    ::=   <numeric value expression>

<width bucket bound 2>    ::=   <numeric value expression>

<width bucket count>    ::=   <numeric value expression>

TODO: lots more expressions above

== 6.28 <string value expression> (p251)

Specify a character string value or a binary string value.

<string value expression>    ::=   <character value expression> | <blob value expression>

<character value expression>    ::=   <concatenation> | <character factor>

<concatenation>    ::=   <character value expression> <concatenation operator> <character factor>

<character factor>    ::=   <character primary> [ <collate clause> ]

<character primary>    ::=   <value expression primary> | <string value function>

<blob value expression>    ::=   <blob concatenation> | <blob factor>

<blob factor>    ::=   <blob primary>

<blob primary>    ::=   <value expression primary> | <string value function>

<blob concatenation>    ::=   <blob value expression> <concatenation operator> <blob factor>

TODO: string value expressions

== 6.29 <string value function> (p255)

Specify a function yielding a value of type character string or binary string.

<string value function>    ::=   <character value function> | <blob value function>

<character value function>    ::= 
         <character substring function>
     |     <regular expression substring function>
     |     <fold>
     |     <transcoding>
     |     <character transliteration>
     |     <trim function>
     |     <character overlay function>
     |     <normalize function>
     |     <specific type method>

<character substring function>    ::= 
         SUBSTRING <left paren> <character value expression> FROM <start position>
         [ FOR <string length> ] [ USING <char length units> ] <right paren>

<regular expression substring function>    ::= 
         SUBSTRING <left paren> <character value expression>
         SIMILAR <character value expression> ESCAPE <escape character> <right paren>

<fold>    ::=   { UPPER | LOWER } <left paren> <character value expression> <right paren>

<transcoding>    ::=   CONVERT <left paren> <character value expression> USING <transcoding name> <right paren>

<character transliteration>    ::=   TRANSLATE <left paren> <character value expression> USING <transliteration name> <right paren>

<trim function>    ::=   TRIM <left paren> <trim operands> <right paren>

<trim operands>    ::=   [ [ <trim specification> ] [ <trim character> ] FROM ] <trim source>

<trim source>    ::=   <character value expression>

<trim specification>    ::=   LEADING | TRAILING | BOTH

<trim character>    ::=   <character value expression>

<character overlay function>    ::= 
         OVERLAY <left paren> <character value expression> PLACING <character value expression>
         FROM <start position> [ FOR <string length> ] [ USING <char length units> ] <right paren>

<normalize function>    ::=   NORMALIZE <left paren> <character value expression> <right paren>

<specific type method>    ::=   <user-defined type value expression> <period> SPECIFICTYPE

<blob value function>    ::= 
         <blob substring function>
     |     <blob trim function>
     |     <blob overlay function>

<blob substring function>    ::= 
         SUBSTRING <left paren> <blob value expression> FROM <start position> [ FOR <string length> ] <right paren>

<blob trim function>    ::=   TRIM <left paren> <blob trim operands> <right paren>

<blob trim operands>    ::=   [ [ <trim specification> ] [ <trim octet> ] FROM ] <blob trim source>

<blob trim source>    ::=   <blob value expression>

<trim octet>    ::=   <blob value expression>

<blob overlay function>    ::= 
         OVERLAY <left paren> <blob value expression> PLACING <blob value expression>
         FROM <start position> [ FOR <string length> ] <right paren>

<start position>    ::=   <numeric value expression>

<string length>    ::=   <numeric value expression>

TODO: string value functions

== 6.30 <datetime value expression> (p266)

Specify a datetime value.

<datetime value expression>    ::= 
         <datetime term>
     |     <interval value expression> <plus sign> <datetime term>
     |     <datetime value expression> <plus sign> <interval term>
     |     <datetime value expression> <minus sign> <interval term>

<datetime term>    ::=   <datetime factor>

<datetime factor>    ::=   <datetime primary> [ <time zone> ]

<datetime primary>    ::=   <value expression primary> | <datetime value function>

<time zone>    ::=   AT <time zone specifier>

<time zone specifier>    ::=   LOCAL | TIME ZONE <interval primary>

TODO: datetime value expressions

== 6.31 <datetime value function> (p269)

Specify a function yielding a value of type datetime.

<datetime value function>    ::= 
         <current date value function>
     |     <current time value function>
     |     <current timestamp value function>
     |     <current local time value function>
     |     <current local timestamp value function>

<current date value function>    ::=   CURRENT_DATE

<current time value function>    ::=   CURRENT_TIME [ <left paren> <time precision> <right paren> ]

<current local time value function>    ::=   LOCALTIME [ <left paren> <time precision> <right paren> ]

<current timestamp value function>    ::=   CURRENT_TIMESTAMP [ <left paren> <timestamp precision> <right paren> ]

<current local timestamp value function>    ::=   LOCALTIMESTAMP [ <left paren> <timestamp precision> <right paren> ]

6.32 <interval value expression> (p271)

Specify an interval value.

<interval value expression>    ::= 
         <interval term>
     |     <interval value expression 1> <plus sign> <interval term 1>
     |     <interval value expression 1> <minus sign> <interval term 1>
     |     <left paren> <datetime value expression> <minus sign> <datetime term> <right paren> <interval qualifier>

<interval term>    ::= 
         <interval factor>
     |     <interval term 2> <asterisk> <factor>
     |     <interval term 2> <solidus> <factor>
     |     <term> <asterisk> <interval factor>

<interval factor>    ::=   [ <sign> ] <interval primary>

<interval primary>    ::= 
         <value expression primary> [ <interval qualifier> ]
     |     <interval value function>

<interval value expression 1>    ::=   <interval value expression>

<interval term 1>    ::=   <interval term>

<interval term 2>    ::=   <interval term>

TODO: datetime value functions

== 6.33 <interval value function> (p276)

<interval value function>    ::=   <interval absolute value function>

<interval absolute value function>    ::=   ABS <left paren> <interval value expression> <right paren>

TODO interval value functions

== 6.34 <boolean value expression> (p277)

<boolean value expression>    ::= 
         <boolean term>
     |     <boolean value expression> OR <boolean term>

<boolean term>    ::= 
         <boolean factor>
     |     <boolean term> AND <boolean factor>

<boolean factor>    ::=   [ NOT ] <boolean test>

<boolean test>    ::=   <boolean primary> [ IS [ NOT ] <truth value> ]

<truth value>    ::=   TRUE | FALSE | UNKNOWN

<boolean primary>    ::=   <predicate> | <boolean predicand>

<boolean predicand>    ::= 
         <parenthesized boolean value expression>
     |     <nonparenthesized value expression primary>

<parenthesized boolean value expression>    ::=   <left paren> <boolean value expression> <right paren>

> booleanValueExpression :: TestItem
> booleanValueExpression = Group "boolean value expression" $ map (uncurry TestValueExpr)
>     [("a or b", undefined)
>     ,("a and b", undefined)
>     ,("not a", undefined)
>     ,("a is true", undefined)
>     ,("a is false", undefined)
>     ,("a is unknown", undefined)
>     ,("a is not true", undefined)
>     ,("a is not false", undefined)
>     ,("a is not unknown", undefined)
>     ,("(a or b)", undefined)
>     ]

TODO: check precedence


== 6.35 <array value expression> (p284)

<array value expression>    ::=   <array concatenation> | <array factor>

<array concatenation>    ::=   <array value expression 1> <concatenation operator> <array factor>

<array value expression 1>    ::=   <array value expression>

<array factor>    ::=   <value expression primary>

I think all the syntax is covered already. The array concatenation
operator is ||, same as the string concatenation operator.

== 6.36 <array value constructor> (p284)

<array value constructor>    ::= 
         <array value constructor by enumeration>
     |     <array value constructor by query>

<array value constructor by enumeration>    ::= 
         ARRAY <left bracket or trigraph> <array element list> <right bracket or trigraph>

<array element list>    ::=   <array element> [ { <comma> <array element> }... ]

<array element>    ::=   <value expression>

<array value constructor by query>    ::= 
         ARRAY <left paren> <query expression> [ <order by clause> ] <right paren>

> arrayValueConstructor :: TestItem
> arrayValueConstructor = Group "array value constructor" $ map (uncurry TestValueExpr)
>     [("array[1,2,3]", undefined)
>     ,("array[a,b,c]", undefined)
>     ,("array(select * from t)", undefined)
>     ,("array(select * from t order by a)", undefined)
>     ]

== 6.37 <multiset value expression> (p286)

<multiset value expression>    ::= 
         <multiset term>
     |     <multiset value expression> MULTISET UNION [ ALL | DISTINCT ] <multiset term>
     |     <multiset value expression> MULTISET EXCEPT [ ALL | DISTINCT ] <multiset term>

<multiset term>    ::= 
         <multiset primary>
     |     <multiset term> MULTISET INTERSECT [ ALL | DISTINCT ] <multiset primary>

<multiset primary>    ::=   <multiset value function> | <value expression primary>

TODO: multiset value expression

== 6.38 <multiset value function> (p289)

<multiset value function>    ::=   <multiset set function>

<multiset set function>    ::=   SET <left paren> <multiset value expression> <right paren>

TODO: multiset value function

== 6.39 <multiset value constructor> (p290)

<multiset value constructor>    ::= 
         <multiset value constructor by enumeration>
     |     <multiset value constructor by query>
     |     <table value constructor by query>

<multiset value constructor by enumeration>    ::=   MULTISET <left bracket or trigraph> <multiset element list> <right bracket or trigraph>

<multiset element list>    ::=   <multiset element> [ { <comma> <multiset element> } ]

<multiset element>    ::=   <value expression>

<multiset value constructor by query>    ::=   MULTISET <left paren> <query expression> <right paren>

<table value constructor by query>    ::=   TABLE <left paren> <query expression> <right paren>

TODO: multiset value constructor

= 7 Query expressions

== 7.1 <row value constructor> (p293)

Specify a value or list of values to be constructed into a row or partial row.

<row value constructor>    ::= 
         <common value expression>
     |     <boolean value expression>
     |     <explicit row value constructor>

<explicit row value constructor>    ::= 
         <left paren> <row value constructor element> <comma> <row value constructor element list> <right paren>
     |     ROW <left paren> <row value constructor element list> <right paren>
     |     <row subquery>

<row value constructor element list>    ::= 
         <row value constructor element> [ { <comma> <row value constructor element> }... ]

<row value constructor element>    ::=   <value expression>

<contextually typed row value constructor>    ::= 
         <common value expression>
     |     <boolean value expression>
     |     <contextually typed value specification>
     |     <left paren> <contextually typed row value constructor element> <comma> <contextually typed row value constructor element list> <right paren>
     |     ROW <left paren> <contextually typed row value constructor element list> <right paren>

<contextually typed row value constructor element list>    ::= 
         <contextually typed row value constructor element>
         [ { <comma> <contextually typed row value constructor element> }... ]

<contextually typed row value constructor element>    ::= 
         <value expression>
     |     <contextually typed value specification>

<row value constructor predicand>    ::= 
         <common value expression>
     |     <boolean predicand>
     |     <explicit row value constructor>

TODO: row value constructor

== 7.2 <row value expression> (p296)

Specify a row value.

<row value expression>    ::= 
         <row value special case>
     |     <explicit row value constructor>

<table row value expression>    ::= 
         <row value special case>
     |     <row value constructor>

<contextually typed row value expression>    ::= 
         <row value special case>
     |     <contextually typed row value constructor>

<row value predicand>    ::= 
         <row value special case>
     |     <row value constructor predicand>

<row value special case>    ::=   <nonparenthesized value expression primary>

TODO: row value expression

== 7.3 <table value constructor> (p298)

Specify a set of <row value expression>s to be constructed into a table.

<table value constructor>    ::=   VALUES <row value expression list>

<row value expression list>    ::=   <table row value expression> [ { <comma> <table row value expression> }... ]

<contextually typed table value constructor>    ::=   VALUES <contextually typed row value expression list>

<contextually typed row value expression list>    ::=   <contextually typed row value expression> [ { <comma> <contextually typed row value expression> }... ]

> tableValueConstructor :: TestItem
> tableValueConstructor = Group "table value constructor" $ map (uncurry TestValueExpr)
>     [("values (1,2), (a+b,(select count(*) from t));", undefined)
>     ]

== 7.4 <table expression> (p300)

Specify a table or a grouped table.

<table expression>    ::= 
         <from clause>
         [ <where clause> ]
         [ <group by clause> ]
         [ <having clause> ]
         [ <window clause> ]

== 7.5 <from clause> (p301)

TODO: expand on these tests and review uncovered grammar

> fromClause :: TestItem
> fromClause = Group "from clause" $ map (uncurry TestValueExpr)
>     [("select * from t,u", undefined)

>     ,("select * from t as a", undefined)
>     ,("select * from t as a(b,c)", undefined)

>     ,("select * from (select c,d from t) as a", undefined)

Specify a table derived from one or more tables.

<from clause>    ::=   FROM <table reference list>

<table reference list>    ::=   <table reference> [ { <comma> <table reference> }... ]

7.6 <table reference> (p303)

Reference a table.

<table reference>    ::=   <table primary or joined table> [ <sample clause> ]

<table primary or joined table>    ::=   <table primary> | <joined table>

<sample clause>    ::= 
         TABLESAMPLE <sample method> <left paren> <sample percentage> <right paren> [ <repeatable clause> ]

<sample method>    ::=   BERNOULLI | SYSTEM

<repeatable clause>    ::=   REPEATABLE <left paren> <repeat argument> <right paren>

<sample percentage>    ::=   <numeric value expression>

<repeat argument>    ::=   <numeric value expression>

TODO: table samples

<table primary>    ::= 
         <table or query name> [ [ AS ] <correlation name> [ <left paren> <derived column list> <right paren> ] ]
     |     <derived table> [ AS ] <correlation name> [ <left paren> <derived column list> <right paren> ]
     |     <lateral derived table> [ AS ] <correlation name> [ <left paren> <derived column list> <right paren> ]
     |     <collection derived table> [ AS ] <correlation name> [ <left paren> <derived column list> <right paren> ]
     |     <table function derived table> [ AS ] <correlation name> [ <left paren> <derived column list> <right paren> ]
     |     <only spec> [ [ AS ] <correlation name> [ <left paren> <derived column list> <right paren> ] ]
     |     <left paren> <joined table> <right paren>

<only spec>    ::=   ONLY <left paren> <table or query name> <right paren>

TODO: only spec

<lateral derived table>    ::=   LATERAL <table subquery>

TODO: lateral

<collection derived table>    ::=   UNNEST <left paren> <collection value expression> <right paren> [ WITH ORDINALITY ]

TODO: unnest

<table function derived table>    ::=   TABLE <left paren> <collection value expression> <right paren>

<derived table>    ::=   <table subquery>

<table or query name>    ::=   <table name> | <query name>

<derived column list>    ::=   <column name list>

<column name list>    ::=   <column name> [ { <comma> <column name> }... ]

TODO: table function derived table

7.7 <joined table> (p312)

Specify a table derived from a Cartesian product, inner or outer join, or union join.

<joined table>    ::= 
         <cross join>
     |     <qualified join>
     |     <natural join>
     |     <union join>

<cross join>    ::=   <table reference> CROSS JOIN <table primary>

<qualified join>    ::=   <table reference> [ <join type> ] JOIN <table reference> <join specification>

<natural join>    ::=   <table reference> NATURAL [ <join type> ] JOIN <table primary>

<union join>    ::=   <table reference> UNION JOIN <table primary>

<join specification>    ::=   <join condition> | <named columns join>

<join condition>    ::=   ON <search condition>

<named columns join>    ::=   USING <left paren> <join column list> <right paren>

<join type>    ::=   INNER | <outer join type> [ OUTER ]

<outer join type>    ::=   LEFT | RIGHT | FULL

<join column list>    ::=   <column name list>

>     ,("select * from t cross join u", undefined)

>     ,("select * from t inner join u on a=b", undefined)
>     ,("select * from t natual inner join u", undefined)
>     ,("select * from t left join u on a=b", undefined)
>     ,("select * from t left outer join u on a=b", undefined)
>     ,("select * from t right outer join u on a=b", undefined)
>     ,("select * from t full outer join u on a=b", undefined)
>     ,("select * from t right join u on a=b", undefined)
>     ,("select * from t full join u on a=b", undefined)
>     ,("select * from t union join u on a=b", undefined)
>     ,("select * from t full join u using(a,b)", undefined)
>     ]

TODO: make sure to cover a good set of join variations

7.8 <where clause> (p319)

Specify a table derived by the application of a <search condition> to the result of the preceding <from clause>.

<where clause>    ::=   WHERE <search condition>

> whereClause :: TestItem
> whereClause = Group "where clause" $ map (uncurry TestValueExpr)
>     [("select * from t where a = 5", undefined)]


== 7.9 <group by clause> (p320)

Specify a grouped table derived by the application of the <group by clause> to the result of the previously specified clause.

<group by clause>    ::=   GROUP BY [ <set quantifier> ] <grouping element list>

<grouping element list>    ::=   <grouping element> [ { <comma> <grouping element> }... ]

<grouping element>    ::= 
         <ordinary grouping set>
     |     <rollup list>
     |     <cube list>
     |     <grouping sets specification>
     |     <empty grouping set>

<ordinary grouping set>    ::= 
         <grouping column reference>
     |     <left paren> <grouping column reference list> <right paren>

<grouping column reference>    ::=   <column reference> [ <collate clause> ]

<grouping column reference list>    ::=   <grouping column reference> [ { <comma> <grouping column reference> }... ]

<rollup list>    ::=   ROLLUP <left paren> <ordinary grouping set list> <right paren>

<ordinary grouping set list>    ::=   <ordinary grouping set> [ { <comma> <ordinary grouping set> }... ]

<cube list>    ::=   CUBE <left paren> <ordinary grouping set list> <right paren>

<grouping sets specification>    ::=   GROUPING SETS <left paren> <grouping set list> <right paren>

<grouping set list>    ::=   <grouping set> [ { <comma> <grouping set> }... ]

<grouping set>    ::= 
         <ordinary grouping set>
     |     <rollup list>
     |     <cube list>
     |     <grouping sets specification>
     |     <empty grouping set>

<empty grouping set>    ::=   <left paren> <right paren>

TODO: grouping set variations

It seems even in sql 2003, you can only put column references in the
groups, and not general value expressions.

> groupbyClause :: TestItem
> groupbyClause = Group "group by clause" $ map (uncurry TestValueExpr)
>     [("select a, sum(b) from t group by a", undefined)
>     ,("select a, c,sum(b) from t group by a,c", undefined)
>     ,("select a, c,sum(b) from t group by a,c collate x", undefined)
>     ,("select a, c,sum(b) from t group by a,c collate x having sum(b) > 100", undefined)
>     ]


7.10 <having clause> (p329)

Specify a grouped table derived by the elimination of groups that do not satisfy a <search condition>.

<having clause>    ::=   HAVING <search condition>

done above

== 7.11 <window clause> (p331)

TODO: window clauses

Specify one or more window definitions.

<window clause>    ::=   WINDOW <window definition list>

<window definition list>    ::=   <window definition> [ { <comma> <window definition> }... ]

<window definition>    ::=   <new window name> AS <window specification>

<new window name>    ::=   <window name>

<window specification>    ::=   <left paren> <window specification details> <right paren>

<window specification details>    ::= 
         [ <existing window name> ] [ <window partition clause> ] [ <window order clause> ] [ <window frame clause> ]

<existing window name>    ::=   <window name>

<window partition clause>    ::=   PARTITION BY <window partition column reference list>

<window partition column reference list>    ::=   <window partition column reference> [ { <comma> <window partition column reference> }... ]

<window partition column reference>    ::=   <column reference> [ <collate clause> ]

<window order clause>    ::=   ORDER BY <sort specification list>

<window frame clause>    ::=   <window frame units> <window frame extent> [ <window frame exclusion> ]

<window frame units>    ::=   ROWS | RANGE

<window frame extent>    ::=   <window frame start> | <window frame between>

<window frame start>    ::=   UNBOUNDED PRECEDING | <window frame preceding> | CURRENT ROW

<window frame preceding>    ::=   <unsigned value specification> PRECEDING

<window frame between>    ::=   BETWEEN <window frame bound 1> AND <window frame bound 2>

<window frame bound 1>    ::=   <window frame bound>

<window frame bound 2>    ::=   <window frame bound>

<window frame bound>    ::= 
         <window frame start>
     |     UNBOUNDED FOLLOWING 
     |     <window frame following>

<window frame following>    ::=   <unsigned value specification> FOLLOWING

<window frame exclusion>    ::= 
         EXCLUDE CURRENT ROW 
     |     EXCLUDE GROUP 
     |     EXCLUDE TIES 
     |     EXCLUDE NO OTHERS

== 7.12 <query specification> (p341)

Specify a table derived from the result of a <table expression>.

<query specification>    ::=   SELECT [ <set quantifier> ] <select list> <table expression>

<select list>    ::=   <asterisk> | <select sublist> [ { <comma> <select sublist> }... ]

<select sublist>    ::=   <derived column> | <qualified asterisk>

<qualified asterisk>    ::= 
         <asterisked identifier chain> <period> <asterisk>
     |     <all fields reference>

<asterisked identifier chain>    ::=   <asterisked identifier> [ { <period> <asterisked identifier> }... ]

<asterisked identifier>    ::=   <identifier>

<derived column>    ::=   <value expression> [ <as clause> ]

<as clause>    ::=   [ AS ] <column name>

<all fields reference>    ::=   <value expression primary> <period> <asterisk> [ AS <left paren> <all fields column name list> <right paren> ]

<all fields column name list>    ::=   <column name list>

TODO: review this and add more variants


> querySpecification :: TestItem
> querySpecification = Group "query specification" $ map (uncurry TestValueExpr)
>     [("select a from t", undefined)
>     ,("select all a from t", undefined)
>     ,("select distinct a from t", undefined)
>     ,("select * from t", undefined)
>     ,("select a,b from t", undefined)

>     ,("select a,b.* from t", undefined)

>     ,("select a a,b as b from t", undefined)
>     ]


== 7.13 <query expression> (p350)

Specify a table.

<query expression>    ::=   [ <with clause> ] <query expression body>

<with clause>    ::=   WITH [ RECURSIVE ] <with list>

<with list>    ::=   <with list element> [ { <comma> <with list element> }... ]

<with list element>    ::= 
         <query name> [ <left paren> <with column list> <right paren> ]
         AS <left paren> <query expression> <right paren> [ <search or cycle clause> ]

<with column list>    ::=   <column name list>

TODO: common table expressions

<query expression body>    ::=   <non-join query expression> | <joined table>

<non-join query expression>    ::= 
         <non-join query term>
     |     <query expression body> UNION [ ALL | DISTINCT ] [ <corresponding spec> ] <query term>
     |     <query expression body> EXCEPT [ ALL | DISTINCT ] [ <corresponding spec> ] <query term>

<query term>    ::=   <non-join query term> | <joined table>

<non-join query term>    ::= 
         <non-join query primary>
     |     <query term> INTERSECT [ ALL | DISTINCT ] [ <corresponding spec> ] <query primary>

<query primary>    ::=   <non-join query primary> | <joined table>

<non-join query primary>    ::=   <simple table> | <left paren> <non-join query expression> <right paren>

<simple table>    ::= 
         <query specification>
     |     <table value constructor>
     |     <explicit table>

<explicit table>    ::=   TABLE <table or query name>

<corresponding spec>    ::=   CORRESPONDING [ BY <left paren> <corresponding column list> <right paren> ]

<corresponding column list>    ::=   <column name list>

> queryExpressions :: TestItem
> queryExpressions = Group "query expressions" $ map (uncurry TestValueExpr)

>     [("select a from t union select a from u", undefined)
>     ,("select a from t union all select a from u", undefined)
>     ,("select a from t union corresponding select a from u", undefined)
>     ,("select a,b from t union corresponding by (a,b) select b,a from u", undefined)
>     ,("select a from t except select a from u", undefined)
>     ,("select a from t intersect select a from u", undefined)
>     ,("table t", undefined)
>     ]

TODO: review this and add more variations

== 7.14 <search or cycle clause> (p363)

Specify the generation of ordering and cycle detection information in the result of recursive query expressions.

<search or cycle clause>    ::= 
         <search clause>
     |     <cycle clause>
     |     <search clause> <cycle clause>

<search clause>    ::=   SEARCH <recursive search order> SET <sequence column>

<recursive search order>    ::= 
         DEPTH FIRST BY <sort specification list>
     |     BREADTH FIRST BY <sort specification list>

<sequence column>    ::=   <column name>

<cycle clause>    ::= 
         CYCLE <cycle column list>
         SET <cycle mark column> TO <cycle mark value>
         DEFAULT <non-cycle mark value>
         USING <path column>

<cycle column list>    ::=   <cycle column> [ { <comma> <cycle column> }... ]

<cycle column>    ::=   <column name>

<cycle mark column>    ::=   <column name>

<path column>    ::=   <column name>

<cycle mark value>    ::=   <value expression>

<non-cycle mark value>    ::=   <value expression>

TODO: searach or cycle clause: for recursive cte?

== 7.15 <subquery> (p368)

Specify a scalar value, a row, or a table derived from a <query expression>.

<scalar subquery>    ::=   <subquery>

<row subquery>    ::=   <subquery>

<table subquery>    ::=   <subquery>

<subquery>    ::=   <left paren> <query expression> <right paren>

covered elsewhere

= 8 Predicates

== 8.1 <predicate> (p371)

Specify a condition that can be evaluated to give a boolean value.

<predicate>    ::= 
         <comparison predicate>
     |     <between predicate>
     |     <in predicate>
     |     <like predicate>
     |     <similar predicate>
     |     <null predicate>
     |     <quantified comparison predicate>
     |     <exists predicate>
     |     <unique predicate>
     |     <normalized predicate>
     |     <match predicate>
     |     <overlaps predicate>
     |     <distinct predicate>
     |     <member predicate>
     |     <submultiset predicate>
     |     <set predicate>
     |     <type predicate>

== 8.2 <comparison predicate> (p373)

Specify a comparison of two row values.

<comparison predicate>    ::=   <row value predicand> <comparison predicate part 2>

<comparison predicate part 2>    ::=   <comp op> <row value predicand>

<comp op>    ::= 
         <equals operator>
     |     <not equals operator>
     |     <less than operator>
     |     <greater than operator>
     |     <less than or equals operator>
     |     <greater than or equals operator>

TODO: comparison predicate

== 8.3 <between predicate> (p380)

Specify a range comparison.

<between predicate>    ::=   <row value predicand> <between predicate part 2>

<between predicate part 2>    ::=   [ NOT ] BETWEEN [ ASYMMETRIC | SYMMETRIC ] <row value predicand> AND <row value predicand>

TODO: between predicate

== 8.4 <in predicate> (p381)

Specify a quantified comparison.

<in predicate>    ::=   <row value predicand> <in predicate part 2>

<in predicate part 2>    ::=   [ NOT ] IN <in predicate value>

<in predicate value>    ::= 
         <table subquery>
     |     <left paren> <in value list> <right paren>

<in value list>    ::=   <row value expression> [ { <comma> <row value expression> }... ]

TODO: in predicate

== 8.5 <like predicate> (p383)

Specify a pattern-match comparison.

<like predicate>    ::=   <character like predicate> | <octet like predicate>

<character like predicate>    ::=   <row value predicand> <character like predicate part 2>

<character like predicate part 2>    ::=   [ NOT ] LIKE <character pattern> [ ESCAPE <escape character> ]

<character pattern>    ::=   <character value expression>

<escape character>    ::=   <character value expression>

<octet like predicate>    ::=   <row value predicand> <octet like predicate part 2>

<octet like predicate part 2>    ::=   [ NOT ] LIKE <octet pattern> [ ESCAPE <escape octet> ]

<octet pattern>    ::=   <blob value expression>

<escape octet>    ::=   <blob value expression>

TODO: like predicate

== 8.6 <similar predicate> (p389)

Specify a character string similarity by means of a regular expression.

<similar predicate>    ::=   <row value predicand> <similar predicate part 2>

<similar predicate part 2>    ::=   [ NOT ] SIMILAR TO <similar pattern> [ ESCAPE <escape character> ]

<similar pattern>    ::=   <character value expression>

<regular expression>    ::= 
         <regular term>
     |     <regular expression> <vertical bar> <regular term>

<regular term>    ::= 
         <regular factor>
     |     <regular term> <regular factor>

<regular factor>    ::= 
         <regular primary>
     |     <regular primary> <asterisk>
     |     <regular primary> <plus sign>
     |     <regular primary> <question mark>
     |     <regular primary> <repeat factor>

<repeat factor>    ::=   <left brace> <low value> [ <upper limit> ] <right brace>

<upper limit>    ::=   <comma> [ <high value> ]

<low value>    ::=   <unsigned integer>

<high value>    ::=   <unsigned integer>

<regular primary>    ::= 
         <character specifier>
     |     <percent>
     |     <regular character set>
     |     <left paren> <regular expression> <right paren>

<character specifier>    ::=   <non-escaped character> | <escaped character>

<non-escaped character>    ::=   !! See the Syntax Rules.

<escaped character>    ::=   !! See the Syntax Rules.

<regular character set>    ::= 
         <underscore>
     |     <left bracket> <character enumeration> ... <right bracket>
     |     <left bracket> <circumflex> <character enumeration> ... <right bracket>
     |     <left bracket> <character enumeration include> ... <circumflex> <character enumeration exclude> ... <right bracket>

<character enumeration include>    ::=   <character enumeration>

<character enumeration exclude>    ::=   <character enumeration>

<character enumeration>    ::= 
         <character specifier>
     |     <character specifier> <minus sign> <character specifier>
     |     <left bracket> <colon> <regular character set identifier> <colon> <right bracket>

<regular character set identifier>    ::=   <identifier>

TODO: similar predicate

== 8.7 <null predicate> (p395)

Specify a test for a null value.

<null predicate>    ::=   <row value predicand> <null predicate part 2>

<null predicate part 2>    ::=   IS [ NOT ] NULL

TODO: null predicate

== 8.8 <quantified comparison predicate> (p397)

Specify a quantified comparison.

<quantified comparison predicate>    ::=   <row value predicand> <quantified comparison predicate part 2>

<quantified comparison predicate part 2>    ::=   <comp op> <quantifier> <table subquery>

<quantifier>    ::=   <all> | <some>

<all>    ::=   ALL

<some>    ::=   SOME | ANY

TODO: quantified comparison predicate

== 8.9 <exists predicate> (p399)

Specify a test for a non-empty set.

<exists predicate>    ::=   EXISTS <table subquery>

TODO: exists predicate

== 8.10 <unique predicate> (p400)

Specify a test for the absence of duplicate rows

<unique predicate>    ::=   UNIQUE <table subquery>

TODO: unique predicate

== 8.11 <normalized predicate> (p401)

Determine whether a character string value is normalized.

<normalized predicate>    ::=   <string value expression> IS [ NOT ] NORMALIZED

TODO: normalized predicate

== 8.12 <match predicate> (p402)

Specify a test for matching rows.

<match predicate>    ::=   <row value predicand> <match predicate part 2>

<match predicate part 2>    ::=   MATCH [ UNIQUE ] [ SIMPLE | PARTIAL | FULL ] <table subquery>

TODO: match predicate

== 8.13 <overlaps predicate> (p405)

Specify a test for an overlap between two datetime periods.

<overlaps predicate>    ::=   <overlaps predicate part 1> <overlaps predicate part 2>

<overlaps predicate part 1>    ::=   <row value predicand 1>

<overlaps predicate part 2>    ::=   OVERLAPS <row value predicand 2>

<row value predicand 1>    ::=   <row value predicand>

<row value predicand 2>    ::=   <row value predicand>

TODO: overlaps predicate

== 8.14 <distinct predicate> (p407)

Specify a test of whether two row values are distinct

<distinct predicate>    ::=   <row value predicand 3> <distinct predicate part 2>

<distinct predicate part 2>    ::=   IS DISTINCT FROM <row value predicand 4>

<row value predicand 3>    ::=   <row value predicand>

<row value predicand 4>    ::=   <row value predicand>

TODO: distinct predicate

== 8.15 <member predicate> (p409)

Specify a test of whether a value is a member of a multiset.

<member predicate>    ::=   <row value predicand> <member predicate part 2>

<member predicate part 2>    ::=   [ NOT ] MEMBER [ OF ] <multiset value expression>

TODO: member predicate

== 8.16 <submultiset predicate> (p411)

Specify a test of whether a multiset is a submultiset of another multiset.

<submultiset predicate>    ::=   <row value predicand> <submultiset predicate part 2>

<submultiset predicate part 2>    ::=   [ NOT ] SUBMULTISET [ OF ] <multiset value expression>

TODO: submultiset predicate

== 8.17 <set predicate> (p413)

Specify a test of whether a multiset is a set (that is, does not contain any duplicates).

<set predicate>    ::=   <row value predicand> <set predicate part 2>

<set predicate part 2>    ::=   IS [ NOT ] A SET

TODO: set predicate

== 8.18 <type predicate> (p414)

Specify a type test.

<type predicate>    ::=   <row value predicand> <type predicate part 2>

<type predicate part 2>    ::=   IS [ NOT ] OF <left paren> <type list> <right paren>

<type list>    ::=   <user-defined type specification> [ { <comma> <user-defined type specification> }... ]

<user-defined type specification>    ::= 
         <inclusive user-defined type specification>
     |     <exclusive user-defined type specification>

<inclusive user-defined type specification>    ::=   <path-resolved user-defined type name>

<exclusive user-defined type specification>    ::=   ONLY <path-resolved user-defined type name>

TODO: type predicate

== 8.19 <search condition> (p416)

Specify a condition that is True , False , or Unknown , depending on the value of a <boolean value expression>.

<search condition>    ::=   <boolean value expression>

covered elsewhere

section 9 deleted

= 10 Additional common elements

many of these are not relevant for query expressions

TODO: review all of these to eliminate the ones which aren't needed

== 10.1 <interval qualifier> (p465)

Specify the precision of an interval data type.

<interval qualifier>    ::= 
         <start field> TO <end field>
     |     <single datetime field>

<start field>    ::=   <non-second primary datetime field> [ <left paren> <interval leading field precision> <right paren> ]

<end field>    ::= 
         <non-second primary datetime field>
     |     SECOND [ <left paren> <interval fractional seconds precision> <right paren> ]

<single datetime field>    ::= 
         <non-second primary datetime field> [ <left paren> <interval leading field precision> <right paren> ]
     |     SECOND [ <left paren> <interval leading field precision> [ <comma> <interval fractional seconds precision> ] <right paren> ]

<primary datetime field>    ::= 
         <non-second primary datetime field>
     |     SECOND

<non-second primary datetime field>    ::=   YEAR | MONTH | DAY | HOUR | MINUTE

<interval fractional seconds precision>    ::=   <unsigned integer>

<interval leading field precision>    ::=   <unsigned integer>

TODO: interval qualifier

== 10.2 <language clause> (p469)

Specify a standard programming language.

<language clause>    ::=   LANGUAGE <language name>

<language name>    ::=   ADA | C | COBOL | FORTRAN | MUMPS | PASCAL | PLI | SQL

Table 14 -- Standard programming languages

Language keyword	 Relevant standard
ADA	ISO/IEC 8652
C	ISO/IEC 9899
COBOL	ISO 1989
FORTRAN	ISO 1539
MUMPS	ISO/IEC 11756
PASCAL	ISO/IEC 7185 and ISO/IEC 10206
PLI	ISO 6160
SQL	ISO/IEC 9075
10.3 <path specification> (p471)

Specify an order for searching for an SQL-invoked routine.

<path specification>    ::=   PATH <schema name list>

<schema name list>    ::=   <schema name> [ { <comma> <schema name> }... ]

10.4 <routine invocation> (p472)

Invoke an SQL-invoked routine.

<routine invocation>    ::=   <routine name> <SQL argument list>

<routine name>    ::=   [ <schema name> <period> ] <qualified identifier>

<SQL argument list>    ::=   <left paren> [ <SQL argument> [ { <comma> <SQL argument> }... ] ] <right paren>

<SQL argument>    ::= 
         <value expression>
     |     <generalized expression>
     |     <target specification>

<generalized expression>    ::=   <value expression> AS <path-resolved user-defined type name>

== 10.5 <character set specification> (p495)

Identify a character set.

<character set specification>    ::= 
         <standard character set name>
     |     <implementation-defined character set name>
     |     <user-defined character set name>

<standard character set name>    ::=   <character set name>

<implementation-defined character set name>    ::=   <character set name>

<user-defined character set name>    ::=   <character set name>

TODO: character set specification?

== 10.6 <specific routine designator> (p497)

Specify an SQL-invoked routine.

<specific routine designator>    ::= 
         SPECIFIC <routine type> <specific name>
             |     <routine type> <member name>
         [ FOR <schema-resolved user-defined type name> ]

<routine type>    ::= 
         ROUTINE 
     |     FUNCTION 
     |     PROCEDURE 
     |     [ INSTANCE | STATIC | CONSTRUCTOR ] METHOD

<member name>    ::=   <member name alternatives> [ <data type list> ]

<member name alternatives>    ::=   <schema qualified routine name> | <method name>

<data type list>    ::=   <left paren> [ <data type> [ { <comma> <data type> }... ] ] <right paren>

== 10.7 <collate clause> (p500)

Specify a default collating sequence.

<collate clause>    ::=   COLLATE <collation name>

covered elsewhere

10.8 <constraint name definition> and <constraint characteristics> (p501)

Specify the name of a constraint and its characteristics.

<constraint name definition>    ::=   CONSTRAINT <constraint name>

<constraint characteristics>    ::= 
         <constraint check time> [ [ NOT ] DEFERRABLE ]
     |     [ NOT ] DEFERRABLE [ <constraint check time> ]

<constraint check time>    ::=   INITIALLY DEFERRED | INITIALLY IMMEDIATE

== 10.9 <aggregate function> (p503)

Specify a value computed from a collection of rows.

<aggregate function>    ::= 
         COUNT <left paren> <asterisk> <right paren> [ <filter clause> ]
     |     <general set function> [ <filter clause> ]
     |     <binary set function> [ <filter clause> ]
     |     <ordered set function> [ <filter clause> ]

<general set function>    ::=   <set function type> <left paren> [ <set quantifier> ] <value expression> <right paren>

<set function type>    ::=   <computational operation>

<computational operation>    ::= 
         AVG | MAX | MIN | SUM 
     |     EVERY | ANY | SOME 
     |     COUNT 
     |     STDDEV_POP | STDDEV_SAMP | VAR_SAMP | VAR_POP 
     |     COLLECT | FUSION | INTERSECTION

<set quantifier>    ::=   DISTINCT | ALL

<filter clause>    ::=   FILTER <left paren> WHERE <search condition> <right paren>

<binary set function>    ::=   <binary set function type> <left paren> <dependent variable expression> <comma> <independent variable expression> <right paren>

<binary set function type>    ::= 
         COVAR_POP | COVAR_SAMP | CORR | REGR_SLOPE 
     |     REGR_INTERCEPT | REGR_COUNT | REGR_R2 | REGR_AVGX | REGR_AVGY 
     |     REGR_SXX | REGR_SYY | REGR_SXY

<dependent variable expression>    ::=   <numeric value expression>

<independent variable expression>    ::=   <numeric value expression>

<ordered set function>    ::=   <hypothetical set function> | <inverse distribution function>

<hypothetical set function>    ::=   <rank function type> <left paren> <hypothetical set function value expression list> <right paren> <within group specification>

<within group specification>    ::=   WITHIN GROUP <left paren> ORDER BY <sort specification list> <right paren>

<hypothetical set function value expression list>    ::=   <value expression> [ { <comma> <value expression> }... ]

<inverse distribution function>    ::=   <inverse distribution function type> <left paren> <inverse distribution function argument> <right paren> <within group specification>

<inverse distribution function argument>    ::=   <numeric value expression>

<inverse distribution function type>    ::=   PERCENTILE_CONT | PERCENTILE_DISC

TODO: aggregate functions

== 10.10 <sort specification list> (p515)

Specify a sort order.

<sort specification list>    ::=   <sort specification> [ { <comma> <sort specification> }... ]

<sort specification>    ::=   <sort key> [ <ordering specification> ] [ <null ordering> ]

<sort key>    ::=   <value expression>

<ordering specification>    ::=   ASC | DESC

<null ordering>    ::=   NULLS FIRST | NULLS LAST

TODO: review sort specifications

> sortSpecificationList :: TestItem
> sortSpecificationList = Group "sort specification list" $ map (uncurry TestValueExpr)
>     [("select * from t order by a", undefined)
>     ,("select * from t order by a,b", undefined)
>     ,("select * from t order by a asc,b", undefined)
>     ,("select * from t order by a desc,b", undefined)
>     ,("select * from t order by a collate x desc,b", undefined)
>     ,("select * from t order by 1,2", undefined)
>     ]

TODO: what happened to the collation in order by?
