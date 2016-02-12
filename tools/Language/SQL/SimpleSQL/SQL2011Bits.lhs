
Sections 17 and 19 in Foundation

This module covers the tests for transaction management (begin,
commit, savepoint, etc.), and session management (set).


> module Language.SQL.SimpleSQL.SQL2011Bits (sql2011BitsTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax

> sql2011BitsTests :: TestItem
> sql2011BitsTests = Group "sql 2011 bits tests" [

17 Transaction management

17.1 <start transaction statement>

<start transaction statement> ::=
  START TRANSACTION [ <transaction characteristics> ]

BEGIN is not in the standard!

>      (TestStatement ansi2011
>       "start transaction"
>      $ StartTransaction)

17.2 <set transaction statement>

<set transaction statement> ::=
  SET [ LOCAL ] TRANSACTION <transaction characteristics>

17.3 <transaction characteristics>

<transaction characteristics> ::=
      [ <transaction mode> [ { <comma> <transaction mode> }... ] ]

<transaction mode> ::=
    <isolation level>
  | <transaction access mode>
  | <diagnostics size>

<transaction access mode> ::=
    READ ONLY
  | READ WRITE

<isolation level> ::=
  ISOLATION LEVEL <level of isolation>

<level of isolation> ::=
    READ UNCOMMITTED
  | READ COMMITTED
  | REPEATABLE READ
  | SERIALIZABLE

<diagnostics size> ::=
  DIAGNOSTICS SIZE <number of conditions>

<number of conditions> ::=
  <simple value specification>

17.4 <set constraints mode statement>

<set constraints mode statement> ::=
  SET CONSTRAINTS <constraint name list> { DEFERRED | IMMEDIATE }

<constraint name list> ::=
    ALL
  | <constraint name> [ { <comma> <constraint name> }... ]

17.5 <savepoint statement>

<savepoint statement> ::=
  SAVEPOINT <savepoint specifier>

<savepoint specifier> ::=
  <savepoint name>

>     ,(TestStatement ansi2011
>       "savepoint difficult_bit"
>      $ Savepoint $ Name Nothing "difficult_bit")


17.6 <release savepoint statement>

<release savepoint statement> ::=
  RELEASE SAVEPOINT <savepoint specifier>

>     ,(TestStatement ansi2011
>       "release savepoint difficult_bit"
>      $ ReleaseSavepoint $ Name Nothing "difficult_bit")


17.7 <commit statement>

<commit statement> ::=
  COMMIT [ WORK ] [ AND [ NO ] CHAIN ]

>     ,(TestStatement ansi2011
>       "commit"
>      $ Commit)

>     ,(TestStatement ansi2011
>       "commit work"
>      $ Commit)


17.8 <rollback statement>

<rollback statement> ::=
  ROLLBACK [ WORK ] [ AND [ NO ] CHAIN ] [ <savepoint clause> ]

<savepoint clause> ::=
  TO SAVEPOINT <savepoint specifier>

>     ,(TestStatement ansi2011
>       "rollback"
>      $ Rollback Nothing)

>     ,(TestStatement ansi2011
>       "rollback work"
>      $ Rollback Nothing)

>     ,(TestStatement ansi2011
>       "rollback to savepoint difficult_bit"
>      $ Rollback $ Just $ Name Nothing "difficult_bit")


19 Session management

19.1 <set session characteristics statement>

<set session characteristics statement> ::=
  SET SESSION CHARACTERISTICS AS <session characteristic list>

<session characteristic list> ::=
  <session characteristic> [ { <comma> <session characteristic> }... ]

<session characteristic> ::=
  <session transaction characteristics>

<session transaction characteristics> ::=
  TRANSACTION <transaction mode> [ { <comma> <transaction mode> }... ]

19.2 <set session user identifier statement>

<set session user identifier statement> ::=
  SET SESSION AUTHORIZATION <value specification>

19.3 <set role statement>

<set role statement> ::=
  SET ROLE <role specification>

<role specification> ::=
    <value specification>
  | NONE

19.4 <set local time zone statement>

<set local time zone statement> ::=
  SET TIME ZONE <set time zone value>

<set time zone value> ::=
    <interval value expression>
  | LOCAL

19.5 <set catalog statement>

<set catalog statement> ::=
  SET <catalog name characteristic>

<catalog name characteristic> ::=
  CATALOG <value specification>

19.6 <set schema statement>

<set schema statement> ::=
  SET <schema name characteristic>

<schema name characteristic> ::=
  SCHEMA <value specification>

19.7 <set names statement>

<set names statement> ::=
  SET <character set name characteristic>

<character set name characteristic> ::=
  NAMES <value specification>

19.8 <set path statement>

<set path statement> ::=
  SET <SQL-path characteristic>

<SQL-path characteristic> ::=
  PATH <value specification>

19.9 <set transform group statement>

<set transform group statement> ::=
  SET <transform group characteristic>

<transform group characteristic> ::=
    DEFAULT TRANSFORM GROUP <value specification>
  | TRANSFORM GROUP FOR TYPE <path-resolved user-defined type name> <value specification>

19.10 <set session collation statement>

<set session collation statement> ::=
    SET COLLATION <collation specification> [ FOR <character set specification list> ]
  | SET NO COLLATION [ FOR <character set specification list> ]

<collation specification> ::=
  <value specification>

>    ]
