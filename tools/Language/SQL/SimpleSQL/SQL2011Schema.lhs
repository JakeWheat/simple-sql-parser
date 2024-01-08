
Section 11 in Foundation

This module covers the tests for parsing schema and DDL statements.

> module Language.SQL.SimpleSQL.SQL2011Schema (sql2011SchemaTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax

> sql2011SchemaTests :: TestItem
> sql2011SchemaTests = Group "sql 2011 schema tests"
>     [


11.1 <schema definition>

<schema definition> ::=
  CREATE SCHEMA <schema name clause>
      [ <schema character set or path> ]
      [ <schema element>... ]

>      (TestStatement ansi2011 "create schema my_schema"
>      $ CreateSchema [Name Nothing "my_schema"])

todo: schema name can have .
schema name can be quoted iden or unicode quoted iden
add schema element support:
  create a list of schema elements
  then do pairwise combinations in schema element list to test


<schema character set or path> ::=
    <schema character set specification>
  | <schema path specification>
  | <schema character set specification> <schema path specification>
  | <schema path specification> <schema character set specification>

<schema name clause> ::=
    <schema name>
  | AUTHORIZATION <schema authorization identifier>
  | <schema name> AUTHORIZATION <schema authorization identifier>

<schema authorization identifier> ::=
  <authorization identifier>

<schema character set specification> ::=
  DEFAULT CHARACTER SET <character set specification>

<schema path specification> ::=
  <path specification>

<schema element> ::=
      <table definition>
  |   <view definition>
  |   <domain definition>
  |   <character set definition>
  |   <collation definition>
  |   <transliteration definition>
  |   <assertion definition>
  |   <trigger definition>
  |   <user-defined type definition>
  |   <user-defined cast definition>
  |   <user-defined ordering definition>
  |   <transform definition>
  |   <schema routine>
  |   <sequence generator definition>
  |   <grant statement>
  |   <role definition>


11.2 <drop schema statement>

<drop schema statement> ::=
  DROP SCHEMA <schema name> <drop behavior>

<drop behavior> ::=
    CASCADE
  | RESTRICT


>     ,(TestStatement ansi2011 "drop schema my_schema"
>      $ DropSchema [Name Nothing "my_schema"] DefaultDropBehaviour)
>     ,(TestStatement ansi2011 "drop schema my_schema cascade"
>      $ DropSchema [Name Nothing "my_schema"] Cascade)
>     ,(TestStatement ansi2011 "drop schema my_schema restrict"
>      $ DropSchema [Name Nothing "my_schema"] Restrict)

11.3 <table definition>


<table definition> ::=
  CREATE [ <table scope> ] TABLE <table name> <table contents source>
      [ WITH <system versioning clause> ]
      [ ON COMMIT <table commit action> ROWS ]

>     ,(TestStatement ansi2011 "create table t (a int, b int);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableColumnDef $ ColumnDef (Name Nothing "b") (TypeName [Name Nothing "int"]) Nothing []])


<table contents source> ::=
    <table element list>
  | <typed table clause>
  | <as subquery clause>

<table scope> ::=
  <global or local> TEMPORARY

<global or local> ::=
    GLOBAL
  | LOCAL

<system versioning clause> ::=
  SYSTEM VERSIONING

defintely skip

<table commit action> ::=
    PRESERVE
  | DELETE

defintely skip

<table element list> ::=
  <left paren> <table element> [ { <comma> <table element> }... ] <right paren>

<table element> ::=
    <column definition>
  | <table period definition>
  | <table constraint definition>
  | <like clause>

<typed table clause> ::=
  OF <path-resolved user-defined type name> [ <subtable clause> ]
      [ <typed table element list> ]

defintely skip

<typed table element list> ::=
  <left paren> <typed table element>
      [ { <comma> <typed table element> }... ] <right paren>

defintely skip

<typed table element> ::=
    <column options>
 | <table constraint definition>
  | <self-referencing column specification>

defintely skip

<self-referencing column specification> ::=
  REF IS <self-referencing column name> [ <reference generation> ]

defintely skip

<reference generation> ::=
    SYSTEM GENERATED
  | USER GENERATED
  | DERIVED

defintely skip

<self-referencing column name> ::=
  <column name>

defintely skip

<column options> ::=
  <column name> WITH OPTIONS <column option list>

defintely skip

<column option list> ::=
  [ <scope clause> ] [ <default clause> ] [ <column constraint definition>... ]

defintely skip

<subtable clause> ::=
  UNDER <supertable clause>

defintely skip

<supertable clause> ::=
  <supertable name>

defintely skip

<supertable name> ::=
  <table name>

defintely skip

<like clause> ::=
  LIKE <table name> [ <like options> ]

<like options> ::=
  <like option>...

<like option> ::=
    <identity option>
  | <column default option>
  | <generation option>

<identity option> ::=
    INCLUDING IDENTITY
  | EXCLUDING IDENTITY

<column default option> ::=
    INCLUDING DEFAULTS
  | EXCLUDING DEFAULTS

<generation option> ::=
    INCLUDING GENERATED
  | EXCLUDING GENERATED

<as subquery clause> ::=
  [ <left paren> <column name list> <right paren> ] AS <table subquery>
      <with or without data>

<with or without data> ::=
    WITH NO DATA
  | WITH DATA

<table period definition> ::=
  <system or application time period specification>
      <left paren> <period begin column name> <comma> <period end column name> <right paren>

defintely skip

<system or application time period specification> ::=
    <system time period specification>
  | <application time period specification>

defintely skip

<system time period specification> ::=
  PERIOD FOR SYSTEM_TIME

defintely skip

<application time period specification> ::=
  PERIOD FOR <application time period name>

defintely skip

<application time period name> ::=
  <identifier>

defintely skip

<period begin column name> ::=
  <column name>

defintely skip

<period end column name> ::=
  <column name>

defintely skip


11.4 <column definition>

<column definition> ::=
  <column name> [ <data type or domain name> ]
      [ <default clause> | <identity column specification> | <generation clause>
      | <system time period start column specification>
      | <system time period end column specification> ]
      [ <column constraint definition>... ]
      [ <collate clause> ]

<data type or domain name> ::=
    <data type>
  | <domain name>

<system time period start column specification> ::=
  <timestamp generation rule> AS ROW START

defintely skip

<system time period end column specification> ::=
  <timestamp generation rule> AS ROW END

defintely skip

<timestamp generation rule> ::=
  GENERATED ALWAYS

defintely skip

<column constraint definition> ::=
  [ <constraint name definition> ] <column constraint> [ <constraint characteristics> ]

<column constraint> ::=
    NOT NULL
  | <unique specification>
  | <references specification>
  | <check constraint definition>


can have more than one
whitespace separated

one constratint:
optional name: constraint [Name]
not null | unique | references | check
todo: constraint characteristics


>     ,(TestStatement ansi2011
>       "create table t (a int not null);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing ColNotNullConstraint]])

>     ,(TestStatement ansi2011
>       "create table t (a int constraint a_not_null not null);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef (Just [Name Nothing "a_not_null"]) ColNotNullConstraint]])

>     ,(TestStatement ansi2011
>       "create table t (a int unique);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing ColUniqueConstraint]])

>     ,(TestStatement ansi2011
>       "create table t (a int primary key);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing (ColPrimaryKeyConstraint False)]])

>     ,(TestStatement ansi2011 { diAutoincrement = True }
>       "create table t (a int primary key autoincrement);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing (ColPrimaryKeyConstraint True)]])

references t(a,b)
  [ Full |partial| simepl]
  [perm: on update [cascade | set null | set default | restrict | no action]
         on delete ""

>     ,(TestStatement ansi2011
>       "create table t (a int references u);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing $ ColReferencesConstraint
>          [Name Nothing "u"] Nothing DefaultReferenceMatch
>          DefaultReferentialAction DefaultReferentialAction]])

>     ,(TestStatement ansi2011
>       "create table t (a int references u(a));"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing $ ColReferencesConstraint
>          [Name Nothing "u"] (Just $ Name Nothing "a") DefaultReferenceMatch
>          DefaultReferentialAction DefaultReferentialAction]])

>     ,(TestStatement ansi2011
>       "create table t (a int references u match full);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing $ ColReferencesConstraint
>          [Name Nothing "u"] Nothing MatchFull
>          DefaultReferentialAction DefaultReferentialAction]])

>     ,(TestStatement ansi2011
>       "create table t (a int references u match partial);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing $ ColReferencesConstraint
>          [Name Nothing "u"] Nothing MatchPartial
>          DefaultReferentialAction DefaultReferentialAction]])

>     ,(TestStatement ansi2011
>       "create table t (a int references u match simple);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing $ ColReferencesConstraint
>          [Name Nothing "u"] Nothing MatchSimple
>          DefaultReferentialAction DefaultReferentialAction]])

>     ,(TestStatement ansi2011
>       "create table t (a int references u on update cascade );"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing $ ColReferencesConstraint
>          [Name Nothing "u"] Nothing DefaultReferenceMatch
>          RefCascade DefaultReferentialAction]])

>     ,(TestStatement ansi2011
>       "create table t (a int references u on update set null );"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing $ ColReferencesConstraint
>          [Name Nothing "u"] Nothing DefaultReferenceMatch
>          RefSetNull DefaultReferentialAction]])

>     ,(TestStatement ansi2011
>       "create table t (a int references u on update set default );"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing $ ColReferencesConstraint
>          [Name Nothing "u"] Nothing DefaultReferenceMatch
>          RefSetDefault DefaultReferentialAction]])

>     ,(TestStatement ansi2011
>       "create table t (a int references u on update no action );"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing $ ColReferencesConstraint
>          [Name Nothing "u"] Nothing DefaultReferenceMatch
>          RefNoAction DefaultReferentialAction]])

>     ,(TestStatement ansi2011
>       "create table t (a int references u on delete cascade );"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing $ ColReferencesConstraint
>          [Name Nothing "u"] Nothing DefaultReferenceMatch
>          DefaultReferentialAction RefCascade]])


>     ,(TestStatement ansi2011
>       "create table t (a int references u on update cascade on delete restrict );"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing $ ColReferencesConstraint
>          [Name Nothing "u"] Nothing DefaultReferenceMatch
>          RefCascade RefRestrict]])

>     ,(TestStatement ansi2011
>       "create table t (a int references u on delete restrict on update cascade );"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing $ ColReferencesConstraint
>          [Name Nothing "u"] Nothing DefaultReferenceMatch
>          RefCascade RefRestrict]])

TODO: try combinations and permutations of column constraints and
options


>     ,(TestStatement ansi2011
>       "create table t (a int check (a>5));"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing
>         [ColConstraintDef Nothing
>          (ColCheckConstraint $ BinOp (Iden [Name Nothing "a"]) [Name Nothing ">"] (NumLit "5"))]])





<identity column specification> ::=
  GENERATED { ALWAYS | BY DEFAULT } AS IDENTITY
      [ <left paren> <common sequence generator options> <right paren> ]

>     ,(TestStatement ansi2011 "create table t (a int generated always as identity);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"])
>         (Just $ IdentityColumnSpec GeneratedAlways []) []])

>     ,(TestStatement ansi2011 "create table t (a int generated by default as identity);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"])
>         (Just $ IdentityColumnSpec GeneratedByDefault []) []])


>     ,(TestStatement ansi2011
>       "create table t (a int generated always as identity\n\
>       \  ( start with 5 increment by 5 maxvalue 500 minvalue 5 cycle ));"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"])
>         (Just $ IdentityColumnSpec GeneratedAlways
>          [SGOStartWith 5
>          ,SGOIncrementBy 5
>          ,SGOMaxValue 500
>          ,SGOMinValue 5
>          ,SGOCycle]) []])

>     ,(TestStatement ansi2011
>       "create table t (a int generated always as identity\n\
>       \  ( start with -4 no maxvalue no minvalue no cycle ));"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"])
>         (Just $ IdentityColumnSpec GeneratedAlways
>          [SGOStartWith (-4)
>          ,SGONoMaxValue
>          ,SGONoMinValue
>          ,SGONoCycle]) []])

I think <common sequence generator options> is supposed to just
whitespace separated. In db2 it seems to be csv, but the grammar here
just seems to be whitespace separated, and it is just whitespace
separated in oracle... Not completely sure though. Usually db2 is
closer than oracle?

generated always (valueexpr)

<generation clause> ::=
  <generation rule> AS <generation expression>

<generation rule> ::=
  GENERATED ALWAYS

<generation expression> ::=
  <left paren> <value expression> <right paren>

>     ,(TestStatement ansi2011
>       "create table t (a int, \n\
>       \                a2 int generated always as (a * 2));"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableColumnDef $ ColumnDef (Name Nothing "a2") (TypeName [Name Nothing "int"])
>         (Just $ GenerationClause
>          (BinOp (Iden [Name Nothing "a"]) [Name Nothing "*"] (NumLit "2"))) []])



11.5 <default clause>

<default clause> ::=
  DEFAULT <default option>

<default option> ::=
    <literal>
  | <datetime value function>
  | USER
  | CURRENT_USER
  | CURRENT_ROLE
  | SESSION_USER
  | SYSTEM_USER
  | CURRENT_CATALOG
  | CURRENT_SCHEMA
  | CURRENT_PATH
  | <implicitly typed value specification>


>     ,(TestStatement ansi2011 "create table t (a int default 0);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"])
>         (Just $ DefaultClause $ NumLit "0") []])



11.6 <table constraint definition>

<table constraint definition> ::=
  [ <constraint name definition> ] <table constraint>
      [ <constraint characteristics> ]

<table constraint> ::=
    <unique constraint definition>
  | <referential constraint definition>
  | <check constraint definition>

11.7 <unique constraint definition>

<unique constraint definition> ::=
    <unique specification> <left paren> <unique column list> [ <comma> <without overlap
    specification> ] <right paren>
  | UNIQUE ( VALUE )

<unique specification> ::=
    UNIQUE
  | PRIMARY KEY

<unique column list> ::=
  <column name list>

>     ,(TestStatement ansi2011
>       "create table t (a int, unique (a));"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableConstraintDef Nothing $ TableUniqueConstraint [Name Nothing "a"]
>         ])

>     ,(TestStatement ansi2011
>       "create table t (a int, constraint a_unique unique (a));"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableConstraintDef (Just [Name Nothing "a_unique"]) $
>             TableUniqueConstraint [Name Nothing "a"]
>         ])

todo: test permutations of column defs and table constraints

>     ,(TestStatement ansi2011
>       "create table t (a int, b int, unique (a,b));"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableColumnDef $ ColumnDef (Name Nothing "b") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableConstraintDef Nothing $
>             TableUniqueConstraint [Name Nothing "a", Name Nothing "b"]
>         ])

>     ,(TestStatement ansi2011
>       "create table t (a int, b int, primary key (a,b));"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableColumnDef $ ColumnDef (Name Nothing "b") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableConstraintDef Nothing $
>             TablePrimaryKeyConstraint [Name Nothing "a", Name Nothing "b"]
>         ])


<without overlap specification> ::=
  <application time period name> WITHOUT OVERLAPS

defintely skip


11.8 <referential constraint definition>

<referential constraint definition> ::=
  FOREIGN KEY <left paren> <referencing column list>
      [ <comma> <referencing period specification> ] <right paren>
      <references specification>


>     ,(TestStatement ansi2011
>       "create table t (a int, b int,\n\
>       \                foreign key (a,b) references u(c,d) match full on update cascade on delete restrict );"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableColumnDef $ ColumnDef (Name Nothing "b") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableConstraintDef Nothing $
>             TableReferencesConstraint
>                 [Name Nothing "a", Name Nothing "b"]
>                 [Name Nothing "u"]
>                 (Just [Name Nothing "c", Name Nothing "d"])
>                 MatchFull RefCascade RefRestrict
>        ])

>     ,(TestStatement ansi2011
>       "create table t (a int,\n\
>       \                constraint tfku1 foreign key (a) references u);"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableConstraintDef (Just [Name Nothing "tfku1"]) $
>             TableReferencesConstraint
>                 [Name Nothing "a"]
>                 [Name Nothing "u"]
>                 Nothing DefaultReferenceMatch
>                 DefaultReferentialAction DefaultReferentialAction
>        ])

>     ,(TestStatement ansi2011
>       "create table t (a int, b int,\n\
>       \                foreign key (a) references u(c)\n\
>       \                foreign key (b) references v(d));"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableColumnDef $ ColumnDef (Name Nothing "b") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableConstraintDef Nothing $
>             TableReferencesConstraint
>                 [Name Nothing "a"]
>                 [Name Nothing "u"]
>                 (Just [Name Nothing "c"])
>                 DefaultReferenceMatch
>                 DefaultReferentialAction DefaultReferentialAction
>        ,TableConstraintDef Nothing $
>             TableReferencesConstraint
>                 [Name Nothing "b"]
>                 [Name Nothing "v"]
>                 (Just [Name Nothing "d"])
>                 DefaultReferenceMatch
>                 DefaultReferentialAction DefaultReferentialAction
>        ])


<references specification> ::=
  REFERENCES <referenced table and columns>
      [ MATCH <match type> ] [ <referential triggered action> ]

<match type> ::=
    FULL
  | PARTIAL
  | SIMPLE

<referencing column list> ::=
  <column name list>

<referencing period specification> ::=
  PERIOD <application time period name>

defintely skip

<referenced table and columns> ::=
  <table name> [ <left paren> <referenced column list>
      [ <comma> <referenced period specification> ] <right paren> ]

<referenced column list> ::=
  <column name list>

<referenced period specification> ::=
  PERIOD <application time period name>

defintely skip

<referential triggered action> ::=
    <update rule> [ <delete rule> ]
  | <delete rule> [ <update rule> ]

<update rule> ::=
  ON UPDATE <referential action>

<delete rule> ::=
  ON DELETE <referential action>

<referential action> ::=
    CASCADE
  | SET NULL
  | SET DEFAULT
  | RESTRICT
  | NO ACTION



11.9 <check constraint definition>

<check  constraint definition> ::=
     CHECK <left paren> <search condition> <right paren>

>     ,(TestStatement ansi2011
>       "create table t (a int, b int, \n\
>       \                check (a > b));"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableColumnDef $ ColumnDef (Name Nothing "b") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableConstraintDef Nothing $
>             TableCheckConstraint
>             (BinOp (Iden [Name Nothing "a"]) [Name Nothing ">"] (Iden [Name Nothing "b"]))
>        ])


>     ,(TestStatement ansi2011
>       "create table t (a int, b int, \n\
>       \                constraint agtb check (a > b));"
>      $ CreateTable [Name Nothing "t"]
>        [TableColumnDef $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableColumnDef $ ColumnDef (Name Nothing "b") (TypeName [Name Nothing "int"]) Nothing []
>        ,TableConstraintDef (Just [Name Nothing "agtb"]) $
>             TableCheckConstraint
>             (BinOp (Iden [Name Nothing "a"]) [Name Nothing ">"] (Iden [Name Nothing "b"]))
>        ])


TODO: lots more combos of table elements
and types and the other bits in a column def

11.10 <alter table statement>

<alter table statement> ::=
  ALTER TABLE <table name> <alter table action>

<alter table action> ::=
    <add column definition>
  | <alter column definition>
  | <drop column definition>
  | <add table constraint definition>
  | <alter table constraint definition>
  | <drop table constraint definition>
  | <add table period definition>
  | <drop table period definition>
  | <add system versioning clause>
  | <drop system versioning clause>

11.11 <add column definition>

<add column definition> ::=
  ADD [ COLUMN ] <column definition>

alter table t add column a int
alter table t add a int
alter table t add a int unique not null check (a>0)

>     ,(TestStatement ansi2011
>       "alter table t add column a int"
>      $ AlterTable [Name Nothing "t"] $ AddColumnDef
>        $ ColumnDef (Name Nothing "a") (TypeName [Name Nothing "int"]) Nothing []
>        )

todo: more add column

11.12 <alter column definition>

<alter column definition> ::=
  ALTER [ COLUMN ] <column name> <alter column action>

<alter column action> ::=
    <set column default clause>
  | <drop column default clause>
  | <set column not null clause>
  | <drop column not null clause>
  | <add column scope clause>
  | <drop column scope clause>
  | <alter column data type clause>
  | <alter identity column specification>
  | <drop identity property clause>
  | <drop column generation expression clause>


11.13 <set column default clause>

<set column default clause> ::=
  SET <default clause>


>     ,(TestStatement ansi2011
>       "alter table t alter column c set default 0"
>      $ AlterTable [Name Nothing "t"] $ AlterColumnSetDefault (Name Nothing "c")
>        $ NumLit "0")

11.14 <drop column default clause>

<drop column default clause> ::=
  DROP DEFAULT

>     ,(TestStatement ansi2011
>       "alter table t alter column c drop default"
>      $ AlterTable [Name Nothing "t"] $ AlterColumnDropDefault (Name Nothing "c"))


11.15 <set column not null clause>

<set column not null clause> ::=
  SET NOT NULL

>     ,(TestStatement ansi2011
>       "alter table t alter column c set not null"
>      $ AlterTable [Name Nothing "t"] $ AlterColumnSetNotNull (Name Nothing "c"))

11.16 <drop column not null clause>

<drop column not null clause> ::=
  DROP NOT NULL

>     ,(TestStatement ansi2011
>       "alter table t alter column c drop not null"
>      $ AlterTable [Name Nothing "t"] $ AlterColumnDropNotNull (Name Nothing "c"))

11.17 <add column scope clause>

<add column scope clause> ::=
  ADD <scope clause>

11.18 <drop column scope clause>

<drop column scope clause> ::=
  DROP SCOPE <drop behavior>

11.19 <alter column data type clause>

<alter column data type clause> ::=
  SET DATA TYPE <data type>

>     ,(TestStatement ansi2011
>       "alter table t alter column c set data type int;"
>      $ AlterTable [Name Nothing "t"] $
>        AlterColumnSetDataType (Name Nothing "c") (TypeName [Name Nothing "int"]))



11.20 <alter identity column specification>

<alter identity column specification> ::=
    <set identity column generation clause> [ <alter identity column option>... ]
  | <alter identity column option>...

<set identity column generation clause> ::=
  SET GENERATED { ALWAYS | BY DEFAULT }

so you have to write set generated for alter identity?
and you have to use always or by default

makes no sense: if you just want to restart you have to explicitly set
the always or by default? you can't just leave it unchanged?

you don't write as identity like with create table, this is wrong:

alter table t alter column c set generated always as identity

but these are ok?

alter table t alter column c set generated always

alter table t alter column c set generated by default

<alter identity column option> ::=
    <alter sequence generator restart option>
  | SET <basic sequence generator option>

alter table t alter column c set generated always restart
alter table t alter column c set generated always restart with 4

you can just write restart

but to write others you have to repeat set? each time?

alter table t alter column c set generated always set increment by 5 set minvalue 0 set maxvalue 5 set cycle restart with 5
(no set before the restart

in create table, it looks like this:

c int generated generated always as identity (increment by 5 minvalue 0 maxvalue 5 cycle restart with 5)

why gratuituous differences???

is there no way to do this:

alter table t alter column c set generated as (a * 3)
??

UPDATE: alter sequence uses same syntax as create sequence, which is
the same sytnax as identity in create table, so overrule the sql
standard and use the same syntax in alter identity.

PLAN: TODO

don't implement alter table alter column generated now

review the syntax for generated in db2, oracle, sql server, postgres, others?

observe which features are supported, and the consistency between
create table and alter table

try to find some people to ask if the standard really is this much of
a mess or I have misunderstood the grammer, or maybe there is a good
reason for the inconsistencies?


11.21 <drop identity property clause>

<drop identity property clause> ::=
  DROP IDENTITY

alter table t alter column c drop identity

included in the generated plan above

11.22 <drop column generation expression clause>

<drop column generation expression clause> ::=
  DROP EXPRESSION

alter table t alter column c drop expression

included in the generated plan above


11.23 <drop column definition>

<drop column definition> ::=
  DROP [ COLUMN ] <column name> <drop behavior>

>     ,(TestStatement ansi2011
>       "alter table t drop column c"
>      $ AlterTable [Name Nothing "t"] $
>        DropColumn (Name Nothing "c") DefaultDropBehaviour)

>     ,(TestStatement ansi2011
>       "alter table t drop c cascade"
>      $ AlterTable [Name Nothing "t"] $
>        DropColumn (Name Nothing "c") Cascade)

>     ,(TestStatement ansi2011
>       "alter table t drop c restrict"
>      $ AlterTable [Name Nothing "t"] $
>        DropColumn (Name Nothing "c") Restrict)



11.24 <add table constraint definition>

<add table constraint definition> ::=
  ADD <table constraint definition>

>     ,(TestStatement ansi2011
>       "alter table t add constraint c unique (a,b)"
>      $ AlterTable [Name Nothing "t"] $
>        AddTableConstraintDef (Just [Name Nothing "c"])
>             $ TableUniqueConstraint [Name Nothing "a", Name Nothing "b"])

>     ,(TestStatement ansi2011
>       "alter table t add unique (a,b)"
>      $ AlterTable [Name Nothing "t"] $
>        AddTableConstraintDef Nothing
>             $ TableUniqueConstraint [Name Nothing "a", Name Nothing "b"])


11.25 <alter table constraint definition>
<alter table constraint definition> ::=
  ALTER CONSTRAINT <constraint name> <constraint enforcement>

todo

11.26 <drop table constraint definition>

<drop table constraint definition> ::=
  DROP CONSTRAINT <constraint name> <drop behavior>

>     ,(TestStatement ansi2011
>       "alter table t drop constraint c"
>      $ AlterTable [Name Nothing "t"] $
>        DropTableConstraintDef [Name Nothing "c"] DefaultDropBehaviour)

>     ,(TestStatement ansi2011
>       "alter table t drop constraint c restrict"
>      $ AlterTable [Name Nothing "t"] $
>        DropTableConstraintDef [Name Nothing "c"] Restrict)

11.27 <add table period definition>

<add table period definition> ::=
  ADD <table period definition> [ <add system time period column list> ]

defintely skip

<add system time period column list> ::=
  ADD [ COLUMN ] <column definition 1> ADD [ COLUMN ] <column definition 2>

defintely skip

<column definition 1> ::=
  <column definition>

defintely skip

<column definition 2> ::=
  <column definition>

defintely skip

11.28 <drop table period definition>

<drop table period definition> ::=
  DROP <system or application time period specification> <drop behavior>

defintely skip

11.29 <add system versioning clause>

<add system versioning clause> ::=
  ADD <system versioning clause>

defintely skip

11.30 <drop system versioning clause>

<drop system versioning clause> ::=
  DROP SYSTEM VERSIONING <drop behavior>

defintely skip

11.31 <drop table statement>

<drop table statement> ::=
  DROP TABLE <table name> <drop behavior>

>     ,(TestStatement ansi2011
>       "drop table t"
>      $ DropTable [Name Nothing "t"] DefaultDropBehaviour)

>     ,(TestStatement ansi2011
>       "drop table t restrict"
>      $ DropTable [Name Nothing "t"] Restrict)


11.32 <view definition>

<view definition> ::=
  CREATE [ RECURSIVE ] VIEW <table name> <view specification>
      AS <query expression> [ WITH [ <levels clause> ] CHECK OPTION ]

<view specification> ::=
    <regular view specification>
  | <referenceable view specification>

<regular view specification> ::=
  [ <left paren> <view column list> <right paren> ]

<referenceable view specification> ::=
  OF <path-resolved user-defined type name> [ <subview clause> ]
      [ <view element list> ]

<subview clause> ::=
  UNDER <table name>

<view element list> ::=
  <left paren> <view element> [ { <comma> <view element> }... ] <right paren>

<view element> ::=
    <self-referencing column specification>
  | <view column option>

<view column option> ::=
  <column name> WITH OPTIONS <scope clause>

<levels clause> ::=
    CASCADED
  | LOCAL

<view column list> ::=
  <column name list>

>     ,(TestStatement ansi2011
>       "create view v as select * from t"
>      $ CreateView False [Name Nothing "v"] Nothing (makeSelect
>          {qeSelectList = [(Star, Nothing)]
>          ,qeFrom = [TRSimple [Name Nothing "t"]]
>          }) Nothing)


>     ,(TestStatement ansi2011
>       "create recursive view v as select * from t"
>      $ CreateView True [Name Nothing "v"] Nothing (makeSelect
>          {qeSelectList = [(Star, Nothing)]
>          ,qeFrom = [TRSimple [Name Nothing "t"]]
>          }) Nothing)

>     ,(TestStatement ansi2011
>       "create view v(a,b) as select * from t"
>      $ CreateView False [Name Nothing "v"] (Just [Name Nothing "a", Name Nothing "b"])
>          (makeSelect
>          {qeSelectList = [(Star, Nothing)]
>          ,qeFrom = [TRSimple [Name Nothing "t"]]
>          }) Nothing)


>     ,(TestStatement ansi2011
>       "create view v as select * from t with check option"
>      $ CreateView False [Name Nothing "v"] Nothing (makeSelect
>          {qeSelectList = [(Star, Nothing)]
>          ,qeFrom = [TRSimple [Name Nothing "t"]]
>          }) (Just DefaultCheckOption))

>     ,(TestStatement ansi2011
>       "create view v as select * from t with cascaded check option"
>      $ CreateView False [Name Nothing "v"] Nothing (makeSelect
>          {qeSelectList = [(Star, Nothing)]
>          ,qeFrom = [TRSimple [Name Nothing "t"]]
>          }) (Just CascadedCheckOption))

>     ,(TestStatement ansi2011
>       "create view v as select * from t with local check option"
>      $ CreateView False [Name Nothing "v"] Nothing
>          (makeSelect
>          {qeSelectList = [(Star, Nothing)]
>          ,qeFrom = [TRSimple [Name Nothing "t"]]
>          }) (Just LocalCheckOption))


11.33 <drop view statement>

<drop view statement> ::=
  DROP VIEW <table name> <drop behavior>


>     ,(TestStatement ansi2011
>       "drop view v"
>      $ DropView [Name Nothing "v"] DefaultDropBehaviour)

>     ,(TestStatement ansi2011
>       "drop view v cascade"
>      $ DropView [Name Nothing "v"] Cascade)


11.34 <domain definition>

<domain definition> ::=
  CREATE DOMAIN <domain name> [ AS ] <predefined type>
      [ <default clause> ]
      [ <domain constraint>... ]
      [ <collate clause> ]

<domain constraint> ::=
  [ <constraint name definition> ] <check constraint definition> [
      <constraint characteristics> ]

>     ,(TestStatement ansi2011
>       "create domain my_int int"
>      $ CreateDomain [Name Nothing "my_int"]
>           (TypeName [Name Nothing "int"])
>           Nothing [])

>     ,(TestStatement ansi2011
>       "create domain my_int as int"
>      $ CreateDomain [Name Nothing "my_int"]
>           (TypeName [Name Nothing "int"])
>           Nothing [])

>     ,(TestStatement ansi2011
>       "create domain my_int int default 0"
>      $ CreateDomain [Name Nothing "my_int"]
>           (TypeName [Name Nothing "int"])
>           (Just (NumLit "0")) [])

>     ,(TestStatement ansi2011
>       "create domain my_int int check (value > 5)"
>      $ CreateDomain [Name Nothing "my_int"]
>           (TypeName [Name Nothing "int"])
>           Nothing [(Nothing
>                    ,BinOp (Iden [Name Nothing "value"]) [Name Nothing ">"] (NumLit "5"))])

>     ,(TestStatement ansi2011
>       "create domain my_int int constraint gt5 check (value > 5)"
>      $ CreateDomain [Name Nothing "my_int"]
>           (TypeName [Name Nothing "int"])
>           Nothing [(Just [Name Nothing "gt5"]
>                    ,BinOp (Iden [Name Nothing "value"]) [Name Nothing ">"] (NumLit "5"))])



11.35 <alter domain statement>

<alter domain statement> ::=
  ALTER DOMAIN <domain name> <alter domain action>

<alter domain action> ::=
    <set domain default clause>
  | <drop domain default clause>
  | <add domain constraint definition>
  | <drop domain constraint definition>

11.36 <set domain default clause>

<set domain default clause> ::=
  SET <default clause>

>     ,(TestStatement ansi2011
>       "alter domain my_int set default 0"
>      $ AlterDomain [Name Nothing "my_int"]
>        $ ADSetDefault $ NumLit "0")


11.37 <drop domain default clause>

<drop domain default clause> ::=
  DROP DEFAULT

>     ,(TestStatement ansi2011
>       "alter domain my_int drop default"
>      $ AlterDomain [Name Nothing "my_int"]
>        $ ADDropDefault)


11.38 <add domain constraint definition>

<add domain constraint definition> ::=
  ADD <domain constraint>

>     ,(TestStatement ansi2011
>       "alter domain my_int add check (value > 6)"
>      $ AlterDomain [Name Nothing "my_int"]
>        $ ADAddConstraint Nothing
>          $ BinOp (Iden [Name Nothing "value"]) [Name Nothing ">"] (NumLit "6"))

>     ,(TestStatement ansi2011
>       "alter domain my_int add constraint gt6 check (value > 6)"
>      $ AlterDomain [Name Nothing "my_int"]
>        $ ADAddConstraint (Just [Name Nothing "gt6"])
>          $ BinOp (Iden [Name Nothing "value"]) [Name Nothing ">"] (NumLit "6"))


11.39 <drop domain constraint definition>

<drop domain constraint definition> ::=
  DROP CONSTRAINT <constraint name>

>     ,(TestStatement ansi2011
>       "alter domain my_int drop constraint gt6"
>      $ AlterDomain [Name Nothing "my_int"]
>        $ ADDropConstraint [Name Nothing "gt6"])

11.40 <drop domain statement>

<drop domain statement> ::=
  DROP DOMAIN <domain name> <drop behavior>

>     ,(TestStatement ansi2011
>       "drop domain my_int"
>      $ DropDomain [Name Nothing "my_int"] DefaultDropBehaviour)

>     ,(TestStatement ansi2011
>       "drop domain my_int cascade"
>      $ DropDomain [Name Nothing "my_int"] Cascade)



11.41 <character set definition>

<character set definition> ::=
  CREATE CHARACTER SET <character set name> [ AS ]
      <character set source> [ <collate clause> ]

<character set source> ::=
  GET <character set specification>

11.42 <drop character set statement>

<drop character set statement> ::=
  DROP CHARACTER SET <character set name>

11.43 <collation definition>

<collation definition> ::=
  CREATE COLLATION <collation name> FOR <character set specification>
      FROM <existing collation name> [ <pad characteristic> ]

<existing collation name> ::=
  <collation name>

<pad characteristic> ::=
    NO PAD
  | PAD SPACE

11.44 <drop collation statement>

<drop collation statement> ::=
  DROP COLLATION <collation name> <drop behavior>

11.45 <transliteration definition>

<transliteration definition> ::=
  CREATE TRANSLATION <transliteration name> FOR <source character set specification>
      TO <target character set specification> FROM <transliteration source>

<source character set specification> ::=
  <character set specification>

<target character set specification> ::=
  <character set specification>

<transliteration source> ::=
    <existing transliteration name>
  | <transliteration routine>

<existing transliteration name> ::=
  <transliteration name>

<transliteration routine> ::=
  <specific routine designator>

11.46 <drop transliteration statement>

<drop transliteration statement> ::=
  DROP TRANSLATION <transliteration name>

11.47 <assertion definition>

<assertion definition> ::=
     CREATE ASSERTION <constraint name>
         CHECK <left paren> <search condition> <right paren>
         [ <constraint characteristics> ]

>     ,(TestStatement ansi2011
>       "create assertion t1_not_empty CHECK ((select count(*) from t1) > 0);"
>      $ CreateAssertion [Name Nothing "t1_not_empty"]
>         $ BinOp (SubQueryExpr SqSq $
>                  makeSelect
>                  {qeSelectList = [(App [Name Nothing "count"] [Star],Nothing)]
>                  ,qeFrom = [TRSimple [Name Nothing "t1"]]
>                  })
>                 [Name Nothing ">"] (NumLit "0"))

11.48 <drop assertion statement>

<drop assertion statement> ::=
  DROP ASSERTION <constraint name> [ <drop behavior> ]

>     ,(TestStatement ansi2011
>       "drop assertion t1_not_empty;"
>      $ DropAssertion [Name Nothing "t1_not_empty"] DefaultDropBehaviour)

>     ,(TestStatement ansi2011
>       "drop assertion t1_not_empty cascade;"
>      $ DropAssertion [Name Nothing "t1_not_empty"] Cascade)


11.49 <trigger definition>

<trigger definition> ::=
  CREATE TRIGGER <trigger name> <trigger action time> <trigger event>
      ON <table name> [ REFERENCING <transition table or variable list> ]
      <triggered action>

<trigger action time> ::=
    BEFORE
  | AFTER
  | INSTEAD OF

<trigger event> ::=
    INSERT
  | DELETE
  | UPDATE [ OF <trigger column list> ]

<trigger column list> ::=
  <column name list>

<triggered action> ::=
  [ FOR EACH { ROW | STATEMENT } ]
      [ <triggered when clause> ]
      <triggered SQL statement>

<triggered when clause> ::=
  WHEN <left paren> <search condition> <right paren>

<triggered SQL statement> ::=
    <SQL procedure statement>
  | BEGIN ATOMIC { <SQL procedure statement> <semicolon> }... END

<transition table or variable list> ::=
  <transition table or variable>...

<transition table or variable> ::=
    OLD [ ROW ] [ AS     ] <old transition variable name>
  | NEW [ ROW ] [ AS     ] <new transition variable name>
  | OLD TABLE [ AS ]     <old transition table name>
  | NEW TABLE [ AS ]     <new transition table name>

<old transition table name> ::=
  <transition table name>

<new transition table name> ::=
  <transition table name>

<transition table name> ::=
  <identifier>

<old transition variable name> ::=
  <correlation name>

<new transition variable name> ::=
  <correlation name>

11.50 <drop trigger statement>

<drop trigger statement> ::=
  DROP TRIGGER <trigger name>

11.51 <user-defined type definition>

<user-defined type definition> ::=
  CREATE TYPE <user-defined type body>

 <user-defined type body> ::=
  <schema-resolved user-defined type name>
      [ <subtype clause> ]
      [ AS <representation> ]
      [ <user-defined type option list> ]
      [ <method specification list> ]

<user-defined type option list> ::=
  <user-defined type option> [ <user-defined type option>... ]

<user-defined type option> ::=
    <instantiable clause>
  | <finality>
  | <reference type specification>
  | <cast to ref>
  | <cast to type>
  | <cast to distinct>
  | <cast to source>

<subtype clause> ::=
  UNDER <supertype name>

<supertype name> ::=
  <path-resolved user-defined type name>

<representation> ::=
    <predefined type>
  | <collection type>
  | <member list>

<member list> ::=
  <left paren> <member> [ { <comma> <member> }... ] <right paren>

<member> ::=
  <attribute definition>

<instantiable clause> ::=
    INSTANTIABLE
  | NOT INSTANTIABLE

<finality> ::=
    FINAL
  | NOT FINAL

<reference type specification> ::=
    <user-defined representation>
  | <derived representation>
  | <system-generated representation>

<user-defined representation> ::=
  REF USING <predefined type>

<derived representation> ::=
  REF FROM <list of attributes>

<system-generated representation> ::=
  REF IS SYSTEM GENERATED

<cast to ref> ::=
  CAST <left paren> SOURCE AS REF <right paren> WITH <cast to ref identifier>

<cast to ref identifier> ::=
  <identifier>

<cast to type> ::=
  CAST <left paren> REF AS SOURCE <right paren> WITH <cast to type identifier>

<cast to type identifier> ::=
  <identifier>

<list of attributes> ::=
  <left paren> <attribute name> [ { <comma> <attribute name> }... ] <right paren>

<cast to distinct> ::=
  CAST <left paren> SOURCE AS DISTINCT <right paren>
      WITH <cast to distinct identifier>

<cast to distinct identifier> ::=
  <identifier>

<cast to source> ::=
  CAST <left paren> DISTINCT AS SOURCE <right paren>
      WITH <cast to source identifier>

<cast to source identifier> ::=
  <identifier>

<method specification list> ::=
  <method specification> [ { <comma> <method specification> }... ]

<method specification> ::=
    <original method specification>
  | <overriding method specification>

<original method specification> ::=
  <partial method specification> [ SELF AS RESULT ] [ SELF AS LOCATOR ]
      [ <method characteristics> ]

<overriding method specification> ::=
  OVERRIDING <partial method specification>
                                                                                1<partial method specification> ::=
  [ INSTANCE | STATIC | CONSTRUCTOR ]
      METHOD <method name> <SQL parameter declaration list>
      <returns clause>
      [ SPECIFIC <specific method name> ]

<specific method name> ::=
  [ <schema name> <period> ] <qualified identifier>

<method characteristics> ::=
  <method characteristic>...

 <method characteristic> ::=
         <language clause>
     |   <parameter style clause>
     |   <deterministic characteristic>
     |   <SQL-data access indication>
     |   <null-call clause>

11.52 <attribute definition>

<attribute definition> ::=
     <attribute name> <data type>
         [ <attribute default> ]
         [ <collate clause> ]

<attribute default> ::=
  <default clause>

11.53 <alter type statement>

<alter type statement> ::=
  ALTER TYPE <schema-resolved user-defined type name> <alter type action>

<alter type action> ::=
    <add attribute definition>
  | <drop attribute definition>
  | <add original method specification>
  | <add overriding method specification>
  | <drop method specification>

11.54 <add attribute definition>

<add attribute definition> ::=
  ADD ATTRIBUTE <attribute definition>

11.55 <drop attribute definition>

<drop attribute definition> ::=
  DROP ATTRIBUTE <attribute name> RESTRICT

11.56 <add original method specification>

<add original method specification> ::=
  ADD <original method specification>

11.57 <add overriding method specification>

<add overriding method specification> ::=
  ADD <overriding method specification>

11.58 <drop method specification>

<drop method specification> ::=
  DROP <specific method specification designator> RESTRICT

<specific method specification designator> ::=
  [ INSTANCE | STATIC | CONSTRUCTOR ]
      METHOD <method name> <data type list>

11.59 <drop data type statement>

<drop data type statement> ::=
  DROP TYPE <schema-resolved user-defined type name> <drop behavior>

11.60 <SQL-invoked routine>

<SQL-invoked routine> ::=
  <schema routine>

<schema routine> ::=
    <schema procedure>
  | <schema function>

<schema procedure> ::=
  CREATE <SQL-invoked procedure>

<schema function> ::=
  CREATE <SQL-invoked function>

<SQL-invoked procedure> ::=
  PROCEDURE <schema qualified routine name> <SQL parameter declaration list>
      <routine characteristics>
      <routine body>

<SQL-invoked function> ::=
  { <function specification> | <method specification designator> } <routine body>

<SQL parameter declaration list> ::=
  <left paren> [ <SQL parameter declaration>
      [ { <comma> <SQL parameter declaration> }... ] ] <right paren>

<SQL parameter declaration> ::=
  [ <parameter mode> ]
      [ <SQL parameter name> ]
      <parameter type> [ RESULT ]
      [ DEFAULT <parameter default> ]

<parameter default> ::=
    <value expression>
  | <contextually typed value specification>

<parameter mode> ::=
    IN
  | OUT
  | INOUT

<parameter type> ::=
  <data type> [ <locator indication> ]

<locator indication> ::=
  AS LOCATOR

<function specification> ::=
  FUNCTION <schema qualified routine name> <SQL parameter declaration list>
      <returns clause>
      <routine characteristics>
      [ <dispatch clause> ]

<method specification designator> ::=
    SPECIFIC METHOD <specific method name>
  | [ INSTANCE | STATIC | CONSTRUCTOR ]
      METHOD <method name> <SQL parameter declaration list>
      [ <returns clause> ]
      FOR <schema-resolved user-defined type name>

<routine characteristics> ::=
  [ <routine characteristic>... ]

<routine characteristic> ::=
    <language clause>
  | <parameter style clause>
  | SPECIFIC <specific name>
  | <deterministic characteristic>
  | <SQL-data access indication>
  | <null-call clause>
  | <returned result sets characteristic>
  | <savepoint level indication>

<savepoint level indication> ::=
    NEW SAVEPOINT LEVEL
  | OLD SAVEPOINT LEVEL

<returned result sets characteristic> ::=
  DYNAMIC RESULT SETS <maximum returned result sets>

<parameter style clause> ::=
  PARAMETER STYLE <parameter style>

<dispatch clause> ::=
  STATIC DISPATCH

<returns clause> ::=
  RETURNS <returns type>

<returns type> ::=
    <returns data type> [ <result cast> ]
  | <returns table type>

<returns table type> ::=
  TABLE <table function column list>

<table function column list> ::=
  <left paren> <table function column list element>
      [ { <comma> <table function column list element> }... ] <right paren>

<table function column list element> ::=
  <column name> <data type>

<result cast> ::=
  CAST FROM <result cast from type>

<result cast from type> ::=
  <data type> [ <locator indication> ]

<returns data type> ::=
  <data type> [ <locator indication> ]

<routine body> ::=
    <SQL routine spec>
  | <external body reference>

<SQL routine spec> ::=
  [ <rights clause> ] <SQL routine body>

<rights clause> ::=
    SQL SECURITY INVOKER
  | SQL SECURITY DEFINER

<SQL routine body> ::=
  <SQL procedure statement>

<external body reference> ::=
  EXTERNAL [ NAME <external routine name> ]
      [ <parameter style clause> ]
      [ <transform group specification> ]
      [ <external security clause> ]

<external security clause> ::=
    EXTERNAL SECURITY DEFINER
  | EXTERNAL SECURITY INVOKER
  | EXTERNAL SECURITY IMPLEMENTATION DEFINED

<parameter style> ::=
    SQL
  | GENERAL

<deterministic characteristic> ::=
    DETERMINISTIC
  | NOT DETERMINISTIC

<SQL-data access indication> ::=
    NO SQL
  | CONTAINS SQL
  | READS SQL DATA
  | MODIFIES SQL DATA

<null-call clause> ::=
    RETURNS NULL ON NULL INPUT
  | CALLED ON NULL INPUT

<maximum returned result sets> ::=
  <unsigned integer>

<transform group specification> ::=
  TRANSFORM GROUP { <single group specification> | <multiple group specification> }

<single group specification> ::=
  <group name>

<multiple group specification> ::=
  <group specification> [ { <comma> <group specification> }... ]

<group specification> ::=
  <group name> FOR TYPE <path-resolved user-defined type name>

11.61 <alter routine statement>

<alter routine statement> ::=
  ALTER <specific routine designator>
      <alter routine characteristics> <alter routine behavior>

<alter routine characteristics> ::=
  <alter routine characteristic>...

<alter routine characteristic> ::=
    <language clause>
  | <parameter style clause>
  | <SQL-data access indication>
  | <null-call clause>
  | <returned result sets characteristic>
  | NAME <external routine name>

<alter routine behavior> ::=
  RESTRICT

11.62 <drop routine statement>

<drop routine statement> ::=
  DROP <specific routine designator> <drop behavior>

11.63 <user-defined cast definition>

<user-defined cast definition> ::=
  CREATE CAST <left paren> <source data type> AS <target data type> <right paren>
      WITH <cast function>
      [ AS ASSIGNMENT ]

<cast function> ::=
  <specific routine designator>

<source data type> ::=
  <data type>

<target data type> ::=
  <data type>

11.64 <drop user-defined cast statement>

<drop user-defined cast statement> ::=
  DROP CAST <left paren> <source data type> AS <target data type> <right paren>
      <drop behavior>

11.65 <user-defined ordering definition>

<user-defined ordering definition> ::=
  CREATE ORDERING FOR <schema-resolved user-defined type name> <ordering form>

<ordering form> ::=
    <equals ordering form>
  | <full ordering form>

<equals ordering form> ::=
  EQUALS ONLY BY <ordering category>

<full ordering form> ::=
  ORDER FULL BY <ordering category>

<ordering category> ::=
    <relative category>
  | <map category>
  | <state category>

<relative category> ::=
  RELATIVE WITH <relative function specification>

<map category> ::=
  MAP WITH <map function specification>

<state category> ::=
  STATE [ <specific name> ]

<relative function specification> ::=
  <specific routine designator>

<map function specification> ::=
  <specific routine designator>

11.66 <drop user-defined ordering statement>

<drop user-defined ordering statement> ::=
  DROP ORDERING FOR <schema-resolved user-defined type name> <drop behavior>

11.67 <transform definition>

<transform definition> ::=
  CREATE { TRANSFORM | TRANSFORMS } FOR
      <schema-resolved user-defined type name> <transform group>...

<transform group> ::=
  <group name> <left paren> <transform element list> <right paren>

<group name> ::=
  <identifier>

<transform element list> ::=
  <transform element> [ <comma> <transform element> ]

<transform element> ::=
    <to sql>
  | <from sql>

<to sql> ::=
  TO SQL WITH <to sql function>

<from sql> ::=
  FROM SQL WITH <from sql function>

<to sql function> ::=
  <specific routine designator>

<from sql function> ::=
  <specific routine designator>

11.68 <alter transform statement>

<alter transform statement> ::=
  ALTER { TRANSFORM | TRANSFORMS }
      FOR <schema-resolved user-defined type name> <alter group>...

<alter group> ::=
  <group name> <left paren> <alter transform action list> <right paren>

<alter transform action list> ::=
  <alter transform action> [ { <comma> <alter transform action> }... ]

<alter transform action> ::=
    <add transform element list>
  | <drop transform element list>

11.69 <add transform element list>

<add transform element list> ::=
  ADD <left paren> <transform element list> <right paren>

11.70 <drop transform element list>

<drop transform element list> ::=
  DROP <left paren> <transform kind>
      [ <comma> <transform kind> ] <drop behavior> <right paren>

<transform kind> ::=
    TO SQL
  | FROM SQL

11.71 <drop transform statement>

<drop transform statement> ::=
  DROP { TRANSFORM | TRANSFORMS } <transforms to be dropped>
      FOR <schema-resolved user-defined type name> <drop behavior>

<transforms to be dropped> ::=
    ALL
  | <transform group element>

<transform group element> ::=
  <group name>

11.72 <sequence generator definition>

<sequence generator definition> ::=
  CREATE SEQUENCE <sequence generator name> [ <sequence generator options> ]

<sequence generator options> ::=
  <sequence generator option>...

<sequence generator option> ::=
    <sequence generator data type option>
  | <common sequence generator options>

<common sequence generator options> ::=
  <common sequence generator option>...

<common sequence generator option> ::=
    <sequence generator start with option>
  | <basic sequence generator option>

<basic sequence generator option> ::=
    <sequence generator increment by option>
  | <sequence generator maxvalue option>
  | <sequence generator minvalue option>
  | <sequence generator cycle option>

<sequence generator data type option> ::=
  AS <data type>

<sequence generator start with option> ::=
  START WITH <sequence generator start value>

<sequence generator start value> ::=
  <signed numeric literal>

<sequence generator increment by option> ::=
  INCREMENT BY <sequence generator increment>

<sequence generator increment> ::=
  <signed numeric literal>

<sequence generator maxvalue option> ::=
    MAXVALUE <sequence generator max value>
  | NO MAXVALUE

<sequence generator max value> ::=
  <signed numeric literal>

<sequence generator minvalue option> ::=
    MINVALUE <sequence generator min value>
  | NO MINVALUE

<sequence generator min value> ::=
  <signed numeric literal>

<sequence generator cycle option> ::=
    CYCLE
  | NO CYCLE

>     ,(TestStatement ansi2011
>       "create sequence seq"
>      $ CreateSequence [Name Nothing "seq"] [])

>     ,(TestStatement ansi2011
>       "create sequence seq as bigint"
>      $ CreateSequence [Name Nothing "seq"]
>         [SGODataType $ TypeName [Name Nothing "bigint"]])

>     ,(TestStatement ansi2011
>       "create sequence seq as bigint start with 5"
>      $ CreateSequence [Name Nothing "seq"]
>         [SGOStartWith 5
>         ,SGODataType $ TypeName [Name Nothing "bigint"]
>         ])


11.73 <alter sequence generator statement>

<alter sequence generator statement> ::=
  ALTER SEQUENCE <sequence generator name> <alter sequence generator options>

<alter sequence generator options> ::=
  <alter sequence generator option>...

<alter sequence generator option> ::=
    <alter sequence generator restart option>
  | <basic sequence generator option>

<alter sequence generator restart option> ::=
  RESTART [ WITH <sequence generator restart value> ]

<sequence generator restart value> ::=
  <signed numeric literal>

>     ,(TestStatement ansi2011
>       "alter sequence seq restart"
>      $ AlterSequence [Name Nothing "seq"]
>         [SGORestart Nothing])

>     ,(TestStatement ansi2011
>       "alter sequence seq restart with 5"
>      $ AlterSequence [Name Nothing "seq"]
>         [SGORestart $ Just 5])

>     ,(TestStatement ansi2011
>       "alter sequence seq restart with 5 increment by 5"
>      $ AlterSequence [Name Nothing "seq"]
>         [SGORestart $ Just 5
>         ,SGOIncrementBy 5])


11.74 <drop sequence generator statement>

<drop sequence generator statement> ::=
  DROP SEQUENCE <sequence generator name> <drop behavior>

>     ,(TestStatement ansi2011
>       "drop sequence seq"
>      $ DropSequence [Name Nothing "seq"] DefaultDropBehaviour)

>     ,(TestStatement ansi2011
>       "drop sequence seq restrict"
>      $ DropSequence [Name Nothing "seq"] Restrict)


>     ]
