# Overview

This page has more details on the supported SQL in simple-sql-parser.

See the [simple-sql-parser test cases](test_cases.html) page for
examples.

The target dialect of SQL at this time is ISO/ANSI SQL:2011. The
parser supports queries, DDL, non-query DML, access control and
transaction management syntax. The parser and syntax does not follow
the standard grammar closely - they permit a lot of things which the
grammar in the standard forbids. The intended usage is that an
additional pass over the ast can be made if you want to carefully
prohibit everything that the standard doesn't allow.

Apart from this permissiveness, some work has been put into trying to
get good parser error messages.

# Queries

## Select lists

Supports scalar expressions, aliases with optional 'as'.

## Set quantifiers on select

Supports 'select distinct' and explicit 'select all'.

## From clause

* aliases
* subqueries
* functions
* joins
  - natural
  - inner
  - left/right/full outer
  - cross
  - on expressions
  - using lists
  - lateral

## Group by clause

Supports scalar expressions, group by (), cube, rollup, grouping
parentheses and grouping sets with nested grouping expressions.

## Order by clause

Supports scalar expressions, asc/desc and nulls first/last.

## Offset and fetch

Supports 'offset n rows' and 'fetch first n rows only'.

## Set operators

Union, except, intersect + all/distinct and corresponding.

## Table value constructor

Example: 'values (1,2),(3,4)'.

## Explicit table

Example: 'table t', which is shorthand for 'select * from t'.

## Scalar expressions

The scalar expressions type and parser is used in many contexts,
including:

* select lists;
* where clause expressions;
* group by clause expressions;
* having clause expressions;
* order by clause expressions;
* offset and fetch clause expressions;
* table value constructors.

This doesn't exactly follow the ANSI Standards, which have separate
grammars for most of these.

The supported scalar expressions include:

* basic string literals in single quotes
* number literals: digits.digitse+-exp
* explicitly typed literal, e.g. int '3'
* binary operators
  - comparisons: = != <> <= >= < >
  - arithmetic: + - / * % ^
  - logic: and, or
  - bitwise: & | (and ^ as above)
  - string: ||, like, not like
  - other: overlaps, is similar to, is not similar too, is distinct
    from, is not distinct from
* prefix unary operators
  - +, -
  - not
  - ~
* postfix unary
  - is null, is not null
  - is true, is not true, is false, is not false, is unknown, is not unknown
* other operators
  - extract (extract(day from dt))
  - position (position string1 in string2)
  - substring (substring(x from 2 for 4))
  - convert (convert(string using conversion))
  - translate (translate(string using translation))
  - overlay (overlay (string placing embedded_string from start for
    length))
  - trim (trim(leading '_' from s))
  - between (a between 1 and 5)
  - in list (a in (1,2,3,4))
  - cast (cast(a as int))
* subqueries
  - in subquery
  - any/some/all
  - exists
* case expressions
* parentheses
* quoted and unquoted identifiers
* a.b qualified identifiers
* \*, a.*
* functions: f(a,b)
* aggregates: agg(distinct a order by b)
* window functions: sum(x) over (partition by y order by z)
  plus some explicit frame support (same as in postgres 9.3)
* row constructors, e.g. where (a,b) = any (select a,b from t)
* ? used in parameterized queries

# DDL

* schemas
  * create, drop + drop restrict

* tables
  * create table
    * constraints: named, null, unique, primary key, foreign key (matches, on update/delete)
    * identity (the weird ansi version), defaults
    * defaults
  * alter table
    * defaults, null, set data type, drop column, constraints
  * drop table + restrict

* create, drop view
* create, alter, drop domain
  * defaults, constraints
* create, drop assertion
* create, alter, drop sequence

# Non-query DML

* delete
  * delete from
  * as alias
  * where
* truncate
  * with identity options
* insert
  * values, general queries, defaults
* update
  * including row updates

# Access Control

* grant privileges
  * all, grant option, table, domain, type, sequence, role, etc.
* revoke
* create role, drop role

# Transaction management

* begin, commit, rollback
* savepoints
