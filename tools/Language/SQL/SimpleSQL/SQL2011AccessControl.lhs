
Section 12 in Foundation

grant, etc


> module Language.SQL.SimpleSQL.SQL2011AccessControl (sql2011AccessControlTests) where

> import Language.SQL.SimpleSQL.TestTypes

> sql2011AccessControlTests :: TestItem
> sql2011AccessControlTests = Group "sql 2011 access control tests" []

12 Access control

12.1 <grant statement>

<grant statement> ::=
    <grant privilege statement>
  | <grant role statement>

12.2 <grant privilege statement>

<grant privilege statement> ::=
  GRANT <privileges> TO <grantee> [ { <comma> <grantee> }... ]
      [ WITH HIERARCHY OPTION ]
      [ WITH GRANT OPTION ]
      [ GRANTED BY <grantor> ]

12.3 <privileges>
<privileges> ::=
  <object privileges> ON <object name>

<object name> ::=
      [ TABLE ] <table name>
  |   DOMAIN <domain name>
  |   COLLATION <collation name>
  |   CHARACTER SET <character set name>
  |   TRANSLATION <transliteration name>
  |   TYPE <schema-resolved user-defined type name>
  |   SEQUENCE <sequence generator name>
  |   <specific routine designator>

<object privileges> ::=
    ALL PRIVILEGES
  | <action> [ { <comma> <action> }... ]

<action> ::=
    SELECT
  | SELECT <left paren> <privilege column list> <right paren>
  | SELECT <left paren> <privilege method list> <right paren>
  | DELETE
  | INSERT [ <left paren> <privilege column list> <right paren> ]
  | UPDATE [ <left paren> <privilege column list> <right paren> ]
  | REFERENCES [ <left paren> <privilege column list> <right paren> ]
  | USAGE
  | TRIGGER
  | UNDER
  | EXECUTE

<privilege method list> ::=
  <specific routine designator> [ { <comma> <specific routine designator> }... ]

<privilege column list> ::=
  <column name list>

<grantee> ::=
    PUBLIC
  | <authorization identifier>

<grantor> ::=
    CURRENT_USER
  | CURRENT_ROLE

12.4 <role definition>

<role definition> ::=
  CREATE ROLE <role name> [ WITH ADMIN <grantor> ]

12.5 <grant role statement>

<grant role statement> ::=
  GRANT <role granted> [ { <comma> <role granted> }... ]
      TO <grantee> [ { <comma> <grantee> }... ]
      [ WITH ADMIN OPTION ]
      [ GRANTED BY <grantor> ]

<role granted> ::=
  <role name>

12.6 <drop role statement>

<drop role statement> ::=
  DROP ROLE <role name>

12.7 <revoke statement>

<revoke statement> ::=
    <revoke privilege statement>
  | <revoke role statement>

<revoke privilege statement> ::=
  REVOKE [ <revoke option extension> ] <privileges>
      FROM <grantee> [ { <comma> <grantee> }... ]
      [ GRANTED BY <grantor> ]
      <drop behavior>

<revoke option extension> ::=
    GRANT OPTION FOR
  | HIERARCHY OPTION FOR

<revoke role statement> ::=
  REVOKE [ ADMIN OPTION FOR ] <role revoked> [ { <comma> <role revoked> }... ]
      FROM <grantee> [ { <comma> <grantee> }... ]
      [ GRANTED BY <grantor> ]
      <drop behavior>

<role revoked> ::=
  <role name>
