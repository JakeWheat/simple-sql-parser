
{-
Section 12 in Foundation

grant, etc
-}


{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.SQL2011AccessControl (sql2011AccessControlTests) where

import Language.SQL.SimpleSQL.TestTypes
import Language.SQL.SimpleSQL.Syntax

sql2011AccessControlTests :: TestItem
sql2011AccessControlTests = Group "sql 2011 access control tests" [

{-
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
-}

     (TestStatement ansi2011
      "grant all privileges on tbl1 to role1"
     $ GrantPrivilege [PrivAll]
       (PrivTable [Name Nothing "tbl1"])
       [Name Nothing "role1"] WithoutGrantOption)


    ,(TestStatement ansi2011
      "grant all privileges on tbl1 to role1,role2"
     $ GrantPrivilege [PrivAll]
       (PrivTable [Name Nothing "tbl1"])
       [Name Nothing "role1",Name Nothing "role2"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant all privileges on tbl1 to role1 with grant option"
     $ GrantPrivilege [PrivAll]
       (PrivTable [Name Nothing "tbl1"])
       [Name Nothing "role1"] WithGrantOption)

    ,(TestStatement ansi2011
      "grant all privileges on table tbl1 to role1"
     $ GrantPrivilege [PrivAll]
       (PrivTable [Name Nothing "tbl1"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant all privileges on domain mydom to role1"
     $ GrantPrivilege [PrivAll]
       (PrivDomain [Name Nothing "mydom"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant all privileges on type t1 to role1"
     $ GrantPrivilege [PrivAll]
       (PrivType [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant all privileges on sequence s1 to role1"
     $ GrantPrivilege [PrivAll]
       (PrivSequence [Name Nothing "s1"])
       [Name Nothing "role1"] WithoutGrantOption)


    ,(TestStatement ansi2011
      "grant select on table t1 to role1"
     $ GrantPrivilege [PrivSelect []]
       (PrivTable [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant select(a,b) on table t1 to role1"
     $ GrantPrivilege [PrivSelect [Name Nothing "a", Name Nothing "b"]]
       (PrivTable [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant delete on table t1 to role1"
     $ GrantPrivilege [PrivDelete]
       (PrivTable [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant insert on table t1 to role1"
     $ GrantPrivilege [PrivInsert []]
       (PrivTable [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant insert(a,b) on table t1 to role1"
     $ GrantPrivilege [PrivInsert [Name Nothing "a", Name Nothing "b"]]
       (PrivTable [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant update on table t1 to role1"
     $ GrantPrivilege [PrivUpdate []]
       (PrivTable [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant update(a,b) on table t1 to role1"
     $ GrantPrivilege [PrivUpdate [Name Nothing "a", Name Nothing "b"]]
       (PrivTable [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant references on table t1 to role1"
     $ GrantPrivilege [PrivReferences []]
       (PrivTable [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant references(a,b) on table t1 to role1"
     $ GrantPrivilege [PrivReferences [Name Nothing "a", Name Nothing "b"]]
       (PrivTable [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant usage on table t1 to role1"
     $ GrantPrivilege [PrivUsage]
       (PrivTable [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant trigger on table t1 to role1"
     $ GrantPrivilege [PrivTrigger]
       (PrivTable [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)


    ,(TestStatement ansi2011
      "grant execute on specific function f to role1"
     $ GrantPrivilege [PrivExecute]
       (PrivFunction [Name Nothing "f"])
       [Name Nothing "role1"] WithoutGrantOption)

    ,(TestStatement ansi2011
      "grant select,delete on table t1 to role1"
     $ GrantPrivilege [PrivSelect [], PrivDelete]
       (PrivTable [Name Nothing "t1"])
       [Name Nothing "role1"] WithoutGrantOption)

{-
skipping for now:

what is 'under' action?

collation, character set, translation, member thing, methods

for review

some pretty big things missing in the standard:

schema, database

functions, etc., by argument types since they can be overloaded



12.4 <role definition>

<role definition> ::=
  CREATE ROLE <role name> [ WITH ADMIN <grantor> ]
-}

    ,(TestStatement ansi2011
      "create role rolee"
     $ CreateRole (Name Nothing "rolee"))


{-
12.5 <grant role statement>

<grant role statement> ::=
  GRANT <role granted> [ { <comma> <role granted> }... ]
      TO <grantee> [ { <comma> <grantee> }... ]
      [ WITH ADMIN OPTION ]
      [ GRANTED BY <grantor> ]

<role granted> ::=
  <role name>
-}

    ,(TestStatement ansi2011
      "grant role1 to public"
     $ GrantRole [Name Nothing "role1"] [Name Nothing "public"] WithoutAdminOption)

    ,(TestStatement ansi2011
      "grant role1,role2 to role3,role4"
     $ GrantRole [Name Nothing "role1",Name Nothing "role2"]
                 [Name Nothing "role3", Name Nothing "role4"] WithoutAdminOption)

    ,(TestStatement ansi2011
      "grant role1 to role3 with admin option"
     $ GrantRole [Name Nothing "role1"] [Name Nothing "role3"] WithAdminOption)


{-
12.6 <drop role statement>

<drop role statement> ::=
  DROP ROLE <role name>
-}

    ,(TestStatement ansi2011
      "drop role rolee"
     $ DropRole (Name Nothing "rolee"))


{-
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
-}


    ,(TestStatement ansi2011
      "revoke select on t1 from role1"
     $ RevokePrivilege NoGrantOptionFor [PrivSelect []]
              (PrivTable [Name Nothing "t1"])
              [Name Nothing "role1"] DefaultDropBehaviour)

    ,(TestStatement ansi2011
      "revoke grant option for select on t1 from role1,role2 cascade"
     $ RevokePrivilege GrantOptionFor [PrivSelect []]
                       (PrivTable [Name Nothing "t1"])
              [Name Nothing "role1",Name Nothing "role2"] Cascade)


{-
<revoke role statement> ::=
  REVOKE [ ADMIN OPTION FOR ] <role revoked> [ { <comma> <role revoked> }... ]
      FROM <grantee> [ { <comma> <grantee> }... ]
      [ GRANTED BY <grantor> ]
      <drop behavior>

<role revoked> ::=
  <role name>
-}

    ,(TestStatement ansi2011
      "revoke role1 from role2"
     $ RevokeRole NoAdminOptionFor [Name Nothing "role1"]
                  [Name Nothing "role2"] DefaultDropBehaviour)

    ,(TestStatement ansi2011
      "revoke role1,role2 from role3,role4"
     $ RevokeRole NoAdminOptionFor [Name Nothing "role1",Name Nothing "role2"]
                  [Name Nothing "role3",Name Nothing "role4"] DefaultDropBehaviour)


    ,(TestStatement ansi2011
      "revoke admin option for role1 from role2 cascade"
     $ RevokeRole AdminOptionFor [Name Nothing "role1"] [Name Nothing "role2"] Cascade)


   ]
