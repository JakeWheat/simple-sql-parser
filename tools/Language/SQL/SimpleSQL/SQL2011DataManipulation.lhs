
Section 14 in Foundation


> module Language.SQL.SimpleSQL.SQL2011DataManipulation (sql2011DataManipulationTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Syntax

> sql2011DataManipulationTests :: TestItem
> sql2011DataManipulationTests = Group "sql 2011 data manipulation tests"
>     [


14 Data manipulation


14.1 <declare cursor>

<declare cursor> ::=
  DECLARE <cursor name> <cursor properties>
      FOR <cursor specification>

14.2 <cursor properties>

<cursor properties> ::=
  [ <cursor sensitivity> ] [ <cursor scrollability> ] CURSOR
      [ <cursor holdability> ]
      [ <cursor returnability> ]

<cursor sensitivity> ::=
    SENSITIVE
  | INSENSITIVE
  | ASENSITIVE

<cursor scrollability> ::=
    SCROLL
  | NO SCROLL

<cursor holdability> ::=
    WITH HOLD
  | WITHOUT HOLD

<cursor returnability> ::=
    WITH RETURN
  | WITHOUT RETURN

14.3 <cursor specification>

<cursor specification> ::=
  <query expression> [ <updatability clause> ]

<updatability clause> ::=
  FOR { READ ONLY | UPDATE [ OF <column name list> ] }

14.4 <open statement>

<open statement> ::=
  OPEN <cursor name>

14.5 <fetch statement>

<fetch statement> ::=
     FETCH [ [ <fetch orientation> ] FROM ] <cursor name> INTO <fetch target list>

<fetch orientation> ::=
    NEXT
  | PRIOR
  | FIRST
  | LAST
  | { ABSOLUTE | RELATIVE } <simple value specification>

<fetch target list> ::=
     <target specification> [ { <comma> <target specification> }... ]


14.6 <close statement>

<close statement> ::=
  CLOSE <cursor name>

14.7 <select statement: single row>

<select statement: single row> ::=
  SELECT [ <set quantifier> ] <select list>
      INTO <select target list>
      <table expression>

<select target list> ::=
  <target specification> [ { <comma> <target specification> }... ]

14.8 <delete statement: positioned>

<delete statement: positioned> ::=
     DELETE FROM <target table> [ [ AS ] <correlation name> ]
         WHERE CURRENT OF <cursor name>

<target table> ::=
    <table name>
  | ONLY <left paren> <table name> <right paren>

14.9 <delete statement: searched>

<delete statement: searched> ::=
  DELETE FROM <target table>
      [ FOR PORTION OF <application time period name>
        FROM <point in time 1> TO <point in time 2> ]
      [ [ AS ] <correlation name> ]
      [ WHERE <search condition> ]

>      (TestStatement ansi2011 "delete from t"
>      $ Delete [Name "t"] Nothing Nothing)

>     ,(TestStatement ansi2011 "delete from t as u"
>      $ Delete [Name "t"] (Just (Name "u")) Nothing)

>     ,(TestStatement ansi2011 "delete from t where x = 5"
>      $ Delete [Name "t"] Nothing
>        (Just $ BinOp (Iden [Name "x"]) [Name "="] (NumLit "5")))


>     ,(TestStatement ansi2011 "delete from t as u where u.x = 5"
>      $ Delete [Name "t"] (Just (Name "u"))
>        (Just $ BinOp (Iden [Name "u", Name "x"]) [Name "="] (NumLit "5")))

14.10 <truncate table statement>

<truncate table statement> ::=
  TRUNCATE TABLE <target table> [ <identity column restart option> ]

<identity column restart option> ::=
    CONTINUE IDENTITY
  | RESTART IDENTITY

>     ,(TestStatement ansi2011 "truncate table t"
>      $ Truncate [Name "t"] DefaultIdentityRestart)

>     ,(TestStatement ansi2011 "truncate table t continue identity"
>      $ Truncate [Name "t"] ContinueIdentity)

>     ,(TestStatement ansi2011 "truncate table t restart identity"
>      $ Truncate [Name "t"] RestartIdentity)


14.11 <insert statement>

<insert statement> ::=
  INSERT INTO <insertion target> <insert columns and source>

<insertion target> ::=
  <table name>

<insert columns and source> ::=
    <from subquery>
  | <from constructor>
  | <from default>

<from subquery> ::=
  [ <left paren> <insert column list> <right paren> ]
      [ <override clause> ]
      <query expression>

<from constructor> ::=
  [ <left paren> <insert column list> <right paren> ]
      [ <override clause> ]
      <contextually typed table value constructor>

<override clause> ::=
    OVERRIDING USER VALUE
  | OVERRIDING SYSTEM VALUE

<from default> ::=
  DEFAULT VALUES

<insert column list> ::=
  <column name list>

>     ,(TestStatement ansi2011 "insert into t select * from u"
>      $ Insert [Name "t"] Nothing
>        $ InsertQuery makeSelect
>          {qeSelectList = [(Star, Nothing)]
>          ,qeFrom = [TRSimple [Name "u"]]})

>     ,(TestStatement ansi2011 "insert into t(a,b,c) select * from u"
>      $ Insert [Name "t"] (Just [Name "a", Name "b", Name "c"])
>        $ InsertQuery makeSelect
>          {qeSelectList = [(Star, Nothing)]
>          ,qeFrom = [TRSimple [Name "u"]]})

>     ,(TestStatement ansi2011 "insert into t default values"
>      $ Insert [Name "t"] Nothing DefaultInsertValues)

>     ,(TestStatement ansi2011 "insert into t values(1,2)"
>      $ Insert [Name "t"] Nothing
>        $ InsertQuery $ Values [[NumLit "1", NumLit "2"]])

>     ,(TestStatement ansi2011 "insert into t values (1,2),(3,4)"
>      $ Insert [Name "t"] Nothing
>        $ InsertQuery $ Values [[NumLit "1", NumLit "2"]
>                               ,[NumLit "3", NumLit "4"]])

>     ,(TestStatement ansi2011
>       "insert into t values (default,null,array[],multiset[])"
>      $ Insert [Name "t"] Nothing
>        $ InsertQuery $ Values [[Iden [Name "default"]
>                                ,Iden [Name "null"]
>                                ,Array (Iden [Name "array"]) []
>                                ,MultisetCtor []]])


14.12 <merge statement>

<merge statement> ::=
  MERGE INTO <target table> [ [ AS ] <merge correlation name> ]
      USING <table reference>
      ON <search condition> <merge operation specification>

merge into t
  using t
  on a = b
  merge operation specification

merge into t as u
using (table factor | joined expression)

 MERGE INTO tablename USING table_reference ON (condition)
   WHEN MATCHED THEN
   UPDATE SET column1 = value1 [, column2 = value2 ...]
   WHEN NOT MATCHED THEN
   INSERT (column1 [, column2 ...]) VALUES (value1 [, value2 ...

merge into t23
using t42
on t42.id = t23.id
when matched then
    update
    set     t23.col1 = t42.col1
when not matched then
    insert  (id, col1)
    values  (t42.id, t42.col1)



MERGE INTO TableA u

USING (SELECT b.Key1, b.ColB1, c.ColC1

FROM TableB b

INNER JOIN TableC c ON c.KeyC1 = b.KeyB1

) s

ON (u.KeyA1 = s.KeyA1)

WHEN MATCHED THEN

UPDATE SET u.ColA1 = s.ColB1, u.ColA2 = s.ColC1


MERGE INTO Department 
USING NewDept AS ND 
ON nd.Department_Number = Department.
Department_Number 
WHEN MATCHED THEN UPDATE 
SET budget_amount = nd.Budget_Amount 
WHEN NOT MATCHED THEN INSERT 
VALUES 
(nd.Department_Number, nd.Department_
Name, nd.Budget_Amount, 
 nd.Manager_Employee_Number);


MERGE INTO Orders2 
USING Orders3 
ON ORDERS3.Order_Number = Orders2.
Order_Number 
WHEN NOT MATCHED THEN INSERT 
Orders3.order_number, Orders3.
invoice_number, 
 Orders3.customer_number, Orders3.
initial_order_date, 
 Orders3.invoice_date, Orders3.
invoice_amount);

MERGE INTO Orders2 
USING Orders3 
ON ORDERS3.Order_Number = Orders2.
Order_Number AND 1=0 
WHEN NOT MATCHED THEN INSERT 
(Orders3.order_number, Orders3.invoice_number, 
 Orders3.customer_number, Orders3.
initial_order_date, 
 Orders3.invoice_date, Orders3.
invoice_amount);

MERGE INTO Department 
USING NewDept AS ND 
ON nd.Department_Number = Department.
Department_Number 
WHEN MATCHED THEN UPDATE 
SET budget_amount = nd.Budget_Amount 
LOGGING ALL ERRORS WITH NO LIMIT;


MERGE INTO Department 
USING 
 (SELECT Department_Number,
department_name, 
        Budget_Amount, 
Manager_Employee_Number 
    FROM NewDept 
    WHERE Department_Number IN 
(SELECT Department_Number 
        FROM Employee)) AS m
ON m.Department_Number = Department.
Department_Number 
WHEN MATCHED THEN UPDATE 
SET budget_amount = m.Budget_Amount 
WHEN NOT MATCHED THEN INSERT 
(m.Department_Number, m.Department_
Name, m.Budget_Amount, 
m.Manager_Employee_Number) 
LOGGING ALL ERRORS WITH NO LIMIT;

 
MERGE INTO Customers AS c
USING      Moved     AS m
      ON   m.SSN      = c.SSN
WHEN MATCHED
THEN UPDATE
SET        Street     = m.Street,
           HouseNo    = m.HouseNo,
           City       = m.City;

MERGE INTO CentralOfficeAccounts AS C    -- Target
USING BranchOfficeAccounts AS B          -- Source
   ON C.account_nbr = B.account_nbr
WHEN MATCHED THEN                        -- On match update
     UPDATE SET C.company_name = B.company_name,
                C.primary_contact = B.primary_contact,
                C.contact_phone = B.contact_phone
WHEN NOT MATCHED THEN                    -- Add missing
     INSERT (account_nbr, company_name, primary_contact, contact_phone)
     VALUES (B.account_nbr, B.company_name, B.primary_contact, B.contact_phone);
 
SELECT account_nbr, company_name, primary_contact, contact_phone 
FROM CentralOfficeAccounts;



MERGE INTO CentralOfficeAccounts AS C   -- Target
USING BranchOfficeAccounts AS B         -- Source
   ON C.account_nbr = B.account_nbr
WHEN MATCHED                            -- On match update
 AND (C.company_name <> B.company_name  -- Additional search conditions
   OR C.primary_contact <> B.primary_contact
   OR C.contact_phone <> B.contact_phone) THEN                        
     UPDATE SET C.company_name = B.company_name,
                C.primary_contact = B.primary_contact,
                C.contact_phone = B.contact_phone
WHEN NOT MATCHED THEN                   -- Add missing
     INSERT (account_nbr, company_name, primary_contact, contact_phone)
     VALUES (B.account_nbr, B.company_name, B.primary_contact, B.contact_phone);



MERGE INTO CentralOfficeAccounts AS C   -- Target
USING BranchOfficeAccounts AS B         -- Source
   ON C.account_nbr = B.account_nbr
WHEN MATCHED                            -- On match update
 AND (C.company_name <> B.company_name  -- Additional search conditions
   OR C.primary_contact <> B.primary_contact
   OR C.contact_phone <> B.contact_phone) THEN                        
     UPDATE SET C.company_name = B.company_name,
                C.primary_contact = B.primary_contact,
                C.contact_phone = B.contact_phone
WHEN NOT MATCHED THEN                   -- Add missing
     INSERT (account_nbr, company_name, primary_contact, contact_phone)
     VALUES (B.account_nbr, B.company_name, B.primary_contact, B.contact_phone)
WHEN SOURCE NOT MATCHED THEN            -- Delete missing from source
     DELETE;
 
SELECT account_nbr, company_name, primary_contact, contact_phone 
FROM CentralOfficeAccounts; 




<merge correlation name> ::=
  <correlation name>

<merge operation specification> ::=
  <merge when clause>...

<merge when clause> ::=
    <merge when matched clause>
  | <merge when not matched clause>

<merge when matched clause> ::=
  WHEN MATCHED [ AND <search condition> ]
      THEN <merge update or delete specification>

<merge update or delete specification> ::=
    <merge update specification>
  | <merge delete specification>

<merge when not matched clause> ::=
  WHEN NOT MATCHED [ AND <search condition> ]
      THEN <merge insert specification>

<merge update specification> ::=
  UPDATE SET <set clause list>

<merge delete specification> ::=
  DELETE

<merge insert specification> ::=
  INSERT [ <left paren> <insert column list> <right paren> ]
      [ <override clause> ]
      VALUES <merge insert value list>

<merge insert value list> ::=
  <left paren>
      <merge insert value element> [ { <comma> <merge insert value element> }... ]
      <right paren>

<merge insert value element> ::=
    <value expression>
  | <contextually typed value specification>

14.13 <update statement: positioned>

<updatestatement: positioned> ::=
     UPDATE <target table> [ [ AS ] <correlation name> ]
         SET <set clause list>
         WHERE CURRENT OF <cursor name>

14.14 <update statement: searched>

<update statement: searched> ::=
  UPDATE <target table>
      [ FOR PORTION OF <application time period name>
        FROM <point in time 1> TO <point in time 2> ]
      [ [ AS ] <correlation name> ]
      SET <set clause list>
      [ WHERE <search condition> ]


>     ,(TestStatement ansi2011 "update t set a=b"
>      $ Update [Name "t"] Nothing
>        [Set [Name "a"] (Iden [Name "b"])] Nothing)

>     ,(TestStatement ansi2011 "update t set a=b, c=5"
>      $ Update [Name "t"] Nothing
>        [Set [Name "a"] (Iden [Name "b"])
>        ,Set [Name "c"] (NumLit "5")] Nothing)


>     ,(TestStatement ansi2011 "update t set a=b where a>5"
>      $ Update [Name "t"] Nothing
>        [Set [Name "a"] (Iden [Name "b"])]
>        $ Just $ BinOp (Iden [Name "a"]) [Name ">"] (NumLit "5"))


>     ,(TestStatement ansi2011 "update t as u set a=b where u.a>5"
>      $ Update [Name "t"] (Just $ Name "u")
>        [Set [Name "a"] (Iden [Name "b"])]
>        $ Just $ BinOp (Iden [Name "u",Name "a"])
>                       [Name ">"] (NumLit "5"))

>     ,(TestStatement ansi2011 "update t set (a,b)=(3,5)"
>      $ Update [Name "t"] Nothing
>        [SetMultiple [[Name "a"],[Name "b"]]
>                     [NumLit "3", NumLit "5"]] Nothing)



14.15 <set clause list>

<set clause list> ::=
  <set clause> [ { <comma> <set clause> }... ]

<set clause> ::=
    <multiple column assignment>
  | <set target> <equals operator> <update source>

<set target> ::=
    <update target>
  | <mutated set clause>

<multiple column assignment> ::=
  <set target list> <equals operator> <assigned row>

<set target list> ::=
  <left paren> <set target> [ { <comma> <set target> }... ] <right paren>

<assigned row> ::=
  <contextually typed row value expression>

<update target> ::=
    <object column>
  | <object column>
      <left bracket or trigraph> <simple value specification> <right bracket or trigraph>

<object column> ::=
  <column name>

<mutated set clause> ::=
  <mutated target> <period> <method name>

<mutated target> ::=
    <object column>
  | <mutated set clause>

<update source> ::=
    <value expression>
  | <contextually typed value specification>

14.16 <temporary table declaration>

<temporary table declaration> ::=
  DECLARE LOCAL TEMPORARY TABLE <table name> <table element list>
      [ ON COMMIT <table commit action> ROWS ]

declare local temporary table t (a int) [on commit {preserve | delete} rows]

14.17 <free locator statement>

<free locator statement> ::=
  FREE LOCATOR <locator reference> [ { <comma> <locator reference> }... ]

<locator reference> ::=
    <host parameter name>
  | <embedded variable name>
  | <dynamic parameter specification>

14.18 <hold locator statement>

<hold locator statement> ::=
  HOLD LOCATOR <locator reference> [ { <comma> <locator reference> }... ]


>    ]
