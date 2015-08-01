
Section 14 in Foundation


> module Language.SQL.SimpleSQL.SQL2011DataManipulation (sql2011DataManipulationTests) where

> import Language.SQL.SimpleSQL.TestTypes

> sql2011DataManipulationTests :: TestItem
> sql2011DataManipulationTests = Group "sql 2011 data manipulation tests" []


14 Data manipulation


14.1 <declare cursor>

This Subclause is modified by Subclause 12.1, “<declare cursor>”, in ISO/IEC 9075-4.


Function
Declare a standing cursor.


Format
<declare cursor> ::=
  DECLARE <cursor name> <cursor properties>
      FOR <cursor specification>


Syntax Rules
1)    04    If a <declare cursor> is contained in an <SQL-client module definition> M, then:
     a)       The <cursor name> shall not be equivalent to the <cursor name> of any other <declare cursor>,
              <dynamic declare cursor>, or <allocate received cursor statement> in M.
     b) The scope of the <cursor name> is M with the exception of any <SQL schema statement> contained
        in M.
     c)       Any <host parameter name> contained in the <cursor specification> shall be defined in a <host
              parameter declaration> in the <externally-invoked procedure> that contains an <open statement> that
              specifies the <cursor name> and is contained in the scope of that <cursor name>.
             NOTE 463 — See the Syntax Rules of Subclause 13.1, “<SQL-client module definition>”.


Access Rules
     None.


General Rules
1) A cursor declaration descriptor CDD is created. CDD includes indications that:
     a)       The kind of cursor is a standing cursor.
     b)        04  The provenance of the cursor is an indication of the SQL-client module whose <SQL-client module

              definition> contains the <declare cursor>.


                                                                                                    Data manipulation 929
IWD 9075-2:201?(E)
14.1 <declare cursor>

    c)   The name of the cursor is the <cursor name>.
    d) The cursor's origin is the <cursor specification> contained in the <declare cursor>.
    e)   The cursor's declared properties are as determined by the <cursor properties>.


Conformance Rules
1) Without Feature F831, “Full cursor update”, conforming SQL language shall not contain a <declare cursor>
   that contains both a <cursor specification> that contains an <updatability clause> that specifies FOR
   UPDATE and <cursor properties> that contain a <cursor scrollability>.




930 Foundation (SQL/Foundation)
                                                                                            IWD 9075-2:201?(E)
                                                                                       14.2 <cursor properties>


14.2 <cursor properties>

Function
Specify the declared properties of a cursor.


Format
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


Syntax Rules
1) If <cursor sensitivity> is not specified, then ASENSITIVE is implicit.
2) If <cursor scrollability> is not specified, then NO SCROLL is implicit.
3) If <cursor holdability> is not specified, then WITHOUT HOLD is implicit.
4) If <cursor returnability> is not specified, then WITHOUT RETURN is implicit.


Access Rules
    None.


General Rules
1) The declared properties of the cursor declaration descriptor associated with <cursor properties> are given
   by:
    a)   The declared sensitivity property is the explicit or implicit <cursor sensitivity>.
    b) The declared scrollability property is the explicit or implicit <cursor scrollability>.


                                                                                         Data manipulation 931
IWD 9075-2:201?(E)
14.2 <cursor properties>

    c)   The declared holdability property is the explicit or implicit <cursor holdability>.
    d) The declared returnability property is the explicit or implicit <cursor returnability>.


Conformance Rules
1) Without Feature T231, “Sensitive cursors”, conforming SQL language shall not contain a <cursor sensitivity>
   that immediately contains SENSITIVE.
2) Without Feature F791, “Insensitive cursors”, conforming SQL language shall not contain a <cursor sensi-
   tivity> that immediately contains INSENSITIVE.
3) Without Feature F791, “Insensitive cursors”, or Feature T231, “Sensitive cursors”, conforming SQL language
   shall not contain a <cursor sensitivity> that immediately contains ASENSITIVE.
4) Without Feature F431, “Read-only scrollable cursors”, conforming SQL language shall not contain a
   <cursor scrollability>.
5) Without Feature T471, “Result sets return value”, conforming SQL language shall not contain a <cursor
   returnability>.
6) Without Feature T551, “Optional key words for default syntax”, conforming SQL language shall not contain
   a <cursor holdability> that immediately contains WITHOUT HOLD.




932 Foundation (SQL/Foundation)
                                                                                         IWD 9075-2:201?(E)
                                                                                   14.3 <cursor specification>


14.3 <cursor specification>

Function
Define a result set.


Format
<cursor specification> ::=
  <query expression> [ <updatability clause> ]

<updatability clause> ::=
  FOR { READ ONLY | UPDATE [ OF <column name list> ] }


Syntax Rules
1) Let CS be the <cursor specification>.
2) Let QE be the <query expression> immediately contained in CS.
3) Case:
    a)   If CS is contained in a <declare cursor> DC that contains a <cursor properties>, then let CP be that
         <cursor properties>.
    b) If CS is a <preparable statement> being prepared by a <prepare statement> or re-prepared by an
       <allocate extended dynamic cursor statement> or a <dynamic open statement>, then:
         i)      Let STMT be the <prepare statement> that is preparing CS, or the <allocate extended dynamic
                 cursor statement> or <dynamic open statement> that is re-preparing CS. Let SCMD be the
                 <SQL-client module definition> that contains STMT.
         ii)     Case:
                 1) If CS is being prepared by a <prepare statement>, then let PS be that <prepare statement>.
                 2) Otherwise, let PS be the <prepare statement> that previously prepared CS.
         iii)    Let SSV be the <SQL statement variable> contained in PS.
         iv)     Case:
                 1) If CS is being prepared by a <prepare statement>, then:
                       Case:
                       A) If SSV is a <statement name> and there is exactly one <dynamic declare cursor> DDC
                          contained in SCMD whose <statement name> is equivalent to SSV, then let CP1 be
                          the <cursor properties> contained in DDC.
                       B) If SSV is an <extended statement name> that specifies or implies a <scope option>
                          that is LOCAL, and there is exactly one <allocate extended dynamic cursor statement>
                          ACS contained in SCMD whose <extended statement name> specifies or implies
                          LOCAL, then let CP1 be the <cursor properties> contained in ACS.


                                                                                       Data manipulation 933
IWD 9075-2:201?(E)
14.3 <cursor specification>

                     C) Otherwise, let CP1 be a zero-length string.
                 2) If CS is being re-prepared by an <allocate extended dynamic cursor statement> ACS, then
                    let CP1 be the <cursor properties> contained in ACS.
                 3) If CS is being re-prepared by a <dynamic open statement>, then let DDC be the <dynamic
                    declare cursor> whose <cursor name> is equivalent to the <cursor name> contained in
                    STMT. Let CP1 be the <cursor properties> contained in DDC.
         v)      If PS contains an <attributes variable>, then let CP2 be the value of that <attributes variable>;
                 otherwise, let CP2 be the zero-length string.
         vi)     Case:
                 1) If CP2 contains <cursor sensitivity>, then let SENS be that <cursor sensitivity>.
                 2) If CP1 contains <cursor sensitivity>, then let SENS be that <cursor sensitivity>.
                 3) Otherwise, let SENS be the zero-length string.
         vii)    Case:
                 1) If CP2 contains <cursor scrollability>, then let SCRO be that <cursor scrollability>.
                 2) If CP1 contains <cursor scrollability>, then let SCRO be that <cursor scrollability>.
                 3) Otherwise, let SCRO be the zero-length string.
         viii)   Case:
                 1) If CP2 contains <cursor holdability>, then let HOLD be that <cursor holdability>.
                 2) If CP1 contains <cursor holdability>, then let HOLD be that <cursor holdability>.
                 3) Otherwise, let HOLD be the zero-length string.
         ix)     Case:
                 1) If CP2 contains <cursor returnability>, then let RET be that <cursor returnability>.
                 2) If CP1 contains <cursor returnability>, then let RET be that <cursor returnability>.
                 3) Otherwise, let RET be the zero-length string.
         x)      Let CP be the <cursor properties>:

                 SENS SCRO CURSOR HOLD RET

4) If <updatability clause> is not specified and either CS is contained in a <declare cursor> or is being re-
   prepared by an <allocate extended dynamic cursor statement> or a <dynamic open statement>, then
    Case:
    a)   If CP contains INSENSITIVE or SCROLL, or QE immediately contains an <order by clause>, or QE
         is not a simply updatable <query specification>, then an <updatability clause> of READ ONLY is
         implicit.
    b) Otherwise, an <updatability clause> of FOR UPDATE without a <column name list> is implicit.



934 Foundation (SQL/Foundation)
                                                                                                       IWD 9075-2:201?(E)
                                                                                                 14.3 <cursor specification>

         NOTE 464 — If CS is being prepared by a <prepare statement>, then defaulting the <updatability clause> is postponed until
         CS is re-prepared.

5) If an <updatability clause> of FOR UPDATE with or without a <column name list> is specified, then CP
   shall not contain INSENSITIVE, QE shall be updatable, and QE shall have only one leaf underlying table
   LUT such that QE is one-to-one with respect to LUT.
6) Case:
    a)   If an <updatability clause> specifying FOR UPDATE is specified or implicit, then CS is updatable.
    b) If an <updatability clause> specifying FOR READ ONLY is specified or implicit, then CS is not
       updatable.
    c)   Otherwise, the determination of updatability of CS is postponed until CS is re-prepared.
7) If CS is updatable, then let LUTN be a <table name> that references LUT. LUTN is an exposed <table or
   query name> whose scope is <updatability clause>.
8) If an <updatability clause> of FOR UPDATE without a <column name list> is specified or implicit, then
   a <column name list> that consists of the <column name> of every column of LUT is implicit.
9) If an <updatability clause> of FOR UPDATE with a <column name list> is specified, then each <column
   name> in the <column name list> shall be the <column name> of a column of LUT.


Access Rules
    None.


General Rules
    None.


Conformance Rules
1) Without Feature F831, “Full cursor update”, conforming SQL language shall not contain a <cursor speci-
   fication> that contains both an <updatability clause> that specifies FOR UPDATE and an <order by clause>.
2) Without Feature T111, “Updatable joins, unions, and columns”, in conforming SQL language, if FOR
   UPDATE is specified, then QE shall be simply updatable.




                                                                                                     Data manipulation 935
IWD 9075-2:201?(E)
14.4 <open statement>


14.4 <open statement>

This Subclause is modified by Subclause 12.2, “<open statement>”, in ISO/IEC 9075-4.


Function
Open a standing cursor.


Format
<open statement> ::=
  OPEN <cursor name>


Syntax Rules
1)    04  Let CN be the <cursor name> in the <open statement>. CN shall be contained within the scope of a

     <cursor name> that is equivalent to CN.
2) CN shall identify a standing cursor.
3) Let CDD be the cursor declaration descriptor of the standing cursor identified by CN.


Access Rules
1) The Access Rules for the <query expression> simply contained in the <declare cursor> identified by the
   <cursor name> are applied.


General Rules
1) Let CR be the cursor instance descriptor in the current SQL-session whose cursor declaration descriptor
   is CDD.
2) The General Rules of Subclause 15.1, “Effect of opening a cursor”, are applied with CR as CURSOR.


Conformance Rules
     None.




936 Foundation (SQL/Foundation)
                                                                                               IWD 9075-2:201?(E)
                                                                                            14.5 <fetch statement>


14.5 <fetch statement>

This Subclause is modified by Subclause 12.3, “<fetch statement>”, in ISO/IEC 9075-4.
This Subclause is modified by Subclause 11.15, “<fetch statement>”, in ISO/IEC 9075-10.
This Subclause is modified by Subclause 14.1, “<fetch statement>”, in ISO/IEC 9075-14.


Function
Position a standing cursor on a specified row of the standing cursor's result set and retrieve values from that
row.


Format
 10  <fetch  statement> ::=
     FETCH [ [ <fetch orientation> ] FROM ] <cursor name> INTO <fetch target list>

<fetch orientation> ::=
    NEXT
  | PRIOR
  | FIRST
  | LAST
  | { ABSOLUTE | RELATIVE } <simple value specification>

 14  <fetch  target list> ::=
     <target specification> [ { <comma> <target specification> }... ]


Syntax Rules
1) <fetch target list> shall not contain a <target specification> that specifies a <column reference>.
2) If the <fetch orientation> is omitted, then NEXT is implicit.
3)     04   10  Let CN be the <cursor name> in the <fetch statement>. CN shall be contained within the scope of a

      <cursor name> that is equivalent to CN.
4) CN shall identify a standing cursor.
5) Let CDD be the cursor declaration descriptor of the standing cursor identified by CN.
6) Let T be the result set defined by the <cursor specification> of CDD.
7) If the implicit or explicit <fetch orientation> is not NEXT, then the declared scrollability property of CDD
   shall be SCROLL.
8) If a <fetch orientation> that contains a <simple value specification> is specified, then the declared type of
   that <simple value specification> shall be exact numeric with a scale of 0 (zero).
9) Case:
      a)   If the <fetch target list> contains a single <target specification> TS and the degree of T is greater than
           1 (one), then the declared type of TS shall be a row type.
           Case:



                                                                                            Data manipulation 937
IWD 9075-2:201?(E)
14.5 <fetch statement>

         i)      04  If TS is an <SQL parameter reference>, then the Syntax Rules of Subclause 9.2, “Store

                assignment”, are applied with TS as TARGET and an arbitrary value of the row type of T as
                VALUE.
         ii)    Otherwise, the Syntax Rules of Subclause 9.1, “Retrieval assignment”, are applied with TS as
                TARGET and an arbitrary value of the row type of T as VALUE.
    b) Otherwise:
         i)     The number of <target specification>s NTS in the <fetch target list> shall be the same as the
                degree of T. The i-th <target specification>, 1 (one) ≤ i ≤ NTS, in the <fetch target list> corre-
                sponds with the i-th column of T.
         ii)    For i varying from 1 (one) to NTS, let CSi be an arbitrary value of the declared type of the i-th
                column of T.
         iii)    04  For each <target specification> TS1i, 1 (one) ≤ i ≤ NTS, that is either an <SQL parameter

                reference> or a <target array element specification>,
                Case:
                1) If TS1i contains a <simple value specification>, then the Syntax Rules of Subclause 9.2,
                   “Store assignment”, are applied with an arbitrary site whose declared type is the declared
                   type of TS1i as TARGET and CSi as VALUE.

                2) Otherwise, the Syntax Rules of Subclause 9.2, “Store assignment”, are applied with TS1i
                   as TARGET and CSi as VALUE.

         iv)     10  For each <target specification> TS2i, 1 (one) ≤ i ≤ NTS, that is a <host parameter specifica-

                tion>, the Syntax Rules of Subclause 9.1, “Retrieval assignment”, are applied with TS2i as
                TARGET and CSi as VALUE.

         v)     For each <target specification> TS2i, 1 (one) ≤ i, ≤ NTS, that is an <embedded variable specifi-
                cation>, the Syntax Rules of Subclause 9.1, “Retrieval assignment”, are applied with TS2i as
                TARGET and CSi as VALUE.


Access Rules
    None.


General Rules
1) Let CR be the cursor instance descriptor of the current SQL-session whose cursor declaration descriptor
   is CDD.
2) If CR is not in the open state, then an exception condition is raised: invalid cursor state.
3) The General Rules of Subclause 15.3, “Determination of the current row of a cursor”, are applied with CR
   as CURSOR and <fetch orientation> as FETCH ORIENTATION.
4) If a completion condition no data has been raised, then no further General Rules of this Subclause are
   applied.


938 Foundation (SQL/Foundation)
                                                                                               IWD 9075-2:201?(E)
                                                                                            14.5 <fetch statement>

5) Case:
   a)   If the <fetch target list> contains a single <target specification> TS and the degree of T is greater than
        1 (one), then the current row is assigned to TS and
        Case:
        i)       04   14  If TS is an <SQL parameter reference>, then the General Rules of Subclause 9.2, “Store

                assignment”, are applied with TS as TARGET and the current row as VALUE.
        ii)     Otherwise, the General Rules of Subclause 9.1, “Retrieval assignment”, are applied with TS as
                TARGET and the current row as VALUE.
   b)    10  Otherwise, if the <fetch target list> contains more than one <target specification>, then values from

        the current row are assigned to their corresponding targets identified by the <fetch target list>. The
        assignments are made in an implementation-dependent order. Let TV be a target and let SV denote its
        corresponding value in the current row of CR.
        Case:
        i)       04  If TV is either an <SQL parameter reference> or a <target array element specification>, then

                for each <target specification> in the <fetch target list>, let TVi be the i-th <target specification>
                in the <fetch target list> and let SVi denote the i-th corresponding value in the current row of
                CR.
                Case:
                1) If <target array element specification> is specified, then
                    Case:
                    A) If the value of TVi is the null value, then an exception condition is raised: data
                       exception — null value in array target.
                    B) Otherwise:
                         I)      Let N be the maximum cardinality of TVi.

                         II)     Let M be the cardinality of the value of TVi.

                         III)    Let I be the value of the <simple value specification> immediately contained
                                 in TVi.

                         IV)     Let EDT be the element type of TVi.

                         V)      Case:
                                 1) If I is greater than zero and less than or equal to M, then the value of TVi
                                    is replaced by an array A with element type EDT and cardinality M derived
                                    as follows:
                                      a)   For j varying from 1 (one) to I–1 and from I+1 to M, the j-th element
                                           in A is the value of the j-th element in TVi.

                                      b)    14  The General Rules of Subclause 9.2, “Store assignment”, are applied

                                           with I-th element of A as TARGET and SVi as VALUE.



                                                                                            Data manipulation 939
IWD 9075-2:201?(E)
14.5 <fetch statement>

                                     2) If I is greater than M and less than or equal to N, then the value of TVi is
                                        replaced by an array A with element type EDT and cardinality I derived
                                        as follows:
                                          a)   For j varying from 1 (one) to M, the j-th element in A is the value of
                                               the j-th element in TVi.

                                          b) For j varying from M+1 to I, the j-th element in A is the null value.
                                          c)    14  The General Rules of Subclause 9.2, “Store assignment”, are applied

                                               with I-th element of A as TARGET and SVi as VALUE.

                                     3) Otherwise, an exception condition is raised: data exception — array element
                                        error.
                   2)    14  Otherwise, the General Rules of Subclause 9.2, “Store assignment”, are applied with

                        TVi as TARGET and SVi as VALUE.

         ii)       If TV is a <host parameter name>, then the General Rules of Subclause 9.1, “Retrieval assign-
                   ment”, are applied with TV as TARGET and SV as VALUE.
         iii)      If TV is an <embedded variable specification>, then the General Rules of Subclause 9.1,
                   “Retrieval assignment”, are applied with TV as TARGET and SV as VALUE.
                NOTE 465 — SQL parameters cannot have as their data types any row type.

6) If an exception condition occurs during the assignment of a value to a target, then the values of all targets
   are implementation-dependent.
        NOTE 466 — It is implementation-dependent whether CR remains positioned on the current row when an exception condition
        is raised during the derivation of any <derived column>.


Conformance Rules
1) Without Feature F431, “Read-only scrollable cursors”, in conforming SQL language, a <fetch statement>
   shall not contain a <fetch orientation>.




940 Foundation (SQL/Foundation)
                                                                                          IWD 9075-2:201?(E)
                                                                                       14.6 <close statement>


14.6 <close statement>

This Subclause is modified by Subclause 12.4, “<close statement>”, in ISO/IEC 9075-4.


Function
Close a standing cursor.


Format
<close statement> ::=
  CLOSE <cursor name>


Syntax Rules
1)    04  Let CN be the <cursor name> in the <close statement>. CN shall be contained within the scope of a

     <cursor name> that is equivalent to CN.
2) CN shall identify a standing cursor.
3) Let CDD be the cursor declaration descriptor of the standing cursor identified by CN.


Access Rules
     None.


General Rules
1) Let CR be the cursor instance descriptor of the current SQL-session whose cursor declaration descriptor
   is CDD.
2) The General Rules of Subclause 15.4, “Effect of closing a cursor”, are applied with CR as CURSOR and
   DESTROY as DISPOSITION.


Conformance Rules
     None.




                                                                                      Data manipulation 941
IWD 9075-2:201?(E)
14.7 <select statement: single row>


14.7 <select statement: single row>

This Subclause is modified by Subclause 12.5, “<select statement: single row>”, in ISO/IEC 9075-4.
This Subclause is modified by Subclause 11.14, “<select statement: single row>”, in ISO/IEC 9075-10.
This Subclause is modified by Subclause 14.2, “<select statement: single row>”, in ISO/IEC 9075-14.


Function
Retrieve values from a specified row of a table.


Format
<select statement: single row> ::=
  SELECT [ <set quantifier> ] <select list>
      INTO <select target list>
      <table expression>

 14  <select
           target list> ::=
  <target specification> [ { <comma> <target specification> }... ]


Syntax Rules
1) <select target list> shall not contain a <target specification> that specifies a <column reference>.
2) Let T be the table defined by the <table expression>.
3) Case:
    a)   If the <select target list> contains a single <target specification> TS and the degree of T is greater than
         1 (one), then the declared type of TS shall be a row type.
         Case:
         i)      If TS is an <SQL parameter reference>, then the Syntax Rules of Subclause 9.2, “Store
                 assignment”, are applied with TS as TARGET and an arbitrary value of the row type of T as
                 VALUE.
         ii)     Otherwise, the Syntax Rules of Subclause 9.1, “Retrieval assignment”, are applied with TS as
                 TARGET and an arbitrary value of the row type of T as VALUE.
    b) Otherwise:
         i)      The number of elements NOE in the <select list> shall be the same as the number of elements
                 in the <select target list>. The i-th <target specification>, 1 (one) ≤ i ≤ NOE, in the <select
                 target list> corresponds with the i-th element of the <select list>.
         ii)        For i varying from 1 (one) to NOE, let TS1i be the i-th <target specification> in the <select
                  04 

                 target list> that is either an <SQL parameter reference> or a <target array element specification>,
                 and let SLi be the i-th element of the <select list> that corresponds to the <target specification>
                 in the <select target list>.
                 Case:



942 Foundation (SQL/Foundation)
                                                                                               IWD 9075-2:201?(E)
                                                                                14.7 <select statement: single row>

                 1) If <target array element specification> is specified, then the Syntax Rules of Subclause 9.2,
                    “Store assignment”, are applied with a temporary site whose declared type is the declared
                    type of TS1i as TARGET and SLi as VALUE.

                 2) Otherwise, the Syntax Rules of Subclause 9.2, “Store assignment”, are applied with TS1i
                    as TARGET and the corresponding element of the <select list> as VALUE.
         iii)    For each <target specification> TS2i, 1 (one) ≤ i ≤ NOE, that is a <host parameter specification>,
                 the Syntax Rules of Subclause 9.1, “Retrieval assignment”, are applied with TS2i as TARGET
                 and corresponding element of the <select list> as VALUE.
         iv)        For each <target specification> TS2i, 1 (one) ≤ i ≤ NOE, that is an <embedded variable
                  10 

                 specification>, the Syntax Rules of Subclause 9.1, “Retrieval assignment”, are applied with
                 TS2i as TARGET and the corresponding element of the <select list> as VALUE.

4) Let S be a <query specification> whose <select list> and <table expression> are those specified in the
   <select statement: single row> and that specifies the <set quantifier> if it is specified in the <select statement:
   single row>. S shall be a valid <query specification>.
5) A column in the result of the <select statement: single row> is known not null if the corresponding column
   in the result of S is known not null.
6) The <select statement: single row> is possibly non-deterministic if S is possibly non-deterministic.


Access Rules
    None.


General Rules
1) Let Q be the result of <query specification> S.
2) Case:
    a)   If the cardinality of Q is greater than 1 (one), then an exception condition is raised: cardinality violation.
         It is implementation-dependent whether or not SQL-data values are assigned to the targets identified
         by the <select target list>.
    b) If Q is empty, then no SQL-data values are assigned to any targets identified by the <select target
       list>, and a completion condition is raised: no data.
    c)   Otherwise, values in the row of Q are assigned to their corresponding targets.
3) If a completion condition no data has been raised, then no further General Rules of this Subclause are
   applied.
4) Case:
    a)   If the <select target list> contains a single <target specification> TS and the degree of table T is greater
         than 1 (one), then the current row is assigned to TS and
         Case:




                                                                                             Data manipulation 943
IWD 9075-2:201?(E)
14.7 <select statement: single row>

         i)      14  If TS is an <SQL parameter reference>, then the General Rules of Subclause 9.2, “Store

                assignment”, are applied with TS as TARGET and the current row as VALUE.
         ii)    Otherwise, the General Rules of Subclause 9.1, “Retrieval assignment”, are applied with TS as
                TARGET and the current row as VALUE.
    b) Otherwise:
         i)     Let NOE be the number of elements in the <select list>.
         ii)       For i varying from 1 (one) to NOE, let TSi be the i-th <target specification> in the <select
                 04 

                target list> that is either an <SQL parameter reference> or a <target array element specification>,
                and let SLi denote the corresponding (i-th) value in the row of Q. The assignment of values to
                targets in the <select target list> is in an implementation-dependent order.
                Case:
                1) If <target array element specification> is specified, then
                       Case:
                       A) If the value of TSi is the null value, then an exception condition is raised: data exception
                          — null value in array target.
                       B) Otherwise:
                           I)      Let N be the maximum cardinality of TSi.

                           II)     Let M be the cardinality of the value of TSi.

                           III)    Let I be the value of the <simple value specification> immediately contained
                                   in TSi.

                           IV)     Let EDT be the element type of TSi.

                           V)      Case:
                                   1) If I is greater than zero and less than or equal to M, then the value of TSi
                                      is replaced by an array A with element type EDT and cardinality M derived
                                      as follows:
                                       a)   For j varying from 1 (one) to I–1 and from I+1 to M, the j-th element
                                            in A is the value of the j-th element in TSi.

                                       b)    14  The General Rules of Subclause 9.2, “Store assignment”, are applied

                                            with I-th element of A as TARGET and SLi as VALUE.

                                   2) If I is greater than M and less than or equal to N, then the value of TSi is
                                      replaced by an array A with element type EDT and cardinality I derived
                                      as follows:
                                       a)   For j varying from 1 (one) to M, the j-th element in A is the value of
                                            the j-th element in TSi.

                                       b) For j varying from M+1 to I–1, the j-th element in A is the null value.



944 Foundation (SQL/Foundation)
                                                                                             IWD 9075-2:201?(E)
                                                                              14.7 <select statement: single row>

                                      c)    14  The General Rules of Subclause 9.2, “Store assignment”, are applied

                                           with I-th element of A as TARGET and SLi as VALUE.

                                 3) Otherwise, an exception condition is raised: data exception — array element
                                    error.
                2)    14  Otherwise, the General Rules of Subclause 9.2, “Store assignment”, are applied with

                     TSi as TARGET and corresponding value SLi in the row of Q as VALUE.

         iii)   For each <target specification> TS that is a <host parameter specification>, the General Rules
                of Subclause 9.1, “Retrieval assignment”, are applied with TS as TARGET and the corresponding
                value in the row of Q as VALUE. The assignment of values to targets in the <select target list>
                is in an implementation-dependent order.
         iv)     10  For each <target specification> TS that is an <embedded variable specification>, the General

                Rules of Subclause 9.1, “Retrieval assignment”, are applied with TS as TARGET and the corre-
                sponding value in the row of Q as VALUE. The assignment of values to targets in the <select
                target list> is in an implementation-dependent order.
5) If an exception condition is raised during the assignment of a value to a target, then the values of all targets
   are implementation-dependent.


Conformance Rules
    None.




                                                                                          Data manipulation 945
IWD 9075-2:201?(E)
14.8 <delete statement: positioned>


14.8 <delete statement: positioned>

This Subclause is modified by Subclause 12.6, “<delete statement: positioned>”, in ISO/IEC 9075-4.
This Subclause is modified by Subclause 11.12, “<delete statement: positioned>”, in ISO/IEC 9075-10.


Function
Delete a row of a table.


Format
 10  <delete  statement: positioned> ::=
     DELETE FROM <target table> [ [ AS ] <correlation name> ]
         WHERE CURRENT OF <cursor name>

<target table> ::=
    <table name>
  | ONLY <left paren> <table name> <right paren>


Syntax Rules
1)     04  Let DSP be the <delete statement: positioned> and let CN be the <cursor name> immediately contained

      in DSP. CN shall be contained within the scope of a <cursor name> that is equivalent to CN.
2) CN shall identify a standing cursor.
3) Let CDD be the cursor declaration descriptor of the standing cursor identified by CN.
4) The cursor specification of CDD shall be updatable.
5) Let TU be the simply underlying table of the cursor identified by CN. Let LUT be the leaf underlying table
   of TU such that TU is one-to-one with respect to LUT.
6) Let TT be the <target table> and let TN be the <table name> contained in TT. TN shall identify LUT.
7) LUT shall not be an old transition table or a new transition table.
8) If TT immediately contains ONLY and LUT is not a typed table, then TT is equivalent to TN.
9) TT shall specify ONLY if and only if the <table reference> contained in TU that references LUT specifies
   ONLY.
10) The schema identified by the explicit or implicit qualifier of TN shall include the descriptor of LUT.
11) Case:
      a)   If <correlation name> is specified, then let COR be that <correlation name>. COR is an exposed
           <correlation name>.
      b) Otherwise, let COR be TN. COR is an exposed <table or query name>.
           NOTE 467 — CN has no scope.




946 Foundation (SQL/Foundation)
                                                                                       IWD 9075-2:201?(E)
                                                                        14.8 <delete statement: positioned>


Access Rules
1) Case:
    a)   If DSP is contained, without an intervening <SQL routine spec> that specifies SQL SECURITY
         INVOKER, in an <SQL schema statement>, then let A be the authorization identifier that owns that
         schema. The applicable privileges for A shall include DELETE for the table identified by TN.
    b) Otherwise, the current privileges shall include DELETE for the table identified by TN.


General Rules
1) Let CR be the cursor instance descriptor of the current SQL-session whose cursor declaration descriptor
   is CDD.
2) The General Rules of Subclause 15.5, “Effect of a positioned delete”, are applied with CR as CURSOR,
   DSP as STATEMENT, and TT as TARGET.


Conformance Rules
1) Without Feature S111, “ONLY in query expressions”, conforming SQL language shall not contain a
   <target table> that contains ONLY.




                                                                                    Data manipulation 947
IWD 9075-2:201?(E)
14.9 <delete statement: searched>


14.9 <delete statement: searched>

This Subclause is modified by Subclause 14.3, “<delete statement: searched>”, in ISO/IEC 9075-14.


Function
Delete rows of a table.


Format
 14  <delete
           statement: searched> ::=
  DELETE FROM <target table>
      [ FOR PORTION OF <application time period name>
        FROM <point in time 1> TO <point in time 2> ]
      [ [ AS ] <correlation name> ]
      [ WHERE <search condition> ]


Syntax Rules
1) Let DSS be the <delete statement: searched> and let TT be the <target table>.
2) Let TN be the <table name> contained in TT. Let T be the table identified by TN.
3) T shall be an updatable table or a trigger deletable table.
4) T shall not be an old transition table or a new transition table.
5) If WHERE <search condition> is not specified, then WHERE TRUE is implicit.
6) Let DSC be the implicit or explicit <search condition>. DSC shall not generally contain a <routine invoca-
   tion> whose subject routine is an SQL-invoked routine that possibly modifies SQL-data.
    Case:
    a)   If T is a system-versioned table, then let ENDCOL be the system-time period end column of T. Let
         ENDVAL be the highest value supported by the declared type of ENDCOL. Let TSC be

         ( DSC ) AND ( ENDCOL = ENDVAL )

    b) Otherwise, let TSC be DSC.
7) Case:
    a)   If FOR PORTION OF <application time period name> ATPN is specified, then the table descriptor
         of T shall include a period descriptor whose period name is equivalent to ATPN.
         i)     Let BSTARTCOL be the name of the ATPN period start column of T; let BENDCOL be the
                name of the ATPN period end column of T.
         ii)    Let FROMVAL be <point in time 1>. FROMVAL shall not generally contain a reference to a
                column of T or a <routine invocation> whose subject routine is an SQL-invoked routine that
                is possibly non-deterministic or that possibly modifies SQL-data.



948 Foundation (SQL/Foundation)
                                                                                           IWD 9075-2:201?(E)
                                                                             14.9 <delete statement: searched>

         iii)   Let TOVAL be <point in time 2>. TOVAL shall not generally contain a reference to a column
                of T or a <routine invocation> whose subject routine is an SQL-invoked routine that is possibly
                nondeterministic or that possibly modifies SQL-data.
         iv)    Let SC be

                TSC AND
                (FROMVAL < TOVAL) AND
                (BENDCOL > FROMVAL) AND
                (BSTARTCOL < TOVAL)

    b) Otherwise, let SC be TSC.
8) If DSS is contained in a <triggered SQL statement>, then SC shall not contain a <value specification> that
   specifies a parameter reference.
9) Case:
    a)   If <correlation name> is specified, then let CN be that <correlation name>. CN is an exposed <corre-
         lation name>.
    b) Otherwise, let CN be TN. CN is an exposed <table or query name>.
10) The scope of CN is SC.


Access Rules
1) Case:
    a)   If DSS is contained, without an intervening <SQL routine spec> that specifies SQL SECURITY
         INVOKER, in an <SQL schema statement>, then let A be the <authorization identifier> that owns
         that schema.
         i)     The applicable privileges for A shall include DELETE for T.
         ii)    If TT immediately contains ONLY, then the applicable privileges for A shall include SELECT
                WITH HIERARCHY OPTION on at least one supertable of T.
    b) Otherwise,
         i)     The current privileges shall include DELETE for T.
         ii)    If TT immediately contains ONLY, then the current privileges shall include SELECT WITH
                HIERARCHY OPTION on at least one supertable of T.


General Rules
1) If the transaction access mode of the current SQL-transaction or the transaction access mode of the branch
   of the current SQL-transaction at the current SQL-connection is read-only, and T is not a temporary table,
   then an exception condition is raised: invalid transaction state — read-only SQL-transaction.
2) If there is any sensitive cursor CR that is currently open in the SQL-transaction in which this SQL-statement
   is being executed, then
    Case:


                                                                                        Data manipulation 949
IWD 9075-2:201?(E)
14.9 <delete statement: searched>

    a)   If CR has not been held into a subsequent SQL-transaction, then either the change resulting from the
         successful execution of this statement shall be made visible to CR or an exception condition is raised:
         cursor sensitivity exception — request failed.
    b) Otherwise, whether the change resulting from the successful execution of this SQL-statement is made
       visible to CR is implementation-defined.
3) If there is any open, insensitive cursor CR, then either the change resulting from the successful execution
   of this statement shall be invisible to CR, or an exception condition is raised: cursor sensitivity exception
   — request failed.
4) The extent to which an SQL-implementation may disallow independent changes that are not significant is
   implementation-defined.
5) SC is effectively evaluated for each row of T with the exposed <correlation name>s or <table or query
   name>s bound to that row.
6) Case:
    a)   If TT contains ONLY, then the rows for which the result of SC is True and for which there is no subrow
         in a proper subtable of T are identified for deletion from T.
    b) Otherwise, the rows for which the result of SC is True are identified for deletion from T.
         NOTE 468 — Identifying a row for deletion is an implementation-dependent mechanism.

7) Let S be the set consisting of every row identified for deletion from T. S is the old delta table of delete
   operation on T. If FOR PORTION OF is specified, then FROMVAL and TOVAL are associated with every
   row in S as the associated for portion of from-value and the associated for portion of to-value, respectively.
8) Case:
    a)   If T is a base table, then:
         i)      Case:
                 1) If TT specifies ONLY, then T is identified for deletion processing without subtables.
                 2) Otherwise, T is identified for deletion processing with subtables.
                     NOTE 469 — Identifying a base table for deletion processing, with or without subtables, is an implementation-
                     dependent mechanism.

         ii)     The General Rules of Subclause 15.7, “Effect of deleting rows from base tables”, are applied.
    b) If T is a viewed table, then the General Rules of Subclause 15.9, “Effect of deleting some rows from
       a viewed table”, are applied with TT as VIEW NAME.
9) If any row that is marked for deletion by DSS has been marked for deletion by any <delete statement:
   positioned>, <dynamic delete statement: positioned>, or <preparable dynamic delete statement: positioned>
   that identifies some open cursor CR or updated by any <update statement: positioned>, <dynamic update
   statement: positioned>, or <preparable dynamic update statement: positioned> that identifies some open
   cursor CR, then a completion condition is raised: warning — cursor operation conflict.
10) If no rows are marked for deletion, then a completion condition is raised: no data.




950 Foundation (SQL/Foundation)
                                                                                         IWD 9075-2:201?(E)
                                                                           14.9 <delete statement: searched>


Conformance Rules
1) Without Feature F781, “Self-referencing operations”, conforming SQL language shall not contain a <delete
   statement: searched> in which either of the following is true:
    a)   A leaf generally underlying table of T is an underlying table of any <query expression> broadly con-
         tained in the <search condition>.
    b) The <search condition> broadly contains a <routine invocation>, <method invocation>, <static method
       invocation>, or <method reference> whose subject routine is an external routine that possibly reads
       SQL-data.
2) Without Feature T111, “Updatable joins, unions, and columns”, conforming SQL language shall not contain
   a <delete statement: searched> that contains a <target table> that identifies a table that is not simply
   updatable.
3) Without Feature T181, “Application-time period tables”, in conforming SQL language, a <delete statement:
   searched> shall not contain FOR PORTION OF.




                                                                                     Data manipulation 951
IWD 9075-2:201?(E)
14.10 <truncate table statement>


14.10 <truncate table statement>

Function
Delete all rows of a base table without causing any triggered action.


Format
<truncate table statement> ::=
  TRUNCATE TABLE <target table> [ <identity column restart option> ]

<identity column restart option> ::=
    CONTINUE IDENTITY
  | RESTART IDENTITY


Syntax Rules
1) Let TTS be the <truncate table statement> and let TT be the <target table> contained in TTS.
2) Let TN be the <table name> contained in TT and let T be the table identified by TN. The schema identified
   by the explicit or implicit <schema name> of TN shall include the descriptor of T.
3) T shall be a base table and shall not be a system-versioned table.
4) T shall not be identified by the name of the referenced table in any referential constraint descriptor.
5) If <identity column restart option> is not specified, then CONTINUE IDENTITY is implicit.


Access Rules
1) Let A be the <authorization identifier> that owns the schema identified by the <schema name> of T.
2) The enabled authorization identifiers shall include A.


General Rules
1) If the transaction access mode of the current SQL-transaction or the transaction access mode of the branch
   of the current SQL-transaction at the current SQL-connection is read-only, and T is not a temporary table,
   then an exception condition is raised: invalid transaction state — read-only SQL-transaction.
2) If there is any sensitive cursor CR that is currently open in the SQL-transaction in which this SQL-statement
   is being executed, then
    Case:
    a)   If CR has not been held into a subsequent SQL-transaction, then either the change resulting from the
         successful execution of this statement shall be made visible to CR or an exception condition is raised:
         cursor sensitivity exception — request failed.
    b) Otherwise, whether the change resulting from the successful execution of this SQL-statement is made
       visible to CR is implementation-defined.


952 Foundation (SQL/Foundation)
                                                                                         IWD 9075-2:201?(E)
                                                                              14.10 <truncate table statement>

3) If there is any open, insensitive cursor CR, then either the change resulting from the successful execution
   of this statement shall be invisible to CR, or an exception condition is raised: cursor sensitivity exception
   — request failed.
4) The extent to which an SQL-implementation may disallow independent changes that are not significant is
   implementation-defined.
5) Case:
    a)   If TT contains ONLY, then the rows for which there is no subrow in a proper subtable of T are deleted
         from T.
    b) Otherwise, all rows are deleted from T.
6) If any row that is deleted from T by TTS has been marked for deletion by any <delete statement: positioned>,
   <dynamic delete statement: positioned>, or <preparable dynamic delete statement: positioned> that iden-
   tifies some open cursor CR or updated by any <update statement: positioned>, <dynamic update statement:
   positioned>, or <preparable dynamic update statement: positioned> that identifies some open cursor CR,
   then a completion condition is raised: warning — cursor operation conflict.
7) If no rows are deleted from T, then a completion condition is raised: no data.
8) If RESTART IDENTITY is specified and the table descriptor of T includes a column descriptor IDCD of
   an identity column, then:
    a)   Let CN be the column name included in IDCD and let SV be the start value included in IDCD.
    b) The following <alter table statement> is effectively executed without further Access Rule checking:

         ALTER TABLE TN ALTER COLUMN CN RESTART WITH SV


Conformance Rules
1) Without Feature F200, “TRUNCATE TABLE statement”, conforming SQL language shall not contain a
   <truncate table statement>.
2) Without Feature F202, “TRUNCATE TABLE: identity column restart option”, conforming SQL language
   shall not contain an <identity column restart option>.




                                                                                        Data manipulation 953
IWD 9075-2:201?(E)
14.11 <insert statement>


14.11 <insert statement>

This Subclause is modified by Subclause 14.4, “<insert statement>”, in ISO/IEC 9075-14.


Function
Create new rows in a table.


Format
 14  <insert
           statement> ::=
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


Syntax Rules
1) Let IS be the <insert statement>.
2) Let TN be the <table name> contained in <insertion target>. Let T be the table identified by TN.
3) T shall be an insertable-into table or a trigger insertable-into table.
4) T shall not be an old transition table or a new transition table.
5) For each leaf generally underlying table of T whose descriptor includes a user-defined type name UDTN,
   the data type descriptor of the user-defined type UDT identified by UDTN shall indicate that UDT is
   instantiable.


954 Foundation (SQL/Foundation)
                                                                                                            IWD 9075-2:201?(E)
                                                                                                       14.11 <insert statement>

6) An <insert columns and source> that specifies DEFAULT VALUES is implicitly replaced by an <insert
   columns and source> that specifies a <contextually typed table value constructor> of the form

    VALUES (DEFAULT, DEFAULT, ..., DEFAULT)

    where the number of instances of “DEFAULT” equal to the number of columns of T.
7) If the <insert column list> is omitted, then an <insert column list> that identifies all columns of T in the
   ascending sequence of their ordinal positions within T is implicit.
8) A column identified by the <insert column list> is an object column. No <column name> of T shall be
   identified more than once.
9) If T is not trigger insertable-into, then T shall be an updatable table; each object column shall be an
   updatable column of T .
         NOTE 470 — The notion of updatable columns of base tables is defined in Subclause 4.15, “Tables”. The notion of updatable
         columns of viewed tables is defined in Subclause 11.32, “<view definition>”.

10) If <contextually typed table value constructor> CTTVC is specified, then every <contextually typed row
    value constructor element> simply contained in CTTVC whose positionally corresponding <column name>
    in <insert column list> references a column of which some underlying column is a generated column shall
    be a <default specification>.
11) Case:
    a)   If some underlying column of a column referenced by a <column name> contained in <insert column
         list> is a system-generated self-referencing column or a derived self-referencing column, then <override
         clause> shall be specified.
    b) If for some n, some underlying column of the column referenced by the <column name> CN contained
       in the n-th ordinal position in <insert column list> is an identity column, system-time period start
       column, or system-time period end column whose descriptor includes an indication that values are
       always generated, then
         Case:
         i)        If <from subquery> is specified, then <override clause> shall be specified.
         ii)       If any <contextually typed row value expression> simply contained in the <contextually typed
                   table value constructor> is a <row value special case>, then <override clause> shall be specified.
         iii)      If the n-th <contextually typed row value constructor element> simply contained in any <con-
                   textually typed row value constructor> simply contained in the <contextually typed table value
                   constructor> is not a <default specification>, then <override clause> shall be specified.
                NOTE 471 — The preceding subrule does not cover all possibilities. The remaining possibilities are where <default
                specification> is specified for every identity column, or for a system-time period start column or system-time period
                end column, in which case it is immaterial whether <override clause> is specified or not.

    c)   If for some n, some underlying column of the column referenced by the <column name> CN contained
         in the n-th ordinal position in <insert column list> is an identity column whose descriptor includes an
         indication that values are generated by default, then if <override clause> is specified, then <override
         clause> shall specify OVERRIDING USER VALUE.
    d) If for some n, some underlying column of the column referenced by the <column name> CN contained
       in the n-th ordinal position in <insert column list> is a system-time period start column or a system-



                                                                                                          Data manipulation 955
IWD 9075-2:201?(E)
14.11 <insert statement>

         time period end column whose descriptor includes an indication that values are always generated and
         <override clause> is specified, then <override clause> shall specify OVERRIDING USER VALUE.
    e)   Otherwise, <override clause> shall not be specified.
12) If <contextually typed table value constructor> CVC is specified, then the data type of every <contextually
    typed value specification> CVS specified in every <contextually typed row value expression> CRVS con-
    tained in CVC is the data type DT indicated in the column descriptor for the positionally corresponding
    column in the explicit or implicit <insert column list>. If CVS is an <empty specification> that specifies
    ARRAY, then DT shall be an array type. If CVS is an <empty specification> that specifies MULTISET,
    then DT shall be a multiset type.
13) Let QT be the table specified by the <query expression> or <contextually typed table value constructor>.
    The degree of QT shall be equal to the number of <column name>s in the <insert column list>. The column
    of table T identified by the i-th <column name> in the <insert column list> corresponds with the i-th column
    of QT.
14) The Syntax Rules of Subclause 9.2, “Store assignment”, are applied with corresponding columns of T as
    TARGET and QT as VALUE.
15) If IS is contained in a <triggered SQL statement>, then <insert columns and source> shall not contain a
    <value specification> that specifies a parameter reference.
16) A <query expression> simply contained in a <from subquery> shall not be a <table value constructor>.
         NOTE 472 — This rule removes a syntactic ambiguity; otherwise, “VALUES (1)” could be parsed either as

         <insert columns and source> ::=
            <from subquery> ::=
            <query expression> ::=
            <table value constructor> ::=
            VALUES (1)

         or

         <insert columns and source> ::=
            <from constructor> ::=
            <contextually typed table value constructor> ::=
            VALUES (1)


Access Rules
1) Case:
    a)   If IS is contained in, without an intervening <SQL routine spec> that specifies SQL SECURITY
         INVOKER, an <SQL schema statement>, then let A be the <authorization identifier> that owns that
         schema. The applicable privileges for A for T shall include INSERT for each object column.
    b) Otherwise, the current privileges for T shall include INSERT for each object column.




956 Foundation (SQL/Foundation)
                                                                                             IWD 9075-2:201?(E)
                                                                                        14.11 <insert statement>


General Rules
1) If the transaction access mode of the current SQL-transaction or the transaction access mode of the branch
   of the current SQL-transaction at the current SQL-connection is read-only, and T is not a temporary table,
   then an exception condition is raised: invalid transaction state — read-only SQL-transaction.
2) If there is any sensitive cursor CR that is currently open in the SQL-transaction in which this SQL-statement
   is being executed, then
    Case:
    a)   If CR has not been held into a subsequent SQL-transaction, then either the change resulting from the
         successful execution of this statement shall be made visible to CR or an exception condition is raised:
         cursor sensitivity exception — request failed.
    b) Otherwise, whether the change resulting from the successful execution of this SQL-statement is made
       visible to CR is implementation-defined.
3) If there is any open, insensitive cursor CR, then either the change resulting from the successful execution
   of this statement shall be invisible to CR, or an exception condition is raised: cursor sensitivity exception
   — request failed.
4) The extent to which an SQL-implementation may disallow independent changes that are not significant is
   implementation-defined.
5) QT is effectively evaluated before insertion of any rows into T.
6) Let Q be the result of evaluating QT.
7) For each row R of Q:
    a)   A candidate row of T is effectively created in which the value of each column is its default value, as
         specified in the General Rules of Subclause 11.5, “<default clause>”. The candidate row consists of
         every column of T.
    b) For each object column in the candidate row, let Ci be the object column identified by the i-th <column
       name> in the <insert column list> and let SVi be the i-th value of R.

    c)   For every Ci such that Ci is not marked as unassigned and no underlying column of Ci is a self-refer-
         encing column, the General Rules of Subclause 9.2, “Store assignment”, are applied with Ci as TARGET
         and SVi as VALUE. Ci is no longer marked as unassigned.

    d) If T has a column RC of which some underlying column is a self-referencing column, then
         Case:
         i)      If RC is a system-generated self-referencing column, then the value of RC is effectively replaced
                 by the REF value of the candidate row.
         ii)     If RC is a derived self-referencing column, then the value of RC is effectively replaced by a
                 value derived from the columns in the candidate row that correspond to the list of attributes of
                 the derived representation of the reference type of RC in an implementation-dependent manner.
    e)   For every Ci for which one of the following conditions is true:

         i)      Some underlying column of Ci is a user-generated self-referencing column.


                                                                                         Data manipulation 957
IWD 9075-2:201?(E)
14.11 <insert statement>

         ii)      Some underlying column of Ci is a self-referencing column and OVERRIDING SYSTEM
                  VALUE is specified.
         iii)     Some underlying column of Ci is an identity column and the i-th column of R is not derived
                  from <default specification> and OVERRIDING SYSTEM VALUE is specified.
         iv)      Some underlying column of Ci is an identity column whose descriptor includes an indication
                  that values are generated by default and neither OVERRIDING USER VALUE is specified
                  nor is the i-th column derived from <default specification>.
         The General Rules of Subclause 9.2, “Store assignment”, are applied with Ci as TARGET and SVi as
         VALUE. Ci is no longer marked as unassigned.
         NOTE 473 — If OVERRIDING USER VALUE is specified, then some columns of the candidate row(s) may continue to
         be marked as unassigned as a result of the preceding rules. The value of such columns is ultimately determined by the General
         Rules of Subclause 15.10, “Effect of inserting tables into base tables”, which has the effect of overriding user values specified
         in <insert columns and source>.
         NOTE 474 — The data values allowable in the candidate row may be constrained by a WITH CHECK OPTION constraint.
         The effect of a WITH CHECK OPTION constraint is defined in the General Rules of Subclause 15.12, “Effect of inserting
         a table into a viewed table”.

8) Let S be the table consisting of the candidate rows.
    Case:
    a)   If T is a base table, then:
         i)       T is identified for insertion of source table S.
                       NOTE 475 — Identifying a base table for insertion of a source table is an implementation-dependent operation.

         ii)      The General Rules of Subclause 15.10, “Effect of inserting tables into base tables”, are applied.
    b) If T is a viewed table, then the General Rules of Subclause 15.12, “Effect of inserting a table into a
       viewed table”, are applied with S as SOURCE and T as TARGET.
9) If Q is empty, then a completion condition is raised: no data.


Conformance Rules
1) Without Feature F781, “Self-referencing operations”, conforming SQL language shall not contain an
   <insert statement> in which either of the following is true:
    a)   The <table name> of a leaf generally underlying table of T is broadly contained in the <from subquery>
         except as the table name of a qualifying table of a column reference.
    b) The <from subquery> broadly contains a <routine invocation>, <method invocation>, <static method
       invocation>, or <method reference> whose subject routine is an external routine that possibly reads
       SQL-data.
2) Without Feature F222, “INSERT statement: DEFAULT VALUES clause”, conforming SQL language
   shall not contain a <from default>.
3) Without Feature S024, “Enhanced structured types”, in conforming SQL language, for each column C
   identified in the explicit or implicit <insert column list>, if the declared type of C is a structured type TY,




958 Foundation (SQL/Foundation)
                                                                                            IWD 9075-2:201?(E)
                                                                                       14.11 <insert statement>

    then the declared type of the corresponding column of the <query expression> or <contextually typed table
    value constructor> shall be TY.
4) Without Feature S043, “Enhanced reference types”, conforming SQL language shall not contain an
   <override clause>.
5) Without Feature T111, “Updatable joins, unions, and columns”, conforming SQL language shall not contain
   an <insert statement> that contains an <insertion target> that identifies a table that is not simply updatable.




                                                                                         Data manipulation 959
IWD 9075-2:201?(E)
14.12 <merge statement>


14.12 <merge statement>

This Subclause is modified by Subclause 14.5, “<merge statement>”, in ISO/IEC 9075-14.


Function
Conditionally update and/or delete rows of a table and/or insert new rows into a table.


Format
 14  <merge
          statement> ::=
  MERGE INTO <target table> [ [ AS ] <merge correlation name> ]
      USING <table reference>
      ON <search condition> <merge operation specification>

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



960 Foundation (SQL/Foundation)
                                                                                           IWD 9075-2:201?(E)
                                                                                      14.12 <merge statement>


Syntax Rules
1) Let TN be the <table name> contained in <target table> TT and let T be the table identified by TN.
2) If <merge when not matched clause> is specified, then T shall be insertable-into or trigger insertable-into.
3) If <merge update specification> is specified, then T shall be updatable or trigger updatable.
4) If <merge delete specification> is specified, then T shall be updatable or trigger deletable.
5) T shall not be an old transition table or a new transition table.
6) For each leaf generally underlying table of T whose descriptor includes a user-defined type name UDTN,
   the data type descriptor of the user-defined type UDT identified by UDTN shall indicate that UDT is
   instantiable.
7) If T is a view, then <target table> is effectively replaced by:

    ONLY ( TN )

8) Case:
    a)   If <merge correlation name> is specified, then let CN be the <correlation name> contained in <merge
         correlation name>. CN is an exposed <correlation name>.
    b) Otherwise, let CN be the <table name> contained in <target table>. CN is an exposed <table or query
       name>.
9) The scope of CN is the <search condition> immediately contained in the <merge statement>, the <search
   condition> immediately contained in a <merge when matched clause>, the <search condition> immediately
   contained in a <merge when not matched clause>, and the <set clause list>.
10) Let TR be the <table reference> immediately contained in <merge statement>. TR shall not directly contain
    a <joined table>.
11) The <correlation name> or <table or query name> that is exposed by TR shall not be equivalent to CN.
12) If an <insert column list> is omitted, then an <insert column list> that identifies all columns of T in the
    ascending sequence of their ordinal position within T is implicit.
13) Case:
    a)   If some underlying column of a column referenced by a <column name> contained in <insert column
         list> is a system-generated self-referencing column or a derived self-referencing column, then <override
         clause> shall be specified.
    b) If for some n, some underlying column of the column referenced by the <column name> contained
       in the n-th ordinal position in <insert column list> is an identity column, system-time period start
       column, or system-time period end column whose descriptor includes an indication that values are
       always generated, and the n-th <contextually typed value specification> simply contained in any
       <merge insert value element> simply contained in the <merge insert value list> is not a <default
       specification>, then <override clause> shall be specified.
    c)   If for some n, some underlying column of the column referenced by the <column name> contained
         in the n-th ordinal position in <insert column list> is an identity column whose descriptor includes an
         indication that values are generated by default and <override clause> is specified, then <override
         clause> shall specify OVERRIDING USER VALUE.


                                                                                         Data manipulation 961
IWD 9075-2:201?(E)
14.12 <merge statement>

    d) If for some n, some underlying column of the column referenced by the <column name> contained
       in the n-th ordinal position in <insert column list> is a system-time period start column or a system-
       time period end column whose descriptor includes an indication that values are always generated and
       <override clause> is specified, then <override clause> shall specify OVERRIDING USER VALUE.
    e)   Otherwise, <override clause> shall not be specified.
14) The <search condition> immediately contained in a <merge statement>, the <search condition> immediately
    contained in a <merge when matched clause>, and the <search condition> immediately contained in a
    <merge when not matched clause> shall not generally contain a <routine invocation> whose subject routine
    is an SQL-invoked routine that possibly modifies SQL-data.
15) Each column identified by an <object column> in a <set clause list> is an update object column. Each
    column identified by a <column name> in an implicit or explicit <insert column list> is an insert object
    column. Each update object column and each insert object column is an object column.
16) If <merge when not matched clause> is specified and if T is not trigger insertable-into or if <merge when
    matched clause> is specified and if T is not trigger updatable, then every object column shall identify an
    updatable column of T.
         NOTE 476 — The notion of updatable columns of base tables is defined in Subclause 4.15, “Tables”. The notion of updatable
         columns of viewed tables is defined in Subclause 11.32, “<view definition>”.

17) No <column name> of T shall be identified more than once in an <insert column list>.
18) For each <merge when not matched clause>:
    a)   Let NI be the number of <merge insert value element>s contained in <merge insert value list>. Let
         EXP1, EXP2, ... , EXPNI be those <merge insert value element>s.

    b) The number of <column name>s in the <insert column list> shall be equal to NI.
    c)   The declared type of every <contextually typed value specification> CVS in a <merge insert value
         list> is the data type DT indicated in the column descriptor for the positionally corresponding column
         in the explicit or implicit <insert column list>. If CVS is an <empty specification> that specifies
         ARRAY, then DT shall be an array type. If CVS is an <empty specification> that specifies MULTISET,
         then DT shall be a multiset type.
    d) Every <merge insert value element> whose positionally corresponding <column name> in <insert
       column list> references a column of which some underlying column is a generated column shall be a
       <default specification>.
    e)   For 1 (one) ≤ i ≤ NI, the Syntax Rules of Subclause 9.2, “Store assignment”, are applied with EXPi
         as VALUE and the column of table T identified by the i-th <column name> in the <insert column list>
         as TARGET.
19) Let DSC be the <search condition> immediately contained in <merge statement>.
    Case:
    a)   If T is a system-versioned table, then let ENDCOL be the system-time period end column of T. Let
         ENDVAL be the highest value supported by the declared type of ENDCOL. Let SC1 be

         (DSC) AND (ENDCOL = ENDVAL)

    b) Otherwise, let SC1 be DSC.



962 Foundation (SQL/Foundation)
                                                                                            IWD 9075-2:201?(E)
                                                                                       14.12 <merge statement>


Access Rules
1) Case:
    a)   If <merge statement> is contained, without an intervening <SQL routine spec> that specifies SQL
         SECURITY INVOKER, in an <SQL schema statement>, then let A be the <authorization identifier>
         that owns that schema.
         i)     If <merge update specification> is specified, then the applicable privileges for A shall include
                UPDATE for each update object column.
         ii)    If <merge delete specification> is specified, then the applicable privileges for A shall include
                DELETE for T.
         iii)   If <merge insert specification> is specified, then the applicable privileges for A shall include
                INSERT for each insert object column.
         iv)    If TT immediately contains ONLY, then the applicable privileges for A shall include SELECT
                WITH HIERARCHY OPTION on at least one supertable of T.
    b) Otherwise,
         i)     If <merge update specification> is specified, then the current privileges shall include UPDATE
                for each update object column.
         ii)    If <merge delete specification> is specified, then the applicable privileges for A shall include
                DELETE for T.
         iii)   If <merge insert specification> is specified, then the current privileges shall include INSERT
                for each insert object column.
         iv)    If TT immediately contains ONLY, then the current privileges shall include SELECT WITH
                HIERARCHY OPTION on at least one supertable of T.


General Rules
1) If the transaction access mode of the current SQL-transaction or the transaction access mode of the branch
   of the current SQL-transaction at the current SQL-connection is read-only, and T is not a temporary table,
   then an exception condition is raised: invalid transaction state — read-only SQL-transaction.
2) If there is any sensitive cursor CR that is currently open in the SQL-transaction in which this SQL-statement
   is being executed, then
    Case:
    a)   If CR has not been held into a subsequent SQL-transaction, then either the change resulting from the
         successful execution of this statement shall be made visible to CR or an exception condition is raised:
         cursor sensitivity exception — request failed.
    b) Otherwise, whether the change resulting from the successful execution of this SQL-statement is made
       visible to CR is implementation-defined.
3) If there is any open, insensitive cursor CR, then either the change resulting from the successful execution
   of this statement shall be invisible to CR, or an exception condition is raised: cursor sensitivity exception
   — request failed.


                                                                                         Data manipulation 963
IWD 9075-2:201?(E)
14.12 <merge statement>

4) The extent to which an SQL-implementation may disallow independent changes that are not significant is
   implementation-defined.
5) Let QT be the table specified by the <table reference>. QT is effectively evaluated before update, deletion,
   or insertion of any rows in T. Let Q be the result of evaluating QT.
6) For each <merge when clause>, in the order specified in the <merge operation specification>,
    Case:
    a)   If <merge when matched clause> MWMC is specified, then:
         i)     For each row R1 of T:
                1) SC1 and the <search condition> SC2 immediately contained in MWMC, if any, are effec-
                   tively evaluated for R1 with the exposed <table name> of the TT bound to R1 and to each
                   row of Q with the exposed <correlation name>s or <table or query name>s of the <table
                   reference> bound to that row. Both SC1 and SC2 are effectively evaluated for R1 before
                   updating or deleting any row of T and prior to the invocation of any <triggered action>
                   caused by the update or deletion of any row of T and before inserting any rows into T and
                   prior to the invocation of any <triggered action> caused by the insert of any row of T.
                    Case:
                    A) If TT contains ONLY, then R1 is a subject row if R1 has no subrow in a proper subtable
                       of T and the result of both SC1 and SC2 are True for some row R2 of Q and R1 is not
                       a subject row identified by any other <merge when matched clause> that precedes
                       MWMC in the <merge operation specification>. R2 is the matching row.
                    B) Otherwise, R1 is a subject row if the result of both SC1 and SC2 are True for some
                       row R2 of Q and R1 is not a subject row identified by any other <merge when matched
                       clause> that precedes MWMC in the <merge operation specification>. R2 is the
                       matching row.
                2) If R1 is a subject row, then:
                    A) Let M be the number of matching rows in Q for R1.
                    B) If M is greater than 1 (one), then an exception condition is raised: cardinality violation.
                    C) If <merge update specification> is specified, then:
                         I)     The <update source> of each <set clause> is effectively evaluated for R1 before
                                any row of T is updated and prior to the invocation of any <triggered action>
                                caused by the update of any row of T. The resulting value is the update value.
                         II)    A candidate new row is constructed by copying the subject row and updating
                                it as specified by each <set clause> by applying the General Rules of
                                Subclause 14.15, “<set clause list>”.
         ii)    Let S be the set consisting of every subject row. S is the old delta table of merge operation on
                T.
         iii)   If T is a base table, then each subject row is also an object row; otherwise, an object row is any
                row of a leaf generally underlying table of T from which a subject row is derived.




964 Foundation (SQL/Foundation)
                                                                                            IWD 9075-2:201?(E)
                                                                                       14.12 <merge statement>

          NOTE 477 — The data values allowable in the object rows may be constrained by a WITH CHECK OPTION
          constraint. The effect of a WITH CHECK OPTION constraint is defined in the General Rules of Subclause 15.15,
          “Effect of replacing some rows in a viewed table”.

iv)   If any row in the set of object rows has been marked for deletion by any <delete statement:
      positioned>, <dynamic delete statement: positioned>, or <preparable dynamic delete statement:
      positioned> that identifies some open cursor CR or updated by any <update statement: posi-
      tioned>, <dynamic update statement: positioned>, or <preparable dynamic update statement:
      positioned> that identifies some open cursor, then a completion condition is raised: warning
      — cursor operation conflict.
v)    If <merge update specification> is specified, then:
      1) Let CL be the columns of T identified by the <object column>s contained in the <set clause
         list>.
      2) Each subject row SR is identified for replacement, by its corresponding candidate new row
         CNR, in T. The set of (SR, CNR) pairs is the replacement set for T.
               NOTE 478 — Identifying a row for replacement, associating a replacement row with an identified row,
               and associating a replacement set with a table are implementation-dependent operations.

      3) Case:
          A) If T is a base table, then:
                I)      Case:
                        1) If TT specifies ONLY, then T is identified for replacement processing
                           without subtables with respect to object columns CL.
                        2) Otherwise, T is identified for replacement processing with subtables with
                           respect to object columns CL.
                                  NOTE 479 — Identifying a base table for replacement processing, with or without
                                  subtables, is an implementation-dependent mechanism. In general, though not here,
                                  the list of object columns can be empty.

                II)     The General Rules of Subclause 15.13, “Effect of replacing rows in base tables”,
                        are applied.
          B) If T is a viewed table, then the General Rules of Subclause 15.15, “Effect of replacing
             some rows in a viewed table”, are applied with TT as VIEW NAME and the replacement
             set for T as REPLACEMENT SET FOR VIEW NAME.
vi)   If <merge delete specification> is specified, then:
      1) Each subject row is identified for deletion from T.
      2) Case:
          A) If T is a base table, then:
                I)      Case:
                        1) If TT specifies ONLY, then T is identified for deletion processing without
                           subtables.
                        2) Otherwise, T is identified for deletion processing with subtables.



                                                                                         Data manipulation 965
IWD 9075-2:201?(E)
14.12 <merge statement>

                                          NOTE 480 — Identifying a base table for deletion processing, with or without subta-
                                          bles, is an implementation-dependent mechanism.

                        II)    The General Rules of Subclause 15.7, “Effect of deleting rows from base tables”,
                               are applied.
                   B) Otherwise, T is a viewed table and the General Rules of Subclause 15.9, “Effect of
                      deleting some rows from a viewed table”, are applied with TT as VIEW NAME.
    b) If <merge when not matched clause> MWNMC is specified, then:
        i)     Let TR1 be the <target table> immediately contained in <merge statement> and let TR2 be the
               <table reference> immediately contained in <merge statement>. If <merge correlation name>
               is specified, then let MCN be “AS <merge correlation name>”; otherwise, let MCN be a zero-
               length string. If MWNMC immediately contains a <search condition> SC2, then let ONSC2 be
               “OR NOT SC2”; otherwise, let ONSC2 be a zero-length string. Let S1 be the result of

               SELECT *
               FROM TR1 MCN, TR2
               WHERE SC1 ONSC2

        ii)    Let S2 be the collection of rows of Q for which there exists in S1 some row that is the concate-
               nation of some row R1 of T and some row R2 of Q.
        iii)   Let S3 be the collection of rows of Q that are not in S2. Let SN3 be the effective distinct name
               for S3. Let EN be the exposed <correlation name> or <table or query name> of TR2.
        iv)    Let S4 be the result of:

               SELECT EXP1, EXP2, ... , EXPNI
               FROM SN3 AS EN

        v)     Let S5 be the collection of rows of S4 for which no candidate rows have been effectively created
               by any other <merge when not matched clause> that precedes MWNMC in the <merge operation
               specification>.
        vi)    S5 is effectively evaluated before deletion of any rows from, insertion of any rows into, or
               update of any rows in T.
        vii)   For each row R of S5:
               1) A candidate row of T is effectively created in which the value of each column is its default
                  value, as specified in the General Rules of Subclause 11.5, “<default clause>”. The candidate
                  row consists of every column of T.
               2) If T has a column RC of which some underlying column is a self-referencing column, then
                   Case:
                   A) If RC is a system-generated self-referencing column, then the value of RC is effectively
                      replaced by the REF value of the candidate row.
                   B) If RC is a derived self-referencing column, then the value of RC is effectively replaced
                      by a value derived from the columns in the candidate row that correspond to the list
                      of attributes of the derived representation of the reference type of RC in an implemen-
                      tation-dependent manner.



966 Foundation (SQL/Foundation)
                                                                                                      IWD 9075-2:201?(E)
                                                                                                 14.12 <merge statement>

                 3) For each object column in the candidate row, let Ci be the object column identified by the
                    i-th <column name> in the <insert column list> and let SVi be the i-th value of R.

                 4) For every Ci for which one of the following conditions is true:

                     A) Ci is not marked as unassigned and no underlying column of Ci is a self-referencing
                        column.
                     B) Some underlying column of Ci is a user-generated self-referencing column.

                     C) Some underlying column of Ci is a self-referencing column and OVERRIDING
                        SYSTEM VALUE is specified.
                     D) Some underlying column of Ci is an identity column and the i-th column of R is not
                        derived from <default specification> and OVERRIDING SYSTEM VALUE is speci-
                        fied.
                     E) Some underlying column of Ci is an identity column whose descriptor includes an
                        indication that values are generated by default and neither OVERRIDING USER
                        VALUE is specified nor is the i-th column derived from <default specification>.
                     the General Rules of Subclause 9.2, “Store assignment”, are applied with Ci as TARGET
                     and SVi as VALUE. Ci is no longer marked as unassigned.
                         NOTE 481 — If OVERRIDING USER VALUE is specified, then some columns of the candidate row(s)
                         may continue to be marked as unassigned as a result of the preceding rules. The value of such columns
                         is ultimately determined by the General Rules of Subclause 15.10, “Effect of inserting tables into base
                         tables”, which has the effect of overriding user values specified in <insert columns and source>.
                         NOTE 482 — The data values allowable in the candidate row may be constrained by a WITH CHECK
                         OPTION constraint. The effect of a WITH CHECK OPTION constraint is defined in the General Rules
                         of Subclause 15.12, “Effect of inserting a table into a viewed table”.

         viii)   Let S be the table consisting of the candidate rows.
                 Case:
                 1) If T is a base table, then:
                     A) T is identified for insertion of source table S.
                              NOTE 483 — Identifying a base table for insertion of a source table is an implementation-dependent
                              operation.

                     B) The General Rules of Subclause 15.10, “Effect of inserting tables into base tables”,
                        are applied.
                 2) If T is a viewed table, then the General Rules of Subclause 15.12, “Effect of inserting a
                    table into a viewed table”, are applied with S as SOURCE and T as TARGET.
7) If Q is empty, then a completion condition is raised: no data.


Conformance Rules
1) Without Feature F781, “Self-referencing operations”, conforming SQL language shall not contain a <merge
   statement> in which either of the following is true:



                                                                                                   Data manipulation 967
IWD 9075-2:201?(E)
14.12 <merge statement>

    a)   A leaf generally underlying table of T is broadly contained in a <query expression> immediately
         contained in the <table reference> except as the <table or query name> or <correlation name> of a
         column reference.
    b) A <query expression> immediately contained in the <table reference> broadly contains a <routine
       invocation>, <method invocation>, <static method invocation>, or <method reference> whose subject
       routine is an external routine that possibly reads SQL-data.
2) Without Feature F781, “Self-referencing operations”, conforming SQL language shall not contain a <merge
   statement> in which either of the following is true:
    a)   A leaf generally underlying table of T is an underlying table of any <query expression> broadly con-
         tained in any <search condition>.
    b) Any <search condition> broadly contains a <routine invocation>, <method invocation>, <static method
       invocation>, or <method reference> whose subject routine is an external routine that possibly reads
       SQL-data.
3) Without Feature S024, “Enhanced structured types”, conforming SQL language shall not contain a <merge
   statement> that does not satisfy the condition: for each column C identified in the explicit or implicit <insert
   column list>, if the declared type of C is a structured type TY, then the declared type of the corresponding
   column of the <query expression> or <contextually typed table value constructor> is TY.
4) Without Feature F312, “MERGE statement”, conforming SQL language shall not contain a <merge state-
   ment>.
5) Without Feature T111, “Updatable joins, unions, and columns”, conforming SQL language shall not contain
   a <merge statement> that contains an <target table> that identifies a table that is not simply updatable.
6) Without Feature F313, “Enhanced MERGE statement”, in conforming SQL language, a <merge statement>
   shall not contain each of <merge when matched clause> and <merge when not matched clause> more than
   once.
7) Without Feature F313, “Enhanced MERGE statement”, in conforming SQL language, a <merge when
   matched clause> or a <merge when not matched clause> shall not immediately contain a <search condition>.
8) Without Feature F314, “MERGE statement with DELETE branch”, in conforming SQL language, a <merge
   when matched clause> shall not immediately contain a <merge delete specification>.




968 Foundation (SQL/Foundation)
                                                                                         IWD 9075-2:201?(E)
                                                                         14.13 <update statement: positioned>


14.13 <update statement: positioned>

This Subclause is modified by Subclause 12.7, “<update statement: positioned>”, in ISO/IEC 9075-4.
This Subclause is modified by Subclause 11.13, “<update statement: positioned>”, in ISO/IEC 9075-10.
This Subclause is modified by Subclause 14.6, “<update statement: positioned>”, in ISO/IEC 9075-14.


Function
Update a row of a table.


Format
 10   14  <updatestatement: positioned> ::=
     UPDATE <target table> [ [ AS ] <correlation name> ]
         SET <set clause list>
         WHERE CURRENT OF <cursor name>


Syntax Rules
1)     04  Let USP be the <update statement: positioned> and let CN be the <cursor name> immediately contained

      in USP. CN shall be contained within the scope of a <cursor name> that is equivalent to CN.
2) CN shall identify a standing cursor.
3) Let CDD be the cursor declaration descriptor of the standing cursor identified by CN.
4) The cursor specification of CDD shall be updatable.
5) Let TU be the simply underlying table of the cursor identified by CN. Let LUT be the leaf underlying table
   of TU such that TU is one-to-one with respect to LUT.
6) Let TT be the <target table> and let TN be the <table name> contained in TT. TN shall identify LUT.
7) LUT shall not be an old transition table or a new transition table.
8) It TT immediately contains ONLY and LUT is not a typed table, then TT is equivalent to TN.
9) TT shall specify ONLY if and only if the <table reference> contained in TU that references LUT specifies
   ONLY.
10) The schema identified by the explicit or implicit qualifier of TN shall include the descriptor of LUT.
11) Case:
      a)   If <correlation name> is specified, then let COR be that <correlation name>. COR is an exposed
           <correlation name>.
      b) Otherwise, let COR be the <table name> contained in TT. COR is an exposed <table or query name>.
12) The scope of COR is <set clause list>.
13) If the declared <cursor specification> CS of CDD is ordered, then for each <object column> OC contained
    in <set clause list>, no generally underlying column of a <sort key> in the <order by clause> simply con-
    tained in the <query expression> of CS shall be OC or a generally underlying column of OC.


                                                                                       Data manipulation 969
IWD 9075-2:201?(E)
14.13 <update statement: positioned>

14) Each <column name> specified as an <object column> shall identify a column in the explicit or implicit
    <column name list> contained in the explicit or implicit <updatability clause> of the <cursor specification>
    of CDD.


Access Rules
1) Case:
    a)   If USP is contained, without an intervening <SQL routine spec> that specifies SQL SECURITY
         INVOKER, in an <SQL schema statement>, then let A be the <authorization identifier> that owns
         that schema. The applicable privileges for A shall include UPDATE for each <object column>.
    b) Otherwise, the current privileges shall include UPDATE for each <object column>.


General Rules
1) Let CR be the cursor instance descriptor of the current SQL-session whose cursor declaration descriptor
   is CDD.
2) Let SCL be the <set clause list>.
3) The General Rules of Subclause 15.6, “Effect of a positioned update”, are applied with CR as CURSOR,
   SCL as SET CLAUSE LIST, USP as STATEMENT, and TT as TARGET.


Conformance Rules
1) Without Feature F831, “Full cursor update”, conforming SQL language shall not contain an <update
   statement: positioned> in which the declared <cursor specification> of CDD is ordered.




970 Foundation (SQL/Foundation)
                                                                                        IWD 9075-2:201?(E)
                                                                         14.14 <update statement: searched>


14.14 <update statement: searched>

This Subclause is modified by Subclause 14.7, “<update statement: searched>”, in ISO/IEC 9075-14.


Function
Update rows of a table.


Format
 14  <update
           statement: searched> ::=
  UPDATE <target table>
      [ FOR PORTION OF <application time period name>
        FROM <point in time 1> TO <point in time 2> ]
      [ [ AS ] <correlation name> ]
      SET <set clause list>
      [ WHERE <search condition> ]


Syntax Rules
1) Let USS be the <update statement: searched>, let TT be the <target table> contained in USS, and let SCL
   be the <set clause list> contained in USS.
2) Let TN be the <table name> contained in TT and let T be the table identified by TN.
3) T shall be an updatable table or a trigger updatable table.
4) T shall not be an old transition table or a new transition table.
5) If WHERE is not specified, then WHERE TRUE is implicit.
6) Let DSC be the implicit or explicit <search condition>. DSC shall not generally contain a <routine invoca-
   tion> whose subject routine is an SQL-invoked routine that possibly modifies SQL-data.
    Case:
    a)   If T is a system-versioned table, then let ENDCOL be the system-time period end column of T. Let
         ENDVAL be the highest value supported by the declared type of ENDCOL. Let TSC be

         (DSC) AND (ENDCOL = ENDVAL)

    b) Otherwise, let TSC be DSC.
7) Case:
    a)   If FOR PORTION OF <application time period name> ATPN is specified, then the table descriptor
         of T shall include a ATPN period descriptor.
         i)     Let BSTARTCOL be the name of the ATPN period start column of T; let BENDCOL be the
                name of the ATPN period end column of T. Let BCD be the declared type of the ATPN period
                start column of T.




                                                                                     Data manipulation 971
IWD 9075-2:201?(E)
14.14 <update statement: searched>

         ii)    Neither BSTARTCOL nor BENDCOL shall be an explicit <object column> contained in the
                <set clause list>.
         iii)   Let FROMVAL be <point in time 1>. FROMVAL shall not generally contain a reference to a
                column of T or a <routine invocation> whose subject routine is an SQL-invoked routine that
                is possibly non-deterministic or that possibly modifies SQL-data.
         iv)    Let TOVAL be <point in time 2>. TOVAL shall not generally contain a reference to a column
                of T or a <routine invocation> whose subject routine is an SQL-invoked routine that is possibly
                non-deterministic or that possibly modifies SQL-data.
         v)     Let SC be

                TSC AND
                (FROMVAL < TOVAL) AND
                (BENDCOL > FROMVAL) AND
                (BSTARTCOL < TOVAL)

         vi)    The following two <set clause>s are implicitly added to SCL:

                BSTARTCOL = CASE
                              WHEN BSTARTCOL > FROMVAL
                                 THEN BSTARTCOL
                              ELSE CAST ( FROMVAL AS BCD )
                            END,
                BENDCOL   = CASE
                              WHEN BENDCOL < TOVAL
                                 THEN BENDCOL
                              ELSE CAST ( TOVAL AS BCD )
                            END

    b) Otherwise, let SC be TSC.
8) If UPS is contained in a <triggered SQL statement>, then SC shall not contain a <value specification> that
   specifies a parameter reference.
9) Case:
    a)   If <correlation name> is specified, then let CN be that <correlation name>. CN is an exposed <corre-
         lation name>.
    b) Otherwise, let CN be the <table name> contained in TT. CN is an exposed <table or query name>.
10) The scope of CN is SCL and SC.


Access Rules
1) Case:
    a)   If USS is contained, without an intervening <SQL routine spec> that specifies SQL SECURITY
         INVOKER, in an <SQL schema statement>, then let A be the <authorization identifier> that owns
         that schema.
         i)     The applicable privileges for A for T shall include UPDATE for each <object column>.




972 Foundation (SQL/Foundation)
                                                                                          IWD 9075-2:201?(E)
                                                                           14.14 <update statement: searched>

         ii)    If TT immediately contains ONLY, then the applicable privileges for A shall include SELECT
                WITH HIERARCHY OPTION on at least one supertable of T.
    b) Otherwise,
         i)     The current privileges for T shall include UPDATE for each <object column>.
         ii)    If TT immediately contains ONLY, then the current privileges shall include SELECT WITH
                HIERARCHY OPTION on at least one supertable of T.


General Rules
1) If the transaction access mode of the current SQL-transaction or the transaction access mode of the branch
   of the current SQL-transaction at the current SQL-connection is read-only and T is not a temporary table,
   then an exception condition is raised: invalid transaction state — read-only SQL-transaction.
2) If there is any sensitive cursor CR that is currently open in the SQL-transaction in which this SQL-statement
   is being executed, then
    Case:
    a)   If CR has not been held into a subsequent SQL-transaction, then either the change resulting from the
         successful execution of this statement shall be made visible to CR or an exception condition is raised:
         cursor sensitivity exception — request failed.
    b) Otherwise, whether the change resulting from the successful execution of this SQL-statement is made
       visible to CR is implementation-defined.
3) If there is any open, insensitive cursor CR, then either the change resulting from the successful execution
   of this statement shall be invisible to CR, or an exception condition is raised: cursor sensitivity exception
   — request failed.
4) The extent to which an SQL-implementation may disallow independent changes that are not significant is
   implementation-defined.
5) Case:
    a)   If TT contains ONLY, then SC is effectively evaluated for each row of T with the exposed <correlation
         name>s or <table or query name>s bound to that row, and the subject rows are those rows for which
         the result of SC is True and for which there is no subrow in a proper subtable of T. SC is effectively
         evaluated for each row of T before updating any row of T.
    b) Otherwise, SC is effectively evaluated for each row of T with the exposed <correlation name>s or
       <table or query name>s of TT bound to that row, and the subject rows are those rows for which the
       result of SC is True. SC is effectively evaluated for each row of T before updating any row of T.
6) Let S be the set consisting of every subject row. S is the old delta table of update operation on T. If FOR
   PORTION OF is specified, then FROMVAL and TOVAL are associated with every row in S as the associated
   for portion of from-value and the associated for portion of to-value, respectively.
7) If T is a base table, then each subject row is also an object row; otherwise, an object row is any row of a
   leaf generally underlying table of T from which a subject row is derived.
8) If any row in the set of object rows has been marked for deletion by any <delete statement: positioned>,
   <dynamic delete statement: positioned>, or <preparable dynamic delete statement: positioned> that iden-



                                                                                        Data manipulation 973
IWD 9075-2:201?(E)
14.14 <update statement: searched>

    tifies some open cursor CR or updated by any <update statement: positioned>, <dynamic update statement:
    positioned>, or <preparable dynamic update statement: positioned> that identifies some open cursor CR,
    then a completion condition is raised: warning — cursor operation conflict.
9) SC is evaluated for each row of T prior to the invocation of any <triggered action> caused by the update
   of any row of T.
10) The <update source> of each <set clause> contained in SCL is effectively evaluated for each row of T
    before any row of T is updated.
11) For each subject row, a candidate new row is constructed by copying the subject row and updating it as
    specified by each <set clause> contained in SCL by applying the General Rules of Subclause 14.15, “<set
    clause list>”.
         NOTE 484 — The data values allowable in the object rows may be constrained by a WITH CHECK OPTION constraint.
         The effect of a WITH CHECK OPTION constraint is defined in the General Rules of Subclause 15.15, “Effect of replacing
         some rows in a viewed table”.

12) Let CL be the columns of T identified by the <object column>s contained in SCL.
13) Each subject row SR is identified for replacement, by its corresponding candidate new row CNR, in T. The
    set of (SR, CNR) pairs is the replacement set for T.
         NOTE 485 — Identifying a row for replacement, associating a replacement row with an identified row, and associating a
         replacement set with a table are implementation-dependent operations.

14) Case:
    a)   If T is a base table, then:
         i)      Case:
                 1) If TT specifies ONLY, then T is identified for replacement processing without subtables
                    with respect to object columns CL.
                 2) Otherwise, T is identified for replacement processing with subtables with respect to object
                    columns CL.
                      NOTE 486 — Identifying a base table for replacement processing, with or without subtables, is an implemen-
                      tation-dependent mechanism. In general, though not here, the list of object columns can be empty.

         ii)     The General Rules of Subclause 15.13, “Effect of replacing rows in base tables”, are applied.
    b) If T is a viewed table, then the General Rules of Subclause 15.15, “Effect of replacing some rows in
       a viewed table”, are applied with TT as VIEW NAME and the replacement set for T as REPLACEMENT
       SET FOR VIEW NAME.
15) If the set of object rows is empty, then a completion condition is raised: no data.


Conformance Rules
1) Without Feature F781, “Self-referencing operations”, conforming SQL language shall not contain an
   <update statement: searched> in which either of the following is true:
    a)   A leaf generally underlying table of T is an underlying table of any <query expression> broadly con-
         tained in the <search condition>.




974 Foundation (SQL/Foundation)
                                                                                       IWD 9075-2:201?(E)
                                                                        14.14 <update statement: searched>

    b) The <search condition> broadly contains a <routine invocation>, <method invocation>, <static method
       invocation>, or <method reference> whose subject routine is an external routine that possibly reads
       SQL-data.
2) Without Feature T111, “Updatable joins, unions, and columns”, conforming SQL language shall not contain
   an <update statement: searched> that contains a <target table> that identifies a table that is not simply
   updatable.
3) Without Feature T181, “Application-time period tables”, in conforming SQL language, an <update statement:
   searched> shall not contain FOR PORTION OF.




                                                                                    Data manipulation 975
IWD 9075-2:201?(E)
14.15 <set clause list>


14.15 <set clause list>

Function
Specify a list of updates.


Format
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


Syntax Rules
1) Let T be the table identified by the <target table> contained in the containing <update statement: positioned>,
   <update statement: searched>, or <merge statement>.
2) If T is not trigger updatable, then each <column name> specified as an <object column> shall identify an
   updatable column of T.



976 Foundation (SQL/Foundation)
                                                                                                             IWD 9075-2:201?(E)
                                                                                                           14.15 <set clause list>

         NOTE 487 — The notion of updatable columns of base tables is defined in Subclause 4.15, “Tables”. The notion of updatable
         columns of viewed tables is defined in Subclause 11.32, “<view definition>”.

3) No <object column> shall reference a column of which some underlying column is a system-time period
   start column or a system-time period end column.
4) Each <set clause> SC that immediately contains a <multiple column assignment> is effectively replaced
   by a <set clause list> MSCL as follows:
    a)   Let STN be the number of <set target>s contained in <set target list>.
    b) STN shall be equal to the degree of the <assigned row> AR contained in SC.
    c)   Let STi, 1 (one) ≤ i ≤ STN, be the i-th <set target> contained in the <set target list> of SC and let DTi
         be the declared type of the i-th field of AR. The i-th <set clause> in MSCL is:

         STi =
         CAST ( AR AS ROW ( F1 DT1,
         F2 DT2, ...,
         FSTN DTSTN ) ).Fi

                NOTE 488 — “Fn” here stands for the <field name> consisting of the letter “F” followed, with no intervening <separator>
                by the decimal <digit> or <digit>s comprising a <literal> corresponding to the value n.

5) If <set clause> SC specifies an <object column> that references a column of which some underlying column
   is either a generated column or an identity column whose descriptor indicates that values are always gen-
   erated, then the <update source> specified in SC shall consist of a <default specification>.
6) A <value expression> simply contained in an <update source> in a <set clause> shall not directly contain
   a <set function specification>.
7) If the <set clause list> OSCL contains one or more <set clause>s that contain a <mutated set clause>, then:
    a)   Let N be the number of <set clause>s in OSCL that contain a <mutated set clause>.
    b) For 1 (one) ≤ i ≤ N:
         i)        Let SCi be the i-th <set clause> that contains a <mutated set clause>.

         ii)       Let RCVEi be the <update source> immediately contained in SCi.

         iii)      Let MSCi be the <mutated set clause> immediately contained in the <set target> immediately
                   contained in SCi.

         iv)       Let OCi be the <object column> contained in MSCi. The declared type of the column identified
                   by OCi shall be a structured type.

         v)        Let Mi be the number of <method name>s contained in MSCi.

         vi)       For 1 (one) ≤ j ≤ Mi:

                   Case:
                   1) If j = 1 (one), then
                        A) Let MTi,1 be the <mutated target> immediately contained in MSCi.



                                                                                                          Data manipulation 977
IWD 9075-2:201?(E)
14.15 <set clause list>

                      B) Let MNi,1 be the <method name> immediately contained in MSCi.

                      C) Let Vi,1 be:

                           MTi,1 . MNi,1 ( RCVEi )

                 2) Otherwise:
                      A) Let MTi,j be the <mutated target> immediately contained in the <mutated set clause>
                         immediately contained in MTi,j-1.

                      B) Let MNi,j be the <method name> immediately contained in the <mutated set clause>
                         immediately contained in MTi,j-1.

                      C) Let Vi,j be

                           MTi,j . MNi,j ( Vi,j-1 )

    c)   OSCL is equivalent to a <set clause list> NSCL derived as follows:
         i)      Let NSCL be a <set clause list> derived from OSCL by replacing every <set clause> SCa, 1
                 (one) ≤ a ≤ N, that contains a <mutated set clause> with:

                 MTa,Ma = Va,Ma

         ii)     For 1 (one) ≤ b ≤ N, if there exists a c such that c < b and OCc is equivalent to OCb, then:

                 1) Every occurrence of OCb in Vb,Mb is replaced by Vc,Mc.

                 2) SCc is deleted from NSCL.

8) Equivalent <object column>s shall not appear more than once in a <set clause list>.
         NOTE 489 — Multiple occurrences of equivalent <object column>s within <mutated set clause>s are eliminated by the
         preceding Syntax Rule of this Subclause.

9) If the <update source> of <set clause> SC specifies a <contextually typed value specification> CVS, then
   the data type of CVS is the data type DT of the <update target> or <mutated set clause> specified in SC.
10) If CVS is an <empty specification>, then DT shall be a collection type or a distinct type whose source type
    is a collection type. If CVS specifies ARRAY, then DT shall be an array type or a distinct type whose
    source type is an array type. If CVS specifies MULTISET, then DT shall be a multiset type or a distinct
    type whose source type is a multiset type.
11) For every <object column> in a <set clause>,
    Case:
    a)   If the <update target> immediately contains <simple value specification>, then the declared type of
         the column of T identified by the <object column> shall be an array type or a distinct type whose
         source type is an array type. The Syntax Rules of Subclause 9.2, “Store assignment”, are applied with
         a temporary site whose declared type is element type of the column of T identified by the <object
         column> as TARGET and the <update source> of the <set clause> as VALUE.




978 Foundation (SQL/Foundation)
                                                                                             IWD 9075-2:201?(E)
                                                                                           14.15 <set clause list>

    b) Otherwise, the Syntax Rules of Subclause 9.2, “Store assignment”, are applied with the column of T
       identified by the <object column> as TARGET and the <update source> of the <set clause> as VALUE.


Access Rules
    None.


General Rules
1) A <set clause> specifies one or more object columns and an update value. An object column is a column
   identified by an <object column> in the <set clause>. The update value is the value specified by the <update
   source> contained in the <set clause>.
2) The value of the i-th object column denoted by C, is replaced as follows.
    Case:
    a)   If the i-th <set clause> contains an <update target> that immediately contains a <simple value specifi-
         cation>, then
         Case:
         i)      If the value of C is the null value, then an exception condition is raised: data exception — null
                 value in array target.
         ii)     Otherwise:
                 1) Let N be the maximum cardinality of C.
                 2) Let M be the cardinality of the value of C.
                 3) Let I be the value of the <simple value specification> immediately contained in <update
                    target>.
                 4) Let EDT be the element type of C.
                 5) Case:
                     A) If I is greater than zero and less than or equal to M, then the value of C is replaced by
                        an array A with element type EDT and cardinality M derived as follows:
                          I)     For j varying from 1 (one) to I–1 and from I+1 to M, the j-th element in A is
                                 the value of the j-th element in C.
                          II)    The General Rules of Subclause 9.2, “Store assignment”, are applied with I-th
                                 element of A as TARGET and the i-th update value, denoted by SV as VALUE.
                     B) If I is greater than M and less than or equal to N, then the value of C is replaced by an
                        array A with element type EDT and cardinality I derived as follows:
                          I)     For j varying from 1 (one) to M, the j-th element in A is the value of the j-th
                                 element in C.
                          II)    For j varying from M+1 to I–1, the j-th element in A is the null value.




                                                                                         Data manipulation 979
IWD 9075-2:201?(E)
14.15 <set clause list>

                          III)   The General Rules of Subclause 9.2, “Store assignment”, are applied with I-th
                                 element of A as TARGET and the i-th update value, denoted by SV as VALUE.
                     C) Otherwise, an exception condition is raised: data exception — array element error.
    b) Otherwise, the value of C is replaced by the i-th update value, denoted by SV. The General Rules of
       Subclause 9.2, “Store assignment”, are applied with C as TARGET and SV as VALUE.


Conformance Rules
1) Without Feature F781, “Self-referencing operations”, conforming SQL language shall not contain a <set
   clause> in which either of the following is true:
    a)   A leaf generally underlying table of T is an underlying table of any <query expression> broadly con-
         tained in any <value expression> simply contained in an <update source> or <assigned row> immedi-
         ately contained in the <set clause>.
    b) An <update source> or <assigned row> immediately contained in the <set clause> broadly contains
       a <routine invocation>, <method invocation>, <static method invocation>, or <method reference>
       whose subject routine is an external routine that possibly reads SQL-data.
2) Without Feature S091, “Basic array support”, conforming SQL language shall not contain an <update
   target> that immediately contains a <simple value specification>.
3) Without Feature S024, “Enhanced structured types”, conforming SQL language shall not contain a <set
   clause> in which the declared type of the <update target> in the <set clause> is a structured type TY and
   the declared type of the <update source> or corresponding field of the <assigned row> contained in the
   <set clause> is not TY.
4) Without Feature S024, “Enhanced structured types”, conforming SQL language shall not contain a <set
   clause> that contains a <mutated set clause> and in which the declared type of the last <method name>
   identifies a structured type TY, and the declared type of the <update source> contained in the <set clause>
   is not TY.
5) Without Feature T641, “Multiple column assignment”, conforming SQL language shall not contain a
   <multiple column assignment>.




980 Foundation (SQL/Foundation)
                                                                                         IWD 9075-2:201?(E)
                                                                           14.16 <temporary table declaration>


14.16 <temporary table declaration>

This Subclause is modified by Subclause 12.8, “<temporary table declaration>”, in ISO/IEC 9075-4.


Function
Declare a declared local temporary table.


Format
<temporary table declaration> ::=
  DECLARE LOCAL TEMPORARY TABLE <table name> <table element list>
      [ ON COMMIT <table commit action> ROWS ]


Syntax Rules
1) Let TN be the <table name> of a <temporary table declaration> TTD, and let T be the <qualified identifier>
   of TN.
2)    04    TTD shall be contained in an <SQL-client module definition>.
3) Case:
     a)       If TN contains a <local or schema qualifier> LSQ, then LSQ shall be “MODULE”.
     b) If TN does not contain a <local or schema qualifier>, then “MODULE” is implicit.
4)    04  If a <temporary table declaration> is contained in an <SQL-client module definition> M, then the

     <qualified identifier> of TN shall not be equivalent to the <qualified identifier> of the <table name> of
     any other <temporary table declaration> that is contained in M.
5) The descriptor of the table defined by a <temporary table declaration> includes TN and the column
   descriptor specified by each <column definition>. The i-th column descriptor is given by the i-th <column
   definition>.
6) <table element list> shall contain at least one <column definition> or at least one <like clause>.
7) <table element list> shall not contain a <table element> that is a <table period definition>.
8) If ON COMMIT is not specified, then ON COMMIT DELETE ROWS is implicit.


Access Rules
     None.


General Rules
1) Let U be the implementation-dependent <schema name> of the schema that contains the declared local
   temporary table such that U does not contain a table whose <table name> is equivalent to TN.
2) Let UI be the current user identifier and let R be the current role name.


                                                                                        Data manipulation 981
IWD 9075-2:201?(E)
14.16 <temporary table declaration>

     Case:
     a)   If UI is not the null value, then let A be UI.
     b) Otherwise, let A be R.
3)    04  The definition of T within an SQL-client module is effectively equivalent to the definition of a persistent

     base table U.T. Within the SQL-client module, any reference to MODULE.T is equivalent to a reference
     to U.T.
4) A set of privilege descriptors is created that define the privileges INSERT, SELECT, UPDATE, DELETE,
   and REFERENCES on this table and INSERT, SELECT, UPDATE, and REFERENCES for every <column
   definition> in the table definition to A. These privileges are not grantable. The grantor for each of these
   privilege descriptors is set to the special grantor value “_SYSTEM”. The grantee is “PUBLIC”.
5) The definition of a temporary table persists for the duration of the SQL-session. The termination of the
   SQL-session is effectively followed by the execution of the following <drop table statement> with the
   current authorization identifier A and current <schema name> U without further Access Rule checking:

     DROP TABLE T CASCADE

6) The definition of a declared local temporary table does not appear in any view of the Information Schema.
          NOTE 490 — The Information Schema is defined in [ISO9075-11].


Conformance Rules
1) Without Feature F531, “Temporary tables”, conforming SQL language shall not contain a <temporary
   table declaration>.




982 Foundation (SQL/Foundation)
                                                                                           IWD 9075-2:201?(E)
                                                                                 14.17 <free locator statement>


14.17 <free locator statement>

Function
Remove the association between a locator variable and the value that is represented by that locator.


Format
<free locator statement> ::=
  FREE LOCATOR <locator reference> [ { <comma> <locator reference> }... ]

<locator reference> ::=
    <host parameter name>
  | <embedded variable name>
  | <dynamic parameter specification>


Syntax Rules
1) Each host parameter identified by <host parameter name> immediately contained in <locator reference>
   shall be a binary large object locator parameter, a character large object locator parameter, an array locator
   parameter, a multiset locator parameter, or a user-defined type locator parameter.
2) Each host variable identified by the <embedded variable name> immediately contained in <locator refer-
   ence> shall be a binary large object locator variable, a character large object locator variable, an array
   locator variable, a multiset locator variable, or a user-defined type locator variable.


Access Rules
    None.


General Rules
1) For every <locator reference> LR immediately contained in <free locator statement>, let L be the value of
   LR.
    Case:
    a)   If L is not a valid locator value, then an exception condition is raised: locator exception — invalid
         specification.
    b) Otherwise, L is marked invalid.


Conformance Rules
1) Without Feature T561, “Holdable locators”, conforming SQL language shall not contain a <free locator
   statement>.




                                                                                        Data manipulation 983
IWD 9075-2:201?(E)
14.18 <hold locator statement>


14.18 <hold locator statement>

Function
Mark a locator variable as being holdable.


Format
<hold locator statement> ::=
  HOLD LOCATOR <locator reference> [ { <comma> <locator reference> }... ]


Syntax Rules
1) Each host parameter identified by <host parameter name> immediately contained in <locator reference>
   shall be a binary large object locator parameter, a character large object locator parameter, an array locator
   parameter, a multiset locator parameter, or a user-defined type locator parameter.


Access Rules
    None.


General Rules
1) For every <locator reference> LR immediately contained in <hold locator statement>, let L be the value
   of LR.
    Case:
    a)   If L is not a valid locator value, then an exception condition is raised: locator exception — invalid
         specification.
    b) Otherwise, L is marked holdable.


Conformance Rules
1) Without Feature T561, “Holdable locators”, conforming SQL language shall not contain a <hold locator
   statement>.



