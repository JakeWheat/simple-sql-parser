
-- | These is the pretty printing functions, which produce SQL
-- source from ASTs. The code attempts to format the output in a
-- readable way.
{-# LANGUAGE OverloadedStrings #-}
module Language.SQL.SimpleSQL.Pretty
    (prettyQueryExpr
    ,prettyScalarExpr
    ,prettyStatement
    ,prettyStatements
    ) where

{-
TODO: there should be more comments in this file, especially the bits
which have been changed to try to improve the layout of the output.
-}

import Prelude hiding (show)
import qualified Prelude as P

import Prettyprinter (Doc
                     ,parens
                     ,nest
                     ,(<+>)
                     ,sep
                     ,punctuate
                     ,comma
                     ,squotes
                     ,vsep
                     ,hsep
                     ,layoutPretty
                     ,defaultLayoutOptions
                     ,brackets
                     )
import qualified Prettyprinter as P

import Prettyprinter.Render.Text (renderStrict)

import Data.Maybe (maybeToList, catMaybes)
import Data.List (intercalate)

import qualified Data.Text as T
import Data.Text (Text)

import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Dialect


-- | Convert a query expr ast to concrete syntax.
prettyQueryExpr :: Dialect -> QueryExpr -> Text
prettyQueryExpr d = render . queryExpr d

-- | Convert a value expr ast to concrete syntax.
prettyScalarExpr :: Dialect -> ScalarExpr -> Text
prettyScalarExpr d = render . scalarExpr d

-- | A terminating semicolon.
terminator :: Doc a
terminator = pretty ";\n"

-- | Convert a statement ast to concrete syntax.
prettyStatement :: Dialect -> Statement -> Text
prettyStatement _ EmptyStatement = render terminator
prettyStatement d s = render (statement d s)

-- | Convert a list of statements to concrete syntax. A semicolon
-- is inserted after each statement.
prettyStatements :: Dialect -> [Statement] -> Text
prettyStatements d = render . vsep . map prettyStatementWithSemicolon
  where
    prettyStatementWithSemicolon :: Statement -> Doc a
    prettyStatementWithSemicolon s = statement d s <> terminator

render :: Doc a -> Text
render = renderStrict . layoutPretty defaultLayoutOptions

-- = scalar expressions

scalarExpr :: Dialect -> ScalarExpr -> Doc a
scalarExpr _ (StringLit s e t) = pretty s <> pretty t <> pretty e

scalarExpr _ (NumLit s) = pretty s
scalarExpr _ (IntervalLit s v f t) =
    pretty "interval"
    <+> me (\x -> pretty $ case x of
                             Plus -> "+"
                             Minus -> "-") s
    <+> squotes (pretty v)
    <+> intervalTypeField f
    <+> me (\x -> pretty "to" <+> intervalTypeField x) t
scalarExpr _ (Iden i) = names i
scalarExpr _ Star = pretty "*"
scalarExpr _ Parameter = pretty "?"
scalarExpr _ (PositionalArg n) = pretty $ T.cons '$' $ show n
scalarExpr _ (HostParameter p i) =
    pretty p
    <+> me (\i' -> pretty "indicator" <+> pretty i') i

scalarExpr d (App f es) = names f <> parens (commaSep (map (scalarExpr d) es))

scalarExpr dia (AggregateApp f d es od fil) =
    names f
    <> parens ((case d of
                  Distinct -> pretty "distinct"
                  All -> pretty "all"
                  SQDefault -> mempty)
               <+> commaSep (map (scalarExpr dia) es)
               <+> orderBy dia od)
    <+> me (\x -> pretty "filter"
                  <+> parens (pretty "where" <+> scalarExpr dia x)) fil

scalarExpr d (AggregateAppGroup f es od) =
    names f
    <> parens (commaSep (map (scalarExpr d) es))
    <+> if null od
        then mempty
        else pretty "within group" <+> parens (orderBy d od)

scalarExpr d (WindowApp f es pb od fr) =
    names f <> parens (commaSep $ map (scalarExpr d) es)
    <+> pretty "over"
    <+> parens ((case pb of
                    [] -> mempty
                    _ -> pretty "partition by"
                          <+> nest 13 (commaSep $ map (scalarExpr d) pb))
                <+> orderBy d od
    <+> me frd fr)
  where
    frd (FrameFrom rs fp) = rsd rs <+> fpd fp
    frd (FrameBetween rs fps fpe) =
        rsd rs <+> pretty "between" <+> fpd fps
        <+> pretty "and" <+> fpd fpe
    rsd rs = case rs of
                 FrameRows -> pretty "rows"
                 FrameRange -> pretty "range"
    fpd UnboundedPreceding = pretty "unbounded preceding"
    fpd UnboundedFollowing = pretty "unbounded following"
    fpd Current = pretty "current row"
    fpd (Preceding e) = scalarExpr d e <+> pretty "preceding"
    fpd (Following e) = scalarExpr d e <+> pretty "following"

scalarExpr dia (SpecialOp nm [a,b,c]) | nm `elem` [[Name Nothing "between"]
                                                 ,[Name Nothing "not between"]] =
  sep [scalarExpr dia a
      ,names nm <+> scalarExpr dia b
      ,nest (T.length (unnames nm) + 1) $ pretty "and" <+> scalarExpr dia c]

scalarExpr d (SpecialOp [Name Nothing "rowctor"] as) =
    parens $ commaSep $ map (scalarExpr d) as

scalarExpr d (SpecialOp nm es) =
  names nm <+> parens (commaSep $ map (scalarExpr d) es)

scalarExpr d (SpecialOpK nm fs as) =
    names nm <> parens (sep $ catMaybes
        (fmap (scalarExpr d) fs
         : map (\(n,e) -> Just (pretty n <+> scalarExpr d e)) as))

scalarExpr d (PrefixOp f e) = names f <+> scalarExpr d e
scalarExpr d (PostfixOp f e) = scalarExpr d e <+> names f
scalarExpr d e@(BinOp _ op _) | op `elem` [[Name Nothing "and"]
                                         ,[Name Nothing "or"]] =
    -- special case for and, or, get all the ands so we can vsep them
    -- nicely
    case ands e of
      (e':es) -> vsep (scalarExpr d e'
                       : map ((names op <+>) . scalarExpr d) es)
      [] -> mempty -- shouldn't be possible
  where
    ands (BinOp a op' b) | op == op' = ands a <> ands b
    ands x = [x]
-- special case for . we don't use whitespace
scalarExpr d (BinOp e0 [Name Nothing "."] e1) =
    scalarExpr d e0 <> pretty "." <> scalarExpr d e1
scalarExpr d (BinOp e0 f e1) =
    scalarExpr d e0 <+> names f <+> scalarExpr d e1

scalarExpr dia (Case t ws els) =
    sep $ [pretty "case" <+> me (scalarExpr dia) t]
          <> map w ws
          <> maybeToList (fmap e els)
          <> [pretty "end"]
  where
    w (t0,t1) =
      pretty "when" <+> nest 5 (commaSep $ map (scalarExpr dia) t0)
      <+> pretty "then" <+> nest 5 (scalarExpr dia t1)
    e el = pretty "else" <+> nest 5 (scalarExpr dia el)
scalarExpr d (Parens e) = parens $ scalarExpr d e
scalarExpr d (Cast e tn) =
    pretty "cast" <> parens (sep [scalarExpr d e
                                 ,pretty "as"
                                 ,typeName tn])

scalarExpr _ (TypedLit tn s) =
    typeName tn <+> squotes (pretty s)

scalarExpr d (SubQueryExpr ty qe) =
    (case ty of
        SqSq -> mempty
        SqExists -> pretty "exists"
        SqUnique -> pretty "unique"
    ) <+> parens (queryExpr d qe)

scalarExpr d (QuantifiedComparison v c cp sq) =
    scalarExpr d v
    <+> names c
    <+> (pretty $ case cp of
             CPAny -> "any"
             CPSome -> "some"
             CPAll -> "all")
    <+> parens (queryExpr d sq)

scalarExpr d (Match v u sq) =
    scalarExpr d v
    <+> pretty "match"
    <+> (if u then pretty "unique" else mempty)
    <+> parens (queryExpr d sq)

scalarExpr d (In b se x) =
    scalarExpr d se <+>
    (if b then mempty else pretty "not")
    <+> pretty "in"
    <+> parens (nest (if b then 3 else 7) $
                 case x of
                     InList es -> commaSep $ map (scalarExpr d) es
                     InQueryExpr qe -> queryExpr d qe)

scalarExpr d (Array v es) =
    scalarExpr d v <> brackets (commaSep $ map (scalarExpr d) es)

scalarExpr d (ArrayCtor q) =
    pretty "array" <> parens (queryExpr d q)

scalarExpr d (MultisetCtor es) =
    pretty "multiset" <> brackets (commaSep $ map (scalarExpr d) es)

scalarExpr d (MultisetQueryCtor q) =
    pretty "multiset" <> parens (queryExpr d q)

scalarExpr d (MultisetBinOp a c q b) =
    sep
    [scalarExpr d a
    ,pretty "multiset"
    ,pretty $ case c of
                Union -> "union"
                Intersect -> "intersect"
                Except -> "except"
    ,case q of
         SQDefault -> mempty
         All -> pretty "all"
         Distinct -> pretty "distinct"
    ,scalarExpr d b]

{-scalarExpr d (Escape v e) =
    scalarExpr d v <+> pretty "escape" <+> pretty [e]

scalarExpr d (UEscape v e) =
    scalarExpr d v <+> pretty "uescape" <+> pretty [e]-}

scalarExpr d (Collate v c) =
    scalarExpr d v <+> pretty "collate" <+> names c

scalarExpr _ (NextValueFor ns) =
    pretty "next value for" <+> names ns

scalarExpr d (VEComment cmt v) =
    vsep $ map comment cmt <> [scalarExpr d v]

scalarExpr _ (OdbcLiteral t s) =
    pretty "{" <> lt t <+> squotes (pretty s) <> pretty "}"
  where
    lt OLDate = pretty "d"
    lt OLTime = pretty "t"
    lt OLTimestamp = pretty "ts"

scalarExpr d (OdbcFunc e) =
    pretty "{fn" <+> scalarExpr d e <> pretty "}"

scalarExpr d (Convert t e Nothing) =
    pretty "convert(" <> typeName t <> pretty "," <+> scalarExpr d e <> pretty ")"
scalarExpr d (Convert t e (Just i)) =
    pretty "convert(" <> typeName t <> pretty "," <+> scalarExpr d e <> pretty "," <+> pretty (show i) <> pretty ")"

unname :: Name -> Text
unname (Name Nothing n) = n
unname (Name (Just (s,e)) n) =
    s <> n <> e

unnames :: [Name] -> Text
unnames ns = T.intercalate "." $ map unname ns


name :: Name -> Doc a
name (Name Nothing n) = pretty n
name (Name (Just (s,e)) n) = pretty s <> pretty n <> pretty e

names :: [Name] -> Doc a
names ns = hsep $ punctuate (pretty ".") $ map name ns

typeName :: TypeName -> Doc a
typeName (TypeName t) = names t
typeName (PrecTypeName t a) = names t <+> parens (pretty $ show a)
typeName (PrecScaleTypeName t a b) =
    names t <+> parens (pretty (show a) <+> comma <+> pretty (show b))
typeName (PrecLengthTypeName t i m u) =
    names t
    <> parens (pretty (show i)
               <> me (\x -> case x of
                           PrecK -> pretty "K"
                           PrecM -> pretty "M"
                           PrecG -> pretty "G"
                           PrecT -> pretty "T"
                           PrecP -> pretty "P") m
               <+> me (\x -> case x of
                       PrecCharacters -> pretty "CHARACTERS"
                       PrecOctets -> pretty "OCTETS") u)
typeName (CharTypeName t i cs col) =
    names t
    <> me (\x -> parens (pretty $ show x)) i
    <+> (if null cs
         then mempty
         else pretty "character set" <+> names cs)
    <+> (if null col
         then mempty
         else pretty "collate" <+> names col)
typeName (TimeTypeName t i tz) =
    names t
    <> me (\x -> parens (pretty $ show x)) i
    <+> pretty (if tz
              then "with time zone"
              else "without time zone")
typeName (RowTypeName cs) =
    pretty "row" <> parens (commaSep $ map f cs)
  where
    f (n,t) = name n <+> typeName t
typeName (IntervalTypeName f t) =
    pretty "interval"
    <+> intervalTypeField f
    <+> me (\x -> pretty "to" <+> intervalTypeField x) t

typeName (ArrayTypeName tn sz) =
    typeName tn <+> pretty "array" <+> me (brackets . pretty . show) sz

typeName (MultisetTypeName tn) =
    typeName tn <+> pretty "multiset"

intervalTypeField :: IntervalTypeField -> Doc a
intervalTypeField (Itf n p) =
    pretty n
    <+> me (\(x,x1) ->
             parens (pretty (show x)
                     <+> me (\y -> (sep [comma,pretty (show y)])) x1)) p


-- = query expressions

queryExpr :: Dialect -> QueryExpr -> Doc a
queryExpr dia (Select d sl fr wh gb hv od off fe) =
  sep [pretty "select"
      ,case d of
          SQDefault -> mempty
          All -> pretty "all"
          Distinct -> pretty "distinct"
      ,nest 7 $ sep [selectList dia sl]
      ,from dia fr
      ,maybeScalarExpr dia "where" wh
      ,grpBy dia gb
      ,maybeScalarExpr dia "having" hv
      ,orderBy dia od
      ,me (\e -> pretty "offset" <+> scalarExpr dia e <+> pretty "rows") off
      ,fetchFirst
      ]
  where
    fetchFirst =
      me (\e -> if diLimit dia
                then pretty "limit" <+> scalarExpr dia e
                else pretty "fetch first" <+> scalarExpr dia e
                     <+> pretty "rows only") fe

queryExpr dia (QueryExprSetOp q1 ct d c q2) =
  sep [queryExpr dia q1
      ,pretty (case ct of
                Union -> "union"
                Intersect -> "intersect"
                Except -> "except")
       <+> case d of
               SQDefault -> mempty
               All -> pretty "all"
               Distinct -> pretty "distinct"
       <+> case c of
               Corresponding -> pretty "corresponding"
               Respectively -> mempty
      ,queryExpr dia q2]
queryExpr d (With rc withs qe) =
  pretty "with" <+> (if rc then pretty "recursive" else mempty)
  <+> vsep [nest 5
            (vsep $ punctuate comma $ flip map withs $ \(n,q) ->
             withAlias n <+> pretty "as" <+> parens (queryExpr d q))
           ,queryExpr d qe]
  where
    withAlias (Alias nm cols) = name nm
                                <+> me (parens . commaSep . map name) cols


queryExpr d (Values vs) =
    pretty "values"
    <+> nest 7 (commaSep (map (parens . commaSep . map (scalarExpr d)) vs))
queryExpr _ (Table t) = pretty "table" <+> names t
queryExpr d (QEComment cmt v) =
    vsep $ map comment cmt <> [queryExpr d v]


alias :: Alias -> Doc a
alias (Alias nm cols) =
    pretty "as" <+> name nm
    <+> me (parens . commaSep . map name) cols

selectList :: Dialect -> [(ScalarExpr,Maybe Name)] -> Doc a
selectList d is = commaSep $ map si is
  where
    si (e,al) = scalarExpr d e <+> me als al
    als al = pretty "as" <+> name al

from :: Dialect -> [TableRef] -> Doc a
from _ [] = mempty
from d ts =
    sep [pretty "from"
        ,nest 5 $ vsep $ punctuate comma $ map tr ts]
  where
    tr (TRSimple t) = names t
    tr (TRLateral t) = pretty "lateral" <+> tr t
    tr (TRFunction f as) =
        names f <> parens (commaSep $ map (scalarExpr d) as)
    tr (TRAlias t a) = sep [tr t, alias a]
    tr (TRParens t) = parens $ tr t
    tr (TRQueryExpr q) = parens $ queryExpr d q
    tr (TRJoin t0 b jt t1 jc) =
       sep [tr t0
           ,if b then pretty "natural" else mempty
           ,joinText jt <+> tr t1
           ,joinCond jc]
    tr (TROdbc t) = pretty "{oj" <+> tr t <+> pretty "}"
    joinText jt =
      sep [case jt of
              JInner -> pretty "inner"
              JLeft -> pretty "left"
              JRight -> pretty "right"
              JFull -> pretty "full"
              JCross -> pretty "cross"
          ,pretty "join"]
    joinCond (Just (JoinOn e)) = pretty "on" <+> scalarExpr d e
    joinCond (Just (JoinUsing es)) =
        pretty "using" <+> parens (commaSep $ map name es)
    joinCond Nothing = mempty

maybeScalarExpr :: Dialect -> Text -> Maybe ScalarExpr -> Doc a
maybeScalarExpr d k = me
      (\e -> sep [pretty k
                 ,nest (T.length k + 1) $ scalarExpr d e])

grpBy :: Dialect -> [GroupingExpr] -> Doc a
grpBy _ [] = mempty
grpBy d gs = sep [pretty "group by"
                 ,nest 9 $ commaSep $ map ge gs]
  where
    ge (SimpleGroup e) = scalarExpr d e
    ge (GroupingParens g) = parens (commaSep $ map ge g)
    ge (Cube es) = pretty "cube" <> parens (commaSep $ map ge es)
    ge (Rollup es) = pretty "rollup" <> parens (commaSep $ map ge es)
    ge (GroupingSets es) = pretty "grouping sets" <> parens (commaSep $ map ge es)

orderBy :: Dialect -> [SortSpec] -> Doc a
orderBy _ [] = mempty
orderBy dia os = sep [pretty "order by"
                 ,nest 9 $ commaSep $ map f os]
  where
    f (SortSpec e d n) =
        scalarExpr dia e
        <+> (case d of
                  Asc -> pretty "asc"
                  Desc -> pretty "desc"
                  DirDefault -> mempty)
        <+> (case n of
                NullsOrderDefault -> mempty
                NullsFirst -> pretty "nulls" <+> pretty "first"
                NullsLast -> pretty "nulls" <+> pretty "last")

-- = statements

statement :: Dialect -> Statement -> Doc a


-- == ddl

statement _ (CreateSchema nm) =
    pretty "create" <+> pretty "schema" <+> names nm

statement d (CreateTable nm cds) =
    pretty "create" <+> pretty "table" <+> names nm
    <+> parens (commaSep $ map cd cds)
  where
    cd (TableConstraintDef n con) =
        maybe mempty (\s -> pretty "constraint" <+> names s) n
        <+> tableConstraint d con
    cd (TableColumnDef cd') = columnDef d cd'

statement d (AlterTable t act) =
    texts ["alter","table"] <+> names t
    <+> alterTableAction d act

statement _ (DropSchema nm db) =
    pretty "drop" <+> pretty "schema" <+> names nm <+> dropBehav db

statement d (CreateDomain nm ty def cs) =
    pretty "create" <+> pretty "domain" <+> names nm
    <+> typeName ty
    <+> maybe mempty (\def' -> pretty "default" <+> scalarExpr d def') def
    <+> sep (map con cs)
  where
    con (cn, e) =
        maybe mempty (\cn' -> pretty "constraint" <+> names cn') cn
        <+> pretty "check" <> parens (scalarExpr d e)

statement d (AlterDomain nm act) =
    texts ["alter","domain"]
    <+> names nm
    <+> a act
  where
    a (ADSetDefault v) = texts ["set","default"] <+> scalarExpr d v
    a (ADDropDefault) = texts ["drop","default"]
    a (ADAddConstraint cnm e) =
        pretty "add"
        <+> maybe mempty (\cnm' -> pretty "constraint" <+> names cnm') cnm
        <+> pretty "check" <> parens (scalarExpr d e)
    a (ADDropConstraint cnm) = texts ["drop", "constraint"]
                               <+> names cnm


statement _ (DropDomain nm db) =
    pretty "drop" <+> pretty "domain" <+> names nm <+> dropBehav db

statement _ (CreateSequence nm sgos) =
  texts ["create","sequence"] <+> names nm
  <+> sep (map sequenceGeneratorOption sgos)

statement _ (AlterSequence nm sgos) =
  texts ["alter","sequence"] <+> names nm
  <+> sep (map sequenceGeneratorOption sgos)

statement _ (DropSequence nm db) =
    pretty "drop" <+> pretty "sequence" <+> names nm <+> dropBehav db


statement d (CreateAssertion nm ex) =
  texts ["create","assertion"] <+> names nm
  <+> pretty "check" <+> parens (scalarExpr d ex)

statement _ (DropAssertion nm db) =
    pretty "drop" <+> pretty "assertion" <+> names nm <+> dropBehav db

statement _ (CreateIndex un nm tbl cols) =
  texts (if un
         then ["create","unique","index"]
         else  ["create","index"])
  <+> names nm
  <+> pretty "on"
  <+> names tbl
  <+> parens (commaSep $ map name cols)

-- == dml

statement d (SelectStatement q) = queryExpr d q

statement d (Delete t a w) =
    pretty "delete" <+> pretty "from"
    <+> names t <+> maybe mempty (\x -> pretty "as" <+> name x) a
    <+> maybeScalarExpr d "where" w

statement _ (Truncate t ir) =
    pretty "truncate" <+> pretty "table" <+> names t
    <+> case ir of
            DefaultIdentityRestart -> mempty
            ContinueIdentity -> pretty "continue" <+> pretty "identity"
            RestartIdentity -> pretty "restart" <+> pretty "identity"

statement d (Insert t cs s) =
    pretty "insert" <+> pretty "into" <+> names t
    <+> maybe mempty (\cs' -> parens (commaSep $ map name cs')) cs
    <+> case s of
            DefaultInsertValues -> pretty "default" <+> pretty "values"
            InsertQuery q -> queryExpr d q

statement d (Update t a sts whr) =
    pretty "update" <+> names t
    <+> maybe mempty (\x -> pretty "as" <+> name x) a
    <+> pretty "set" <+> commaSep (map sc sts)
    <+> maybeScalarExpr d "where" whr
  where
    sc (Set tg v) = names tg <+> pretty "=" <+> scalarExpr d v
    sc (SetMultiple ts vs) = parens (commaSep $ map names ts) <+> pretty "="
                             <+> parens (commaSep $ map (scalarExpr d) vs)

statement _ (DropTable n b) =
    pretty "drop" <+> pretty "table" <+> names n <+> dropBehav b

statement d (CreateView r nm al q co) =
    pretty "create" <+> (if r then pretty "recursive" else mempty)
    <+> pretty "view" <+> names nm
    <+> (maybe mempty (\al' -> parens $ commaSep $ map name al')) al
    <+> pretty "as"
    <+> queryExpr d q
    <+> case co of
            Nothing -> mempty
            Just DefaultCheckOption -> texts ["with", "check", "option"]
            Just CascadedCheckOption -> texts ["with", "cascaded", "check", "option"]
            Just LocalCheckOption -> texts ["with", "local", "check", "option"]

statement _ (DropView n b) =
    pretty "drop" <+> pretty "view" <+> names n <+> dropBehav b


-- == transactions

statement _ StartTransaction =
    texts ["start", "transaction"]

statement _ (Savepoint nm) =
    pretty "savepoint" <+> name nm

statement _ (ReleaseSavepoint nm) =
    texts ["release", "savepoint"] <+> name nm

statement _ Commit =
    pretty "commit"

statement _ (Rollback mn) =
    pretty "rollback"
    <+> maybe mempty (\n -> texts ["to","savepoint"] <+> name n) mn

-- == access control

statement _ (GrantPrivilege pas po rs go) =
    pretty "grant" <+> commaSep (map privAct pas)
    <+> pretty "on" <+> privObj po
    <+> pretty "to" <+> commaSep (map name rs)
    <+> grantOpt go
  where
    grantOpt WithGrantOption = texts ["with","grant","option"]
    grantOpt WithoutGrantOption = mempty

statement _ (GrantRole rs trs ao) =
    pretty "grant" <+> commaSep (map name rs)
    <+> pretty "to" <+> commaSep (map name trs)
    <+> adminOpt ao
  where
    adminOpt WithAdminOption = texts ["with","admin","option"]
    adminOpt WithoutAdminOption = mempty

statement _ (CreateRole nm) =
    texts ["create","role"] <+> name nm

statement _ (DropRole nm) =
    texts ["drop","role"] <+> name nm

statement _ (RevokePrivilege go pas po rs db) =
    pretty "revoke"
    <+> grantOptFor go
    <+> commaSep (map privAct pas)
    <+> pretty "on" <+> privObj po
    <+> pretty "from" <+> commaSep (map name rs)
    <+> dropBehav db
  where
    grantOptFor GrantOptionFor = texts ["grant","option","for"]
    grantOptFor NoGrantOptionFor = mempty

statement _ (RevokeRole ao rs trs db) =
    pretty "revoke"
    <+> adminOptFor ao
    <+> commaSep (map name rs)
    <+> pretty "from" <+> commaSep (map name trs)
    <+> dropBehav db
  where
    adminOptFor AdminOptionFor = texts ["admin","option","for"]
    adminOptFor NoAdminOptionFor = mempty


statement _ (StatementComment cs) = vsep $ map comment cs
statement _ EmptyStatement = mempty


{-
== sessions


== extras
-}

dropBehav :: DropBehaviour -> Doc a
dropBehav DefaultDropBehaviour = mempty
dropBehav Cascade = pretty "cascade"
dropBehav Restrict = pretty "restrict"


columnDef :: Dialect -> ColumnDef -> Doc a
columnDef d (ColumnDef n t mdef cons) =
      name n <+> typeName t
      <+> case mdef of
             Nothing -> mempty
             Just (DefaultClause def) ->
                 pretty "default" <+> scalarExpr d def
             Just (GenerationClause e) ->
                 texts ["generated","always","as"] <+> parens (scalarExpr d e)
             Just (IdentityColumnSpec w o) ->
                 pretty "generated"
                 <+> (case w of
                         GeneratedAlways -> pretty "always"
                         GeneratedByDefault -> pretty "by" <+> pretty "default")
                 <+> pretty "as" <+> pretty "identity"
                 <+> (case o of
                         [] -> mempty
                         os -> parens (sep $ map sequenceGeneratorOption os))
      <+> sep (map cdef cons)
  where
    cdef (ColConstraintDef cnm con) =
        maybe mempty (\s -> pretty "constraint" <+> names s) cnm
        <+> pcon con
    pcon ColNotNullConstraint = texts ["not","null"]
    pcon ColNullableConstraint = texts ["null"]
    pcon ColUniqueConstraint = pretty "unique"
    pcon (ColPrimaryKeyConstraint autoincrement) = 
      texts $ ["primary","key"] <> ["autoincrement"|autoincrement]
    --pcon ColPrimaryKeyConstraint = texts ["primary","key"]
    pcon (ColCheckConstraint v) = pretty "check" <+> parens (scalarExpr d v)
    pcon (ColReferencesConstraint tb c m u del) =
        pretty "references"
        <+> names tb
        <+> maybe mempty (\c' -> parens (name c')) c
        <+> refMatch m
        <+> refAct "update" u
        <+> refAct "delete" del

sequenceGeneratorOption :: SequenceGeneratorOption -> Doc a
sequenceGeneratorOption (SGODataType t) =
    pretty "as" <+> typeName t
sequenceGeneratorOption (SGORestart mi) =
    pretty "restart" <+> maybe mempty (\mi' -> texts ["with", show mi']) mi
sequenceGeneratorOption (SGOStartWith i) = texts ["start",  "with", show i]
sequenceGeneratorOption (SGOIncrementBy i) = texts ["increment", "by", show i]
sequenceGeneratorOption (SGOMaxValue i) = texts ["maxvalue", show i]
sequenceGeneratorOption SGONoMaxValue = texts ["no", "maxvalue"]
sequenceGeneratorOption (SGOMinValue i) = texts ["minvalue", show i]
sequenceGeneratorOption SGONoMinValue = texts ["no", "minvalue"]
sequenceGeneratorOption SGOCycle = pretty "cycle"
sequenceGeneratorOption SGONoCycle = pretty "no cycle"

refMatch :: ReferenceMatch -> Doc a
refMatch m = case m of
                     DefaultReferenceMatch -> mempty
                     MatchFull -> texts ["match", "full"]
                     MatchPartial -> texts ["match","partial"]
                     MatchSimple -> texts ["match", "simple"]

refAct :: Text -> ReferentialAction -> Doc a
refAct t a = case a of
                     DefaultReferentialAction -> mempty
                     RefCascade -> texts ["on", t, "cascade"]
                     RefSetNull -> texts ["on", t, "set", "null"]
                     RefSetDefault -> texts ["on", t, "set", "default"]
                     RefRestrict -> texts ["on", t, "restrict"]
                     RefNoAction -> texts ["on", t, "no", "action"]

alterTableAction :: Dialect -> AlterTableAction -> Doc a
alterTableAction d (AddColumnDef cd) =
    texts ["add", "column"] <+> columnDef d cd

alterTableAction d (AlterColumnSetDefault n v) =
    texts ["alter", "column"]
    <+> name n
    <+> texts ["set","default"] <+> scalarExpr d v
alterTableAction _ (AlterColumnDropDefault n) =
    texts ["alter", "column"]
    <+> name n
    <+> texts ["drop","default"]

alterTableAction _ (AlterColumnSetNotNull n) =
    texts ["alter", "column"]
    <+> name n
    <+> texts ["set","not","null"]

alterTableAction _ (AlterColumnDropNotNull n) =
    texts ["alter", "column"]
    <+> name n
    <+> texts ["drop","not","null"]

alterTableAction _ (AlterColumnSetDataType n t) =
    texts ["alter", "column"]
    <+> name n
    <+> texts ["set","data","Type"]
    <+> typeName t

alterTableAction _ (DropColumn n b) =
    texts ["drop", "column"]
    <+> name n
    <+> dropBehav b

alterTableAction d (AddTableConstraintDef n con) =
    pretty "add"
    <+> maybe mempty (\s -> pretty "constraint" <+> names s) n
    <+> tableConstraint d con

alterTableAction _ (DropTableConstraintDef n b) =
    texts ["drop", "constraint"]
    <+> names n
    <+> dropBehav b


tableConstraint :: Dialect -> TableConstraint -> Doc a
tableConstraint _ (TableUniqueConstraint ns) =
         pretty "unique" <+> parens (commaSep $ map name ns)
tableConstraint _ (TablePrimaryKeyConstraint ns) =
        texts ["primary","key"] <+> parens (commaSep $ map name ns)
tableConstraint _ (TableReferencesConstraint cs t tcs m u del) =
        texts ["foreign", "key"]
        <+> parens (commaSep $ map name cs)
        <+> pretty "references"
        <+> names t
        <+> maybe mempty (\c' -> parens (commaSep $ map name c')) tcs
        <+> refMatch m
        <+> refAct "update" u
        <+> refAct "delete" del
tableConstraint d (TableCheckConstraint v) = pretty "check" <+> parens (scalarExpr d v)


privAct :: PrivilegeAction -> Doc a
privAct PrivAll = texts ["all","privileges"]
privAct (PrivSelect cs) = pretty "select" <+> maybeColList cs
privAct (PrivInsert cs) = pretty "insert" <+> maybeColList cs
privAct (PrivUpdate cs) = pretty "update" <+> maybeColList cs
privAct (PrivReferences cs) = pretty "references" <+> maybeColList cs
privAct PrivDelete = pretty "delete"
privAct PrivUsage = pretty "usage"
privAct PrivTrigger = pretty "trigger"
privAct PrivExecute = pretty "execute"

maybeColList :: [Name] -> Doc a
maybeColList cs =
    if null cs
    then mempty
    else parens (commaSep $ map name cs)

privObj :: PrivilegeObject -> Doc a
privObj (PrivTable nm) = names nm
privObj (PrivDomain nm) = pretty "domain" <+> names nm
privObj (PrivType nm) = pretty "type" <+> names nm
privObj (PrivSequence nm) = pretty "sequence" <+> names nm
privObj (PrivFunction nm) = texts ["specific", "function"] <+> names nm

-- = utils

commaSep :: [Doc a] -> Doc a
commaSep ds = sep $ punctuate comma ds

me :: (b -> Doc a) -> Maybe b -> Doc a
me = maybe mempty

comment :: Comment -> Doc a
comment (BlockComment str) = pretty "/*" <+> pretty str <+> pretty "*/"

texts :: [Text] -> Doc a
texts ts = sep $ map pretty ts

-- regular pretty completely defeats the type checker when you want
-- to change the ast and get type errors, instead it just produces
-- incorrect code.
pretty :: Text -> Doc a
pretty = P.pretty

show :: Show a => a -> Text
show = T.pack . P.show
