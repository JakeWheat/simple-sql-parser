
> -- | These is the pretty printing functions, which produce SQL
> -- source from ASTs. The code attempts to format the output in a
> -- readable way.
> module Language.SQL.SimpleSQL.Pretty
>     (prettyQueryExpr
>     ,prettyScalarExpr
>     ,prettyStatement
>     ,prettyStatements
>     ) where

TODO: there should be more comments in this file, especially the bits
which have been changed to try to improve the layout of the output.

> import Language.SQL.SimpleSQL.Syntax
> import Language.SQL.SimpleSQL.Dialect
> import Text.PrettyPrint (render, vcat, text, (<>), (<+>), empty, parens,
>                          nest, Doc, punctuate, comma, sep, quotes,
>                          brackets,hcat)
> import Data.Maybe (maybeToList, catMaybes)
> import Data.List (intercalate)

> -- | Convert a query expr ast to concrete syntax.
> prettyQueryExpr :: Dialect -> QueryExpr -> String
> prettyQueryExpr d = render . queryExpr d

> -- | Convert a value expr ast to concrete syntax.
> prettyScalarExpr :: Dialect -> ScalarExpr -> String
> prettyScalarExpr d = render . scalarExpr d

> -- | Convert a statement ast to concrete syntax.
> prettyStatement :: Dialect -> Statement -> String
> prettyStatement d = render . statement d

> -- | Convert a list of statements to concrete syntax. A semicolon
> -- is inserted after each statement.
> prettyStatements :: Dialect -> [Statement] -> String
> prettyStatements d = render . vcat . map ((<> text ";\n") . statement d)

= scalar expressions

> scalarExpr :: Dialect -> ScalarExpr -> Doc
> scalarExpr _ (StringLit s e t) = text s <> text t <> text e

> scalarExpr _ (NumLit s) = text s
> scalarExpr _ (IntervalLit s v f t) =
>     text "interval"
>     <+> me (\x -> if x then text "+" else text "-") s
>     <+> quotes (text v)
>     <+> intervalTypeField f
>     <+> me (\x -> text "to" <+> intervalTypeField x) t
> scalarExpr _ (Iden i) = names i
> scalarExpr _ Star = text "*"
> scalarExpr _ Parameter = text "?"
> scalarExpr _ (PositionalArg n) = text $ "$" ++ show n
> scalarExpr _ (HostParameter p i) =
>     text p
>     <+> me (\i' -> text "indicator" <+> text i') i

> scalarExpr d (App f es) = names f <> parens (commaSep (map (scalarExpr d) es))

> scalarExpr dia (AggregateApp f d es od fil) =
>     names f
>     <> parens ((case d of
>                   Distinct -> text "distinct"
>                   All -> text "all"
>                   SQDefault -> empty)
>                <+> commaSep (map (scalarExpr dia) es)
>                <+> orderBy dia od)
>     <+> me (\x -> text "filter"
>                   <+> parens (text "where" <+> scalarExpr dia x)) fil

> scalarExpr d (AggregateAppGroup f es od) =
>     names f
>     <> parens (commaSep (map (scalarExpr d) es))
>     <+> if null od
>         then empty
>         else text "within group" <+> parens (orderBy d od)

> scalarExpr d (WindowApp f es pb od fr) =
>     names f <> parens (commaSep $ map (scalarExpr d) es)
>     <+> text "over"
>     <+> parens ((case pb of
>                     [] -> empty
>                     _ -> text "partition by"
>                           <+> nest 13 (commaSep $ map (scalarExpr d) pb))
>                 <+> orderBy d od
>     <+> me frd fr)
>   where
>     frd (FrameFrom rs fp) = rsd rs <+> fpd fp
>     frd (FrameBetween rs fps fpe) =
>         rsd rs <+> text "between" <+> fpd fps
>         <+> text "and" <+> fpd fpe
>     rsd rs = case rs of
>                  FrameRows -> text "rows"
>                  FrameRange -> text "range"
>     fpd UnboundedPreceding = text "unbounded preceding"
>     fpd UnboundedFollowing = text "unbounded following"
>     fpd Current = text "current row"
>     fpd (Preceding e) = scalarExpr d e <+> text "preceding"
>     fpd (Following e) = scalarExpr d e <+> text "following"

> scalarExpr dia (SpecialOp nm [a,b,c]) | nm `elem` [[Name Nothing "between"]
>                                                  ,[Name Nothing "not between"]] =
>   sep [scalarExpr dia a
>       ,names nm <+> scalarExpr dia b
>       ,nest (length (unnames nm) + 1) $ text "and" <+> scalarExpr dia c]

> scalarExpr d (SpecialOp [Name Nothing "rowctor"] as) =
>     parens $ commaSep $ map (scalarExpr d) as

> scalarExpr d (SpecialOp nm es) =
>   names nm <+> parens (commaSep $ map (scalarExpr d) es)

> scalarExpr d (SpecialOpK nm fs as) =
>     names nm <> parens (sep $ catMaybes
>         (fmap (scalarExpr d) fs
>          : map (\(n,e) -> Just (text n <+> scalarExpr d e)) as))

> scalarExpr d (PrefixOp f e) = names f <+> scalarExpr d e
> scalarExpr d (PostfixOp f e) = scalarExpr d e <+> names f
> scalarExpr d e@(BinOp _ op _) | op `elem` [[Name Nothing "and"]
>                                          ,[Name Nothing "or"]] =
>     -- special case for and, or, get all the ands so we can vcat them
>     -- nicely
>     case ands e of
>       (e':es) -> vcat (scalarExpr d e'
>                        : map ((names op <+>) . scalarExpr d) es)
>       [] -> empty -- shouldn't be possible
>   where
>     ands (BinOp a op' b) | op == op' = ands a ++ ands b
>     ands x = [x]
> -- special case for . we don't use whitespace
> scalarExpr d (BinOp e0 [Name Nothing "."] e1) =
>     scalarExpr d e0 <> text "." <> scalarExpr d e1
> scalarExpr d (BinOp e0 f e1) =
>     scalarExpr d e0 <+> names f <+> scalarExpr d e1

> scalarExpr dia (Case t ws els) =
>     sep $ [text "case" <+> me (scalarExpr dia) t]
>           ++ map w ws
>           ++ maybeToList (fmap e els)
>           ++ [text "end"]
>   where
>     w (t0,t1) =
>       text "when" <+> nest 5 (commaSep $ map (scalarExpr dia) t0)
>       <+> text "then" <+> nest 5 (scalarExpr dia t1)
>     e el = text "else" <+> nest 5 (scalarExpr dia el)
> scalarExpr d (Parens e) = parens $ scalarExpr d e
> scalarExpr d (Cast e tn) =
>     text "cast" <> parens (sep [scalarExpr d e
>                                ,text "as"
>                                ,typeName tn])

> scalarExpr _ (TypedLit tn s) =
>     typeName tn <+> quotes (text s)

> scalarExpr d (SubQueryExpr ty qe) =
>     (case ty of
>         SqSq -> empty
>         SqExists -> text "exists"
>         SqUnique -> text "unique"
>     ) <+> parens (queryExpr d qe)

> scalarExpr d (QuantifiedComparison v c cp sq) =
>     scalarExpr d v
>     <+> names c
>     <+> (text $ case cp of
>              CPAny -> "any"
>              CPSome -> "some"
>              CPAll -> "all")
>     <+> parens (queryExpr d sq)

> scalarExpr d (Match v u sq) =
>     scalarExpr d v
>     <+> text "match"
>     <+> (if u then text "unique" else empty)
>     <+> parens (queryExpr d sq)

> scalarExpr d (In b se x) =
>     scalarExpr d se <+>
>     (if b then empty else text "not")
>     <+> text "in"
>     <+> parens (nest (if b then 3 else 7) $
>                  case x of
>                      InList es -> commaSep $ map (scalarExpr d) es
>                      InQueryExpr qe -> queryExpr d qe)

> scalarExpr d (Array v es) =
>     scalarExpr d v <> brackets (commaSep $ map (scalarExpr d) es)

> scalarExpr d (ArrayCtor q) =
>     text "array" <> parens (queryExpr d q)

> scalarExpr d (MultisetCtor es) =
>     text "multiset" <> brackets (commaSep $ map (scalarExpr d) es)

> scalarExpr d (MultisetQueryCtor q) =
>     text "multiset" <> parens (queryExpr d q)

> scalarExpr d (MultisetBinOp a c q b) =
>     sep
>     [scalarExpr d a
>     ,text "multiset"
>     ,text $ case c of
>                 Union -> "union"
>                 Intersect -> "intersect"
>                 Except -> "except"
>     ,case q of
>          SQDefault -> empty
>          All -> text "all"
>          Distinct -> text "distinct"
>     ,scalarExpr d b]

> {-scalarExpr d (Escape v e) =
>     scalarExpr d v <+> text "escape" <+> text [e]

> scalarExpr d (UEscape v e) =
>     scalarExpr d v <+> text "uescape" <+> text [e]-}

> scalarExpr d (Collate v c) =
>     scalarExpr d v <+> text "collate" <+> names c

> scalarExpr _ (NextValueFor ns) =
>     text "next value for" <+> names ns

> scalarExpr d (VEComment cmt v) =
>     vcat $ map comment cmt ++ [scalarExpr d v]

> scalarExpr _ (OdbcLiteral t s) =
>     text "{" <> lt t <+> quotes (text s) <> text "}"
>   where
>     lt OLDate = text "d"
>     lt OLTime = text "t"
>     lt OLTimestamp = text "ts"

> scalarExpr d (OdbcFunc e) =
>     text "{fn" <+> scalarExpr d e <> text "}"

> unname :: Name -> String
> unname (Name Nothing n) = n
> unname (Name (Just (s,e)) n) =
>     s ++ n ++ e

> unnames :: [Name] -> String
> unnames ns = intercalate "." $ map unname ns


> name :: Name -> Doc
> name (Name Nothing n) = text n
> name (Name (Just (s,e)) n) = text s <> text n <> text e

> names :: [Name] -> Doc
> names ns = hcat $ punctuate (text ".") $ map name ns

> typeName :: TypeName -> Doc
> typeName (TypeName t) = names t
> typeName (PrecTypeName t a) = names t <+> parens (text $ show a)
> typeName (PrecScaleTypeName t a b) =
>     names t <+> parens (text (show a) <+> comma <+> text (show b))
> typeName (PrecLengthTypeName t i m u) =
>     names t
>     <> parens (text (show i)
>                <> me (\x -> case x of
>                            PrecK -> text "K"
>                            PrecM -> text "M"
>                            PrecG -> text "G"
>                            PrecT -> text "T"
>                            PrecP -> text "P") m
>                <+> me (\x -> case x of
>                        PrecCharacters -> text "CHARACTERS"
>                        PrecOctets -> text "OCTETS") u)
> typeName (CharTypeName t i cs col) =
>     names t
>     <> me (\x -> parens (text $ show x)) i
>     <+> (if null cs
>          then empty
>          else text "character set" <+> names cs)
>     <+> (if null col
>          then empty
>          else text "collate" <+> names col)
> typeName (TimeTypeName t i tz) =
>     names t
>     <> me (\x -> parens (text $ show x)) i
>     <+> text (if tz
>               then "with time zone"
>               else "without time zone")
> typeName (RowTypeName cs) =
>     text "row" <> parens (commaSep $ map f cs)
>   where
>     f (n,t) = name n <+> typeName t
> typeName (IntervalTypeName f t) =
>     text "interval"
>     <+> intervalTypeField f
>     <+> me (\x -> text "to" <+> intervalTypeField x) t

> typeName (ArrayTypeName tn sz) =
>     typeName tn <+> text "array" <+> me (brackets . text . show) sz

> typeName (MultisetTypeName tn) =
>     typeName tn <+> text "multiset"

> intervalTypeField :: IntervalTypeField -> Doc
> intervalTypeField (Itf n p) =
>     text n
>     <+> me (\(x,x1) ->
>              parens (text (show x)
>                      <+> me (\y -> (sep [comma,text (show y)])) x1)) p


= query expressions

> queryExpr :: Dialect -> QueryExpr -> Doc
> queryExpr dia (Select d sl fr wh gb hv od off fe) =
>   sep [text "select"
>       ,case d of
>           SQDefault -> empty
>           All -> text "all"
>           Distinct -> text "distinct"
>       ,nest 7 $ sep [selectList dia sl]
>       ,from dia fr
>       ,maybeScalarExpr dia "where" wh
>       ,grpBy dia gb
>       ,maybeScalarExpr dia "having" hv
>       ,orderBy dia od
>       ,me (\e -> text "offset" <+> scalarExpr dia e <+> text "rows") off
>       ,fetchFirst
>       ]
>   where
>     fetchFirst =
>       me (\e -> if diSyntaxFlavour dia == MySQL
>                 then text "limit" <+> scalarExpr dia e
>                 else text "fetch first" <+> scalarExpr dia e
>                      <+> text "rows only") fe

> queryExpr dia (QueryExprSetOp q1 ct d c q2) =
>   sep [queryExpr dia q1
>       ,text (case ct of
>                 Union -> "union"
>                 Intersect -> "intersect"
>                 Except -> "except")
>        <+> case d of
>                SQDefault -> empty
>                All -> text "all"
>                Distinct -> text "distinct"
>        <+> case c of
>                Corresponding -> text "corresponding"
>                Respectively -> empty
>       ,queryExpr dia q2]
> queryExpr d (With rc withs qe) =
>   text "with" <+> (if rc then text "recursive" else empty)
>   <+> vcat [nest 5
>             (vcat $ punctuate comma $ flip map withs $ \(n,q) ->
>              alias n <+> text "as" <+> parens (queryExpr d q))
>            ,queryExpr d qe]
> queryExpr d (Values vs) =
>     text "values"
>     <+> nest 7 (commaSep (map (parens . commaSep . map (scalarExpr d)) vs))
> queryExpr _ (Table t) = text "table" <+> names t
> queryExpr d (QEComment cmt v) =
>     vcat $ map comment cmt ++ [queryExpr d v]


> alias :: Alias -> Doc
> alias (Alias nm cols) =
>     text "as" <+> name nm
>     <+> me (parens . commaSep . map name) cols

> selectList :: Dialect -> [(ScalarExpr,Maybe Name)] -> Doc
> selectList d is = commaSep $ map si is
>   where
>     si (e,al) = scalarExpr d e <+> me als al
>     als al = text "as" <+> name al

> from :: Dialect -> [TableRef] -> Doc
> from _ [] = empty
> from d ts =
>     sep [text "from"
>         ,nest 5 $ vcat $ punctuate comma $ map tr ts]
>   where
>     tr (TRSimple t) = names t
>     tr (TRLateral t) = text "lateral" <+> tr t
>     tr (TRFunction f as) =
>         names f <> parens (commaSep $ map (scalarExpr d) as)
>     tr (TRAlias t a) = sep [tr t, alias a]
>     tr (TRParens t) = parens $ tr t
>     tr (TRQueryExpr q) = parens $ queryExpr d q
>     tr (TRJoin t0 b jt t1 jc) =
>        sep [tr t0
>            ,if b then text "natural" else empty
>            ,joinText jt <+> tr t1
>            ,joinCond jc]
>     tr (TROdbc t) = text "{oj" <+> tr t <+> text "}"
>     joinText jt =
>       sep [case jt of
>               JInner -> text "inner"
>               JLeft -> text "left"
>               JRight -> text "right"
>               JFull -> text "full"
>               JCross -> text "cross"
>           ,text "join"]
>     joinCond (Just (JoinOn e)) = text "on" <+> scalarExpr d e
>     joinCond (Just (JoinUsing es)) =
>         text "using" <+> parens (commaSep $ map name es)
>     joinCond Nothing = empty

> maybeScalarExpr :: Dialect -> String -> Maybe ScalarExpr -> Doc
> maybeScalarExpr d k = me
>       (\e -> sep [text k
>                  ,nest (length k + 1) $ scalarExpr d e])

> grpBy :: Dialect -> [GroupingExpr] -> Doc
> grpBy _ [] = empty
> grpBy d gs = sep [text "group by"
>                ,nest 9 $ commaSep $ map ge gs]
>   where
>     ge (SimpleGroup e) = scalarExpr d e
>     ge (GroupingParens g) = parens (commaSep $ map ge g)
>     ge (Cube es) = text "cube" <> parens (commaSep $ map ge es)
>     ge (Rollup es) = text "rollup" <> parens (commaSep $ map ge es)
>     ge (GroupingSets es) = text "grouping sets" <> parens (commaSep $ map ge es)

> orderBy :: Dialect -> [SortSpec] -> Doc
> orderBy _ [] = empty
> orderBy dia os = sep [text "order by"
>                  ,nest 9 $ commaSep $ map f os]
>   where
>     f (SortSpec e d n) =
>         scalarExpr dia e
>         <+> (case d of
>                   Asc -> text "asc"
>                   Desc -> text "desc"
>                   DirDefault -> empty)
>         <+> (case n of
>                 NullsOrderDefault -> empty
>                 NullsFirst -> text "nulls" <+> text "first"
>                 NullsLast -> text "nulls" <+> text "last")

= statements

> statement :: Dialect -> Statement -> Doc


== ddl

> statement _ (CreateSchema nm) =
>     text "create" <+> text "schema" <+> names nm

> statement d (CreateTable nm cds) =
>     text "create" <+> text "table" <+> names nm
>     <+> parens (commaSep $ map cd cds)
>   where
>     cd (TableConstraintDef n con) =
>         maybe empty (\s -> text "constraint" <+> names s) n
>         <+> tableConstraint d con
>     cd (TableColumnDef cd') = columnDef d cd'

> statement d (AlterTable t act) =
>     texts ["alter","table"] <+> names t
>     <+> alterTableAction d act

> statement _ (DropSchema nm db) =
>     text "drop" <+> text "schema" <+> names nm <+> dropBehav db

> statement d (CreateDomain nm ty def cs) =
>     text "create" <+> text "domain" <+> names nm
>     <+> typeName ty
>     <+> maybe empty (\def' -> text "default" <+> scalarExpr d def') def
>     <+> sep (map con cs)
>   where
>     con (cn, e) =
>         maybe empty (\cn' -> text "constraint" <+> names cn') cn
>         <+> text "check" <> parens (scalarExpr d e)

> statement d (AlterDomain nm act) =
>     texts ["alter","domain"]
>     <+> names nm
>     <+> a act
>   where
>     a (ADSetDefault v) = texts ["set","default"] <+> scalarExpr d v
>     a (ADDropDefault) = texts ["drop","default"]
>     a (ADAddConstraint cnm e) =
>         text "add"
>         <+> maybe empty (\cnm' -> text "constraint" <+> names cnm') cnm
>         <+> text "check" <> parens (scalarExpr d e)
>     a (ADDropConstraint cnm) = texts ["drop", "constraint"]
>                                <+> names cnm


> statement _ (DropDomain nm db) =
>     text "drop" <+> text "domain" <+> names nm <+> dropBehav db

> statement _ (CreateSequence nm sgos) =
>   texts ["create","sequence"] <+> names nm
>   <+> sep (map sequenceGeneratorOption sgos)

> statement _ (AlterSequence nm sgos) =
>   texts ["alter","sequence"] <+> names nm
>   <+> sep (map sequenceGeneratorOption sgos)

> statement _ (DropSequence nm db) =
>     text "drop" <+> text "sequence" <+> names nm <+> dropBehav db


> statement d (CreateAssertion nm ex) =
>   texts ["create","assertion"] <+> names nm
>   <+> text "check" <+> parens (scalarExpr d ex)

> statement _ (DropAssertion nm db) =
>     text "drop" <+> text "assertion" <+> names nm <+> dropBehav db

== dml

> statement d (SelectStatement q) = queryExpr d q

> statement d (Delete t a w) =
>     text "delete" <+> text "from"
>     <+> names t <+> maybe empty (\x -> text "as" <+> name x) a
>     <+> maybeScalarExpr d "where" w

> statement _ (Truncate t ir) =
>     text "truncate" <+> text "table" <+> names t
>     <+> case ir of
>             DefaultIdentityRestart -> empty
>             ContinueIdentity -> text "continue" <+> text "identity"
>             RestartIdentity -> text "restart" <+> text "identity"

> statement d (Insert t cs s) =
>     text "insert" <+> text "into" <+> names t
>     <+> maybe empty (\cs' -> parens (commaSep $ map name cs')) cs
>     <+> case s of
>             DefaultInsertValues -> text "default" <+> text "values"
>             InsertQuery q -> queryExpr d q

> statement d (Update t a sts whr) =
>     text "update" <+> names t
>     <+> maybe empty (\x -> text "as" <+> name x) a
>     <+> text "set" <+> commaSep (map sc sts)
>     <+> maybeScalarExpr d "where" whr
>   where
>     sc (Set tg v) = names tg <+> text "=" <+> scalarExpr d v
>     sc (SetMultiple ts vs) = parens (commaSep $ map names ts) <+> text "="
>                              <+> parens (commaSep $ map (scalarExpr d) vs)

> statement _ (DropTable n b) =
>     text "drop" <+> text "table" <+> names n <+> dropBehav b

> statement d (CreateView r nm al q co) =
>     text "create" <+> (if r then text "recursive" else empty)
>     <+> text "view" <+> names nm
>     <+> (maybe empty (\al' -> parens $ commaSep $ map name al')) al
>     <+> text "as"
>     <+> queryExpr d q
>     <+> case co of
>             Nothing -> empty
>             Just DefaultCheckOption -> texts ["with", "check", "option"]
>             Just CascadedCheckOption -> texts ["with", "cascaded", "check", "option"]
>             Just LocalCheckOption -> texts ["with", "local", "check", "option"]

> statement _ (DropView n b) =
>     text "drop" <+> text "view" <+> names n <+> dropBehav b


== transactions

> statement _ StartTransaction =
>     texts ["start", "transaction"]

> statement _ (Savepoint nm) =
>     text "savepoint" <+> name nm

> statement _ (ReleaseSavepoint nm) =
>     texts ["release", "savepoint"] <+> name nm

> statement _ Commit =
>     text "commit"

> statement _ (Rollback mn) =
>     text "rollback"
>     <+> maybe empty (\n -> texts ["to","savepoint"] <+> name n) mn

== access control

> statement _ (GrantPrivilege pas po rs go) =
>     text "grant" <+> commaSep (map privAct pas)
>     <+> text "on" <+> privObj po
>     <+> text "to" <+> commaSep (map name rs)
>     <+> grantOpt go
>   where
>     grantOpt WithGrantOption = texts ["with","grant","option"]
>     grantOpt WithoutGrantOption = empty

> statement _ (GrantRole rs trs ao) =
>     text "grant" <+> commaSep (map name rs)
>     <+> text "to" <+> commaSep (map name trs)
>     <+> adminOpt ao
>   where
>     adminOpt WithAdminOption = texts ["with","admin","option"]
>     adminOpt WithoutAdminOption = empty

> statement _ (CreateRole nm) =
>     texts ["create","role"] <+> name nm

> statement _ (DropRole nm) =
>     texts ["drop","role"] <+> name nm

> statement _ (RevokePrivilege go pas po rs db) =
>     text "revoke"
>     <+> grantOptFor go
>     <+> commaSep (map privAct pas)
>     <+> text "on" <+> privObj po
>     <+> text "from" <+> commaSep (map name rs)
>     <+> dropBehav db
>   where
>     grantOptFor GrantOptionFor = texts ["grant","option","for"]
>     grantOptFor NoGrantOptionFor = empty

> statement _ (RevokeRole ao rs trs db) =
>     text "revoke"
>     <+> adminOptFor ao
>     <+> commaSep (map name rs)
>     <+> text "from" <+> commaSep (map name trs)
>     <+> dropBehav db
>   where
>     adminOptFor AdminOptionFor = texts ["admin","option","for"]
>     adminOptFor NoAdminOptionFor = empty


== sessions


== extras

> dropBehav :: DropBehaviour -> Doc
> dropBehav DefaultDropBehaviour = empty
> dropBehav Cascade = text "cascade"
> dropBehav Restrict = text "restrict"


> columnDef :: Dialect -> ColumnDef -> Doc
> columnDef d (ColumnDef n t mdef cons) =
>       name n <+> typeName t
>       <+> case mdef of
>              Nothing -> empty
>              Just (DefaultClause def) ->
>                  text "default" <+> scalarExpr d def
>              Just (GenerationClause e) ->
>                  texts ["generated","always","as"] <+> parens (scalarExpr d e)
>              Just (IdentityColumnSpec w o) ->
>                  text "generated"
>                  <+> (case w of
>                          GeneratedAlways -> text "always"
>                          GeneratedByDefault -> text "by" <+> text "default")
>                  <+> text "as" <+> text "identity"
>                  <+> (case o of
>                          [] -> empty
>                          os -> parens (sep $ map sequenceGeneratorOption os))
>       <+> sep (map cdef cons)
>   where
>     cdef (ColConstraintDef cnm con) =
>         maybe empty (\s -> text "constraint" <+> names s) cnm
>         <+> pcon con
>     pcon ColNotNullConstraint = texts ["not","null"]
>     pcon ColUniqueConstraint = text "unique"
>     pcon ColPrimaryKeyConstraint = texts ["primary","key"]
>     pcon (ColCheckConstraint v) = text "check" <+> parens (scalarExpr d v)
>     pcon (ColReferencesConstraint tb c m u del) =
>         text "references"
>         <+> names tb
>         <+> maybe empty (\c' -> parens (name c')) c
>         <+> refMatch m
>         <+> refAct "update" u
>         <+> refAct "delete" del

> sequenceGeneratorOption :: SequenceGeneratorOption -> Doc
> sequenceGeneratorOption (SGODataType t) =
>     text "as" <+> typeName t
> sequenceGeneratorOption (SGORestart mi) =
>     text "restart" <+> maybe empty (\mi' -> texts ["with", show mi']) mi
> sequenceGeneratorOption (SGOStartWith i) = texts ["start",  "with", show i]
> sequenceGeneratorOption (SGOIncrementBy i) = texts ["increment", "by", show i]
> sequenceGeneratorOption (SGOMaxValue i) = texts ["maxvalue", show i]
> sequenceGeneratorOption SGONoMaxValue = texts ["no", "maxvalue"]
> sequenceGeneratorOption (SGOMinValue i) = texts ["minvalue", show i]
> sequenceGeneratorOption SGONoMinValue = texts ["no", "minvalue"]
> sequenceGeneratorOption SGOCycle = text "cycle"
> sequenceGeneratorOption SGONoCycle = text "no cycle"

> refMatch :: ReferenceMatch -> Doc
> refMatch m = case m of
>                      DefaultReferenceMatch -> empty
>                      MatchFull -> texts ["match", "full"]
>                      MatchPartial -> texts ["match","partial"]
>                      MatchSimple -> texts ["match", "simple"]

> refAct :: String -> ReferentialAction -> Doc
> refAct t a = case a of
>                      DefaultReferentialAction -> empty
>                      RefCascade -> texts ["on", t, "cascade"]
>                      RefSetNull -> texts ["on", t, "set", "null"]
>                      RefSetDefault -> texts ["on", t, "set", "default"]
>                      RefRestrict -> texts ["on", t, "restrict"]
>                      RefNoAction -> texts ["on", t, "no", "action"]

> alterTableAction :: Dialect -> AlterTableAction -> Doc
> alterTableAction d (AddColumnDef cd) =
>     texts ["add", "column"] <+> columnDef d cd

> alterTableAction d (AlterColumnSetDefault n v) =
>     texts ["alter", "column"]
>     <+> name n
>     <+> texts ["set","default"] <+> scalarExpr d v
> alterTableAction _ (AlterColumnDropDefault n) =
>     texts ["alter", "column"]
>     <+> name n
>     <+> texts ["drop","default"]

> alterTableAction _ (AlterColumnSetNotNull n) =
>     texts ["alter", "column"]
>     <+> name n
>     <+> texts ["set","not","null"]

> alterTableAction _ (AlterColumnDropNotNull n) =
>     texts ["alter", "column"]
>     <+> name n
>     <+> texts ["drop","not","null"]

> alterTableAction _ (AlterColumnSetDataType n t) =
>     texts ["alter", "column"]
>     <+> name n
>     <+> texts ["set","data","Type"]
>     <+> typeName t

> alterTableAction _ (DropColumn n b) =
>     texts ["drop", "column"]
>     <+> name n
>     <+> dropBehav b

> alterTableAction d (AddTableConstraintDef n con) =
>     text "add"
>     <+> maybe empty (\s -> text "constraint" <+> names s) n
>     <+> tableConstraint d con

> alterTableAction _ (DropTableConstraintDef n b) =
>     texts ["drop", "constraint"]
>     <+> names n
>     <+> dropBehav b


> tableConstraint :: Dialect -> TableConstraint -> Doc
> tableConstraint _ (TableUniqueConstraint ns) =
>          text "unique" <+> parens (commaSep $ map name ns)
> tableConstraint _ (TablePrimaryKeyConstraint ns) =
>         texts ["primary","key"] <+> parens (commaSep $ map name ns)
> tableConstraint _ (TableReferencesConstraint cs t tcs m u del) =
>         texts ["foreign", "key"]
>         <+> parens (commaSep $ map name cs)
>         <+> text "references"
>         <+> names t
>         <+> maybe empty (\c' -> parens (commaSep $ map name c')) tcs
>         <+> refMatch m
>         <+> refAct "update" u
>         <+> refAct "delete" del
> tableConstraint d (TableCheckConstraint v) = text "check" <+> parens (scalarExpr d v)


> privAct :: PrivilegeAction -> Doc
> privAct PrivAll = texts ["all","privileges"]
> privAct (PrivSelect cs) = text "select" <+> maybeColList cs
> privAct (PrivInsert cs) = text "insert" <+> maybeColList cs
> privAct (PrivUpdate cs) = text "update" <+> maybeColList cs
> privAct (PrivReferences cs) = text "references" <+> maybeColList cs
> privAct PrivDelete = text "delete"
> privAct PrivUsage = text "usage"
> privAct PrivTrigger = text "trigger"
> privAct PrivExecute = text "execute"

> maybeColList :: [Name] -> Doc
> maybeColList cs =
>     if null cs
>     then empty
>     else parens (commaSep $ map name cs)

> privObj :: PrivilegeObject -> Doc
> privObj (PrivTable nm) = names nm
> privObj (PrivDomain nm) = text "domain" <+> names nm
> privObj (PrivType nm) = text "type" <+> names nm
> privObj (PrivSequence nm) = text "sequence" <+> names nm
> privObj (PrivFunction nm) = texts ["specific", "function"] <+> names nm

= utils

> commaSep :: [Doc] -> Doc
> commaSep ds = sep $ punctuate comma ds

> me :: (a -> Doc) -> Maybe a -> Doc
> me = maybe empty

> comment :: Comment -> Doc
> comment (BlockComment str) = text "/*" <+> text str <+> text "*/"

> texts :: [String] -> Doc
> texts ts = sep $ map text ts
