
> -- | These is the pretty printing functions, which produce SQL
> -- source from ASTs. The code attempts to format the output in a
> -- readable way.
> module Language.SQL.SimpleSQL.Pretty
>     (prettyQueryExpr
>     ,prettyValueExpr
>     ,prettyQueryExprs
>     ) where

TODO: there should be more comments in this file, especially the bits
which have been changed to try to improve the layout of the output.

> import Language.SQL.SimpleSQL.Syntax
> import Text.PrettyPrint (render, vcat, text, (<>), (<+>), empty, parens,
>                          nest, Doc, punctuate, comma, sep, quotes,
>                          doubleQuotes, brackets,hcat)
> import Data.Maybe (maybeToList, catMaybes)
> import Data.List (intercalate)

> -- | Convert a query expr ast to concrete syntax.
> prettyQueryExpr :: QueryExpr -> String
> prettyQueryExpr = render . queryExpr

> -- | Convert a value expr ast to concrete syntax.
> prettyValueExpr :: ValueExpr -> String
> prettyValueExpr = render . valueExpr

> -- | Convert a list of query exprs to concrete syntax. A semi colon
> -- is inserted after each query expr.
> prettyQueryExprs :: [QueryExpr] -> String
> prettyQueryExprs = render . vcat . map ((<> text ";\n") . queryExpr)

= value expressions

> valueExpr :: ValueExpr -> Doc
> valueExpr (StringLit s) = quotes $ text $ doubleUpQuotes s

> valueExpr (NumLit s) = text s
> valueExpr (IntervalLit s v f t) =
>     text "interval"
>     <+> me (\x -> if x then text "+" else text "-") s
>     <+> quotes (text v)
>     <+> intervalTypeField f
>     <+> me (\x -> text "to" <+> intervalTypeField x) t
> valueExpr (Iden i) = names i
> valueExpr Star = text "*"
> valueExpr Parameter = text "?"
> valueExpr (HostParameter p i) =
>     text (':':p)
>     <+> me (\i' -> text "indicator" <+> text (':':i')) i

> valueExpr (App f es) = names f <> parens (commaSep (map valueExpr es))

> valueExpr (AggregateApp f d es od fil) =
>     names f
>     <> parens ((case d of
>                   Distinct -> text "distinct"
>                   All -> text "all"
>                   SQDefault -> empty)
>                <+> commaSep (map valueExpr es)
>                <+> orderBy od)
>     <+> me (\x -> text "filter"
>                   <+> parens (text "where" <+> valueExpr x)) fil

> valueExpr (AggregateAppGroup f es od) =
>     names f
>     <> parens (commaSep (map valueExpr es))
>     <+> if null od
>         then empty
>         else text "within group" <+> parens(orderBy od)

> valueExpr (WindowApp f es pb od fr) =
>     names f <> parens (commaSep $ map valueExpr es)
>     <+> text "over"
>     <+> parens ((case pb of
>                     [] -> empty
>                     _ -> text "partition by"
>                           <+> nest 13 (commaSep $ map valueExpr pb))
>                 <+> orderBy od
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
>     fpd (Preceding e) = valueExpr e <+> text "preceding"
>     fpd (Following e) = valueExpr e <+> text "following"

> valueExpr (SpecialOp nm [a,b,c]) | nm `elem` [[Name "between"]
>                                              ,[Name "not between"]] =
>   sep [valueExpr a
>       ,names nm <+> valueExpr b
>       ,nest (length (unnames nm) + 1) $ text "and" <+> valueExpr c]

> valueExpr (SpecialOp [Name "rowctor"] as) =
>     parens $ commaSep $ map valueExpr as

> valueExpr (SpecialOp nm es) =
>   names nm <+> parens (commaSep $ map valueExpr es)

> valueExpr (SpecialOpK nm fs as) =
>     names nm <> parens (sep $ catMaybes
>         (fmap valueExpr fs
>          : map (\(n,e) -> Just (text n <+> valueExpr e)) as))

> valueExpr (PrefixOp f e) = names f <+> valueExpr e
> valueExpr (PostfixOp f e) = valueExpr e <+> names f
> valueExpr e@(BinOp _ op _) | op `elem` [[Name "and"], [Name "or"]] =
>     -- special case for and, or, get all the ands so we can vcat them
>     -- nicely
>     case ands e of
>       (e':es) -> vcat (valueExpr e'
>                        : map ((names op <+>) . valueExpr) es)
>       [] -> empty -- shouldn't be possible
>   where
>     ands (BinOp a op' b) | op == op' = ands a ++ ands b
>     ands x = [x]
> -- special case for . we don't use whitespace
> valueExpr (BinOp e0 [Name "."] e1) =
>     valueExpr e0 <> text "." <> valueExpr e1
> valueExpr (BinOp e0 f e1) =
>     valueExpr e0 <+> names f <+> valueExpr e1

> valueExpr (Case t ws els) =
>     sep $ [text "case" <+> me valueExpr t]
>           ++ map w ws
>           ++ maybeToList (fmap e els)
>           ++ [text "end"]
>   where
>     w (t0,t1) =
>       text "when" <+> nest 5 (commaSep $ map valueExpr t0)
>       <+> text "then" <+> nest 5 (valueExpr t1)
>     e el = text "else" <+> nest 5 (valueExpr el)
> valueExpr (Parens e) = parens $ valueExpr e
> valueExpr (Cast e tn) =
>     text "cast" <> parens (sep [valueExpr e
>                                ,text "as"
>                                ,typeName tn])

> valueExpr (TypedLit tn s) =
>     typeName tn <+> quotes (text s)

> valueExpr (SubQueryExpr ty qe) =
>     (case ty of
>         SqSq -> empty
>         SqExists -> text "exists"
>         SqUnique -> text "unique"
>     ) <+> parens (queryExpr qe)

> valueExpr (QuantifiedComparison v c cp sq) =
>     valueExpr v
>     <+> names c
>     <+> (text $ case cp of
>              CPAny -> "any"
>              CPSome -> "some"
>              CPAll -> "all")
>     <+> parens (queryExpr sq)

> valueExpr (Match v u sq) =
>     valueExpr v
>     <+> text "match"
>     <+> (if u then text "unique" else empty)
>     <+> parens (queryExpr sq)

> valueExpr (In b se x) =
>     valueExpr se <+>
>     (if b then empty else text "not")
>     <+> text "in"
>     <+> parens (nest (if b then 3 else 7) $
>                  case x of
>                      InList es -> commaSep $ map valueExpr es
>                      InQueryExpr qe -> queryExpr qe)

> valueExpr (Array v es) =
>     valueExpr v <> brackets (commaSep $ map valueExpr es)

> valueExpr (ArrayCtor q) =
>     text "array" <> parens (queryExpr q)

> valueExpr (MultisetCtor es) =
>     text "multiset" <> brackets (commaSep $ map valueExpr es)

> valueExpr (MultisetQueryCtor q) =
>     text "multiset" <> parens (queryExpr q)

> valueExpr (MultisetBinOp a c q b) =
>     sep
>     [valueExpr a
>     ,text "multiset"
>     ,text $ case c of
>                 Union -> "union"
>                 Intersect -> "intersect"
>                 Except -> "except"
>     ,case q of
>          SQDefault -> empty
>          All -> text "all"
>          Distinct -> text "distinct"
>     ,valueExpr b]



> valueExpr (CSStringLit cs st) =
>   text cs <> quotes (text $ doubleUpQuotes st)

> valueExpr (Escape v e) =
>     valueExpr v <+> text "escape" <+> text [e]

> valueExpr (UEscape v e) =
>     valueExpr v <+> text "uescape" <+> text [e]

> valueExpr (Collate v c) =
>     valueExpr v <+> text "collate" <+> names c


> doubleUpQuotes :: String -> String
> doubleUpQuotes [] = []
> doubleUpQuotes ('\'':cs) = '\'':'\'':doubleUpQuotes cs
> doubleUpQuotes (c:cs) = c:doubleUpQuotes cs

> doubleUpDoubleQuotes :: String -> String
> doubleUpDoubleQuotes [] = []
> doubleUpDoubleQuotes ('"':cs) = '"':'"':doubleUpDoubleQuotes cs
> doubleUpDoubleQuotes (c:cs) = c:doubleUpDoubleQuotes cs



> unname :: Name -> String
> unname (QName n) = "\"" ++ doubleUpDoubleQuotes n ++ "\""
> unname (UQName n) = "U&\"" ++ doubleUpDoubleQuotes n ++ "\""
> unname (Name n) = n

> unnames :: [Name] -> String
> unnames ns = intercalate "." $ map unname ns


> name :: Name -> Doc
> name (QName n) = doubleQuotes $ text $ doubleUpDoubleQuotes n
> name (UQName n) =
>     text "U&" <> doubleQuotes (text $ doubleUpDoubleQuotes n)
> name (Name n) = text n

> names :: [Name] -> Doc
> names ns = hcat $ punctuate (text ".") $ map name ns

> typeName :: TypeName -> Doc
> typeName (TypeName t) = names t
> typeName (PrecTypeName t a) = names t <+> parens (text $ show a)
> typeName (PrecScaleTypeName t a b) =
>     names t <+> parens (text (show a) <+> comma <+> text (show b))
> typeName (LobTypeName t i m u) =
>     names t
>     <> parens (text (show i)
>                <> me (\x -> case x of
>                            LobK -> text "K"
>                            LobM -> text "M"
>                            LobG -> text "G") m
>                <+> me (\x -> case x of
>                        LobCharacters -> text "CHARACTERS"
>                        LobCodeUnits -> text "CODE_UNITS"
>                        LobOctets -> text "OCTETS") u)
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

> typeName (RefTypeName rt sc) =
>     text "ref"
>     <> parens (names rt)
>     <+> me (\x -> text "scope" <+> names x) sc

> intervalTypeField :: IntervalTypeField -> Doc
> intervalTypeField (Itf n p) =
>     text n
>     <+> me (\(x,x1) ->
>              parens (text (show x)
>                      <+> me (\y -> (sep [comma,text (show y)])) x1)) p


= query expressions

> queryExpr :: QueryExpr -> Doc
> queryExpr (Select d sl fr wh gb hv od off fe) =
>   sep [text "select"
>       ,case d of
>           SQDefault -> empty
>           All -> text "all"
>           Distinct -> text "distinct"
>       ,nest 7 $ sep [selectList sl]
>       ,from fr
>       ,maybeValueExpr "where" wh
>       ,grpBy gb
>       ,maybeValueExpr "having" hv
>       ,orderBy od
>       ,me (\e -> text "offset" <+> valueExpr e <+> text "rows") off
>       ,me (\e -> text "fetch first" <+> valueExpr e
>                  <+> text "rows only") fe
>       ]
> queryExpr (CombineQueryExpr q1 ct d c q2) =
>   sep [queryExpr q1
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
>       ,queryExpr q2]
> queryExpr (With rc withs qe) =
>   text "with" <+> (if rc then text "recursive" else empty)
>   <+> vcat [nest 5
>             (vcat $ punctuate comma $ flip map withs $ \(n,q) ->
>              alias n <+> text "as" <+> parens (queryExpr q))
>            ,queryExpr qe]
> queryExpr (Values vs) =
>     text "values"
>     <+> nest 7 (commaSep (map (parens . commaSep . map valueExpr) vs))
> queryExpr (Table t) = text "table" <+> names t


> alias :: Alias -> Doc
> alias (Alias nm cols) =
>     text "as" <+> name nm
>     <+> me (parens . commaSep . map name) cols

> selectList :: [(ValueExpr,Maybe Name)] -> Doc
> selectList is = commaSep $ map si is
>   where
>     si (e,al) = valueExpr e <+> me als al
>     als al = text "as" <+> name al

> from :: [TableRef] -> Doc
> from [] = empty
> from ts =
>     sep [text "from"
>         ,nest 5 $ vcat $ punctuate comma $ map tr ts]
>   where
>     tr (TRSimple t) = names t
>     tr (TRLateral t) = text "lateral" <+> tr t
>     tr (TRFunction f as) =
>         names f <> parens (commaSep $ map valueExpr as)
>     tr (TRAlias t a) = sep [tr t, alias a]
>     tr (TRParens t) = parens $ tr t
>     tr (TRQueryExpr q) = parens $ queryExpr q
>     tr (TRJoin t0 b jt t1 jc) =
>        sep [tr t0
>            ,if b then text "natural" else empty
>            ,joinText jt <+> tr t1
>            ,joinCond jc]
>     joinText jt =
>       sep [case jt of
>               JInner -> text "inner"
>               JLeft -> text "left"
>               JRight -> text "right"
>               JFull -> text "full"
>               JCross -> text "cross"
>           ,text "join"]
>     joinCond (Just (JoinOn e)) = text "on" <+> valueExpr e
>     joinCond (Just (JoinUsing es)) =
>         text "using" <+> parens (commaSep $ map name es)
>     joinCond Nothing = empty

> maybeValueExpr :: String -> Maybe ValueExpr -> Doc
> maybeValueExpr k = me
>       (\e -> sep [text k
>                  ,nest (length k + 1) $ valueExpr e])

> grpBy :: [GroupingExpr] -> Doc
> grpBy [] = empty
> grpBy gs = sep [text "group by"
>                ,nest 9 $ commaSep $ map ge gs]
>   where
>     ge (SimpleGroup e) = valueExpr e
>     ge (GroupingParens g) = parens (commaSep $ map ge g)
>     ge (Cube es) = text "cube" <> parens (commaSep $ map ge es)
>     ge (Rollup es) = text "rollup" <> parens (commaSep $ map ge es)
>     ge (GroupingSets es) = text "grouping sets" <> parens (commaSep $ map ge es)

> orderBy :: [SortSpec] -> Doc
> orderBy [] = empty
> orderBy os = sep [text "order by"
>                  ,nest 9 $ commaSep $ map f os]
>   where
>     f (SortSpec e d n) =
>         valueExpr e
>         <+> (case d of
>                   Asc -> text "asc"
>                   Desc -> text "desc"
>                   DirDefault -> empty)
>         <+> (case n of
>                 NullsOrderDefault -> empty
>                 NullsFirst -> text "nulls" <+> text "first"
>                 NullsLast -> text "nulls" <+> text "last")

= utils

> commaSep :: [Doc] -> Doc
> commaSep ds = sep $ punctuate comma ds

> me :: (a -> Doc) -> Maybe a -> Doc
> me = maybe empty
