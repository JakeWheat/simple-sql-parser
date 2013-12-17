
> -- | These is the pretty printing functions, which produce SQL
> -- source from ASTs. The code attempts to format the output in a
> -- readable way.
> module Language.SQL.SimpleSQL.Pretty
>     (prettyQueryExpr
>     ,prettyScalarExpr
>     ,prettyQueryExprs
>     ) where

> import Language.SQL.SimpleSQL.Syntax
> import Text.PrettyPrint
> import Data.Maybe

> -- | Convert a query expr ast to concrete syntax.
> prettyQueryExpr :: QueryExpr -> String
> prettyQueryExpr = render . queryExpr

> -- | Convert a scalar expr ast to concrete syntax.
> prettyScalarExpr :: ScalarExpr -> String
> prettyScalarExpr = render . scalarExpr

> -- | Convert a list of query exprs to concrete syntax. A semi colon
> -- is inserted after each query expr.
> prettyQueryExprs :: [QueryExpr] -> String
> prettyQueryExprs = render . vcat . map ((<> text ";\n") . queryExpr)

= scalar expressions

> scalarExpr :: ScalarExpr -> Doc
> scalarExpr (StringLit s) = quotes $ text s
> scalarExpr (NumLit s) = text s
> scalarExpr (IntervalLit v u p) =
>     text "interval" <+> quotes (text v)
>     <+> text u
>     <+> maybe empty (parens . text . show ) p
> scalarExpr (Iden i) = name i
> scalarExpr (Iden2 q i) = name q <> text "." <> name i
> scalarExpr Star = text "*"
> scalarExpr (Star2 q) = name q <> text "." <> text "*"

> scalarExpr (App f es) = name f <> parens (commaSep (map scalarExpr es))

> scalarExpr (AggregateApp f d es od) =
>     name f
>     <> parens ((case d of
>                   Just Distinct -> text "distinct"
>                   Just All -> text "all"
>                   Nothing -> empty)
>                <+> commaSep (map scalarExpr es)
>                <+> orderBy od)

> scalarExpr (WindowApp f es pb od) =
>     name f <> parens (commaSep $ map scalarExpr es)
>     <+> text "over"
>     <+> parens ((case pb of
>                     [] -> empty
>                     _ -> text "partition by"
>                           <+> nest 13 (commaSep $ map scalarExpr pb))
>                 <+> orderBy od)

> scalarExpr (SpecialOp nm [a,b,c]) | nm `elem` [Name "between"
>                                               ,Name "not between"] =
>   sep [scalarExpr a
>       ,name nm <+> scalarExpr b
>       ,nest (length (unname nm) + 1)
>        $ text "and" <+> scalarExpr c]

> scalarExpr (SpecialOp (Name "extract") [a,n]) =
>   text "extract" <> parens (scalarExpr a
>                             <+> text "from"
>                             <+> scalarExpr n)

> scalarExpr (SpecialOp (Name "substring") [a,s,e]) =
>   text "substring" <> parens (scalarExpr a
>                             <+> text "from"
>                             <+> scalarExpr s
>                             <+> text "for"
>                             <+> scalarExpr e)

> scalarExpr (SpecialOp nm es) =
>   name nm <+> parens (commaSep $ map scalarExpr es)

> scalarExpr (PrefixOp f e) = name f <+> scalarExpr e
> scalarExpr (PostfixOp f e) = scalarExpr e <+> name f
> scalarExpr e@(BinOp _ op _) | op `elem` [(Name "and"), (Name "or")] =
>     -- special case for and, or, get all the ands so we can vcat them
>     -- nicely
>     case ands e of
>       (e':es) -> vcat (scalarExpr e'
>                        : map ((name op <+>) . scalarExpr) es)
>       [] -> empty -- shouldn't be possible
>   where
>     ands (BinOp a op' b) | op == op' = ands a ++ ands b
>     ands x = [x]
> scalarExpr (BinOp e0 f e1) =
>     scalarExpr e0 <+> name f <+> scalarExpr e1

> scalarExpr (Case t ws els) =
>     sep $ [text "case" <+> maybe empty scalarExpr t]
>           ++ map w ws
>           ++ maybeToList (fmap e els)
>           ++ [text "end"]
>   where
>     w (t0,t1) =
>       text "when" <+> nest 5 (scalarExpr t0)
>       <+> text "then" <+> nest 5 (scalarExpr t1)
>     e el = text "else" <+> nest 5 (scalarExpr el)
> scalarExpr (Parens e) = parens $ scalarExpr e
> scalarExpr (Cast e (TypeName tn)) =
>     text "cast" <> parens (sep [scalarExpr e
>                                ,text "as"
>                                ,text tn])

> scalarExpr (TypedLit (TypeName tn) s) =
>     text tn <+> quotes (text s)

> scalarExpr (SubQueryExpr ty qe) =
>     (case ty of
>         SqSq -> empty
>         SqExists -> text "exists"
>         SqAll -> text "all"
>         SqSome -> text "some"
>         SqAny -> text "any"
>     ) <+> parens (queryExpr qe)

> scalarExpr (In b se x) =
>     scalarExpr se <+>
>     (if b then empty else text "not")
>     <+> text "in"
>     <+> parens (nest (if b then 3 else 7) $
>                  case x of
>                      InList es -> commaSep $ map scalarExpr es
>                      InQueryExpr qe -> queryExpr qe)

> unname :: Name -> String
> unname (QName n) = "\"" ++ n ++ "\""
> unname (Name n) = n

> name :: Name -> Doc
> name (QName n) = doubleQuotes $ text n
> name (Name n) = text n

= query expressions

> queryExpr :: QueryExpr -> Doc
> queryExpr (Select d sl fr wh gb hv od lm off) =
>   sep [text "select"
>       ,case d of
>           All -> empty
>           Distinct -> text "distinct"
>       ,nest 7 $ sep [selectList sl]
>       ,from fr
>       ,maybeScalarExpr "where" wh
>       ,grpBy gb
>       ,maybeScalarExpr "having" hv
>       ,orderBy od
>       ,maybeScalarExpr "limit" lm
>       ,maybeScalarExpr "offset" off
>       ]
> queryExpr (CombineQueryExpr q1 ct d c q2) =
>   sep [queryExpr q1
>       ,text (case ct of
>                 Union -> "union"
>                 Intersect -> "intersect"
>                 Except -> "except")
>        <+> case d of
>                All -> empty
>                Distinct -> text "distinct"
>        <+> case c of
>                Corresponding -> text "corresponding"
>                Respectively -> empty
>       ,queryExpr q2]
> queryExpr (With withs qe) =
>   text "with"
>   <+> vcat [nest 5
>             (vcat $ punctuate comma $ flip map withs $ \(n,q) ->
>              name n <+> text "as" <+> parens (queryExpr q))
>            ,queryExpr qe]

> selectList :: [(Maybe Name, ScalarExpr)] -> Doc
> selectList is = commaSep $ map si is
>   where
>     si (al,e) = scalarExpr e <+> maybe empty alias al
>     alias al = text "as" <+> name al

> from :: [TableRef] -> Doc
> from [] = empty
> from ts =
>     sep [text "from"
>         ,nest 5 $ vcat $ punctuate comma $ map tr ts]
>   where
>     tr (TRSimple t) = name t
>     tr (TRLateral t) = text "lateral" <+> tr t
>     tr (TRFunction f as) =
>         name f <> parens (commaSep $ map scalarExpr as)
>     tr (TRAlias t a cs) =
>         sep [tr t
>             ,text "as" <+> name a
>              <+> maybe empty (parens . commaSep . map name) cs]
>     tr (TRParens t) = parens $ tr t
>     tr (TRQueryExpr q) = parens $ queryExpr q
>     tr (TRJoin t0 jt t1 jc) =
>        sep [tr t0
>            ,joinText jt jc <+> tr t1
>            ,joinCond jc]
>     joinText jt jc =
>       sep [case jc of
>               Just JoinNatural -> text "natural"
>               _ -> empty
>           ,case jt of
>               JInner -> text "inner"
>               JLeft -> text "left"
>               JRight -> text "right"
>               JFull -> text "full"
>               JCross -> text "cross"
>           ,text "join"]
>     joinCond (Just (JoinOn e)) = text "on" <+> scalarExpr e
>     joinCond (Just (JoinUsing es)) =
>         text "using" <+> parens (commaSep $ map name es)
>     joinCond Nothing = empty
>     joinCond (Just JoinNatural) = empty

> maybeScalarExpr :: String -> Maybe ScalarExpr -> Doc
> maybeScalarExpr k = maybe empty
>       (\e -> sep [text k
>                  ,nest (length k + 1) $ scalarExpr e])

> grpBy :: [ScalarExpr] -> Doc
> grpBy [] = empty
> grpBy gs = sep [text "group by"
>                ,nest 9 $ commaSep $ map scalarExpr gs]

> orderBy :: [(ScalarExpr,Direction)] -> Doc
> orderBy [] = empty
> orderBy os = sep [text "order by"
>                  ,nest 9 $ commaSep $ map f os]
>   where
>     f (e,Asc) = scalarExpr e
>     f (e,Desc) = scalarExpr e <+> text "desc"

= utils

> commaSep :: [Doc] -> Doc
> commaSep ds = sep $ punctuate comma ds
