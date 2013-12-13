
This is the pretty printer code which takes AST values and turns them
back into SQL source text. It attempts to format the output nicely.

> module Language.SQL.SimpleSQL.Pretty
>     (prettyQueryExpr
>     ,prettyScalarExpr
>     ) where

> import Language.SQL.SimpleSQL.Syntax
> import Text.PrettyPrint
> import Data.Maybe

> prettyQueryExpr :: QueryExpr -> String
> prettyQueryExpr = render . queryExpr

> prettyScalarExpr :: ScalarExpr -> String
> prettyScalarExpr = render . scalarExpr


= scalar expressions

> scalarExpr :: ScalarExpr -> Doc
> scalarExpr (StringLit s) = quotes $ text s
> scalarExpr (NumLit s) = text s
> scalarExpr (Iden i) = text i
> scalarExpr (Iden2 q i) = text q <> text "." <> text i
> scalarExpr Star = text "*"
> scalarExpr (Star2 q) = text q <> text "." <> text "*"

> scalarExpr (App f es) = text f <> parens (commaSep (map scalarExpr es))
> scalarExpr (Op f [e]) = text f <+> scalarExpr e
> scalarExpr (Op f [e0,e1]) =
>     sep [scalarExpr e0, text f, scalarExpr e1]

> scalarExpr (Op f es) =
>     -- TODO: how to handle this? error or either seems poor
>     text f <> parens (commaSep (map scalarExpr es))

> scalarExpr (Case t ws els) =
>     sep [text "case" <+> (maybe empty scalarExpr t)
>         ,nest 4 (sep ((map w ws)
>                       ++ maybeToList (fmap e els)))
>         ,text "end"]
>   where
>     w (t0,t1) = sep [text "when" <+> scalarExpr t0
>                     ,text "then" <+> scalarExpr t1]
>     e el = text "else" <+> scalarExpr el
> scalarExpr (Parens e) = parens $ scalarExpr e
> scalarExpr (Cast e (TypeName tn)) =
>     text "cast" <> parens (sep [scalarExpr e
>                                ,text "as"
>                                ,text tn])

> scalarExpr (CastOp (TypeName tn) s) =
>     text tn <+> quotes (text s)

> scalarExpr (SubQueryExpr ty qe) =
>     (case ty of
>         SqSq -> empty
>         SqExists -> text "exists"
>         SqIn -> text "in"
>         SqAll -> text "all"
>         SqSome -> text "some"
>         SqAny -> text "any"
>     ) <+> parens (queryExpr qe)

= query expressions

> queryExpr :: QueryExpr -> Doc
> queryExpr (Select d sl fr wh gb hv od lm off) =
>   sep [text "select"
>       ,case d of
>           All -> empty
>           Distinct -> text "distinct"
>       ,nest 4 $ sep [selectList sl]
>       ,from fr
>       ,maybeScalarExpr "where" wh
>       ,grpBy gb
>       ,maybeScalarExpr "having" hv
>       ,orderBy od
>       ,maybeScalarExpr "limit" lm
>       ,maybeScalarExpr "offset" off
>       ]

> selectList :: [(Maybe String, ScalarExpr)] -> Doc
> selectList is = commaSep $ map si is
>   where
>     si (al,e) = scalarExpr e <+> maybe empty alias al
>     alias al = text "as" <+> text al

> from :: [TableRef] -> Doc
> from [] = empty
> from ts =
>     sep [text "from"
>         ,nest 4 $ commaSep $ map tr ts]
>   where
>     tr (SimpleTableRef t) = text t
>     tr (JoinAlias t a) = tr t <+> text "as" <+> text a
>     tr (JoinParens t) = parens $ tr t
>     tr (JoinQueryExpr q) = parens $ queryExpr q
>     tr (JoinTableRef jt t0 t1 jc) =
>        sep [tr t0
>            ,joinText jt jc
>            ,tr t1
>            ,joinCond jc]
>     joinText jt jc =
>       sep [case jc of
>               Just JoinNatural -> text "natural"
>               _ -> empty
>           ,case jt of
>               Inner -> text "inner"
>               JLeft -> text "left"
>               JRight -> text "right"
>               Full -> text "full"
>               Cross -> text "cross"
>           ,text "join"]
>     joinCond (Just (JoinOn e)) = text "on" <+> scalarExpr e
>     joinCond (Just (JoinUsing es)) = text "using" <+> parens (commaSep $ map text es)
>     joinCond Nothing = empty
>     joinCond (Just JoinNatural) = empty

> maybeScalarExpr :: String -> Maybe ScalarExpr -> Doc
> maybeScalarExpr k = maybe empty
>       (\e -> sep [text k
>                  ,nest 4 $ scalarExpr e])

> grpBy :: [ScalarExpr] -> Doc
> grpBy [] = empty
> grpBy gs = sep [text "group by"
>                ,nest 4 $ commaSep $ map scalarExpr gs]

> orderBy :: [(ScalarExpr,Direction)] -> Doc
> orderBy [] = empty
> orderBy os = sep [text "order by"
>                  ,nest 4 $ commaSep $ map f os]
>   where
>     f (e,Asc) = scalarExpr e
>     f (e,Desc) = scalarExpr e <+> text "desc"


= utils

> commaSep :: [Doc] -> Doc
> commaSep ds = sep $ punctuate comma ds
