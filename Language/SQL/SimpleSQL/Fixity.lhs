
This is the module which deals with fixing up the scalar expression
trees for the operator precedence and associativity (aka 'fixity').

It currently uses haskell-src-exts as a hack, the algorithm from there
should be ported to work on these trees natively. Maybe it could be
made generic?

> module Language.SQL.SimpleSQL.Fixity
>        (fixFixities
>        ,Fixity(..)
>        ,Assoc(..)
>        ,infixl_
>        ,infixr_
>        ,infix_
>        ) where

> import qualified Language.Haskell.Exts.Syntax as HSE
> import qualified Language.Haskell.Exts.Fixity as HSE
> import Control.Monad.Identity
> import Control.Applicative


> import Language.SQL.SimpleSQL.Syntax

> data Fixity = Fixity String --name of op
>                      Assoc
>               deriving (Eq,Show)

> data Assoc = AssocLeft | AssocRight | AssocNone
>              deriving (Eq,Show)

> infixl_ :: [String] -> [Fixity]
> infixl_ = map (`Fixity` AssocLeft)

> infixr_ :: [String] -> [Fixity]
> infixr_ = map (`Fixity` AssocRight)

> infix_ :: [String] -> [Fixity]
> infix_ = map (`Fixity` AssocNone)

> toHSEFixity :: [[Fixity]] -> [HSE.Fixity]
> toHSEFixity fs =
>     let fs' = zip [0..] $ reverse fs
>     in concatMap f fs'
>   where
>     f :: (Int, [Fixity]) -> [HSE.Fixity]
>     f (n,fs') = flip concatMap fs' $ \(Fixity nm assoc) ->
>                 case assoc of
>                     AssocLeft -> HSE.infixl_ n [nm]
>                     AssocRight -> HSE.infixr_ n [nm]
>                     AssocNone -> HSE.infix_ n [nm]

fix the fixities in the given scalar expr. All the expressions to be
fixed should be left associative and equal precedence to be fixed
correctly. It doesn't descend into query expressions in subqueries and
the scalar expressions they contain.

TODO: get it to work on prefix and postfix unary operators also maybe
it should work on some of the other syntax (such as in).

> fixFixities :: [[Fixity]] -> ScalarExpr -> ScalarExpr
> fixFixities fs se = runIdentity $
>      toSql <$> HSE.applyFixities (toHSEFixity fs) (toHaskell se)

Now have to convert all our scalar exprs to haskell and back again.
Have to come up with a recipe for each ctor.

> toHaskell :: ScalarExpr -> HSE.Exp
> toHaskell e = case e of
>     BinOp e0 op e1 -> HSE.InfixApp
>                       (toHaskell e0)
>                       (HSE.QVarOp $ HSE.UnQual $ HSE.Symbol op)
>                       (toHaskell e1)
>     Iden i -> HSE.Var $ HSE.UnQual $ HSE.Ident i
>     StringLit l -> HSE.Lit $ HSE.String ('s':l)
>     NumLit n -> HSE.Lit $ HSE.String ('n':n)
>     App n es -> HSE.App (toHaskell $ Iden n) $ ltoh es
>     Parens e0 -> HSE.Paren $ toHaskell e0
>     {-
>     Identifier i -> HSE.Var $ HSE.UnQual $ HSE.Ident i
>     Literal l -> HSE.Lit $ HSE.String l
>     App n es -> HSE.App (toHaskell $ Identifier n) $ ltoh es
>     Op n [e0,e1] -> HSE.InfixApp (toHaskell e0)
>                                  (HSE.QVarOp $ HSE.UnQual $ HSE.Symbol n)
>                                  (toHaskell e1)
>     Op "not" [e0] -> toHaskell $ App "not" [e0]
>     Op {} -> error $ "bad args to operator " ++ groom e
>     Star -> HSE.Var $ HSE.UnQual $ HSE.Ident "*"
>     Identifier2 a b -> HSE.Var $ HSE.Qual (HSE.ModuleName a) (HSE.Ident b)
>     Star2 q -> HSE.Var $ HSE.Qual (HSE.ModuleName q) (HSE.Ident "*")
>     Parens e0 -> HSE.Paren $ toHaskell e0
>     -- map the two maybes to lists with either 0 or 1 element
>     Case v ts el -> HSE.App (toHaskell $ Identifier "$case")
>                     (HSE.List [ltoh $ maybeToList v
>                               ,HSE.List $ map (ltoh . (\(a,b) -> [a,b])) ts
>                               ,ltoh $ maybeToList el])-}
>   where
>     ltoh = HSE.List . map toHaskell

> toSql :: HSE.Exp -> ScalarExpr
> toSql e = case e of
>     HSE.Var (HSE.UnQual (HSE.Ident i)) -> Iden i
>     HSE.Lit (HSE.String ('s':l)) -> StringLit l
>     HSE.Lit (HSE.String ('n':l)) -> NumLit l
>     HSE.InfixApp e0 (HSE.QVarOp (HSE.UnQual (HSE.Symbol n))) e1 ->
>         BinOp (toSql e0) n (toSql e1)
>     HSE.App (HSE.Var (HSE.UnQual (HSE.Ident i)))
>             (HSE.List es) -> App i $ map toSql es
>     HSE.Paren e0 -> Parens $ toSql e0


>     {-HSE.Var (HSE.UnQual (HSE.Ident "*")) -> Star
>     HSE.Var (HSE.Qual (HSE.ModuleName q) (HSE.Ident "*")) -> Star2 q
>     HSE.Var (HSE.Qual (HSE.ModuleName a) (HSE.Ident b)) -> Identifier2 a b
>     HSE.App (HSE.Var (HSE.UnQual (HSE.Ident "$case"))) (HSE.List [v,ts,el]) ->
>         Case (ltom v) (pairs ts) (ltom el)
>     HSE.App (HSE.Var (HSE.UnQual (HSE.Ident "not")))
>             (HSE.List [ea]) -> Op "not" [toSql ea]
>     HSE.App (HSE.Var (HSE.UnQual (HSE.Ident i)))
>             (HSE.List es) -> App i $ map toSql es
>     HSE.InfixApp e0 (HSE.QVarOp (HSE.UnQual (HSE.Symbol n))) e1 ->
>         Op n [toSql e0, toSql e1]
>     HSE.Paren e0 -> Parens $ toSql e0 -}
>     _ -> error $ "unsupported haskell " ++ show e
>   where
>     ltom (HSE.List []) = Nothing
>     ltom (HSE.List [ex]) = Just $ toSql ex
>     ltom ex = error $ "unsupported haskell " ++ show ex
>     pairs (HSE.List l) = map (\(HSE.List [a,b]) -> (toSql a, toSql b)) l
>     pairs ex = error $ "unsupported haskell " ++ show ex
