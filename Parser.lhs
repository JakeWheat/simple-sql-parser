

> module Parser (parseQueryExpr
>               ,parseScalarExpr
>               ,ParseError(..)) where

> import Text.Groom
> import Text.Parsec
> import Control.Monad.Identity
> import Control.Applicative hiding (many, (<|>), optional)
> import Debug.Trace
> import Data.List
> import Text.Parsec.Expr
> import qualified Language.Haskell.Exts.Syntax as HSE
> import qualified Language.Haskell.Exts.Fixity as HSE
> import Data.Maybe

> import Syntax


> parseQueryExpr :: FilePath -> Maybe (Int,Int) -> String -> Either ParseError QueryExpr
> parseQueryExpr _ _ = parse (whiteSpace *> queryExpr <* eof) ""

> parseScalarExpr :: FilePath -> Maybe (Int,Int) -> String -> Either ParseError ScalarExpr
> parseScalarExpr _ _ = parse (whiteSpace *> scalarExpr <* eof) ""


> type P a = ParsecT String () Identity a


------------------------------------------------

= scalar expressions

> estring :: P ScalarExpr
> estring = Literal <$> (symbol_ "'" *> manyTill anyChar (symbol_ "'"))

> integer :: P ScalarExpr
> integer = Literal <$> (many1 digit <* whiteSpace)

> literal :: P ScalarExpr
> literal = integer <|> estring

> identifierString :: P String
> identifierString = do
>     s <- (:) <$> letterOrUnderscore
>              <*> many letterDigitOrUnderscore <* whiteSpace
>     guard (s `notElem` blacklist)
>     return s
>   where
>     letterOrUnderscore = char '_' <|> letter
>     letterDigitOrUnderscore = char '_' <|> alphaNum
> blacklist :: [String]
> blacklist = ["as", "from", "where", "having", "group", "order"
>                 ,"inner", "left", "right", "full", "natural", "join"
>                 ,"on", "using", "when", "then", "case", "end", "order"]

TODO: talk about what must be in the blacklist, and what doesn't need
to be.

> identifier :: P ScalarExpr
> identifier = Identifier <$> identifierString

> dottedIdentifier :: P ScalarExpr
> dottedIdentifier = Identifier2 <$> identifierString
>                                <*> (symbol "." *> identifierString)

> star :: P ScalarExpr
> star = choice [Star <$ symbol "*"
>               ,Star2 <$> (identifierString <* symbol "." <* symbol "*")]


> app :: P ScalarExpr
> app = App <$> identifierString
>       -- support for count(*)
>       <*> parens (choice[(:[]) <$> try star
>                         ,commaSep scalarExpr'])

> scase :: P ScalarExpr
> scase =
>     Case <$> (try (keyword_ "case") *> optionMaybe (try scalarExpr'))
>          <*> many1 swhen
>          <*> optionMaybe (try (keyword_ "else") *> scalarExpr')
>          <* keyword_ "end"
>   where
>     swhen = keyword_ "when" *>
>             ((,) <$> scalarExpr' <*> (keyword_ "then" *> scalarExpr'))

> binOpSymbolNames :: [String]
> binOpSymbolNames = ["=", "<=", ">="
>                    ,"!=", "<>", "<", ">"
>                    ,"*", "/", "+", "-"
>                    ,"||"]

> binOpKeywordNames :: [String]
> binOpKeywordNames = ["and", "or", "like"]

> unaryOp :: P ScalarExpr
> unaryOp = makeOp <$> (try (keyword_ "not") *> scalarExpr)
>   where makeOp e = Op "not" [e]

> scalarExpr' :: P ScalarExpr
> scalarExpr' = factor >>= trysuffix
>   where
>     factor = choice [literal
>                     ,scase
>                     ,unaryOp
>                     ,try app
>                     ,try dottedIdentifier
>                     ,identifier
>                     ,sparens]
>     trysuffix e = try (suffix e) <|> return e
>     suffix e0 = (makeOp e0 <$> opSymbol <*> factor) >>= trysuffix
>     opSymbol = choice (map (try . symbol) binOpSymbolNames
>                       ++ map (try . keyword) binOpKeywordNames)
>     makeOp e0 op e1 = Op op [e0,e1]

> sparens :: P ScalarExpr
> sparens = Parens <$> parens scalarExpr'

> toHaskell :: ScalarExpr -> HSE.Exp
> toHaskell e = case e of
>     Identifier i -> HSE.Var $ HSE.UnQual $ HSE.Ident i
>     Literal l -> HSE.Lit $ HSE.String l
>     App n es -> HSE.App (toHaskell $ Identifier n) $ ltoh es
>     Op n [e0,e1] -> HSE.InfixApp (toHaskell e0)
>                                  (HSE.QVarOp $ HSE.UnQual $ HSE.Ident n)
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
>                               ,ltoh $ maybeToList el])
>   where
>     ltoh = HSE.List . map toHaskell

> toSql :: HSE.Exp -> ScalarExpr
> toSql e = case e of
>     HSE.Var (HSE.UnQual (HSE.Ident "*")) -> Star
>     HSE.Var (HSE.Qual (HSE.ModuleName q) (HSE.Ident "*")) -> Star2 q
>     HSE.Var (HSE.Qual (HSE.ModuleName a) (HSE.Ident b)) -> Identifier2 a b
>     HSE.Var (HSE.UnQual (HSE.Ident i)) -> Identifier i
>     HSE.Lit (HSE.String l) -> Literal l
>     HSE.App (HSE.Var (HSE.UnQual (HSE.Ident "$case"))) (HSE.List [v,ts,el]) ->
>         Case (ltom v) (pairs ts) (ltom el)
>     HSE.App (HSE.Var (HSE.UnQual (HSE.Ident "not")))
>             (HSE.List [ea]) -> Op "not" [toSql ea]
>     HSE.App (HSE.Var (HSE.UnQual (HSE.Ident i)))
>             (HSE.List es) -> App i $ map toSql es
>     HSE.InfixApp e0 (HSE.QVarOp (HSE.UnQual (HSE.Ident n))) e1 ->
>         Op n [toSql e0, toSql e1]
>     HSE.Paren e0 -> Parens $ toSql e0
>     _ -> error $ "unsupported haskell " ++ groom e
>   where
>     ltom (HSE.List []) = Nothing
>     ltom (HSE.List [ex]) = Just $ toSql ex
>     ltom ex = error $ "unsupported haskell " ++ groom ex
>     pairs (HSE.List l) = map (\(HSE.List [a,b]) -> (toSql a, toSql b)) l
>     pairs ex = error $ "unsupported haskell " ++ groom ex

> sqlFixities :: [HSE.Fixity]
> sqlFixities = HSE.infixl_ 9 ["*", "/"]
>               ++ HSE.infixl_ 8 ["+", "-"]
>               ++ HSE.infixl_ 6 ["<=",">=","!=","<>","||", "like"]
>               ++ HSE.infix_ 4 ["<", ">"]
>               ++ HSE.infixr_ 3 ["="]
>               ++ HSE.infixr_ 2 ["or"]
>               ++ HSE.infixl_ 1 ["and"]
>               ++ HSE.infixl_ 0 ["or"]

> fixFixity :: ScalarExpr -> ScalarExpr
> fixFixity se = runIdentity $
>      toSql <$> HSE.applyFixities sqlFixities (toHaskell se)

> scalarExpr :: P ScalarExpr
> scalarExpr =
>     choice [try star
>            ,fixFixity <$> scalarExpr']

-------------------------------------------------

= query expressions

> selectItem :: P (Maybe String, ScalarExpr)
> selectItem = flip (,) <$> scalarExpr <*> optionMaybe (try alias)
>   where alias = optional (try (keyword_ "as")) *> identifierString

> selectList :: P [(Maybe String,ScalarExpr)]
> selectList = try (keyword_ "select") *> commaSep1 selectItem

> from :: P [TableRef]
> from = option [] (try (keyword_ "from") *> commaSep1 tref)
>   where
>     tref = choice [try (JoinQueryExpr <$> parens queryExpr)
>                   ,JoinParens <$> parens tref
>                   ,SimpleTableRef <$> identifierString]
>            >>= optionSuffix join
>            >>= optionSuffix alias
>     join tref0 =
>         choice
>         [try (keyword_ "natural") *> keyword_ "inner"
>          *> conditionlessSuffix tref0 Inner (Just JoinNatural)
>         ,try (keyword_ "join")
>          *> (JoinTableRef Inner tref0 <$> tref <*> joinExpr)
>         ,try (keyword_ "inner")
>          *> conditionSuffix tref0 Inner
>         ,try (choice [JLeft <$ keyword_ "left"
>                      ,JRight <$ keyword_ "right"
>                      ,Full <$ keyword_ "full"])
>          >>= outerJoinSuffix tref0
>         ,try (keyword_ "cross")
>          *> conditionlessSuffix tref0 Cross Nothing
>         ]
>         >>= optionSuffix join
>     outerJoinSuffix tref0 jt =
>         optional (keyword_ "outer") *> conditionSuffix tref0 jt
>     conditionSuffix tref0 jt =
>         keyword_ "join" *> (JoinTableRef jt tref0 <$> tref <*> joinExpr)
>     conditionlessSuffix tref0 jt jc =
>         keyword_ "join" *> (JoinTableRef jt tref0 <$> tref <*> return jc)
>     joinExpr = choice
>                [(Just . JoinUsing)
>                  <$> (try (keyword_ "using")
>                       *> parens (commaSep1 identifierString))
>                ,(Just . JoinOn) <$> (try (keyword_ "on") *> scalarExpr)
>                ,return Nothing
>                ]
>     alias j = let a1 = optional (try (keyword_ "as")) *> identifierString
>               in option j (JoinAlias j <$> try a1)

> swhere :: P (Maybe ScalarExpr)
> swhere = optionMaybe (try (keyword_ "where") *> scalarExpr)

> sgroupBy :: P [ScalarExpr]
> sgroupBy = option [] (try (keyword_ "group")
>                       *> keyword_ "by"
>                       *> commaSep1 scalarExpr)

> having :: P (Maybe ScalarExpr)
> having = optionMaybe (try (keyword_ "having") *> scalarExpr)

> orderBy :: P [ScalarExpr]
> orderBy = option [] (try (keyword_ "order")
>                      *> keyword_ "by"
>                      *> commaSep1 scalarExpr)

> queryExpr :: P QueryExpr
> queryExpr =
>     Select
>     <$> selectList
>     <*> from
>     <*> swhere
>     <*> sgroupBy
>     <*> having
>     <*> orderBy


------------------------------------------------

= helper functions

> whiteSpace :: P ()
> whiteSpace =
>     choice [simpleWhiteSpace *> whiteSpace
>            ,lineComment *> whiteSpace
>            ,blockComment *> whiteSpace
>            ,return ()]
>   where
>     lineComment = try (string "--")
>                   *> manyTill anyChar (void (char '\n') <|> eof)
>     blockComment = -- no nesting of block comments in SQL
>                    try (string "/*")
>                    -- TODO: why is try used herex
>                    *> manyTill anyChar (try $ string "*/")
>     -- use many1 so we can more easily avoid non terminating loops
>     simpleWhiteSpace = void $ many1 (oneOf " \t\n")

> optionSuffix :: (a -> P a) -> a -> P a
> optionSuffix p a = option a (p a)

> parens :: P a -> P a
> parens = between (symbol_ "(") (symbol_ ")")

> commaSep :: P a -> P [a]
> commaSep = (`sepBy` symbol_ ",")


> symbol :: String -> P String
> symbol s = string s
>            -- <* notFollowedBy (oneOf "+-/*<>=!|")
>            <* whiteSpace

> symbol_ :: String -> P ()
> symbol_ s = symbol s *> return ()

> keyword :: String -> P String
> keyword s = string s
>             <* notFollowedBy (char '_' <|> alphaNum)
>             <* whiteSpace

> keyword_ :: String -> P ()
> keyword_ s = keyword s *> return ()

> commaSep1 :: P a -> P [a]
> commaSep1 = (`sepBy1` symbol_ ",")
