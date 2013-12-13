

> module Language.SQL.SimpleSQL.Parser
>     (parseQueryExpr
>     ,parseScalarExpr
>     ,parseQueryExprs
>     ,ParseError(..)) where

> import Text.Groom
> import Text.Parsec hiding (ParseError)
> import qualified Text.Parsec as P
> import Control.Monad.Identity
> import Control.Applicative hiding (many, (<|>), optional)
> import qualified Language.Haskell.Exts.Syntax as HSE
> import qualified Language.Haskell.Exts.Fixity as HSE
> import Data.Maybe
> import Data.List
> import Data.Char

> import Language.SQL.SimpleSQL.Syntax


> parseQueryExpr :: FilePath
>                -> Maybe (Int,Int)
>                -> String
>                -> Either ParseError QueryExpr
> parseQueryExpr f p src =
>     either (Left . convParseError src) Right
>     $ parse (setPos p *> whiteSpace *> queryExpr <* eof) f src

> parseQueryExprs :: FilePath
>                 -> Maybe (Int,Int)
>                 -> String
>                 -> Either ParseError [QueryExpr]
> parseQueryExprs f p src =
>     either (Left . convParseError src) Right
>     $ parse (setPos p *> whiteSpace *> queryExprs <* eof) f src

> parseScalarExpr :: FilePath
>                 -> Maybe (Int,Int)
>                 -> String
>                 -> Either ParseError ScalarExpr
> parseScalarExpr f p src =
>     either (Left . convParseError src) Right
>     $ parse (setPos p *> whiteSpace *> scalarExpr <* eof) f src

> data ParseError = ParseError
>                   {peErrorString :: String
>                   ,peFilename :: FilePath
>                   ,pePosition :: (Int,Int)
>                   ,peFormattedError :: String
>                   } deriving (Eq,Show)

------------------------------------------------

> type P a = ParsecT String () Identity a

= scalar expressions

> stringLiteral :: P String
> stringLiteral = symbol_ "'" *> manyTill anyChar (symbol_ "'")

> estring :: P ScalarExpr
> estring = StringLit <$> stringLiteral

digits
digits.[digits][e[+-]digits]
[digits].digits[e[+-]digits]
digitse[+-]digits

> number :: P ScalarExpr
> number =
>     NumLit <$> (choice [int
>                         >>= optionSuffix dot
>                         >>= optionSuffix fracts
>                         >>= optionSuffix expon
>                        ,fract "" >>= optionSuffix expon]
>                 <* whiteSpace)
>   where
>     int = many1 digit
>     fract p = dot p >>= fracts
>     dot p = ((p++) . (:[])) <$> char '.'
>     fracts p = (p++) <$> int
>     expon p = do
>         void $ char 'e'
>         s <- option "" ((:[]) <$> (char '+' <|> char '-'))
>         i <- int
>         return (p ++ "e" ++ s ++ i)

> interval :: P ScalarExpr
> interval = try (keyword_ "interval") >>
>     IntervalLit
>     <$> stringLiteral
>     <*> identifierString
>     <*> optionMaybe (try $ parens (read <$> many1 digit))

> literal :: P ScalarExpr
> literal = number <|> estring <|> interval

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
>             ,"inner", "left", "right", "full", "natural", "join"
>             ,"on", "using", "when", "then", "case", "end", "order"
>             ,"limit", "offset", "in"
>             ,"except", "intersect", "union"]

TODO: talk about what must be in the blacklist, and what doesn't need
to be.

> identifier :: P ScalarExpr
> identifier = Iden <$> identifierString

> dottedIden :: P ScalarExpr
> dottedIden = Iden2 <$> identifierString
>                    <*> (symbol "." *> identifierString)

> star :: P ScalarExpr
> star = choice [Star <$ symbol "*"
>               ,Star2 <$> (identifierString <* symbol "." <* symbol "*")]


> app :: P ScalarExpr
> app = do
>       i <- identifierString
>       _ <- symbol "("
>       d <- try duplicates
>       es <- choice [(:[]) <$> try star
>                    ,commaSep scalarExpr']
>       od <- try $ optionMaybe orderBy
>       _ <- symbol ")"
>       case (d,od) of
>           (Nothing,Nothing) ->
>               return $ App i es
>           _ -> return $ AggregateApp i d es (fromMaybe [] od)

> windowSuffix :: ScalarExpr -> P ScalarExpr
> windowSuffix e@(App f es) =
>     choice [try (keyword_ "over")
>             *> parens (WindowApp f es
>                        <$> option [] partitionBy
>                        <*> option [] orderBy)
>            ,return e]
>   where
>     partitionBy = try (keyword_ "partition") >>
>         keyword_ "by" >>
>         commaSep1 scalarExpr'

> windowSuffix e = return e

> scase :: P ScalarExpr
> scase =
>     Case <$> (try (keyword_ "case") *> optionMaybe (try scalarExpr'))
>          <*> many1 swhen
>          <*> optionMaybe (try (keyword_ "else") *> scalarExpr')
>          <* keyword_ "end"
>   where
>     swhen = keyword_ "when" *>
>             ((,) <$> scalarExpr' <*> (keyword_ "then" *> scalarExpr'))

> cast :: P ScalarExpr
> cast = parensCast <|> prefixCast
>   where
>     parensCast = try (keyword_ "cast") >>
>                  parens (Cast <$> scalarExpr'
>                          <*> (keyword_ "as" *> typeName))
>     prefixCast = try (CastOp <$> typeName
>                              <*> stringLiteral)

> extract :: P ScalarExpr
> extract = try (keyword_ "extract") >>
>     parens (makeOp <$> identifierString
>                    <*> (keyword_ "from" *> scalarExpr'))
>   where makeOp n e = SpecialOp "extract" [Iden n, e]

> substring :: P ScalarExpr
> substring = try (keyword_ "substring") >>
>     parens (makeOp <$> scalarExpr'
>                    <*> (keyword_ "from" *> scalarExpr')
>                    <*> (keyword_ "for" *> scalarExpr')
>                    )
>   where makeOp a b c = SpecialOp "substring" [a,b,c]

> inSuffix :: ScalarExpr -> P ScalarExpr
> inSuffix e =
>     In <$> inty
>        <*> return e
>        <*> parens (choice
>                    [InQueryExpr <$> queryExpr
>                    ,InList <$> commaSep1 scalarExpr'])
>   where
>     inty = try $ choice [True <$ keyword_ "in"
>                         ,False <$ keyword_ "not" <* keyword_ "in"]

> betweenSuffix :: ScalarExpr -> P ScalarExpr
> betweenSuffix e =
>     makeOp <$> opName
>            <*> return e
>            <*> scalarExpr'' True
>            <*> (keyword_ "and" *> scalarExpr')
>   where
>     opName = try $ choice
>              ["between" <$ keyword_ "between"
>              ,"not between" <$ keyword_ "not" <* keyword_ "between"]
>     makeOp n a b c = SpecialOp n [a,b,c]

> subquery :: P ScalarExpr
> subquery =
>     choice
>     [try $ SubQueryExpr SqSq <$> parens queryExpr
>     ,SubQueryExpr <$> try sqkw <*> parens queryExpr]
>   where
>     sqkw = try $ choice
>            [SqExists <$ keyword_ "exists"
>            ,SqAll <$ try (keyword_ "all")
>            ,SqAny <$ keyword_ "any"
>            ,SqSome <$ keyword_ "some"]

> typeName :: P TypeName
> typeName = choice
>     [TypeName "double precision"
>      <$ try (keyword_ "double" <* keyword_ "precision")
>     ,TypeName "character varying"
>      <$ try (keyword_ "character" <* keyword_ "varying")
>     ,TypeName <$> identifierString]

> binOpSymbolNames :: [String]
> binOpSymbolNames =
>     ["=", "<=", ">=", "!=", "<>", "<", ">"
>     ,"*", "/", "+", "-"
>     ,"||"]

> binOpKeywordNames :: [String]
> binOpKeywordNames = ["and", "or", "like", "overlaps"]

> binOpMultiKeywordNames :: [[String]]
> binOpMultiKeywordNames = map words
>     ["not like"
>     ,"not similar"
>     ,"is similar to"
>     ,"is not similar to"
>     ,"is distinct from"
>     ,"is not distinct from"]


used for between parsing

> binOpKeywordNamesNoAnd :: [String]
> binOpKeywordNamesNoAnd = filter (/="and") binOpKeywordNames

> prefixUnOpKeywordNames :: [String]
> prefixUnOpKeywordNames = ["not"]

> prefixUnOpSymbolNames :: [String]
> prefixUnOpSymbolNames = ["+", "-"]


> prefixUnaryOp :: P ScalarExpr
> prefixUnaryOp =
>     PrefixOp <$> opSymbol <*> scalarExpr'
>   where
>     opSymbol = choice (map (try . symbol) prefixUnOpSymbolNames
>                       ++ map (try . keyword) prefixUnOpKeywordNames)

> postfixOp :: ScalarExpr -> P ScalarExpr
> postfixOp e =
>     try $ choice $ map makeOp opPairs
>   where
>     -- could left factor here?
>     ops = ["is null"
>           ,"is not null"
>           ,"is true"
>           ,"is not true"
>           ,"is false"
>           ,"is not false"
>           ,"is unknown"
>           ,"is not unknown"]
>     opPairs = flip map ops $ \o -> (o, words o)
>     makeOp (o,ws) = try $ PostfixOp o e <$ keywords_ ws
>     keywords_ = try . mapM_ keyword_

> scalarExpr' :: P ScalarExpr
> scalarExpr' = scalarExpr'' False

the bexpr is to deal with between x and y

when we are parsing the scalar expr for x, we don't allow and as a
binary operator except nested in parens. This is taken from how
postgresql handles this

> scalarExpr'' :: Bool -> P ScalarExpr
> scalarExpr'' bExpr = factor >>= trysuffix
>   where
>     factor = choice [literal
>                     ,scase
>                     ,cast
>                     ,extract
>                     ,substring
>                     ,subquery
>                     ,prefixUnaryOp
>                     ,try app >>= windowSuffix
>                     ,try dottedIden
>                     ,identifier
>                     ,sparens]
>     trysuffix e = try (suffix e) <|> return e
>     suffix e0 = choice
>                 [BinOp <$> opSymbol <*> return e0 <*> factor
>                 ,inSuffix e0
>                 ,betweenSuffix e0
>                 ,postfixOp e0
>                 ] >>= trysuffix
>     opSymbol = choice
>         (map (try . symbol) binOpSymbolNames
>         ++ map (try . keywords) binOpMultiKeywordNames
>         ++ map (try . keyword)
>                (if bExpr
>                 then binOpKeywordNamesNoAnd
>                 else binOpKeywordNames))
>     keywords ks = unwords <$> mapM keyword ks

> sparens :: P ScalarExpr
> sparens = Parens <$> parens scalarExpr'

attempt to fix the precedence and associativity. Doesn't work

> toHaskell :: ScalarExpr -> HSE.Exp
> toHaskell e = case e of
>     Iden i -> HSE.Var $ HSE.UnQual $ HSE.Ident i
>     StringLit l -> HSE.Lit $ HSE.String $ 's':l
>     NumLit l -> HSE.Lit $ HSE.String $ 'n':l
>     App n es -> HSE.App (toHaskell $ Iden n) $ ltoh es
>     Cast e0 (TypeName tn) -> toHaskell $ App ("cast:" ++ tn) [e0]
>     CastOp (TypeName tn) s -> toHaskell $ App ("castop:" ++ tn) [StringLit s]
>     --Op n [e0,e1] -> HSE.InfixApp (toHaskell e0)
>     --                             (HSE.QVarOp $ HSE.UnQual $ HSE.Ident n)
>     --                             (toHaskell e1)
>     --Op o [e0] -> toHaskell $ App ("unary:" ++ o) [e0]
>     --Op {} -> error $ "bad args to operator " ++ groom e
>     Star -> HSE.Var $ HSE.UnQual $ HSE.Ident "*"
>     Iden2 a b -> HSE.Var $ HSE.Qual (HSE.ModuleName a) (HSE.Ident b)
>     Star2 q -> HSE.Var $ HSE.Qual (HSE.ModuleName q) (HSE.Ident "*")
>     Parens e0 -> HSE.Paren $ toHaskell e0
>     -- map the two maybes to lists with either 0 or 1 element
>     Case v ts el -> HSE.App (toHaskell $ Iden "$case")
>                     (HSE.List [ltoh $ maybeToList v
>                               ,HSE.List $ map (ltoh . (\(a,b) -> [a,b])) ts
>                               ,ltoh $ maybeToList el])
>     _ -> error "please fix me 1"
>   where
>     ltoh = HSE.List . map toHaskell

> toSql :: HSE.Exp -> ScalarExpr
> toSql e = case e of
>     HSE.Var (HSE.UnQual (HSE.Ident "*")) -> Star
>     HSE.Var (HSE.Qual (HSE.ModuleName q) (HSE.Ident "*")) -> Star2 q
>     HSE.Var (HSE.Qual (HSE.ModuleName a) (HSE.Ident b)) -> Iden2 a b
>     HSE.Var (HSE.UnQual (HSE.Ident i)) -> Iden i
>     HSE.Lit (HSE.String ('s':l)) -> StringLit l
>     HSE.Lit (HSE.String ('n':l)) -> NumLit l
>     HSE.App (HSE.Var (HSE.UnQual (HSE.Ident "$case"))) (HSE.List [v,ts,el]) ->
>         Case (ltom v) (pairs ts) (ltom el)
>     {-HSE.App (HSE.Var (HSE.UnQual (HSE.Ident x)))
>             (HSE.List [ea])
>         | "unary:" `isPrefixOf` x ->
>           Op (drop 6 x) [toSql ea]
>         | "cast:" `isPrefixOf` x ->
>           Cast (toSql ea) (TypeName $ drop 5 x)-}
>     HSE.App (HSE.Var (HSE.UnQual (HSE.Ident x)))
>             (HSE.List [HSE.Lit (HSE.String ('s':ea))])
>         | "castop:" `isPrefixOf` x ->
>           CastOp (TypeName $ drop 7 x) ea
>     HSE.App (HSE.Var (HSE.UnQual (HSE.Ident i)))
>             (HSE.List es) -> App i $ map toSql es
>     {-HSE.InfixApp e0 (HSE.QVarOp (HSE.UnQual (HSE.Ident n))) e1 ->
>         Op n [toSql e0, toSql e1]-}
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

> _fixFixity :: ScalarExpr -> ScalarExpr
> _fixFixity se = runIdentity $
>      toSql <$> HSE.applyFixities sqlFixities (toHaskell se)

> scalarExpr :: P ScalarExpr
> scalarExpr =
>     choice [try star
>            ,{-fixFixity <$>-} scalarExpr']

-------------------------------------------------

= query expressions

> duplicates :: P (Maybe Duplicates)
> duplicates = optionMaybe $ try $
>     choice [All <$ keyword_ "all"
>            ,Distinct <$ keyword "distinct"]

> selectItem :: P (Maybe String, ScalarExpr)
> selectItem = flip (,) <$> scalarExpr <*> optionMaybe (try alias)
>   where alias = optional (try (keyword_ "as")) *> identifierString

> selectList :: P [(Maybe String,ScalarExpr)]
> selectList = commaSep1 selectItem

> from :: P [TableRef]
> from = option [] (try (keyword_ "from") *> commaSep1 tref)
>   where
>     tref = choice [try (JoinQueryExpr <$> parens queryExpr)
>                   ,JoinParens <$> parens tref
>                   ,SimpleTableRef <$> identifierString]
>            >>= optionSuffix pjoin
>            >>= optionSuffix alias
>     pjoin tref0 =
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
>         >>= optionSuffix pjoin
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
>                   a2 = optionMaybe (try $ parens (commaSep1 identifierString))
>               in option j (JoinAlias j <$> try a1 <*> try a2)

> optionalScalarExpr :: String -> P (Maybe ScalarExpr)
> optionalScalarExpr k = optionMaybe (try (keyword_ k) *> scalarExpr)

> swhere :: P (Maybe ScalarExpr)
> swhere = optionalScalarExpr "where"

> sgroupBy :: P [ScalarExpr]
> sgroupBy = option [] (try (keyword_ "group")
>                       *> keyword_ "by"
>                       *> commaSep1 scalarExpr)

> having :: P (Maybe ScalarExpr)
> having = optionalScalarExpr "having"

> orderBy :: P [(ScalarExpr,Direction)]
> orderBy = try (keyword_ "order") *> keyword_ "by" *> commaSep1 ob
>   where
>     ob = (,) <$> scalarExpr
>              <*> option Asc (choice [Asc <$ keyword_ "asc"
>                                     ,Desc <$ keyword_ "desc"])

> limit :: P (Maybe ScalarExpr)
> limit = optionalScalarExpr "limit"

> offset :: P (Maybe ScalarExpr)
> offset = optionalScalarExpr "offset"

> with :: P QueryExpr
> with = try (keyword_ "with") >>
>     With <$> commaSep1 withQuery <*> queryExpr
>   where
>     withQuery =
>         (,) <$> (identifierString <* optional (try $ keyword_ "as"))
>             <*> parens queryExpr

> queryExpr :: P QueryExpr
> queryExpr =
>   choice [select >>= queryExprSuffix, with]
>   where
>     select = try (keyword_ "select") >>
>         Select
>         <$> (fromMaybe All <$> duplicates)
>         <*> selectList
>         <*> from
>         <*> swhere
>         <*> sgroupBy
>         <*> having
>         <*> option [] orderBy
>         <*> limit
>         <*> offset

> queryExprSuffix :: QueryExpr -> P QueryExpr
> queryExprSuffix qe =
>     choice [(CombineQueryExpr qe
>              <$> try (choice
>                       [Union <$ keyword_ "union"
>                       ,Intersect <$ keyword_ "intersect"
>                       ,Except <$ keyword_ "except"])
>              <*> (fromMaybe All <$> duplicates)
>              <*> option Respectively
>                         (try (Corresponding
>                               <$ keyword_ "corresponding"))
>              <*> queryExpr)
>             >>= queryExprSuffix
>            ,return qe]

> queryExprs :: P [QueryExpr]
> queryExprs = do
>     qe <- queryExpr
>     choice [[qe] <$ eof
>            ,symbol ";" *>
>             choice [[qe] <$ eof
>                    ,(:) qe <$> queryExprs]]

------------------------------------------------

= helper parsers

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
> keyword s = (map toLower <$> string s)
>             <* notFollowedBy (char '_' <|> alphaNum)
>             <* whiteSpace

> keyword_ :: String -> P ()
> keyword_ s = keyword s *> return ()

> commaSep1 :: P a -> P [a]
> commaSep1 = (`sepBy1` symbol_ ",")

--------------------------------------------

= helper functions

> setPos :: Maybe (Int,Int) -> P ()
> setPos Nothing = return ()
> setPos (Just (l,c)) = fmap f getPosition >>= setPosition
>   where f = flip setSourceColumn c
>             . flip setSourceLine l

> convParseError :: String -> P.ParseError -> ParseError
> convParseError src e =
>     ParseError
>     {peErrorString = show e
>     ,peFilename = sourceName p
>     ,pePosition = (sourceLine p, sourceColumn p)
>     ,peFormattedError = formatError src e
>     }
>   where
>     p = errorPos e

format the error more nicely: emacs format for positioning, plus
context

> formatError :: String -> P.ParseError -> String
> formatError src e =
>     sourceName p ++ ":" ++ show (sourceLine p)
>     ++ ":" ++ show (sourceColumn p) ++ ":"
>     ++ context
>     ++ show e
>   where
>     context =
>         let lns = take 1 $ drop (sourceLine p - 1) $ lines src
>         in case lns of
>              [x] -> "\n" ++ x ++ "\n"
>                     ++ replicate (sourceColumn p - 1) ' ' ++ "^\n"
>              _ -> ""
>     p = errorPos e
