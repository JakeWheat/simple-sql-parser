
> -- | This is the module with the parser functions.
> module Language.SQL.SimpleSQL.Parser
>     (parseQueryExpr
>     ,parseScalarExpr
>     ,parseQueryExprs
>     ,ParseError(..)) where

> import Control.Monad.Identity
> import Control.Applicative hiding (many, (<|>), optional)
> import Data.Maybe
> import Data.Char
> import Text.Parsec hiding (ParseError)
> import qualified Text.Parsec as P
> import Text.Parsec.Perm

> import Language.SQL.SimpleSQL.Syntax
> import Language.SQL.SimpleSQL.Fixity

The public API functions.

> -- | Parses a query expr, trailing semicolon optional.
> parseQueryExpr :: FilePath
>                   -- ^ filename to use in errors
>                -> Maybe (Int,Int)
>                   -- ^ line number and column number to use in errors
>                -> String
>                   -- ^ the SQL source to parse
>                -> Either ParseError QueryExpr
> parseQueryExpr = wrapParse topLevelQueryExpr

> -- | Parses a list of query exprs, with semi colons between
> -- them. The final semicolon is optional.
> parseQueryExprs :: FilePath
>                    -- ^ filename to use in errors
>                 -> Maybe (Int,Int)
>                    -- ^ line number and column number to use in errors
>                 -> String
>                    -- ^ the SQL source to parse
>                 -> Either ParseError [QueryExpr]
> parseQueryExprs = wrapParse queryExprs

> -- | Parses a scalar expression.
> parseScalarExpr :: FilePath
>                    -- ^ filename to use in errors
>                 -> Maybe (Int,Int)
>                    -- ^ line number and column number to use in errors
>                 -> String
>                    -- ^ the SQL source to parse
>                 -> Either ParseError ScalarExpr
> parseScalarExpr = wrapParse scalarExpr

This helper function takes the parser given and:

sets the position when parsing
automatically skips leading whitespace
checks the parser parses all the input using eof
converts the error return to the nice wrapper

> wrapParse :: P a
>           -> FilePath
>           -> Maybe (Int,Int)
>           -> String
>           -> Either ParseError a
> wrapParse parser f p src =
>     either (Left . convParseError src) Right
>     $ parse (setPos p *> whiteSpace *> parser <* eof) f src

> -- | Type to represent parse errors.
> data ParseError = ParseError
>                   {peErrorString :: String
>                    -- ^ contains the error message
>                   ,peFilename :: FilePath
>                    -- ^ filename location for the error
>                   ,pePosition :: (Int,Int)
>                    -- ^ line number and column number location for the error
>                   ,peFormattedError :: String
>                    -- ^ formatted error with the position, error
>                    -- message and source context
>                   } deriving (Eq,Show)

------------------------------------------------

> type P a = ParsecT String () Identity a

= scalar expressions

== literals

See the stringLiteral lexer below for notes on string literal syntax.

> estring :: P ScalarExpr
> estring = StringLit <$> stringLiteral

> number :: P ScalarExpr
> number = NumLit <$> numberLiteral

parse SQL interval literals, something like
interval '5' day (3)
or
interval '5' month

wrap the whole lot in try, in case we get something like this:
interval '3 days'
which parses as a typed literal

> interval :: P ScalarExpr
> interval = try (keyword_ "interval" >>
>     IntervalLit
>     <$> stringLiteral
>     <*> identifierString
>     <*> optionMaybe (try $ parens integerLiteral))

> literal :: P ScalarExpr
> literal = number <|> estring <|> interval

== identifiers

Uses the identifierString 'lexer'. See this function for notes on
identifiers.

> name :: P Name
> name = choice [QName <$> quotedIdentifier
>               ,Name <$> identifierString]

> identifier :: P ScalarExpr
> identifier = Iden <$> name

== star

used in select *, select x.*, and agg(*) variations, and some other
places as well. Because it is quite general, the parser doesn't
attempt to check that the star is in a valid context, it parses it ok
in any scalar expression context.

> star :: P ScalarExpr
> star = Star <$ symbol "*"

== function application, aggregates and windows

this represents anything which syntactically looks like regular C
function application: an identifier, parens with comma sep scalar
expression arguments.

The parsing for the aggregate extensions is here as well:

aggregate([all|distinct] args [order by orderitems])

> aggOrApp :: P ScalarExpr
> aggOrApp =
>     makeApp
>     <$> name
>     <*> parens ((,,) <$> try duplicates
>                      <*> choice [commaSep scalarExpr']
>                      <*> try (optionMaybe orderBy))
>   where
>     makeApp i (Nothing,es,Nothing) = App i es
>     makeApp i (d,es,od) = AggregateApp i d es (fromMaybe [] od)

> duplicates :: P (Maybe Duplicates)
> duplicates = optionMaybe $ try $
>     choice [All <$ keyword_ "all"
>            ,Distinct <$ keyword "distinct"]

parse a window call as a suffix of a regular function call
this looks like this:
functionname(args) over ([partition by ids] [order by orderitems])

No support for explicit frames yet.

The convention in this file is that the 'Suffix', erm, suffix on
parser names means that they have been left factored. These are almost
always used with the optionSuffix combinator.

> windowSuffix :: ScalarExpr -> P ScalarExpr
> windowSuffix (App f es) =
>     try (keyword_ "over")
>     *> parens (WindowApp f es
>                <$> option [] partitionBy
>                <*> option [] orderBy
>                <*> optionMaybe frameClause)
>   where
>     partitionBy = try (keyword_ "partition") >>
>         keyword_ "by" >> commaSep1 scalarExpr'
>     frameClause =
>         mkFrame <$> (choice [FrameRows <$ keyword_ "rows"
>                             ,FrameRange <$ keyword_ "range"])
>                 <*> frameStartEnd
>     frameStartEnd =
>         choice
>         [try (keyword_ "between") >>
>          mkFrameBetween <$> frameLimit True
>                         <*> (keyword_ "and" *> frameLimit True)
>         ,mkFrameFrom <$> frameLimit False]
>     -- use the bexpression style from the between parsing for frame between
>     frameLimit useB =
>         choice
>         [Current <$ try (keyword_ "current") <* keyword_ "row"
>         ,try (keyword_ "unbounded") >>
>          choice [UnboundedPreceding <$ keyword_ "preceding"
>                 ,UnboundedFollowing <$ keyword_ "following"]
>         ,do
>          e <- if useB then scalarExprB else scalarExpr
>          choice [Preceding e <$ keyword_ "preceding"
>                 ,Following e <$ keyword_ "following"]
>         ]
>     mkFrameBetween s e rs = FrameBetween rs s e
>     mkFrameFrom s rs = FrameFrom rs s
>     mkFrame rs c = c rs
> windowSuffix _ = fail ""

> app :: P ScalarExpr
> app = aggOrApp >>= optionSuffix windowSuffix

== case expression

> scase :: P ScalarExpr
> scase =
>     Case <$> (try (keyword_ "case") *> optionMaybe (try scalarExpr'))
>          <*> many1 swhen
>          <*> optionMaybe (try (keyword_ "else") *> scalarExpr')
>          <* keyword_ "end"
>   where
>     swhen = keyword_ "when" *>
>             ((,) <$> commaSep1 scalarExpr'
>                  <*> (keyword_ "then" *> scalarExpr'))

== miscellaneous keyword operators

These are keyword operators which don't look like normal prefix,
postfix or infix binary operators. They mostly look like function
application but with keywords in the argument list instead of commas
to separate the arguments.

cast: cast(expr as type)

> cast :: P ScalarExpr
> cast = parensCast <|> prefixCast
>   where
>     parensCast = try (keyword_ "cast") >>
>                  parens (Cast <$> scalarExpr'
>                          <*> (keyword_ "as" *> typeName))
>     prefixCast = try (TypedLit <$> typeName
>                              <*> stringLiteral)

extract(id from expr)

> extract :: P ScalarExpr
> extract = try (keyword_ "extract") >>
>     parens (makeOp <$> name
>                    <*> (keyword_ "from" *> scalarExpr'))
>   where makeOp n e = SpecialOp (Name "extract") [Iden n, e]

substring(x from expr to expr)

todo: also support substring(x from expr)

> substring :: P ScalarExpr
> substring = try (keyword_ "substring") >>
>     parens (makeOp <$> scalarExpr'
>                    <*> (keyword_ "from" *> scalarExpr')
>                    <*> (keyword_ "for" *> scalarExpr')
>                    )
>   where makeOp a b c = SpecialOp (Name "substring") [a,b,c]

in: two variations:
a in (expr0, expr1, ...)
a in (queryexpr)

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

between:
expr between expr and expr

There is a complication when parsing between - when parsing the second
expression it is ambiguous when you hit an 'and' whether it is a
binary operator or part of the between. This code follows what
postgres does, which might be standard across SQL implementations,
which is that you can't have a binary and operator in the middle
expression in a between unless it is wrapped in parens. The 'bExpr
parsing' is used to create alternative scalar expression parser which
is identical to the normal one expect it doesn't recognise the binary
and operator. This is the call to scalarExpr'' True.

> betweenSuffix :: ScalarExpr -> P ScalarExpr
> betweenSuffix e =
>     makeOp <$> (Name <$> opName)
>            <*> return e
>            <*> scalarExpr'' True
>            <*> (keyword_ "and" *> scalarExpr'' True)
>   where
>     opName = try $ choice
>              ["between" <$ keyword_ "between"
>              ,"not between" <$ keyword_ "not" <* keyword_ "between"]
>     makeOp n a b c = SpecialOp n [a,b,c]

subquery expression:
[exists|all|any|some] (queryexpr)

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

typename: used in casts. Special cases for the multi keyword typenames
that SQL supports.

> typeName :: P TypeName
> typeName = choice
>     [TypeName "double precision"
>      <$ try (keyword_ "double" <* keyword_ "precision")
>     ,TypeName "character varying"
>      <$ try (keyword_ "character" <* keyword_ "varying")
>     ,TypeName <$> identifierString]

== scalar parens and row ctor

> sparens :: P ScalarExpr
> sparens =
>     ctor <$> parens (commaSep1 scalarExpr')
>   where
>     ctor [a] = Parens a
>     ctor as = SpecialOp (Name "rowctor") as


== operator parsing

The 'regular' operators in this parsing and in the abstract syntax are
unary prefix, unary postfix and binary infix operators. The operators
can be symbols (a + b), single keywords (a and b) or multiple keywords
(a is similar to b).

First, the list of the regulars operators split by operator type
(prefix, postfix, binary) and by symbol/single keyword/ multiple
keyword.

> binOpSymbolNames :: [String]
> binOpSymbolNames =
>     ["=", "<=", ">=", "!=", "<>", "<", ">"
>     ,"*", "/", "+", "-", "%"
>     ,"||", "."
>     ,"^", "|", "&"
>     ]

> binOpKeywordNames :: [String]
> binOpKeywordNames = ["and", "or", "like", "overlaps"]

> binOpMultiKeywordNames :: [[String]]
> binOpMultiKeywordNames = map words
>     ["not like"
>     ,"is similar to"
>     ,"is not similar to"
>     ,"is distinct from"
>     ,"is not distinct from"]

used for between parsing

> binOpKeywordNamesNoAnd :: [String]
> binOpKeywordNamesNoAnd = filter (/="and") binOpKeywordNames

There aren't any multi keyword prefix operators currently supported.

> prefixUnOpKeywordNames :: [String]
> prefixUnOpKeywordNames = ["not"]

> prefixUnOpSymbolNames :: [String]
> prefixUnOpSymbolNames = ["+", "-", "~"]

There aren't any single keyword postfix operators currently
supported. Maybe all these 'is's can be left factored?

> postfixOpKeywords :: [String]
> postfixOpKeywords = ["is null"
>                     ,"is not null"
>                     ,"is true"
>                     ,"is not true"
>                     ,"is false"
>                     ,"is not false"
>                     ,"is unknown"
>                     ,"is not unknown"]

The parsers:

> prefixUnaryOp :: P ScalarExpr
> prefixUnaryOp =
>     PrefixOp <$> (Name <$> opSymbol) <*> scalarExpr'
>   where
>     opSymbol = choice (map (try . symbol) prefixUnOpSymbolNames
>                        ++ map (try . keyword) prefixUnOpKeywordNames)

TODO: the handling of multikeyword args is different in
postfixopsuffix and binaryoperatorsuffix. It should be the same in
both cases

> postfixOpSuffix :: ScalarExpr -> P ScalarExpr
> postfixOpSuffix e =
>     try $ choice $ map makeOp opPairs
>   where
>     opPairs = flip map postfixOpKeywords $ \o -> (o, words o)
>     makeOp (o,ws) = try $ PostfixOp (Name o) e <$ keywords_ ws
>     keywords_ = try . mapM_ keyword_

All the binary operators are parsed as same precedence and left
associativity. This is fixed with a separate pass over the AST.

> binaryOperatorSuffix :: Bool -> ScalarExpr -> P ScalarExpr
> binaryOperatorSuffix bExpr e0 =
>     BinOp e0 <$> (Name <$> opSymbol) <*> factor
>   where
>     opSymbol = choice
>         (map (try . symbol) binOpSymbolNames
>         ++ map (try . keywords) binOpMultiKeywordNames
>         ++ map (try . keyword)
>                (if bExpr
>                 then binOpKeywordNamesNoAnd
>                 else binOpKeywordNames))
>     keywords ks = unwords <$> mapM keyword ks

> sqlFixities :: [[Fixity]]
> sqlFixities = highPrec ++ defaultPrec ++ lowPrec
>   where
>     allOps = binOpSymbolNames ++ binOpKeywordNames
>              ++ map unwords binOpMultiKeywordNames
>              ++ prefixUnOpKeywordNames ++ prefixUnOpSymbolNames
>              ++ postfixOpKeywords
>     -- these are the ops with the highest precedence in order
>     highPrec = [infixl_ ["."]
>                ,infixl_ ["*","/", "%"]
>                ,infixl_ ["+", "-"]
>                ,infixl_ ["<=",">=","!=","<>","||","like"]
>                ]
>     -- these are the ops with the lowest precedence in order
>     lowPrec = [infix_ ["<",">"]
>               ,infixr_ ["="]
>               ,infixr_ ["not"]
>               ,infixl_ ["and"]
>               ,infixl_ ["or"]]
>     already = concatMap (map fName) highPrec
>               ++ concatMap (map fName)  lowPrec
>     -- all the other ops have equal precedence and go between the
>     -- high and low precedence ops
>     defaultPrecOps = filter (`notElem` already) allOps
>     -- almost correct, have to do some more work to
>     -- get the associativity correct for these operators
>     defaultPrec = [infixl_ defaultPrecOps]
>     fName (Fixity n _) = n


== scalar expressions

TODO:
left factor stuff which starts with identifier

This parses most of the scalar exprs. I'm not sure if factor is the
correct terminology here. The order of the parsers and use of try is
carefully done to make everything work. It is a little fragile and
could at least do with some heavy explanation.

> factor :: P ScalarExpr
> factor = choice [literal
>                 ,scase
>                 ,cast
>                 ,extract
>                 ,substring
>                 ,subquery
>                 ,prefixUnaryOp
>                 ,try app
>                 ,try star
>                 ,identifier
>                 ,sparens]

putting the factor together with the extra bits

> scalarExpr'' :: Bool -> P ScalarExpr
> scalarExpr'' bExpr = factor >>= trysuffix
>   where
>     trysuffix e = try (suffix e) <|> return e
>     suffix e0 = choice
>                 [binaryOperatorSuffix bExpr e0
>                 ,inSuffix e0
>                 ,betweenSuffix e0
>                 ,postfixOpSuffix e0
>                 ] >>= trysuffix

Wrapper for non 'bExpr' parsing. See the between parser for
explanation.

> scalarExpr' :: P ScalarExpr
> scalarExpr' = scalarExpr'' False

The scalarExpr wrapper. The idea is that directly nested scalar
expressions use the scalarExpr' parser, then other code uses the
scalarExpr parser and then everyone gets the fixity fixes and it's
easy to ensure that this fix is only applied once to each scalar
expression tree (for efficiency and code clarity).

> scalarExpr :: P ScalarExpr
> scalarExpr = fixFixities sqlFixities <$> scalarExpr'

expose the b expression for window frame clause range between

> scalarExprB :: P ScalarExpr
> scalarExprB = fixFixities sqlFixities <$> scalarExpr'' True


-------------------------------------------------

= query expressions

== select lists

> selectItem :: P (Maybe Name, ScalarExpr)
> selectItem = flip (,) <$> scalarExpr <*> optionMaybe (try als)
>   where als = optional (try (keyword_ "as")) *> name

> selectList :: P [(Maybe Name,ScalarExpr)]
> selectList = commaSep1 selectItem

== from

Here is the rough grammar for joins

tref
(cross | [natural] ([inner] | (left | right | full) [outer])) join
tref
[on expr | using (...)]

> from :: P [TableRef]
> from = try (keyword_ "from") *> commaSep1 tref
>   where
>     tref = nonJoinTref >>= optionSuffix joinTrefSuffix
>     nonJoinTref = choice [try (TRQueryExpr <$> parens queryExpr)
>                          ,TRParens <$> parens tref
>                          ,TRLateral <$> (try (keyword_ "lateral")
>                                          *> nonJoinTref)
>                          ,try (TRFunction <$> name
>                                           <*> parens (commaSep scalarExpr))
>                          ,TRSimple <$> name]
>                   >>= optionSuffix aliasSuffix
>     aliasSuffix j = option j (TRAlias j <$> alias)
>     joinTrefSuffix t = (do
>          nat <- option False $ try (True <$ try (keyword_ "natural"))
>          TRJoin t <$> joinType
>                   <*> nonJoinTref
>                   <*> optionMaybe (joinCondition nat))
>         >>= optionSuffix joinTrefSuffix
>     joinType =
>         choice [choice
>                 [JCross <$ try (keyword_ "cross")
>                 ,JInner <$ try (keyword_ "inner")
>                 ,choice [JLeft <$ try (keyword_ "left")
>                         ,JRight <$ try (keyword_ "right")
>                         ,JFull <$ try (keyword_ "full")]
>                  <* optional (try $ keyword_ "outer")]
>                 <* keyword "join"
>                ,JInner <$ keyword_ "join"]
>     joinCondition nat =
>         choice [guard nat >> return JoinNatural
>                ,try (keyword_ "on") >>
>                 JoinOn <$> scalarExpr
>                ,try (keyword_ "using") >>
>                 JoinUsing <$> parens (commaSep1 name)
>                ]

> alias :: P Alias
> alias = Alias <$> try tableAlias <*> try columnAliases
>   where
>     tableAlias = optional (try $ keyword_ "as") *> name
>     columnAliases = optionMaybe $ try $ parens $ commaSep1 name

== simple other parts

Parsers for where, group by, having, order by and limit, which are
pretty trivial.

Here is a helper for parsing a few parts of the query expr (currently
where, having, limit, offset).

> keywordScalarExpr :: String -> P ScalarExpr
> keywordScalarExpr k = try (keyword_ k) *> scalarExpr

> swhere :: P ScalarExpr
> swhere = keywordScalarExpr "where"

> sgroupBy :: P [GroupingExpr]
> sgroupBy = try (keyword_ "group")
>            *> keyword_ "by"
>            *> commaSep1 (SimpleGroup <$> scalarExpr)

> having :: P ScalarExpr
> having = keywordScalarExpr "having"

> orderBy :: P [OrderField]
> orderBy = try (keyword_ "order") *> keyword_ "by" *> commaSep1 ob
>   where
>     ob = OrderField
>          <$> scalarExpr
>          <*> option Asc (choice [Asc <$ keyword_ "asc"
>                                 ,Desc <$ keyword_ "desc"])
>          <*> option NullsOrderDefault
>              (try (keyword_ "nulls" >>
>                     choice [NullsFirst <$ keyword "first"
>                            ,NullsLast <$ keyword "last"]))

allows offset and fetch in either order
+ postgresql offset without row(s) and limit instead of fetch also

> offsetFetch :: P (Maybe ScalarExpr, Maybe ScalarExpr)
> offsetFetch = permute ((,) <$?> (Nothing, Just <$> offset)
>                            <|?> (Nothing, Just <$> fetch))

> offset :: P ScalarExpr
> offset = try (keyword_ "offset") *> scalarExpr
>          <* option () (try $ choice [try (keyword_ "rows"),keyword_ "row"])

> fetch :: P ScalarExpr
> fetch = choice [ansiFetch, limit]
>   where
>     ansiFetch = try (keyword_ "fetch") >>
>         choice [keyword_ "first",keyword_ "next"]
>         *> scalarExpr
>         <* choice [keyword_ "rows",keyword_ "row"]
>         <* keyword_ "only"
>     limit = try (keyword_ "limit") *> scalarExpr

== common table expressions

> with :: P QueryExpr
> with = try (keyword_ "with") >>
>     With <$> option False (try (True <$ keyword_ "recursive"))
>          <*> commaSep1 withQuery <*> queryExpr
>   where
>     withQuery =
>         (,) <$> (alias <* optional (try $ keyword_ "as"))
>             <*> parens queryExpr

== query expression

This parser parses any query expression variant: normal select, cte,
and union, etc..

> queryExpr :: P QueryExpr
> queryExpr =
>   choice [with
>          ,choice [values,table, select]
>           >>= optionSuffix queryExprSuffix]
>   where
>     select = try (keyword_ "select") >>
>         mkSelect
>         <$> (fromMaybe All <$> duplicates)
>         <*> selectList
>         <*> option [] from
>         <*> optionMaybe swhere
>         <*> option [] sgroupBy
>         <*> optionMaybe having
>         <*> option [] orderBy
>         <*> offsetFetch
>     mkSelect d sl f w g h od (ofs,fe) =
>         Select d sl f w g h od ofs fe
>     values = try (keyword_ "values")
>              >> Values <$> commaSep (parens (commaSep scalarExpr))
>     table = try (keyword_ "table") >> Table <$> name

> queryExprSuffix :: QueryExpr -> P QueryExpr
> queryExprSuffix qe =
>     (CombineQueryExpr qe
>      <$> try (choice
>               [Union <$ keyword_ "union"
>               ,Intersect <$ keyword_ "intersect"
>               ,Except <$ keyword_ "except"])
>      <*> (fromMaybe All <$> duplicates)
>      <*> option Respectively
>                 (try (Corresponding <$ keyword_ "corresponding"))
>      <*> queryExpr)
>     >>= optionSuffix queryExprSuffix

wrapper for query expr which ignores optional trailing semicolon.

> topLevelQueryExpr :: P QueryExpr
> topLevelQueryExpr =
>      queryExpr >>= optionSuffix ((symbol ";" *>) . return)

wrapper to parse a series of query exprs from a single source. They
must be separated by semicolon, but for the last expression, the
trailing semicolon is optional.

> queryExprs :: P [QueryExpr]
> queryExprs =
>     (:[]) <$> queryExpr
>     >>= optionSuffix ((symbol ";" *>) . return)
>     >>= optionSuffix (\p -> (p++) <$> queryExprs)

------------------------------------------------

= lexing parsers

The lexing is a bit 'virtual', in the usual parsec style. The
convention in this file is to put all the parsers which access
characters directly or indirectly here (i.e. ones which use char,
string, digit, etc.), except for the parsers which only indirectly
access them via these functions, if you follow?

> symbol :: String -> P String
> symbol s = string s
>            -- <* notFollowedBy (oneOf "+-/*<>=!|")
>            <* whiteSpace

> symbol_ :: String -> P ()
> symbol_ s = symbol s *> return ()

TODO: now that keyword has try in it, a lot of the trys above can be
removed

> keyword :: String -> P String
> keyword s = try $ do
>     i <- identifierRaw
>     guard (map toLower i == map toLower s)
>     return i

> keyword_ :: String -> P ()
> keyword_ s = keyword s *> return ()

Identifiers are very simple at the moment: start with a letter or
underscore, and continue with letter, underscore or digit. It doesn't
support quoting other other sorts of identifiers yet. There is a
blacklist of keywords which aren't supported as identifiers.

the identifier raw doesn't check the blacklist since it is used by the
keyword parser also

> identifierRaw :: P String
> identifierRaw = (:) <$> letterOrUnderscore
>                     <*> many letterDigitOrUnderscore <* whiteSpace
>   where
>     letterOrUnderscore = char '_' <|> letter
>     letterDigitOrUnderscore = char '_' <|> alphaNum

> identifierString :: P String
> identifierString = do
>     s <- identifierRaw
>     guard (map toLower s `notElem` blacklist)
>     return s

> blacklist :: [String]
> blacklist =
>     ["select", "as", "from", "where", "having", "group", "order"
>     ,"limit", "offset", "fetch"
>     ,"inner", "left", "right", "full", "natural", "join"
>     ,"cross", "on", "using", "lateral"
>     ,"when", "then", "case", "end", "in"
>     ,"except", "intersect", "union"]

These blacklisted names are mostly needed when we parse something with
an optional alias, e.g. select a a from t. If we write select a from
t, we have to make sure the from isn't parsed as an alias. I'm not
sure what other places strictly need the blacklist, and in theory it
could be tuned differently for each place the identifierString/
identifier parsers are used to only blacklist the bare minimum.

> quotedIdentifier :: P String
> quotedIdentifier = char '"' *> manyTill anyChar (symbol_ "\"")


String literals: limited at the moment, no escaping \' or other
variations.

> stringLiteral :: P String
> stringLiteral = (char '\'' *> manyTill anyChar (char '\'')
>                  >>= optionSuffix moreString) <* whiteSpace
>   where
>     moreString s0 = try $ do
>         void $ char '\''
>         s <- manyTill anyChar (char '\'')
>         optionSuffix moreString (s0 ++ "'" ++ s)

number literals

here is the rough grammar target:

digits
digits.[digits][e[+-]digits]
[digits].digits[e[+-]digits]
digitse[+-]digits

numbers are parsed to strings, not to a numeric type. This is to avoid
making a decision on how to represent numbers, the client code can
make this choice.

> numberLiteral :: P String
> numberLiteral =
>     choice [int
>             >>= optionSuffix dot
>             >>= optionSuffix fracts
>             >>= optionSuffix expon
>            ,fract "" >>= optionSuffix expon]
>     <* whiteSpace
>   where
>     int = many1 digit
>     fract p = dot p >>= fracts
>     dot p = (p++) <$> string "."
>     fracts p = (p++) <$> int
>     expon p = concat <$> sequence
>               [return p
>               ,string "e"
>               ,option "" (string "+" <|> string "-")
>               ,int]

lexer for integer literals which appear in some places in SQL

> integerLiteral :: P Int
> integerLiteral = read <$> many1 digit <* whiteSpace

whitespace parser which skips comments also

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

= generic parser helpers

a possible issue with the option suffix is that it enforces left
associativity when chaining it recursively. Have to review
all these uses and figure out if any should be right associative
instead, and create an alternative suffix parser

> optionSuffix :: (a -> P a) -> a -> P a
> optionSuffix p a = option a (p a)

> parens :: P a -> P a
> parens = between (symbol_ "(") (symbol_ ")")

> commaSep :: P a -> P [a]
> commaSep = (`sepBy` symbol_ ",")


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
