
> {-# LANGUAGE TupleSections #-}
> -- | This is the module with the parser functions.
> module Language.SQL.SimpleSQL.Parser
>     (parseQueryExpr
>     ,parseValueExpr
>     ,parseQueryExprs
>     ,ParseError(..)) where

> import Control.Monad.Identity (Identity)
> import Control.Monad (guard, void)
> import Control.Applicative ((<$), (<$>), (<*>) ,(<*), (*>))
> import Data.Maybe (fromMaybe,catMaybes)
> import Data.Char (toLower)
> import Text.Parsec (errorPos,sourceLine,sourceColumn,sourceName
>                    ,setPosition,setSourceColumn,setSourceLine,getPosition
>                    ,option,between,sepBy,sepBy1,string,manyTill,anyChar
>                    ,try,string,many1,oneOf,digit,(<|>),choice,char,eof
>                    ,optionMaybe,optional,many,letter,parse
>                    ,chainl1, (<?>),notFollowedBy,alphaNum)
> import Text.Parsec.String (Parser)
> import qualified Text.Parsec as P (ParseError)
> import Text.Parsec.Perm (permute,(<$?>), (<|?>))
> import qualified Text.Parsec.Expr as E

> import Language.SQL.SimpleSQL.Syntax

The public API functions.

> -- | Parses a query expr, trailing semicolon optional.
> parseQueryExpr :: FilePath
>                   -- ^ filename to use in errors
>                -> Maybe (Int,Int)
>                   -- ^ line number and column number of the first character
>                   -- in the source (to use in errors)
>                -> String
>                   -- ^ the SQL source to parse
>                -> Either ParseError QueryExpr
> parseQueryExpr = wrapParse topLevelQueryExpr

> -- | Parses a list of query expressions, with semi colons between
> -- them. The final semicolon is optional.
> parseQueryExprs :: FilePath
>                    -- ^ filename to use in errors
>                 -> Maybe (Int,Int)
>                    -- ^ line number and column number of the first character
>                    -- in the source (to use in errors)
>                 -> String
>                    -- ^ the SQL source to parse
>                 -> Either ParseError [QueryExpr]
> parseQueryExprs = wrapParse queryExprs

> -- | Parses a value expression.
> parseValueExpr :: FilePath
>                    -- ^ filename to use in errors
>                 -> Maybe (Int,Int)
>                    -- ^ line number and column number of the first character
>                    -- in the source (to use in errors)
>                 -> String
>                    -- ^ the SQL source to parse
>                 -> Either ParseError ValueExpr
> parseValueExpr = wrapParse valueExpr

This helper function takes the parser given and:

sets the position when parsing
automatically skips leading whitespace
checks the parser parses all the input using eof
converts the error return to the nice wrapper

> wrapParse :: Parser a
>           -> FilePath
>           -> Maybe (Int,Int)
>           -> String
>           -> Either ParseError a
> wrapParse parser f p src =
>     either (Left . convParseError src) Right
>     $ parse (setPos p *> whitespace *> parser <* eof) f src

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

= value expressions

== literals

See the stringToken lexer below for notes on string literal syntax.

> stringValue :: Parser ValueExpr
> stringValue = StringLit <$> stringToken

> number :: Parser ValueExpr
> number = NumLit <$> numberLiteral

parse SQL interval literals, something like
interval '5' day (3)
or
interval '5' month

wrap the whole lot in try, in case we get something like this:
interval '3 days'
which parses as a typed literal

> interval :: Parser ValueExpr
> interval = keyword_ "interval" >>
>     mkIt <$> stringToken
>     <*> optionMaybe
>         ((,) <$> identifierBlacklist blacklist
>              <*> optionMaybe (parens integer))
>   where
>     mkIt val Nothing = TypedLit (TypeName "interval") val
>     mkIt val (Just (a,b)) = IntervalLit val a b

> literal :: Parser ValueExpr
> literal = number <|> stringValue <|> interval

== identifiers

Uses the identifierString 'lexer'. See this function for notes on
identifiers.

> name :: Parser Name
> name = choice [QName <$> quotedIdentifier
>               ,Name <$> identifierBlacklist blacklist]

> iden :: Parser ValueExpr
> iden = Iden <$> name

== star

used in select *, select x.*, and agg(*) variations, and some other
places as well. Because it is quite general, the parser doesn't
attempt to check that the star is in a valid context, it parses it OK
in any value expression context.

> star :: Parser ValueExpr
> star = Star <$ symbol "*"

== parameter

use in e.g. select * from t where a = ?

> parameter :: Parser ValueExpr
> parameter = Parameter <$ questionMark

named parameter:

select x from t where x > :param

> hostParameter :: Parser ValueExpr
> hostParameter =
>     HostParameter
>     <$> hostParameterToken
>     <*> optionMaybe (keyword "indicator" *> hostParameterToken)

== function application, aggregates and windows

this represents anything which syntactically looks like regular C
function application: an identifier, parens with comma sep value
expression arguments.

The parsing for the aggregate extensions is here as well:

aggregate([all|distinct] args [order by orderitems])

> aggOrApp :: Parser ValueExpr
> aggOrApp =
>     makeApp
>     <$> name
>     <*> parens ((,,) <$> duplicates
>                      <*> choice [commaSep valueExpr]
>                      <*> (optionMaybe orderBy))
>   where
>     makeApp i (Nothing,es,Nothing) = App i es
>     makeApp i (d,es,od) = AggregateApp i d es (fromMaybe [] od)

> duplicates :: Parser (Maybe SetQuantifier)
> duplicates = optionMaybe $
>     choice [All <$ keyword_ "all"
>            ,Distinct <$ keyword "distinct"]

parse a window call as a suffix of a regular function call
this looks like this:
functionname(args) over ([partition by ids] [order by orderitems])

No support for explicit frames yet.

The convention in this file is that the 'Suffix', erm, suffix on
parser names means that they have been left factored. These are almost
always used with the optionSuffix combinator.

> windowSuffix :: ValueExpr -> Parser ValueExpr
> windowSuffix (App f es) =
>     keyword_ "over"
>     *> parens (WindowApp f es
>                <$> option [] partitionBy
>                <*> option [] orderBy
>                <*> optionMaybe frameClause)
>   where
>     partitionBy = keyword_ "partition" >>
>         keyword_ "by" >> commaSep1 valueExpr
>     frameClause =
>         mkFrame <$> choice [FrameRows <$ keyword_ "rows"
>                            ,FrameRange <$ keyword_ "range"]
>                 <*> frameStartEnd
>     frameStartEnd =
>         choice
>         [keyword_ "between" >>
>          mkFrameBetween <$> frameLimit True
>                         <*> (keyword_ "and" *> frameLimit True)
>         ,mkFrameFrom <$> frameLimit False]
>     -- use the bexpression style from the between parsing for frame between
>     frameLimit useB =
>         choice
>         [Current <$ keyword_ "current" <* keyword_ "row"
>         ,keyword_ "unbounded" >>
>          choice [UnboundedPreceding <$ keyword_ "preceding"
>                 ,UnboundedFollowing <$ keyword_ "following"]
>         ,do
>          e <- if useB then valueExprB else valueExpr
>          choice [Preceding e <$ keyword_ "preceding"
>                 ,Following e <$ keyword_ "following"]
>         ]
>     mkFrameBetween s e rs = FrameBetween rs s e
>     mkFrameFrom s rs = FrameFrom rs s
>     mkFrame rs c = c rs
> windowSuffix _ = fail ""

> app :: Parser ValueExpr
> app = aggOrApp >>= optionSuffix windowSuffix

== case expression

> caseValue :: Parser ValueExpr
> caseValue =
>     Case <$> (keyword_ "case" *> optionMaybe valueExpr)
>          <*> many1 whenClause
>          <*> optionMaybe elseClause
>          <* keyword_ "end"
>   where
>    whenClause = (,) <$> (keyword_ "when" *> commaSep1 valueExpr)
>                     <*> (keyword_ "then" *> valueExpr)
>    elseClause = keyword_ "else" *> valueExpr

== miscellaneous keyword operators

These are keyword operators which don't look like normal prefix,
postfix or infix binary operators. They mostly look like function
application but with keywords in the argument list instead of commas
to separate the arguments.

cast: cast(expr as type)

> cast :: Parser ValueExpr
> cast = (parensCast <|> prefixCast)
>   where
>     parensCast = keyword_ "cast" >>
>                  parens (Cast <$> valueExpr
>                          <*> (keyword_ "as" *> typeName))
>     prefixCast = try (TypedLit <$> typeName
>                                <*> stringToken)

the special op keywords
parse an operator which is
operatorname(firstArg keyword0 arg0 keyword1 arg1 etc.)

> data SpecialOpKFirstArg = SOKNone
>                         | SOKOptional
>                         | SOKMandatory

> specialOpK :: String -- name of the operator
>            -> SpecialOpKFirstArg -- has a first arg without a keyword
>            -> [(String,Bool)] -- the other args with their keywords
>                               -- and whether they are optional
>            -> Parser ValueExpr
> specialOpK opName firstArg kws =
>     keyword_ opName >> do
>     void openParen
>     let pfa = do
>               e <- valueExpr
>               -- check we haven't parsed the first
>               -- keyword as an identifier
>               guard (case (e,kws) of
>                   (Iden (Name i), (k,_):_) | map toLower i == k -> False
>                   _ -> True)
>               return e
>     fa <- case firstArg of
>          SOKNone -> return Nothing
>          SOKOptional -> optionMaybe (try pfa)
>          SOKMandatory -> Just <$> pfa
>     as <- mapM parseArg kws
>     void closeParen
>     return $ SpecialOpK (Name opName) fa $ catMaybes as
>   where
>     parseArg (nm,mand) =
>         let p = keyword_ nm >> valueExpr
>         in fmap (nm,) <$> if mand
>                           then Just <$> p
>                           else optionMaybe (try p)

The actual operators:

EXTRACT( date_part FROM expression )

POSITION( string1 IN string2 )

SUBSTRING(extraction_string FROM starting_position [FOR length]
[COLLATE collation_name])

CONVERT(char_value USING conversion_char_name)

TRANSLATE(char_value USING translation_name)

OVERLAY(string PLACING embedded_string FROM start
[FOR length])

TRIM( [ [{LEADING | TRAILING | BOTH}] [removal_char] FROM ]
target_string
[COLLATE collation_name] )

> specialOpKs :: Parser ValueExpr
> specialOpKs = choice $ map try
>     [extract, position, substring, convert, translate, overlay, trim]

> extract :: Parser ValueExpr
> extract = specialOpK "extract" SOKMandatory [("from", True)]

> position :: Parser ValueExpr
> position = specialOpK "position" SOKMandatory [("in", True)]

strictly speaking, the substring must have at least one of from and
for, but the parser doens't enforce this

> substring :: Parser ValueExpr
> substring = specialOpK "substring" SOKMandatory
>                 [("from", False),("for", False),("collate", False)]

> convert :: Parser ValueExpr
> convert = specialOpK "convert" SOKMandatory [("using", True)]


> translate :: Parser ValueExpr
> translate = specialOpK "translate" SOKMandatory [("using", True)]

> overlay :: Parser ValueExpr
> overlay = specialOpK "overlay" SOKMandatory
>                 [("placing", True),("from", True),("for", False)]

trim is too different because of the optional char, so a custom parser
the both ' ' is filled in as the default if either parts are missing
in the source

> trim :: Parser ValueExpr
> trim =
>     keyword "trim" >>
>     parens (mkTrim
>             <$> option "both" sides
>             <*> option " " stringToken
>             <*> (keyword_ "from" *> valueExpr)
>             <*> optionMaybe (keyword_ "collate" *> stringToken))
>   where
>     sides = choice ["leading" <$ keyword_ "leading"
>                    ,"trailing" <$ keyword_ "trailing"
>                    ,"both" <$ keyword_ "both"]
>     mkTrim fa ch fr cl =
>       SpecialOpK (Name "trim") Nothing
>           $ catMaybes [Just (fa,StringLit ch)
>                       ,Just ("from", fr)
>                       ,fmap (("collate",) . StringLit) cl]

in: two variations:
a in (expr0, expr1, ...)
a in (queryexpr)

this is parsed as a postfix operator which is why it is in this form

> inSuffix :: Parser (ValueExpr -> ValueExpr)
> inSuffix =
>     mkIn <$> inty
>          <*> parens (choice
>                      [InQueryExpr <$> queryExpr
>                      ,InList <$> commaSep1 valueExpr])
>   where
>     inty = choice [True <$ keyword_ "in"
>                   ,False <$ keyword_ "not" <* keyword_ "in"]
>     mkIn i v = \e -> In i e v


between:
expr between expr and expr

There is a complication when parsing between - when parsing the second
expression it is ambiguous when you hit an 'and' whether it is a
binary operator or part of the between. This code follows what
postgres does, which might be standard across SQL implementations,
which is that you can't have a binary and operator in the middle
expression in a between unless it is wrapped in parens. The 'bExpr
parsing' is used to create alternative value expression parser which
is identical to the normal one expect it doesn't recognise the binary
and operator. This is the call to valueExprB.

> betweenSuffix :: Parser (ValueExpr -> ValueExpr)
> betweenSuffix =
>     makeOp <$> (Name <$> opName)
>            <*> valueExprB
>            <*> (keyword_ "and" *> valueExprB)
>   where
>     opName = choice
>              ["between" <$ keyword_ "between"
>              ,"not between" <$ try (keyword_ "not" <* keyword_ "between")]
>     makeOp n b c = \a -> SpecialOp n [a,b,c]

subquery expression:
[exists|all|any|some] (queryexpr)

> subquery :: Parser ValueExpr
> subquery = SubQueryExpr <$> sqkw <*> parens queryExpr
>   where
>     sqkw = choice
>            [SqExists <$ keyword_ "exists"
>            ,SqAll <$ keyword_ "all"
>            ,SqAny <$ keyword_ "any"
>            ,SqSome <$ keyword_ "some"]

typename: used in casts. Special cases for the multi keyword typenames
that SQL supports.

> typeName :: Parser TypeName
> typeName = choice (multiWordParsers
>                    ++ [TypeName <$> identifierBlacklist blacklist])
>            >>= optionSuffix precision
>   where
>     multiWordParsers =
>         flip map multiWordTypeNames
>         $ \ks -> (TypeName . unwords) <$> try (mapM keyword ks)
>     multiWordTypeNames = map words
>         ["double precision"
>         ,"character varying"
>         ,"char varying"
>         ,"character large object"
>         ,"char large object"
>         ,"national character"
>         ,"national char"
>         ,"national character varying"
>         ,"national char varying"
>         ,"national character large object"
>         ,"nchar large object"
>         ,"nchar varying"
>         ,"bit varying"
>         ]

todo: timestamp types:

    | TIME [ <left paren> <time precision> <right paren> ] [ WITH TIME ZONE ]
     | TIMESTAMParser [ <left paren> <timestamp precision> <right paren> ] [ WITH TIME ZONE ]


>     precision t = parens (commaSep integer) >>= makeWrap t
>     makeWrap (TypeName t) [a] = return $ PrecTypeName t a
>     makeWrap (TypeName t) [a,b] = return $ PrecScaleTypeName t a b
>     makeWrap _ _ = fail "there must be one or two precision components"


== value expression parens, row ctor and scalar subquery

> parensTerm :: Parser ValueExpr
> parensTerm = parens $ choice
>     [SubQueryExpr SqSq <$> queryExpr
>     ,ctor <$> commaSep1 valueExpr]
>   where
>     ctor [a] = Parens a
>     ctor as = SpecialOp (Name "rowctor") as


== operator parsing

The 'regular' operators in this parsing and in the abstract syntax are
unary prefix, unary postfix and binary infix operators. The operators
can be symbols (a + b), single keywords (a and b) or multiple keywords
(a is similar to b).

TODO: carefully review the precedences and associativities.

> opTable :: Bool -> [[E.Operator String () Identity ValueExpr]]
> opTable bExpr =
>         [[binarySym "." E.AssocLeft]
>         ,[prefixSym "+", prefixSym "-"]
>         ,[binarySym "^" E.AssocLeft]
>         ,[binarySym "*" E.AssocLeft
>          ,binarySym "/" E.AssocLeft
>          ,binarySym "%" E.AssocLeft]
>         ,[binarySym "+" E.AssocLeft
>          ,binarySym "-" E.AssocLeft]
>         ,[binarySym ">=" E.AssocNone
>          ,binarySym "<=" E.AssocNone
>          ,binarySym "!=" E.AssocRight
>          ,binarySym "<>" E.AssocRight
>          ,binarySym "||" E.AssocRight
>          ,prefixSym "~"
>          ,binarySym "&" E.AssocRight
>          ,binarySym "|" E.AssocRight
>          ,binaryKeyword "like" E.AssocNone
>          ,binaryKeyword "overlaps" E.AssocNone]
>          ++ map (`binaryKeywords` E.AssocNone)
>          ["not like"
>          ,"is similar to"
>          ,"is not similar to"
>          ,"is distinct from"
>          ,"is not distinct from"]
>          ++ map postfixKeywords
>          ["is null"
>          ,"is not null"
>          ,"is true"
>          ,"is not true"
>          ,"is false"
>          ,"is not false"
>          ,"is unknown"
>          ,"is not unknown"]
>          -- have to use try with inSuffix because of a conflict
>          -- with 'in' in position function
>          -- between also has a try in it to deal with 'not'
>          -- ambiguity
>           ++ [E.Postfix $ try inSuffix,E.Postfix betweenSuffix]
>         ]
>         ++
>         [[binarySym "<" E.AssocNone
>          ,binarySym ">" E.AssocNone]
>         ,[binarySym "=" E.AssocRight]
>         ,[prefixKeyword "not"]]
>         ++
>         if bExpr then [] else [[binaryKeyword "and" E.AssocLeft]]
>         ++
>         [[binaryKeyword "or" E.AssocLeft]]
>   where
>     binarySym nm assoc = binary (symbol_ nm) nm assoc
>     binaryKeyword nm assoc = binary (keyword_ nm) nm assoc
>     -- use try with the multi keywords because of shared
>     -- prefixes to the sets of keywords. Should left factor
>     -- somehow
>     binaryKeywords nm assoc = binary (try $ mapM_ keyword_ (words nm)) nm assoc
>     binary p nm assoc =
>       E.Infix (p >> return (\a b -> BinOp a (Name nm) b)) assoc
>     prefixKeyword nm = prefix (keyword_ nm) nm
>     prefixSym nm = prefix (symbol_ nm) nm
>     prefix p nm = prefix' (p >> return (PrefixOp (Name nm)))
>     postfixKeywords nm = postfix (try $ mapM_ keyword_ (words nm)) nm
>     postfix p nm = postfix' (p >> return (PostfixOp (Name nm)))

>     -- hack from here
>     -- http://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported
>     -- not implemented properly yet
>     -- I don't think this will be enough for all cases
>     -- at least it works for 'not not a'
>     -- ok: "x is not true is not true"
>     -- no work: "x is not true is not null"
>     prefix'  p = E.Prefix  . chainl1 p $ return       (.)
>     postfix' p = E.Postfix . chainl1 p $ return (flip (.))

== value expressions

TODO:
left factor stuff which starts with identifier

This parses most of the value exprs.The order of the parsers and use
of try is carefully done to make everything work. It is a little
fragile and could at least do with some heavy explanation.

> valueExpr :: Parser ValueExpr
> valueExpr = E.buildExpressionParser (opTable False) term

> term :: Parser ValueExpr
> term = choice [literal
>               ,parameter
>               ,hostParameter
>               ,caseValue
>               ,cast
>               ,try specialOpKs
>               ,parensTerm
>               ,subquery
>               ,try app
>               ,star
>               ,iden]
>        <?> "value expression"

expose the b expression for window frame clause range between

> valueExprB :: Parser ValueExpr
> valueExprB = E.buildExpressionParser (opTable True) term


-------------------------------------------------

= query expressions

== select lists

> selectItem :: Parser (ValueExpr,Maybe Name)
> selectItem = (,) <$> valueExpr <*> optionMaybe als
>   where als = optional (keyword_ "as") *> name

> selectList :: Parser [(ValueExpr,Maybe Name)]
> selectList = commaSep1 selectItem

== from

Here is the rough grammar for joins

tref
(cross | [natural] ([inner] | (left | right | full) [outer])) join
tref
[on expr | using (...)]

> from :: Parser [TableRef]
> from = keyword_ "from" *> commaSep1 tref
>   where
>     tref = nonJoinTref >>= optionSuffix joinTrefSuffix
>     nonJoinTref = choice
>         [parens $ choice
>              [TRQueryExpr <$> queryExpr
>              ,TRParens <$> tref]
>         ,TRLateral <$> (keyword_ "lateral"
>                         *> nonJoinTref)
>         ,do
>          n <- name
>          choice [TRFunction n
>                  <$> parens (commaSep valueExpr)
>                 ,do
>                  choice [TRQualified n <$> (symbol "." >> name)
>                         ,return $ TRSimple n]]]
>         >>= optionSuffix aliasSuffix
>     aliasSuffix j = option j (TRAlias j <$> alias)
>     joinTrefSuffix t = (do
>          nat <- option False (True <$ keyword_ "natural")
>          TRJoin t <$> joinType
>                   <*> nonJoinTref
>                   <*> optionMaybe (joinCondition nat))
>         >>= optionSuffix joinTrefSuffix

> joinType :: Parser JoinType
> joinType = choice
>     [JCross <$ keyword_ "cross" <* keyword_ "join"
>     ,JInner <$ keyword_ "inner" <* keyword_ "join"
>     ,JLeft <$ keyword_ "left"
>            <* optional (keyword_ "outer")
>            <* keyword_ "join"
>     ,JRight <$ keyword_ "right"
>             <* optional (keyword_ "outer")
>             <* keyword_ "join"
>     ,JFull <$ keyword_ "full"
>            <* optional (keyword_ "outer")
>            <* keyword_ "join"
>     ,JInner <$ keyword_ "join"]

> joinCondition :: Bool -> Parser JoinCondition
> joinCondition nat =
>     choice [guard nat >> return JoinNatural
>            ,keyword_ "on" >> JoinOn <$> valueExpr
>            ,keyword_ "using" >> JoinUsing <$> parens (commaSep1 name)
>            ]

> alias :: Parser Alias
> alias = Alias <$> tableAlias <*> columnAliases
>   where
>     tableAlias = optional (keyword_ "as") *> name
>     columnAliases = optionMaybe $ parens $ commaSep1 name

== simple other parts

Parsers for where, group by, having, order by and limit, which are
pretty trivial.

> whereClause :: Parser ValueExpr
> whereClause = keyword_ "where" *> valueExpr

> groupByClause :: Parser [GroupingExpr]
> groupByClause = keyword_ "group" *> keyword_ "by"
>            *> commaSep1 groupingExpression
>   where
>     groupingExpression =
>       choice
>       [keyword_ "cube" >>
>        Cube <$> parens (commaSep groupingExpression)
>       ,keyword_ "rollup" >>
>        Rollup <$> parens (commaSep groupingExpression)
>       ,GroupingParens <$> parens (commaSep groupingExpression)
>       ,keyword_ "grouping" >> keyword_ "sets" >>
>        GroupingSets <$> parens (commaSep groupingExpression)
>       ,SimpleGroup <$> valueExpr
>       ]

> having :: Parser ValueExpr
> having = keyword_ "having" *> valueExpr

> orderBy :: Parser [SortSpec]
> orderBy = keyword_ "order" *> keyword_ "by" *> commaSep1 ob
>   where
>     ob = SortSpec
>          <$> valueExpr
>          <*> option Asc (choice [Asc <$ keyword_ "asc"
>                                 ,Desc <$ keyword_ "desc"])
>          <*> option NullsOrderDefault
>              (keyword_ "nulls" >>
>                     choice [NullsFirst <$ keyword "first"
>                            ,NullsLast <$ keyword "last"])

allows offset and fetch in either order
+ postgresql offset without row(s) and limit instead of fetch also

> offsetFetch :: Parser (Maybe ValueExpr, Maybe ValueExpr)
> offsetFetch = permute ((,) <$?> (Nothing, Just <$> offset)
>                            <|?> (Nothing, Just <$> fetch))

> offset :: Parser ValueExpr
> offset = keyword_ "offset" *> valueExpr
>          <* option () (choice [keyword_ "rows"
>                               ,keyword_ "row"])

> fetch :: Parser ValueExpr
> fetch = choice [ansiFetch, limit]
>   where
>     ansiFetch = keyword_ "fetch" >>
>         choice [keyword_ "first",keyword_ "next"]
>         *> valueExpr
>         <* choice [keyword_ "rows",keyword_ "row"]
>         <* keyword_ "only"
>     limit = keyword_ "limit" *> valueExpr

== common table expressions

> with :: Parser QueryExpr
> with = keyword_ "with" >>
>     With <$> option False (True <$ keyword_ "recursive")
>          <*> commaSep1 withQuery <*> queryExpr
>   where
>     withQuery =
>         (,) <$> (alias <* keyword_ "as")
>             <*> parens queryExpr

== query expression

This parser parses any query expression variant: normal select, cte,
and union, etc..

> queryExpr :: Parser QueryExpr
> queryExpr =
>   choice [with
>          ,choice [values,table, select]
>           >>= optionSuffix queryExprSuffix]
>   where
>     select = keyword_ "select" >>
>         mkSelect
>         <$> (fromMaybe All <$> duplicates)
>         <*> selectList
>         <*> optionMaybe tableExpression
>     mkSelect d sl Nothing =
>         makeSelect{qeSetQuantifier = d, qeSelectList = sl}
>     mkSelect d sl (Just (TableExpression f w g h od ofs fe)) =
>         Select d sl f w g h od ofs fe
>     values = keyword_ "values"
>              >> Values <$> commaSep (parens (commaSep valueExpr))
>     table = keyword_ "table" >> Table <$> name

local data type to help with parsing the bit after the select list,
called 'table expression' in the ansi sql grammar. Maybe this should
be in the public syntax?

> data TableExpression
>     = TableExpression
>       {_teFrom :: [TableRef]
>       ,_teWhere :: Maybe ValueExpr
>       ,_teGroupBy :: [GroupingExpr]
>       ,_teHaving :: Maybe ValueExpr
>       ,_teOrderBy :: [SortSpec]
>       ,_teOffset :: Maybe ValueExpr
>       ,_teFetchFirst :: Maybe ValueExpr}

> tableExpression :: Parser TableExpression
> tableExpression =
>    mkTe <$> from
>         <*> optionMaybe whereClause
>         <*> option [] groupByClause
>         <*> optionMaybe having
>         <*> option [] orderBy
>         <*> offsetFetch
>  where
>     mkTe f w g h od (ofs,fe) =
>         TableExpression f w g h od ofs fe

> queryExprSuffix :: QueryExpr -> Parser QueryExpr
> queryExprSuffix qe =
>     (CombineQueryExpr qe
>      <$> (choice
>          [Union <$ keyword_ "union"
>          ,Intersect <$ keyword_ "intersect"
>          ,Except <$ keyword_ "except"] <?> "set operator")
>      <*> (fromMaybe Distinct <$> duplicates)
>      <*> option Respectively
>                 (Corresponding <$ keyword_ "corresponding")
>      <*> queryExpr)
>     >>= optionSuffix queryExprSuffix

wrapper for query expr which ignores optional trailing semicolon.

> topLevelQueryExpr :: Parser QueryExpr
> topLevelQueryExpr =
>      queryExpr >>= optionSuffix ((semi *>) . return)

wrapper to parse a series of query exprs from a single source. They
must be separated by semicolon, but for the last expression, the
trailing semicolon is optional.

> queryExprs :: Parser [QueryExpr]
> queryExprs =
>     (:[]) <$> queryExpr
>     >>= optionSuffix ((semi *>) . return)
>     >>= optionSuffix (\p -> (p++) <$> queryExprs)

------------------------------------------------

= lexing parsers

whitespace parser which skips comments also

> whitespace :: Parser ()
> whitespace =
>     choice [simpleWhitespace *> whitespace
>            ,lineComment *> whitespace
>            ,blockComment *> whitespace
>            ,return ()] <?> "whitespace"
>   where
>     lineComment = try (string "--")
>                   *> manyTill anyChar (void (char '\n') <|> eof)
>     blockComment = -- no nesting of block comments in SQL
>                    try (string "/*")
>                    -- try used here so it doesn't fail when we see a
>                    -- '*' which isn't followed by a '/'
>                    *> manyTill anyChar (try $ string "*/")
>     -- use many1 so we can more easily avoid non terminating loops
>     simpleWhitespace = void $ many1 (oneOf " \t\n")

> lexeme :: Parser a -> Parser a
> lexeme p = p <* whitespace

> integer :: Parser Integer
> integer = read <$> lexeme (many1 digit) <?> "integer"


number literals

here is the rough grammar target:

digits
digits.[digits][e[+-]digits]
[digits].digits[e[+-]digits]
digitse[+-]digits

numbers are parsed to strings, not to a numeric type. This is to avoid
making a decision on how to represent numbers, the client code can
make this choice.

> numberLiteral :: Parser String
> numberLiteral = lexeme (
>     (choice [int
>             >>= optionSuffix dot
>             >>= optionSuffix fracts
>             >>= optionSuffix expon
>            ,fract "" >>= optionSuffix expon])
>     <* notFollowedBy (alphaNum <|> char '.'))
>     <?> "number literal"
>   where
>     int = many1 digit
>     fract p = dot p >>= fracts
>     dot p = (p++) <$> string "."
>     fracts p = (p++) <$> int
>     expon p = concat <$> sequence
>               [return p
>               ,(:[]) <$> oneOf "eE"
>               ,option "" (string "+" <|> string "-")
>               ,int]


> identifier :: Parser String
> identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
>              <?> "identifier"
>   where
>     firstChar = letter <|> char '_'
>     nonFirstChar = digit <|> firstChar

> quotedIdentifier :: Parser String
> quotedIdentifier = char '"' *> manyTill anyChar doubleQuote
>                    <?> "identifier"

TODO: add "" inside quoted identifiers

parses an identifier with a : prefix. The : isn't included in the
return value

> hostParameterToken :: Parser String
> hostParameterToken = lexeme $ char ':' *> identifier

todo: work out the symbol parsing better

> symbol :: String -> Parser String
> symbol s = try (lexeme $ do
>     u <- choice (many1 (char '.') :
>                  map (try . string) [">=","<=","!=","<>","||"]
>                  ++ map (string . (:[])) "+-^*/%~&|<>=")
>     guard (s == u)
>     return s)
>     <?> s

> questionMark :: Parser Char
> questionMark = lexeme $ char '?'

> openParen :: Parser Char
> openParen = lexeme $ char '('

> closeParen :: Parser Char
> closeParen = lexeme $ char ')'

> comma :: Parser Char
> comma = lexeme $ char ','

> semi :: Parser Char
> semi = lexeme $ char ';'

> doubleQuote :: Parser Char
> doubleQuote = lexeme $ char '"'

> --stringToken :: Parser String
> --stringToken = lexeme (char '\'' *> manyTill anyChar (char '\''))
> -- todo: tidy this up, add the prefixes stuff, and add the multiple
> -- string stuff
> stringToken :: Parser String
> stringToken =
>     lexeme (char '\'' *> manyTill anyChar (char '\'')
>     >>= optionSuffix moreString)
>     <?> "string"
>   where
>     moreString s0 = choice
>         [-- handle two adjacent quotes
>          do
>          void $ char '\''
>          s <- manyTill anyChar (char '\'')
>          optionSuffix moreString (s0 ++ "'" ++ s)
>         ,-- handle string in separate parts
>          -- e.g. 'part 1' 'part 2'
>          do
>          try (whitespace <* char '\'')
>          s <- manyTill anyChar (char '\'')
>          optionSuffix moreString (s0 ++ s)
>         ]

= helper functions

> keyword :: String -> Parser String
> keyword k = try (do
>     i <- identifier
>     guard (map toLower i == k)
>     return k) <?> k

> parens :: Parser a -> Parser a
> parens = between openParen closeParen

> commaSep :: Parser a -> Parser [a]
> commaSep = (`sepBy` comma)

> keyword_ :: String -> Parser ()
> keyword_ = void . keyword

> symbol_ :: String -> Parser ()
> symbol_ = void . symbol

> commaSep1 :: Parser a -> Parser [a]
> commaSep1 = (`sepBy1` comma)

a possible issue with the option suffix is that it enforces left
associativity when chaining it recursively. Have to review
all these uses and figure out if any should be right associative
instead, and create an alternative suffix parser

> optionSuffix :: (a -> Parser a) -> a -> Parser a
> optionSuffix p a = option a (p a)

> identifierBlacklist :: [String] -> Parser String
> identifierBlacklist bl = try (do
>     i <- identifier
>     guard (map toLower i `notElem` bl)
>     return i)
>     <?> "identifier"

> blacklist :: [String]
> blacklist =
>     [-- case
>      "case", "when", "then", "else", "end"
>     ,--join
>      "natural","inner","outer","cross","left","right","full","join"
>     ,"on","using","lateral"
>     ,"from","where","group","having","order","limit", "offset", "fetch"
>     ,"as","in"
>     ,"except", "intersect", "union"
>     ]

These blacklisted names are mostly needed when we parse something with
an optional alias, e.g. select a a from t. If we write select a from
t, we have to make sure the from isn't parsed as an alias. I'm not
sure what other places strictly need the blacklist, and in theory it
could be tuned differently for each place the identifierString/
identifier parsers are used to only blacklist the bare minimum.

The standard has a weird mix of reserved keywords and unreserved
keywords (I'm not sure what exactly being an unreserved keyword
means).

--------------------------------------------

= helper functions

> setPos :: Maybe (Int,Int) -> Parser ()
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
