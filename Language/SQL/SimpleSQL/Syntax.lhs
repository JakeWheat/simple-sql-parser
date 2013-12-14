
> -- | The AST for SQL queries
> module Language.SQL.SimpleSQL.Syntax
>     (-- * Scalar expressions
>      ScalarExpr(..)
>     ,TypeName(..)
>     ,Duplicates(..)
>     ,Direction(..)
>     ,InThing(..)
>     ,SubQueryExprType(..)
>      -- * Query expressions
>     ,QueryExpr(..)
>     ,makeSelect
>     ,CombineOp(..)
>     ,Corresponding(..)
>      -- ** From
>     ,TableRef(..)
>     ,JoinType(..)
>     ,JoinCondition(..)
>     ) where

> -- | Represents a scalar expression
> data ScalarExpr = NumLit String
>                 | StringLit String
>                 | IntervalLit String -- text of interval
>                               String -- units of interval
>                               (Maybe Int) -- precision
>                 | Iden String
>                 | Iden2 String String
>                 | Star
>                 | Star2 String
>                 | App String [ScalarExpr]
>                 | AggregateApp String (Maybe Duplicates)
>                                [ScalarExpr]
>                                [(ScalarExpr,Direction)]
>                 | WindowApp String [ScalarExpr] [ScalarExpr] [(ScalarExpr,Direction)]
>                   -- the binop, prefixop and postfix op
>                   -- are used for symbol and keyword operators
>                   -- these are used even for the multiple keyword
>                   -- operators
>                 | BinOp ScalarExpr String ScalarExpr
>                 | PrefixOp String ScalarExpr
>                 | PostfixOp String ScalarExpr
>                   -- the special op is used for ternary, mixfix and other non orthodox operators
>                 | SpecialOp String [ScalarExpr]
>                 | Case (Maybe ScalarExpr) -- test value
>                        [(ScalarExpr,ScalarExpr)] -- when branches
>                        (Maybe ScalarExpr) -- else value
>                 | Parens ScalarExpr
>                 | Cast ScalarExpr TypeName
>                 | CastOp TypeName String
>                 | SubQueryExpr SubQueryExprType QueryExpr
>                 | In Bool -- true if in, false if not in
>                      ScalarExpr InThing
>                   deriving (Eq,Show)

> data TypeName = TypeName String deriving (Eq,Show)


> -- Represents 'expr in (scalar expression list)', and 'expr in
> -- (subquery)' syntax
> data InThing = InList [ScalarExpr]
>              | InQueryExpr QueryExpr
>              deriving (Eq,Show)

> -- | A subquery in a scalar expression
> data SubQueryExprType = SqExists | SqSq | SqAll | SqSome | SqAny
>                         deriving (Eq,Show)

> -- | Represents a query expression, which can be a select, a 'set
> -- operator' (union/except/intersect), a common table expression
> -- (with), a values expression (not yet supported) or the table
> -- syntax - 'table t', shorthand for 'select * from t' (not yet
> -- supported).
> data QueryExpr
>     = Select
>       {qeDuplicates :: Duplicates
>       ,qeSelectList :: [(Maybe String,ScalarExpr)]
>       ,qeFrom :: [TableRef]
>       ,qeWhere :: Maybe ScalarExpr
>       ,qeGroupBy :: [ScalarExpr]
>       ,qeHaving :: Maybe ScalarExpr
>       ,qeOrderBy :: [(ScalarExpr,Direction)]
>       ,qeLimit :: Maybe ScalarExpr
>       ,qeOffset :: Maybe ScalarExpr
>       }
>     | CombineQueryExpr
>       {qe1 :: QueryExpr
>       ,qeCombOp :: CombineOp
>       ,qeDuplicates :: Duplicates
>       ,qeCorresponding :: Corresponding
>       ,qe2 :: QueryExpr
>       }
>     | With [(String,QueryExpr)] QueryExpr
>       deriving (Eq,Show)

TODO: add queryexpr parens to deal with e.g.
(select 1 union select 2) union select 3
I'm not sure if this is valid syntax or not


> -- | represents the Distinct or All keywords, which can be used
> -- before a select list, in an aggregate/window function
> -- application, or in a query expression 'set operator'
> data Duplicates = Distinct | All deriving (Eq,Show)

> -- | The direction for a column in order by.
> data Direction = Asc | Desc deriving (Eq,Show)
> -- | Query expression 'set operators'
> data CombineOp = Union | Except | Intersect deriving (Eq,Show)
> -- | Corresponding, an option for the 'set operators'
> data Corresponding = Corresponding | Respectively deriving (Eq,Show)

> -- | helper/'default' value for query exprs to make creating query expr values a little easier
> makeSelect :: QueryExpr
> makeSelect = Select {qeDuplicates = All
>                     ,qeSelectList = []
>                     ,qeFrom = []
>                     ,qeWhere = Nothing
>                     ,qeGroupBy = []
>                     ,qeHaving = Nothing
>                     ,qeOrderBy = []
>                     ,qeLimit = Nothing
>                     ,qeOffset = Nothing}

> -- | Represents a entry in the csv of tables in the from clause.
> data TableRef = SimpleTableRef String -- from t
>               | JoinTableRef TableRef JoinType TableRef (Maybe JoinCondition) -- from a join b
>               | JoinParens TableRef -- from (a)
>               | JoinAlias TableRef String (Maybe [String]) -- from a as b(c,d)
>               | JoinQueryExpr QueryExpr -- from (query expr)
>                 deriving (Eq,Show)

TODO: add function table ref

> -- | The type of a join
> data JoinType = JInner | JLeft | JRight | JFull | JCross
>                 deriving (Eq,Show)

> -- | The join condition.
> data JoinCondition = JoinOn ScalarExpr -- ^ on expr
>                    | JoinUsing [String] -- ^ using (column list)
>                    | JoinNatural -- ^ natural join was specified
>                      deriving (Eq,Show)
