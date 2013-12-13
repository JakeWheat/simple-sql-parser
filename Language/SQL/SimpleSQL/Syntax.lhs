
> module Language.SQL.SimpleSQL.Syntax
>     (ScalarExpr(..)
>     ,TypeName(..)
>     ,SubQueryExprType(..)
>     ,InThing(..)
>     ,QueryExpr(..)
>     ,makeSelect
>     ,Duplicates(..)
>     ,Direction(..)
>     ,TableRef(..)
>     ,JoinType(..)
>     ,JoinCondition(..)
>     ) where


> data ScalarExpr = NumLit String
>                 | StringLit String
>                 | Iden String
>                 | Iden2 String String
>                 | Star
>                 | Star2 String
>                 | App String [ScalarExpr]
>                 | AggregateApp String (Maybe Duplicates)
>                                [ScalarExpr]
>                                [(ScalarExpr,Direction)]
>                   -- the binop, prefixop and postfix op
>                   -- are used for symbol and keyword operators
>                   -- these are used even for the multiple keyword
>                   -- operators
>                 | BinOp String ScalarExpr ScalarExpr
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
> data InThing = InList [ScalarExpr]
>              | InQueryExpr QueryExpr
>              deriving (Eq,Show)

> data SubQueryExprType = SqExists | SqSq | SqAll | SqSome | SqAny
>                         deriving (Eq,Show)

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
>       } deriving (Eq,Show)

> data Duplicates = Distinct | All deriving (Eq,Show)
> data Direction = Asc | Desc deriving (Eq,Show)

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


> data TableRef = SimpleTableRef String
>               | JoinTableRef JoinType TableRef TableRef (Maybe JoinCondition)
>               | JoinParens TableRef
>               | JoinAlias TableRef String
>               | JoinQueryExpr QueryExpr
>                 deriving (Eq,Show)

> data JoinType = Inner | JLeft | JRight | Full | Cross
>                 deriving (Eq,Show)

> data JoinCondition = JoinOn ScalarExpr
>                    | JoinUsing [String]
>                    | JoinNatural
>                      deriving (Eq,Show)
