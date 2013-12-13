
> module Language.SQL.SimpleSQL.Syntax
>     (QueryExpr(..)
>     ,makeSelect
>     ,ScalarExpr(..)
>     ,TableRef(..)
>     ,JoinType(..)
>     ,JoinCondition(..)
>     ,Direction(..)
>     ) where


> data ScalarExpr = NumLit String
>                 | StringLit String
>                 | Iden String
>                 | Iden2 String String
>                 | Star
>                 | Star2 String
>                 | App String [ScalarExpr]
>                 | Op String [ScalarExpr]
>                 | Case (Maybe ScalarExpr) -- test value
>                        [(ScalarExpr,ScalarExpr)] -- when branches
>                        (Maybe ScalarExpr) -- else value
>                 | Parens ScalarExpr
>                   deriving (Eq,Show)

> data QueryExpr
>     = Select
>       {qeSelectList :: [(Maybe String,ScalarExpr)]
>       ,qeFrom :: [TableRef]
>       ,qeWhere :: Maybe ScalarExpr
>       ,qeGroupBy :: [ScalarExpr]
>       ,qeHaving :: Maybe ScalarExpr
>       ,qeOrderBy :: [(ScalarExpr,Direction)]
>       } deriving (Eq,Show)

> data Direction = Asc | Desc deriving (Eq,Show)

> makeSelect :: QueryExpr
> makeSelect = Select {qeSelectList = []
>                     ,qeFrom = []
>                     ,qeWhere = Nothing
>                     ,qeGroupBy = []
>                     ,qeHaving = Nothing
>                     ,qeOrderBy = []}


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
