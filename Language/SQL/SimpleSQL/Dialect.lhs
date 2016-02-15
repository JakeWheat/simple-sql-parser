

Data types to represent different dialect options

> {-# LANGUAGE DeriveDataTypeable #-}
> module Language.SQL.SimpleSQL.Dialect
>     (SyntaxFlavour(..)
>     ,Dialect(..)
>     ,ansi2011
>     ,mysql
>     ,postgres
>     ,oracle
>     ,sqlserver
>     ) where

> import Data.Data


hack for now, later will expand to flags on a feature by feature basis

> data SyntaxFlavour = ANSI2011
>                    | MySQL
>                    | Postgres
>                    | Oracle
>                    | SQLServer
>                      deriving (Eq,Show,Read,Data,Typeable)

> -- | Used to set the dialect used for parsing and pretty printing,
> -- very unfinished at the moment.
> data Dialect = Dialect {diSyntaxFlavour :: SyntaxFlavour
>                        ,allowOdbc :: Bool}
>                deriving (Eq,Show,Read,Data,Typeable)

> -- | ansi sql 2011 dialect
> ansi2011 :: Dialect
> ansi2011 = Dialect ANSI2011 False

> -- | mysql dialect
> mysql :: Dialect
> mysql = Dialect MySQL False

> -- | postgresql dialect
> postgres :: Dialect
> postgres = Dialect Postgres False

> -- | oracle dialect
> oracle :: Dialect
> oracle = Dialect Oracle False

> -- | microsoft sql server dialect
> sqlserver :: Dialect
> sqlserver = Dialect SQLServer False


