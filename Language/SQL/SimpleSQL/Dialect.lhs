

Data types to represent different dialect options

> {-# LANGUAGE DeriveDataTypeable #-}
> module Language.SQL.SimpleSQL.Dialect
>     (Dialect(..)
>     ,ansi2011
>     ,mysql
>     ,postgres
>     ,oracle
>     ,sqlserver
>     ,ansi2011ReservedKeywords
>     ) where

> import Data.Data

> -- | Used to set the dialect used for parsing and pretty printing,
> -- very unfinished at the moment.
> data Dialect = Dialect
>     { -- | The list of reserved keywords
>      diKeywords :: [String]
>      -- | does the dialect support ansi fetch first syntax
>     ,diFetchFirst :: Bool
>      -- | does the dialect support limit keyword (mysql, postgres, ...)
>     ,diLimit :: Bool
>      -- | allow parsing ODBC syntax
>     ,diOdbc :: Bool
>      -- | allow quoting identifiers with `backquotes`
>     ,diBackquotedIden :: Bool
>      -- | allow quoting identifiers with [square brackets]
>     ,diSquareBracketQuotedIden :: Bool
>      -- | allow identifiers with a leading at @example
>     ,diAtIdentifier :: Bool
>      -- | allow identifiers with a leading # #example
>     ,diHashIdentifier :: Bool
>      -- | allow positional identifiers like this: $1 
>     ,diPositionalArg :: Bool
>      -- | allow postgres style dollar strings
>     ,diDollarString :: Bool
>      -- | allow strings with an e - e"example"
>     ,diEString :: Bool
>      -- | allow postgres style symbols
>     ,diPostgresSymbols :: Bool
>      -- | allow sql server style symbols
>     ,diSqlServerSymbols :: Bool
>     }
>                deriving (Eq,Show,Read,Data,Typeable)

> -- | ansi sql 2011 dialect
> ansi2011 :: Dialect
> ansi2011 = Dialect {diKeywords = ansi2011ReservedKeywords
>                    ,diFetchFirst = True
>                    ,diLimit = False
>                    ,diOdbc = False
>                    ,diBackquotedIden = False
>                    ,diSquareBracketQuotedIden = False
>                    ,diAtIdentifier = False
>                    ,diHashIdentifier = False
>                    ,diPositionalArg = False
>                    ,diDollarString = False
>                    ,diEString = False
>                    ,diPostgresSymbols = False
>                    ,diSqlServerSymbols = False
>                    }

> -- | mysql dialect
> mysql :: Dialect
> mysql = addLimit ansi2011 {diFetchFirst = False
>                           ,diBackquotedIden = True
>                           }

> -- | postgresql dialect
> postgres :: Dialect
> postgres = addLimit ansi2011 {diPositionalArg = True
>                              ,diDollarString = True
>                              ,diEString = True
>                              ,diPostgresSymbols = True}

> -- | oracle dialect
> oracle :: Dialect
> oracle = ansi2011 -- {}

> -- | microsoft sql server dialect
> sqlserver :: Dialect
> sqlserver = ansi2011 {diSquareBracketQuotedIden = True
>                      ,diAtIdentifier = True
>                      ,diHashIdentifier = True
>                      ,diSqlServerSymbols = True }

> addLimit :: Dialect -> Dialect
> addLimit d = d {diKeywords = "limit": diKeywords d
>                ,diLimit = True}

> ansi2011ReservedKeywords :: [String]
> ansi2011ReservedKeywords =
>     [--"abs" -- function
>      "all" -- keyword only?
>     ,"allocate" -- keyword
>     ,"alter" -- keyword
>     ,"and" -- keyword
>     --,"any" -- keyword? and function
>     ,"are" -- keyword
>     ,"array" -- keyword, and used in some special places, like array[...], and array(subquery)
>     --,"array_agg" -- function
>     -- ,"array_max_cardinality" -- function
>     ,"as" -- keyword
>     ,"asensitive" -- keyword
>     ,"asymmetric" -- keyword
>     ,"at" -- keyword
>     ,"atomic"
>     ,"authorization"
>     --,"avg"
>     ,"begin"
>     ,"begin_frame"
>     ,"begin_partition"
>     ,"between"
>     ,"bigint"
>     ,"binary"
>     ,"blob"
>     ,"boolean"
>     ,"both"
>     ,"by"
>     ,"call"
>     ,"called"
>     ,"cardinality"
>     ,"cascaded"
>     ,"case"
>     ,"cast"
>     ,"ceil"
>     ,"ceiling"
>     ,"char"
>     --,"char_length"
>     ,"character"
>     --,"character_length"
>     ,"check"
>     ,"clob"
>     ,"close"
>     ,"coalesce"
>     ,"collate"
>     --,"collect"
>     ,"column"
>     ,"commit"
>     ,"condition"
>     ,"connect"
>     ,"constraint"
>     ,"contains"
>     --,"convert"
>     --,"corr"
>     ,"corresponding"
>     --,"count"
>     --,"covar_pop"
>     --,"covar_samp"
>     ,"create"
>     ,"cross"
>     ,"cube"
>     --,"cume_dist"
>     ,"current"
>     ,"current_catalog"
>     --,"current_date"
>     --,"current_default_transform_group"
>     --,"current_path"
>     --,"current_role"
>     ,"current_row"
>     ,"current_schema"
>     ,"current_time"
>     --,"current_timestamp"
>     ,"current_transform_group_for_type"
>     --,"current_user"
>     ,"cursor"
>     ,"cycle"
>     ,"date"
>     --,"day"
>     ,"deallocate"
>     ,"dec"
>     ,"decimal"
>     ,"declare"
>     --,"default"
>     ,"delete"
>     --,"dense_rank"
>     ,"deref"
>     ,"describe"
>     ,"deterministic"
>     ,"disconnect"
>     ,"distinct"
>     ,"double"
>     ,"drop"
>     ,"dynamic"
>     ,"each"
>     --,"element"
>     ,"else"
>     ,"end"
>     ,"end_frame"
>     ,"end_partition"
>     ,"end-exec"
>     ,"equals"
>     ,"escape"
>     --,"every"
>     ,"except"
>     ,"exec"
>     ,"execute"
>     ,"exists"
>     ,"exp"
>     ,"external"
>     ,"extract"
>     --,"false"
>     ,"fetch"
>     ,"filter"
>     ,"first_value"
>     ,"float"
>     ,"floor"
>     ,"for"
>     ,"foreign"
>     ,"frame_row"
>     ,"free"
>     ,"from"
>     ,"full"
>     ,"function"
>     --,"fusion"
>     ,"get"
>     ,"global"
>     ,"grant"
>     ,"group"
>     --,"grouping"
>     ,"groups"
>     ,"having"
>     ,"hold"
>     --,"hour"
>     ,"identity"
>     ,"in"
>     ,"indicator"
>     ,"inner"
>     ,"inout"
>     ,"insensitive"
>     ,"insert"
>     ,"int"
>     ,"integer"
>     ,"intersect"
>     --,"intersection"
>     ,"interval"
>     ,"into"
>     ,"is"
>     ,"join"
>     ,"lag"
>     ,"language"
>     ,"large"
>     ,"last_value"
>     ,"lateral"
>     ,"lead"
>     ,"leading"
>     ,"left"
>     ,"like"
>     ,"like_regex"
>     ,"ln"
>     ,"local"
>     ,"localtime"
>     ,"localtimestamp"
>     ,"lower"
>     ,"match"
>     --,"max"
>     ,"member"
>     ,"merge"
>     ,"method"
>     --,"min"
>     --,"minute"
>     ,"mod"
>     ,"modifies"
>     --,"module"
>     --,"month"
>     ,"multiset"
>     ,"national"
>     ,"natural"
>     ,"nchar"
>     ,"nclob"
>     ,"new"
>     ,"no"
>     ,"none"
>     ,"normalize"
>     ,"not"
>     ,"nth_value"
>     ,"ntile"
>     --,"null"
>     ,"nullif"
>     ,"numeric"
>     ,"octet_length"
>     ,"occurrences_regex"
>     ,"of"
>     ,"offset"
>     ,"old"
>     ,"on"
>     ,"only"
>     ,"open"
>     ,"or"
>     ,"order"
>     ,"out"
>     ,"outer"
>     ,"over"
>     ,"overlaps"
>     ,"overlay"
>     ,"parameter"
>     ,"partition"
>     ,"percent"
>     --,"percent_rank"
>     --,"percentile_cont"
>     --,"percentile_disc"
>     ,"period"
>     ,"portion"
>     ,"position"
>     ,"position_regex"
>     ,"power"
>     ,"precedes"
>     ,"precision"
>     ,"prepare"
>     ,"primary"
>     ,"procedure"
>     ,"range"
>     --,"rank"
>     ,"reads"
>     ,"real"
>     ,"recursive"
>     ,"ref"
>     ,"references"
>     ,"referencing"
>     --,"regr_avgx"
>     --,"regr_avgy"
>     --,"regr_count"
>     --,"regr_intercept"
>     --,"regr_r2"
>     --,"regr_slope"
>     --,"regr_sxx"
>     --,"regr_sxy"
>     --,"regr_syy"
>     ,"release"
>     ,"result"
>     ,"return"
>     ,"returns"
>     ,"revoke"
>     ,"right"
>     ,"rollback"
>     ,"rollup"
>     --,"row"
>     ,"row_number"
>     ,"rows"
>     ,"savepoint"
>     ,"scope"
>     ,"scroll"
>     ,"search"
>     --,"second"
>     ,"select"
>     ,"sensitive"
>     --,"session_user"
>     ,"set"
>     ,"similar"
>     ,"smallint"
>     --,"some"
>     ,"specific"
>     ,"specifictype"
>     ,"sql"
>     ,"sqlexception"
>     ,"sqlstate"
>     ,"sqlwarning"
>     ,"sqrt"
>     --,"start"
>     ,"static"
>     --,"stddev_pop"
>     --,"stddev_samp"
>     ,"submultiset"
>     ,"substring"
>     ,"substring_regex"
>     ,"succeeds"
>     --,"sum"
>     ,"symmetric"
>     ,"system"
>     ,"system_time"
>     --,"system_user"
>     ,"table"
>     ,"tablesample"
>     ,"then"
>     ,"time"
>     ,"timestamp"
>     ,"timezone_hour"
>     ,"timezone_minute"
>     ,"to"
>     ,"trailing"
>     ,"translate"
>     ,"translate_regex"
>     ,"translation"
>     ,"treat"
>     ,"trigger"
>     ,"truncate"
>     ,"trim"
>     ,"trim_array"
>     --,"true"
>     ,"uescape"
>     ,"union"
>     ,"unique"
>     --,"unknown"
>     ,"unnest"
>     ,"update"
>     ,"upper"
>     --,"user"
>     ,"using"
>     --,"value"
>     ,"values"
>     ,"value_of"
>     --,"var_pop"
>     --,"var_samp"
>     ,"varbinary"
>     ,"varchar"
>     ,"varying"
>     ,"versioning"
>     ,"when"
>     ,"whenever"
>     ,"where"
>     ,"width_bucket"
>     ,"window"
>     ,"with"
>     ,"within"
>     ,"without"
>     --,"year"
>     ]
