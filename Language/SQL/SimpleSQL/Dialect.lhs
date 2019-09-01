

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
>       -- | The list of reserved keywords, which can also be used as
>       -- |  an identifier
>     ,diIdentifierKeywords :: [String]
>       -- | The list of reserved keywords, which can also be used as
>       -- |  a function name (including aggregates and window
>       -- |  functions)
>     ,diAppKeywords :: [String]
>      -- | does the dialect support ansi fetch first syntax
>     ,diFetchFirst :: Bool
>      -- | does the dialect support limit keyword (mysql, postgres,
>      -- |  ...)
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
>                    ,diIdentifierKeywords = []
>                    ,diAppKeywords = ["set"]
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

todo: review this list
add tests

think about how to say if something can safely be made a non keyword
(assuming can only be total keyword or not keyword at all)
-> if something can't appear in a scalar expression or next to one,
then I think it's pretty safe

mostly, things are keywords to avoid them mistakenly being parsed as
aliases or as identifiers/functions/function-like things (aggs,
windows, etc.)

some rationale for having quite string reserved keywords:

1. sql has the unusual (these days) feature of quoting identifiers
    which allows you to use any keyword in any context

2. the user already has to deal with a very long list of keywords in
   sql. this is not very user friendly

3. if the user has to remember which situations which keyword needs
   quoting, and which it doesn't need quoting, this is also not very
   user friendly, even if it means less quoting sometimes. E.g. if
   you only need to quote 'from' in places where it is ambiguous, and
   you want to take advantage of this, this list of good/not-good
   places is based on the weirdness of SQL grammar and the
   implementation details of the parser - and it's especially bad if
   you are using from as an iden without quotes, and you edit the sql
   statement, and now from is in a position where it does need
   quotes, and you get a obscure error message

4. there is a lot more potential for nice clear error messages
   keywords are never allowed without quoting

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
>     ,"atomic" -- keyword
>     ,"authorization" -- keyword
>     --,"avg" -- function
>     ,"begin" -- keyword
>     --,"begin_frame" -- identifier
>     --,"begin_partition" -- identifier
>     ,"between" -- keyword
>     ,"bigint" -- type
>     ,"binary" -- type
>     ,"blob" -- type
>     ,"boolean" -- type
>     ,"both" -- keyword
>     ,"by" -- keyword
>     ,"call" -- keyword
>     ,"called" -- keyword
>     -- ,"cardinality" -- function + identifier?
>     ,"cascaded" -- keyword
>     ,"case" -- keyword
>     ,"cast" -- special function
>     -- ,"ceil" -- function
>     -- ,"ceiling" -- function
>     ,"char"  -- type (+ keyword?)
>     --,"char_length" -- function
>     ,"character" -- type
>     --,"character_length" -- function
>     ,"check" -- keyword
>     ,"clob" -- type
>     ,"close" -- keyword
>     -- ,"coalesce" -- function
>     ,"collate" -- keyword
>     --,"collect" -- function
>     ,"column" -- keyword
>     ,"commit" -- keyword
>     ,"condition" -- keyword
>     ,"connect" -- keyword
>     ,"constraint" --keyword
>     --,"contains" -- keyword?
>     --,"convert" -- function?
>     --,"corr" -- function
>     ,"corresponding" --keyword
>     --,"count" --function
>     --,"covar_pop" -- function
>     --,"covar_samp" --function
>     ,"create" -- keyword
>     ,"cross" -- keyword
>     ,"cube" -- keyword
>     --,"cume_dist" -- function
>     ,"current" -- keyword
>     -- ,"current_catalog" --identifier?
>     --,"current_date" -- identifier
>     --,"current_default_transform_group"  -- identifier
>     --,"current_path"  -- identifier
>     --,"current_role"  -- identifier
>     -- ,"current_row"  -- identifier
>     -- ,"current_schema"  -- identifier
>     -- ,"current_time"  -- identifier
>     --,"current_timestamp"  -- identifier
>     --,"current_transform_group_for_type"  -- identifier, or keyword?
>     --,"current_user" -- identifier
>     ,"cursor" -- keyword
>     ,"cycle" --keyword
>     ,"date" -- type
>     --,"day" -- keyword? - the parser needs it to not be a keyword to parse extract at the moment
>     ,"deallocate" -- keyword
>     ,"dec" -- type
>     ,"decimal" -- type
>     ,"declare" -- keyword
>     --,"default" -- identifier + keyword
>     ,"delete" -- keyword
>     --,"dense_rank" -- functino
>     ,"deref" -- keyword
>     ,"describe"  -- keyword
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
>     -- ,"end_frame"  -- identifier
>     -- ,"end_partition"  -- identifier
>     ,"end-exec" -- no idea what this is
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
>     -- ,"first_value"
>     ,"float"
>     --,"floor"
>     ,"for"
>     ,"foreign"
>     -- ,"frame_row"  -- identifier
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
>     --,"lag"
>     ,"language"
>     ,"large"
>     --,"last_value"
>     ,"lateral"
>     --,"lead"
>     ,"leading"
>     ,"left"
>     ,"like"
>     ,"like_regex"
>     --,"ln"
>     ,"local"
>     ,"localtime"
>     ,"localtimestamp"
>     --,"lower"
>     ,"match"
>     --,"max"
>     ,"member"
>     ,"merge"
>     ,"method"
>     --,"min"
>     --,"minute"
>     --,"mod"
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
>     --,"nth_value"
>     ,"ntile"
>     --,"null"
>     --,"nullif"
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
>     --,"power"
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
>     --,"row_number"
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
>     --,"sqrt"
>     --,"start"
>     ,"static"
>     --,"stddev_pop"
>     --,"stddev_samp"
>     ,"submultiset"
>     --,"substring"
>     ,"substring_regex"
>     ,"succeeds"
>     --,"sum"
>     ,"symmetric"
>     ,"system"
>     --,"system_time"
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
>     --,"trim"
>     --,"trim_array"
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
>     --,"width_bucket"
>     ,"window"
>     ,"with"
>     ,"within"
>     ,"without"
>     --,"year"
>     ]
