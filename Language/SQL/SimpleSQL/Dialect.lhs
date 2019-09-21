

Data types to represent different dialect options

> {-# LANGUAGE DeriveDataTypeable #-}
> module Language.SQL.SimpleSQL.Dialect
>     (Dialect(..)
>     ,ansi2011
>     ,mysql
>     ,postgres
>     ,oracle
>     ,sqlserver
>     ) where

> import Data.Data

> -- | Used to set the dialect used for parsing and pretty printing,
> -- very unfinished at the moment.
> --
> -- The keyword handling works as follows:
> --
> -- There is a list of reserved keywords. These will never parse as
> -- anything other than as a keyword, unless they are in one of the
> -- other lists.
> --
> -- There is a list of \'identifier\' keywords. These are reserved
> -- keywords, with an exception that they will parse as an
> -- identifier in a scalar expression. They won't parse as
> -- identifiers in other places, e.g. column names or aliases.
> --
> -- There is a list of \'app\' keywords. These are reserved keywords,
> -- with an exception that they will also parse in an \'app-like\'
> -- construct - a regular function call, or any of the aggregate and
> -- window variations.
> --
> -- There is a list of special type names. This list serves two
> -- purposes - it is a list of the reserved keywords which are also
> -- type names, and it is a list of all the multi word type names.
> --
> -- Every keyword should appear in the keywords lists, and then you can
> -- add them to the other lists if you want exceptions. Most things
> -- that refer to functions, types or variables that are keywords in
> -- the ansi standard, can be removed from the keywords lists
> -- completely with little effect. With most of the actual SQL
> -- keywords, removing them from the keyword list will result in
> -- lots of valid syntax no longer parsing (and probably bad parse
> -- error messages too).
> --
> -- In the code, all special syntax which looks identical to regular
> -- identifiers or function calls (apart from the name), is treated
> -- like a regular identifier or function call.
> --
> -- It's easy to break the parser by removing the wrong words from
> -- the keywords list or adding the wrong words to the other lists.
>
> data Dialect = Dialect
>     { -- | reserved keywords
>      diKeywords :: [String]
>       -- | keywords with identifier exception
>     ,diIdentifierKeywords :: [String]
>       -- | keywords with app exception
>     ,diAppKeywords :: [String]
>      -- | keywords with type exception plus all the type names which
>      -- are multiple words
>     ,diSpecialTypeNames :: [String]
>      -- | allow ansi fetch first syntax
>     ,diFetchFirst :: Bool
>      -- | allow limit keyword (mysql, postgres,
>      -- ...)
>     ,diLimit :: Bool
>      -- | allow parsing ODBC syntax
>     ,diOdbc :: Bool
>      -- | allow quoting identifiers with \`backquotes\`
>     ,diBackquotedIden :: Bool
>      -- | allow quoting identifiers with [square brackets]
>     ,diSquareBracketQuotedIden :: Bool
>      -- | allow identifiers with a leading at @example
>     ,diAtIdentifier :: Bool
>      -- | allow identifiers with a leading \# \#example
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
>                    ,diSpecialTypeNames = ansi2011TypeNames
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


The keyword handling is quite strong - an alternative way to do it
would be to have as few keywords as possible, and only require them
to be quoted when this is needed to resolve a parsing ambiguity.

I don't think this is a good idea for genuine keywords (it probably is
for all the 'fake' keywords in the standard - things which are
essentially function names, or predefined variable names, or type
names, eetc.).

1. working out exactly when each keyword would need to be quoted is
quite error prone, and might change as the parser implementation is
maintained - which would be terrible for users

2. it's not user friendly for the user to deal with a whole load of
special cases - either something is a keyword, then you know you must
always quote it, or it isn't, then you know you never need to quote
it

3. I think not having exceptions makes for better error messages for
the user, and a better sql code maintenance experience.

This might not match actual existing SQL products that well, some of
which I think have idiosyncratic rules about when a keyword must be
quoted. If you want to match one of these dialects exactly with this
parser, I think it will be a lot of work.

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


> ansi2011TypeNames :: [String]
> ansi2011TypeNames =
>     ["double precision"
>     ,"character varying"
>     ,"char varying"
>     ,"character large object"
>     ,"char large object"
>     ,"national character"
>     ,"national char"
>     ,"national character varying"
>     ,"national char varying"
>     ,"national character large object"
>     ,"nchar large object"
>     ,"nchar varying"
>     ,"bit varying"
>     ,"binary large object"
>     ,"binary varying"
>         -- reserved keyword typenames:
>     ,"array"
>     ,"bigint"
>     ,"binary"
>     ,"blob"
>     ,"boolean"
>     ,"char"
>     ,"character"
>     ,"clob"
>     ,"date"
>     ,"dec"
>     ,"decimal"
>     ,"double"
>     ,"float"
>     ,"int"
>     ,"integer"
>     ,"nchar"
>     ,"nclob"
>     ,"numeric"
>     ,"real"
>     ,"smallint"
>     ,"time"
>     ,"timestamp"
>     ,"varchar"
>     ,"varbinary"
>     ]
