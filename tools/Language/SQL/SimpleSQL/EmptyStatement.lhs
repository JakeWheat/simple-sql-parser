> module Language.SQL.SimpleSQL.EmptyStatement where
>
> import Language.SQL.SimpleSQL.Syntax
> import Language.SQL.SimpleSQL.TestTypes
>
> emptyStatementTests :: TestItem
> emptyStatementTests = Group "empty statement"
>   [ TestStatement ansi2011 ";" EmptyStatement
>   , TestStatements ansi2011 ";" [EmptyStatement]
>   , TestStatements ansi2011 ";;" [EmptyStatement, EmptyStatement]
>   , TestStatements ansi2011 ";;;" [EmptyStatement, EmptyStatement, EmptyStatement]
>   , TestStatement ansi2011 "/* comment */ ;" EmptyStatement
>   , TestStatements ansi2011 "" []
>   , TestStatements ansi2011 "/* comment */" []
>   , TestStatements ansi2011 "/* comment */ ;" [EmptyStatement]
>   , TestStatements ansi2011 "/* comment */ ; /* comment */ ;"
>       [EmptyStatement, EmptyStatement]
>   , TestStatements ansi2011 "/* comment */ ; /* comment */ ; /* comment */ ;"
>       [EmptyStatement, EmptyStatement, EmptyStatement]
>   ]
