
> -- | This module contains some generic combinators used in the
> -- parser. None of the parsing which relies on the local lexers is
> -- in this module. Some of these combinators have been taken from
> -- other parser combinator libraries other than Parsec.

> module Language.SQL.SimpleSQL.Combinators
>     (optionSuffix
>     ,(<??>)
>     ,(<$$>)
>     ,(<??.>)
>     ,(<??*>)) where

> import Control.Applicative --((<$>), (<*>), (<**>))
> import Text.Parsec --(option,many)
> import Text.Parsec.String (Parser)

a possible issue with the option suffix is that it enforces left
associativity when chaining it recursively. Have to review
all these uses and figure out if any should be right associative
instead, and create an alternative suffix parser

This function style is not good, and should be replaced with chain and
<??> which has a different type

> optionSuffix :: (a -> Parser a) -> a -> Parser a
> optionSuffix p a = option a (p a)


parses an optional postfix element and applies its result to its left
hand result, taken from uu-parsinglib

TODO: make sure the precedence higher than <|> and lower than the
other operators so it can be used nicely

> (<??>) :: Parser a -> Parser (a -> a) -> Parser a
> p <??> q = p <**> option id q


this is analogous to <**>, flipped <$>

> (<$$>) :: (a -> b -> c) -> Parser b -> Parser (a -> c)
> (<$$>) = (<$>) . flip


composing suffix parsers, not sure about the name. This is used to add
a second or more suffix parser contingent on the first suffix parser
succeeding.

> (<??.>) :: Parser (a -> a) -> Parser (a -> a) -> Parser (a -> a)
> (<??.>) pa pb = (.) <$$> pa <*> option id pb


0 to many repeated applications of suffix parser

> (<??*>) :: Parser a -> Parser (a -> a) -> Parser a
> p <??*> q = p <**> chainl q (pure (flip (.))) id
>     -- foldr ($) <$> p <*> (reverse <$> many q)
