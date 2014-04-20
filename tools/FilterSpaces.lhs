
> --import System.IO
> import System.Environment

> main :: IO ()
> main = do
>     [a] <- getArgs
>     r <- readFile a
>     let ls = lines r
>     putStrLn $ unlines $ map dedupeSpaces ls


> dedupeSpaces :: String -> String
> dedupeSpaces [] = []
> -- don't start until after the leading spaces
> -- including literate haskell source lines
> dedupeSpaces xs@(x:_) | x `notElem` " >" = dedupeSpaces' xs
> dedupeSpaces (x:xs) = x : dedupeSpaces xs

> dedupeSpaces' :: String -> String
> dedupeSpaces' (' ':xs@(' ':_)) = dedupeSpaces' xs
> dedupeSpaces' (x:xs) = x : dedupeSpaces' xs
> dedupeSpaces' [] = []

