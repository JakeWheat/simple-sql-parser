
import System.IO
import System.Environment


main :: IO ()
main = do
    [a] <- getArgs
    r <- readFile a
    let ls = lines r
        a = noAdjacentBlankLines ls
        b = concat $ combineGroups $ group [] a
    putStrLn $ unlines b

noAdjacentBlankLines [] = []
noAdjacentBlankLines [a] = [a]
noAdjacentBlankLines ("":xs@("":_)) = noAdjacentBlankLines xs
noAdjacentBlankLines (x:xs) = x:noAdjacentBlankLines xs

group :: [String] -> [String] -> [[String]]
group acc [] = [acc]
group acc ("":xs) = reverse ("":acc) : group [] xs
group acc (x:xs) = group (x : acc) xs

combineGroups :: [[String]] -> [[String]]
combineGroups [] = []
combineGroups (x@(('<':_):_):xs) | gs <- map trim x
                                 , ns <- trim $ unwords gs
                                 , length ns < 80 = [ns ++ "\n"] : combineGroups xs
combineGroups (x:xs) = x:combineGroups xs

trim :: String -> String
trim = x . x
   where
     x = dropWhile (==' ') . reverse
