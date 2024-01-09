
-- | helpers to work with parsec errors more nicely
module Language.SQL.SimpleSQL.Errors
    (ParseError(..)
    --,formatError
    ,convParseError
    ) where

import Text.Parsec (sourceColumn,sourceLine,sourceName,errorPos)
import qualified Text.Parsec as P (ParseError)

-- | Type to represent parse errors.
data ParseError = ParseError
                  {peErrorString :: String
                   -- ^ contains the error message
                  ,peFilename :: FilePath
                   -- ^ filename location for the error
                  ,pePosition :: (Int,Int)
                   -- ^ line number and column number location for the error
                  ,peFormattedError :: String
                   -- ^ formatted error with the position, error
                   -- message and source context
                  } deriving (Eq,Show)

convParseError :: String -> P.ParseError -> ParseError
convParseError src e =
    ParseError
    {peErrorString = show e
    ,peFilename = sourceName p
    ,pePosition = (sourceLine p, sourceColumn p)
    ,peFormattedError = formatError src e}
  where
    p = errorPos e

{-
format the error more nicely: emacs format for positioning, plus
context
-}

formatError :: String -> P.ParseError -> String
formatError src e =
    sourceName p ++ ":" ++ show (sourceLine p)
    ++ ":" ++ show (sourceColumn p) ++ ":"
    ++ context
    ++ show e
  where
    context =
        let lns = take 1 $ drop (sourceLine p - 1) $ lines src
        in case lns of
             [x] -> "\n" ++ x ++ "\n"
                    ++ replicate (sourceColumn p - 1) ' ' ++ "^\n"
             _ -> ""
    p = errorPos e
