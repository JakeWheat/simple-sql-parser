
{-
= Fixity fixups

The point of this code is to be able to take a table of fixity
information for unary and binary operators, then adjust an ast to
match these fixities. The standard way of handling this is handling
fixities at the parsing stage.

For the SQL parser, this is difficult because there is lots of weird
syntax for operators (such as prefix and postfix multiple keyword
operators, between, etc.).

An alterative idea which is used in some places is to parse the tree
regarding all the operators to have the same precedence and left
associativity, then correct the fixity in a pass over the ast after
parsing. Would also like to use this to fix the fixity for the join
trees, and set operations, after parsing them. TODO: anything else?


Approach

Really not sure how to get this correct. So: lots of testing

Basic testing idea: create an expression, then write down manually how
the expression should parse with correct fixity. Can write down the
expression in concrete syntax, and the correct fixity version using
parens.

Then can parse the expression, fix it, parse the fixed expression,
remove the parens and compare them to make sure they are equal.

Second layer of testing. For each source expression parsed, run it
through a generator which will generate every version of that tree by
choosing all possibilities of fixities on a token by token basis. This
will ensure the fixity fixer is robust. An alternative approach is to
guarantee the parser will produce trees where all the fixities are
known (e.g. unary operators always bind tighter than binary, binary
are all left associative, prefix unary bind tighter than postfix. This
way, the fix code can make some assumptions and have less code. We
will stick with the full general version which is more robust.

Another testing approach is to parse the tree with our non fixity
respecting parser then fix it, and also parse it with a fixity
respecting expression parser, and check the results are the same. This
is difficult with the parsec build expression parser which doesn't
handle nested unary operators, so have to find or write another build
expression parser. We can test the fixer with simple operators (single
symbol prefix, postfix and binary ops) and then use it on the complex
sql ast trees.

Can also try to generate trees ala quickcheck/smallcheck, then check
them with the fixer and the build expression parser.

generate a tree:

start with a term
then roll dice:
  add a prefix
  add a postfix
  do nothing
then roll dice
  add a binary op
  for the second arg, recurse the algo


algorithm:

consider possible cases:
binop with two binops args
binop with prefix on left
binop with postfix on right
postfix with prefix inside
prefix with postfix inside
postfix with binop inside
prefix with binop inside

write a function to deal with each case and try to compose

Tasks:

write unary op tests: on each other, and with binary ops
figure out how to generate trees
do the step one tests (write the fixity with parens)
check out parsers expression parser
see if can generate trees using smallcheck
try to test these trees against expression parser
  otherwise, generate tree, generate variations, check fixity always
produces same result




todo:

1. more tests for unary operators with each other
2. moving unary operators inside and outside binary operators:
   have to think about how this will work in general case
3. ways to generate lots of tests and check them
   -> what about creating a parser which parses to a list of all possible
      parses with different fixities for each operator it sees?
4. ambiguous fixity cases - need position annotation to do these nicely
5. real sql: how to work with a variety of ast nodes
6. plug into simple-sql-parser
7. refactor the simple-sql-parser parsing code
8. simple-sql-parser todo for sqream: add other dml, dialects,
   procedural?
9. testing idea: write big expressions with explicit parens everywhere
   parse this
   remove the parens
   pretty print, then parse and fixfixity to see if same
   then generate all variations of tree as if the fixities are different
     and then fixfixity to check it restores the original


write fixity tests
write code to do the fixing
add error cases: put it in the either monad to report these

check the descend
then: move to real sql
  different abstract representations of binops, etc.
  what is the best way to deal with this? typeclass? conversion to and
  from a generic tree?





can the binops be fixed on their own (precedence and assocativity)
and then the prefix and postfix ops in separate passes

what about a pass which puts the tree into canonical form:
all left associative, all unary ops tight as possible?
then the fixer can be easier?
-}





{-# LANGUAGE DeriveDataTypeable,TupleSections #-}
import Data.Data

import Text.Parsec.String (Parser)
import Text.Parsec (try)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec (parse,ParseError)
import Control.Applicative ((<|>),many) -- ((<**>),(<$>),(<*), (*>),(<*>), (<$), (<|>), many)
--import qualified Text.Parsec.String.Expr as E
import Control.Monad
--import Data.List (intercalate)
import Data.Maybe ()
--import qualified Test.HUnit as H
--import FunctionsAndTypesForParsing
import Debug.Trace
import Text.Show.Pretty
import Data.List
import Control.Applicative

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as H


data Expr = BinOp Expr String Expr
          | PrefOp String Expr
          | PostOp String Expr
          | Iden String
          | Lit String
          | App String [Expr]
          | Parens Expr
            deriving (Eq,Show,Data,Typeable)

{-
--------

quick parser
-}

parensValue :: Parser Expr
parensValue = Parens <$> parens valueExpr

idenApp :: Parser Expr
idenApp = try $ do
    i <- identifier
    guard (i `notElem` ["not", "and", "or", "is"])
    choice [do
            args <- parens (commaSep valueExpr)
            return $ App i args
           ,return $ Iden i
           ]

lit :: Parser Expr
lit = stringLit <|> numLit
    where
      stringLit = Lit <$> lexeme (char '\'' *> manyTill anyChar (char '\''))
      numLit = do
        x <- lexeme (many1 digit)
        let y :: Integer
            y = read x
        return $ Lit $ show y

prefOp :: Parser Expr
prefOp = sym <|> kw
   where
      sym = do
            let prefOps = ["+", "-"]
            s <- choice $ map symbol prefOps
            v <- term
            return $ PrefOp s v
      kw = do
           let prefOps = ["not"]
           i <- identifier
           guard (i `elem` prefOps)
           v <- term
           return $ PrefOp i v

postOp :: Parser (Expr -> Expr)
postOp = try $ do
    let kws = ["is null"]
        kwsp = map (\a -> try $ do
                       let x :: [String]
                           x = words a
                       mapM_ keyword_ x
                       return $ PostOp a
                       ) kws
    choice kwsp

binOp :: Parser (Expr -> Expr -> Expr)
binOp = symbolBinOp <|> kwBinOp
  where
    symbolBinOp = do
                  let binOps = ["+", "-", "*", "/"]
                  s <- choice $ map symbol binOps
                  return $ \a b -> BinOp a s b
    kwBinOp = do
              let kwBinOps = ["and", "or"]
              i <- identifier
              guard (i `elem` kwBinOps)
              return $ \a b -> BinOp a i b

term :: Parser Expr
term = (parensValue
       <|> try prefOp
       <|> idenApp
       <|> lit)
       <??*> postOp

-- (<??>) :: Parser a -> Parser (a -> a) -> Parser a
-- p <??> q = p <**> option id q

(<??*>) :: Parser a -> Parser (a -> a) -> Parser a
p <??*> q = foldr ($) <$> p <*> (reverse <$> many q)

valueExpr :: Parser Expr
valueExpr = chainl1 term binOp


parens :: Parser a -> Parser a
parens = between openParen closeParen

openParen :: Parser Char
openParen = lexeme $ char '('
closeParen :: Parser Char
closeParen = lexeme $ char ')'

symbol :: String -> Parser String
symbol s = try $ lexeme $ do
    u <- many1 (oneOf "<>=+-^%/*!|")
    guard (s == u)
    return s

identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

keyword :: String -> Parser String
keyword k = try $ do
    i <- identifier
    guard (i == k)
    return k

keyword_ :: String -> Parser ()
keyword_ = void . keyword

whitespace :: Parser ()
whitespace =
    choice [simpleWhitespace *> whitespace
           ,lineComment *> whitespace
           ,blockComment *> whitespace
           ,return ()]
  where
    lineComment = try (string "--")
                  *> manyTill anyChar (void (char '\n') <|> eof)
    blockComment = try (string "/*")
                   *> manyTill anyChar (try $ string "*/")
    simpleWhitespace = void $ many1 (oneOf " \t\n")
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
comma :: Parser Char
comma = lexeme $ char ','

commaSep :: Parser a -> Parser [a]
commaSep = (`sepBy` comma)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (whitespace *> valueExpr <* eof) ""

-- --------------

data Assoc = AssocLeft | AssocRight | AssocNone deriving (Eq,Show)

type Fixities = [(String, (Int, Assoc))]

fixFixity :: Fixities -> Expr -> Expr
fixFixity fixities = fixBinOpPrecedence . fixBinOpAssociativity . fixNestedPrefPostPrec
  where
    fixBinOpAssociativity e = case e of
        BinOp a op b ->
            let a' = fixBinOpAssociativity a
                b' = fixBinOpAssociativity b
                def = BinOp a' op b'
            in case (a',b') of
                -- both
                -- a1 op1 a2 op b1 op2 b2
                (BinOp a1 op1 a2
                 ,BinOp b1 op2 b2)
                  | Just (_p,opa) <- lookupFixity op
                  , Just (_p,op1a) <- lookupFixity op1
                  , Just (_p,op2a) <- lookupFixity op2
                  -> case (opa, op1a, op2a) of
                       (AssocRight, AssocRight, AssocRight) ->
                           BinOp a1 op1 (BinOp a2 op (BinOp b1 op2 b2))
                       (AssocLeft, AssocLeft, AssocLeft) ->
                           BinOp (BinOp (BinOp a1 op1 a2) op b1) op2 b2
                       --todo: other cases
                       _ -> def
                -- just left side
                (BinOp a1 op1 a2, _)
                -- a1 op1 a2 op b'
                  | Just (_p,opa) <- lookupFixity op
                  , Just (_p,op1a) <- lookupFixity op1
                  -> case (opa, op1a) of
                       (AssocRight, AssocRight) ->
                           BinOp a1 op1 (BinOp a2 op b')
                       (AssocLeft, AssocLeft) ->
                           BinOp (BinOp a1 op1 a2) op b'
                       _ -> def

                -- just right side
                (_, BinOp b1 op2 b2)
                -- e op b1 op2 b2
                  | Just (_p,opa) <- lookupFixity op
                  , Just (_p,op2a) <- lookupFixity op2
                  -> case (opa, op2a) of
                       (AssocRight, AssocRight) ->
                           BinOp a' op (BinOp b1 op2 b2)
                       (AssocLeft, AssocLeft) ->
                           BinOp (BinOp a' op b1) op2 b2
                       _ -> def
                _ -> def
        _ -> e

    fixBinOpPrecedence e = case e of
        BinOp a op b ->
            let a' = fixBinOpPrecedence a
                b' = fixBinOpPrecedence b
                def = BinOp a' op b'
            in case (a',b') of
                -- both
                -- a1 op1 a2 op b1 op2 b2
                -- all equal
                -- p > or < p1 == p2
                -- p == p1 < or > p2
                (BinOp a1 op1 a2
                 ,BinOp b1 op2 b2)
                  | Just (p,_opa) <- lookupFixity op
                  , Just (p1,_op1a) <- lookupFixity op1
                  , Just (p2,_op2a) <- lookupFixity op2
                  -> case () of
                        -- _ | trace ("both prec " ++ show (p,p1,p2)) False -> undefined
                        _ | p == p1 && p1 == p2 -> def
                        _ | p > p1 && p1 == p2 -> BinOp a1 op1 b'
                        _ | p < p1 && p1 == p2 -> BinOp (BinOp a1 op1 a2) op b'
                        _ | p == p1 && p2 > p1 -> BinOp a' op (BinOp b1 op2 b2)
                        _ | p == p1 && p2 < p1 -> def -- todo
                        _ | otherwise -> def
                -- just left side
                (BinOp a1 op1 a2, _)
                -- a1 op1 a2 op b'
                  | Just (p,_opa) <- lookupFixity op
                  , Just (p1,_op1a) <- lookupFixity op1
                  -> case () of
                        --  _ | trace ("left prec " ++ show (p,p1)) False -> undefined
                        _ | p < p1 -> {-trace "b1" $ -}BinOp (BinOp a1 op1 a2) op b'
                          | p > p1 -> {-trace "b2" $ -}BinOp a1 op1 (BinOp a2 op b')
                          | otherwise -> def

                -- just right side
                (_, BinOp b1 op2 b2)
                -- a' op b1 op2 b2
                  | Just (p,_opa) <- lookupFixity op
                  , Just (p2,_op1a) <- lookupFixity op2
                  -> case () of
                        --  _ | trace ("right prec " ++ show (p,p2)) False -> undefined
                        _ | p > p2 -> {-trace "b1" $ -}BinOp (BinOp a' op b1) op2 b2
                          | p < p2 -> {-trace "b2" $ -}BinOp a' op (BinOp b1 op2 b2)
                          | otherwise -> {-trace "def" $ -} def
                _ -> def
        _ -> e

    fixNestedPrefPostPrec e = case e of
        PrefOp op a ->
            let a' = fixNestedPrefPostPrec a
            in case a' of
                 PostOp op1 b | Just (p,_) <- lookupFixity op
                              , Just (p1,_) <- lookupFixity op1
                              , p > p1 -> PostOp op1 (PrefOp op b)
                 _ -> PrefOp op a'
        PostOp op a ->
            let a' = fixNestedPrefPostPrec a
            in case a' of
                 PrefOp op1 b | Just (p,_) <- lookupFixity op
                              , Just (p1,_) <- lookupFixity op1
                              , p > p1 -> PrefOp op1 (PostOp op b)
                 _ -> PostOp op a'
        _ -> e



    lookupFixity :: String -> Maybe (Int,Assoc)
    lookupFixity s = maybe (trace ("didn't find " ++ s ++ "\n" ++ ppShow fixities) Nothing)
                     Just $ lookup s fixities


sqlFixity :: [(String, (Int, Assoc))]
sqlFixity = [(".", (13, AssocLeft))
            ,("[]", (12, AssocNone))

{-
unary + -
todo: split the fixity table into prefix, binary and postfix

todo: don't have explicit precedence numbers in the table??
-}

            ,("^", (10, AssocNone))]
            ++ m ["*", "/", "%"] (9, AssocLeft)
            ++ m ["+","-"] (8, AssocLeft)
            ++ m ["<", ">", "=", "<=", ">=", "<>"] (4, AssocNone)
            ++ [("is null", (3, AssocNone))
               ,("not", (2, AssocRight))
               ,("and", (1, AssocLeft))
               ,("or", (0, AssocLeft))]

  where
    m l a = map (,a) l

{-
-------

some simple parser tests
-}

data Test = Group String [Test]
          | ParserTest String Expr
          | FixityTest Fixities Expr Expr

parserTests :: Test
parserTests = Group "parserTests" $ map (uncurry ParserTest) $
    [("a", Iden "a")
    ,("'test'", Lit "test")
    ,("34", Lit "34")
    ,("f()", App "f" [])
    ,("f(3)", App "f" [Lit "3"])
    ,("(7)", Parens (Lit "7"))
    ,("a + 3", BinOp (Iden "a") "+" (Lit "3"))
    ,("1 + 2 + 3", BinOp (BinOp (Lit "1") "+" (Lit "2")) "+" (Lit "3"))

    ,("a or b", BinOp (Iden "a") "or" (Iden "b"))
    ,("-1", PrefOp "-" (Lit "1"))
    ,("not a", PrefOp "not" (Iden "a"))
    ,("not not a", PrefOp "not" (PrefOp "not" (Iden "a")))
    ,("a is null", PostOp "is null" (Iden "a"))
    ,("a is null is null", PostOp "is null" (PostOp "is null" (Iden "a")))
    ,("-a+3", BinOp (PrefOp "-" (Iden "a")) "+" (Lit "3"))
    ,("a is null and b is null", BinOp (PostOp "is null" (Iden "a"))
                                 "and"
                                 (PostOp "is null" (Iden "b")))
    ]

makeParserTest :: String -> Expr -> T.TestTree
makeParserTest s e = H.testCase s $ do
    let a = parseExpr s
    if (Right e == a)
       then putStrLn $ s ++ " OK"
       else putStrLn $ "bad parse " ++ s ++ " " ++ show a

{-
------

fixity checks

test cases:
-}


fixityTests :: Test
fixityTests = Group "fixityTests" $
     map (\(f,s,e) -> FixityTest f s e) $
     [

-- 2 bin ops wrong associativity left + null versions

     (sqlFixity
     ,i "a" `plus` (i "b" `plus` i "c")
     ,(i "a" `plus` i "b") `plus` i "c")
    ,(sqlFixity
     ,(i "a" `plus` i "b") `plus` i "c"
     ,(i "a" `plus` i "b") `plus` i "c")

-- 2 bin ops wrong associativity right

    ,(timesRight
     ,i "a" `times` (i "b" `times` i "c")
     ,i "a" `times` (i "b" `times` i "c"))
    ,(timesRight
     ,(i "a" `times` i "b") `times` i "c"
     ,i "a" `times` (i "b" `times` i "c"))


-- 2 bin ops wrong precedence left

    ,(sqlFixity
     ,i "a" `plus` (i "b" `times` i "c")
     ,i "a" `plus` (i "b" `times` i "c"))

    ,(sqlFixity
     ,(i "a" `plus` i "b") `times` i "c"
     ,i "a" `plus` (i "b" `times` i "c"))

-- 2 bin ops wrong precedence right

    ,(sqlFixity
     ,(i "a" `times` i "b") `plus` i "c"
     ,(i "a" `times` i "b") `plus` i "c")

    ,(sqlFixity
     ,i "a" `times` (i "b" `plus` i "c")
     ,(i "a" `times` i "b") `plus` i "c")

{-
a + b * c + d
a * b + c * d

check all variations
-}

    ] ++
      (let t = (i "a" `plus` i "b")
               `times`
               (i "c" `plus` i "d")
           trs = generateTrees $ splitTree t
       in [(sqlFixity, x
           ,i "a" `plus` (i "b" `times` i "c")
            `plus` i "d")
          | x <- trs])
      ++
      (let t = (i "a" `times` i "b")
               `plus`
               (i "c" `times` i "d")
           trs = generateTrees $ splitTree t
       in [(sqlFixity, x
           ,(i "a" `times` i "b")
               `plus`
               (i "c" `times` i "d"))
          | x <- trs])


    ++ [

-- prefix then postfix wrong precedence

     ([("+", (9, AssocNone))
      ,("is null", (3, AssocNone))]
     ,PrefOp "+" (PostOp "is null" (i "a"))
     ,PostOp "is null" (PrefOp "+" (i "a")))

    ,([("+", (9, AssocNone))
      ,("is null", (3, AssocNone))]
     ,PostOp "is null" (PrefOp "+" (i "a"))
     ,PostOp "is null" (PrefOp "+" (i "a")))

    ,([("+", (3, AssocNone))
      ,("is null", (9, AssocNone))]
     ,PrefOp "+" (PostOp "is null" (i "a"))
     ,PrefOp "+" (PostOp "is null" (i "a")))

    ,([("+", (3, AssocNone))
      ,("is null", (9, AssocNone))]
     ,PostOp "is null" (PrefOp "+" (i "a"))
     ,PrefOp "+" (PostOp "is null" (i "a")))

{-
3-way unary operator movement:
take a starting point and generate variations

postfix on first arg of binop (cannot move) make sure precedence wants
  it to move

prefix on second arg of binop (cannot move)

prefix on binop, precedence wrong
postfix on binop precedence wrong
prefix on first arg of binop, precedence wrong
postfix on second arg of binop, precedence wrong

ambiguous fixity tests

sanity check: parens stops rearrangement

check nesting 1 + f(expr)
-}

    ]
    where
      plus a b = BinOp a "+" b
      times a b = BinOp a "*" b
      i a = Iden a
      timesRight = [("*", (9, AssocRight))]

-- testCase

makeFixityTest :: Fixities -> Expr -> Expr -> T.TestTree
makeFixityTest fs s e = H.testCase (show s) $ do
    let s' = fixFixity fs s
    H.assertEqual "" s' e
    {-if (s' == e)
      then putStrLn $ show s ++ " OK"
      else putStrLn $ "ERROR\nstart: " ++ show s ++ "\nfixed: " ++ show s' ++ "\nshould be: " ++ show e-}

tests :: Test
tests = Group "Tests" [parserTests, fixityTests]

makeTest :: Test -> T.TestTree
makeTest (Group n ts) = T.testGroup n $ map makeTest ts
makeTest (ParserTest s e) = makeParserTest s e
makeTest (FixityTest f s e) = makeFixityTest f s e

{-
--------

 > tests :: T.TestTree
 > tests = T.testGroup "Tests" $ map makeFixityTest fixityTests
-}

main :: IO ()
main = T.defaultMain $ makeTest tests
  {-do
       mapM_ checkTest tests
       mapM_ checkFixity fixityTests
       let plus a b = BinOp a "+" b
           times a b = BinOp a "*" b
           i a = Iden a
       let t = (i "a" `plus` i "b")
               `times`
               (i "c" `plus` i "d")
           spl = splitTree t
           trs = generateTrees spl
       --putStrLn $ "\nSplit\n"
       --putStrLn $ ppShow (fst spl, length $ snd spl)
       --putStrLn $ show $ length trs
       --putStrLn $ "\nTrees\n"
       --putStrLn $ intercalate "\n" $ map show trs
       return ()-}

{-
generating trees

1. tree -> list
val op val op val op ...
(has to be two lists?

generate variations:
pick numbers from 0 to n - 1 (n is the number of ops)
choose the op at this position to be the root
recurse on the two sides
-}

splitTree :: Expr -> ([Expr], [Expr->Expr->Expr])
splitTree (BinOp a op b) = let (x,y) = splitTree a
                               (z,w) = splitTree b
                           in (x++z, y++ [\a b -> BinOp a op b] ++ w)
splitTree x = ([x],[])



generateTrees :: ([Expr], [Expr->Expr->Expr]) -> [Expr]
generateTrees (es,ops) | length es /= length ops + 1 =
    error $ "mismatch in lengths " ++ show (length es, length ops)
    ++"\n" ++ ppShow es ++ "\n"
generateTrees ([a,b], [op]) = [op a b]
generateTrees ([a], []) = [a]
generateTrees (vs, ops) =
    let n = length ops
    in --trace ("generating " ++ show (length vs, n) ++ "trees\n") $
       concat $ flip map [0..n-1] $ \m ->
         let (v1,v2) = splitAt (m + 1) vs
             (ops1,op':ops2) = splitAt m ops
             r = [op' t u | t <- generateTrees (v1,ops1)
                     , u <- generateTrees (v2,ops2)]
         in -- trace ("generated " ++ show (length r) ++ " trees")
            r
generateTrees ([],[]) = []



