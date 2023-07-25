module LRAParser
  ( lra
  , parseLRA
  ) where

-- In this module, we parse propositional formulas.

import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import Text.Parsec.Expr
import Prop
import LRA
import Data.Functor.Identity


-- The string parser we use in this file.
-- You don't have to (and should not) modify this part. 

type Parser = Parsec String ()

operators :: OperatorTable String u Identity (Exp String)
operators = [ --unaries  Neg [ "~",   "¬", "-"]
            --, 
              unaries (Minus)  ["-"]
            , binaries (:*:)   ["*"]
            , binaries (:+:)   ["+"]
            , binaries (:<=:)  ["<="]
            , binaries (:>=:)  [">="]
            , binaries (:=:)   ["=", "=="]
            ]

propOperators :: OperatorTable String u Identity (Prop (Exp String))
propOperators = [ unaries  Neg [ "~",   "¬", "!"]
                , binaries (:&:)   [ "&",   "∧"]
                , binaries (:|:)   [ "|",   "∨"]
                , binaries (-->)   [ "-->",  "→"]
                , binaries (<->) [ "<->", "↔"]
            ]

constant :: Parser (Exp String)
constant = do n <- read <$> many1 digit
              spaces
              return $ Const n

variable :: Parser (Exp String)
variable = do x <- many1 letter
              spaces
              return $ Var x
        <?> "variable"

parens :: Parser a -> Parser a
parens p = do char '('
              spaces
              x <- p
              char ')'
              spaces
              return x
        <?> "parens"

term :: Parser (Exp String)
term =  parens expr
    <|> variable
    <|> constant
    <?> "expression"

expr :: Parser (Exp String)
expr = buildExpressionParser operators term
    <?> "compound expression"

proposition :: Parser (Prop (Exp String))
proposition = do
    x <- expr
    return $ Lit x

logicTerm :: Parser (Prop (Exp String))
logicTerm =  parens logicExpr
    <|> proposition
    <?> "expression"

logicExpr :: Parser (Prop (Exp String))
logicExpr = buildExpressionParser propOperators logicTerm
    <?> "compound expression"

lra :: Parser (Prop (Exp String))
lra = do  spaces
          x <- logicExpr
          spaces
          eof
          return $ x  

unary :: (a -> a)
      -> String
      -> Operator String u Identity a
unary c n = Prefix . chainl1 (string n >> spaces >> return c) $ return (.)

binary :: (a -> a -> a)
       -> String
       -> Operator String u Identity a
binary c n = Infix (string n >> spaces >> return c) AssocRight

unaries :: (a -> a)
        -> [String]
        -> [Operator String u Identity a]
unaries c = map (unary c)

binaries :: (a -> a -> a)
         -> [String]
         -> [Operator String u Identity a]
binaries c = map (binary c)

parseLRA :: String -> Prop (Exp String)
parseLRA raw = case Parsec.parse lra "haskell" raw of
  Right p -> p
  Left x -> error . show $ x
