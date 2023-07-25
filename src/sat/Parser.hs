module Parser
  ( prop
  , parse
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

-- | Const Int      
--     | Exp a :*: Exp a
--     | Exp a :+: Exp a
--     | Exp a :<=: Exp a

operators :: OperatorTable String u Identity (Prop String)
operators = [ unaries  Neg     [ "~",   "¬", "-"]
            -- , binaries (:*:)   ["*"]
            -- , binaries (:+:)   ["+"]
            -- , binaries (:<=:)   ["<="]
            , binaries (:&:)   [ "&",   "∧"]
            , binaries (:|:)   [ "|",   "∨"]
            , binaries (-->)   [ "->",  "→"]
            , binaries (<->) [ "<->", "↔"]
            ]


variable :: Parser (Prop String)
variable = do c <- many1 letter
              spaces
              return $ Lit c
        <?> "variable"

parens :: Parser (Prop String) -> Parser (Prop String)
parens p = do char '('
              spaces
              x <- p
              char ')'
              spaces
              return x
        <?> "parens"

term :: Parser (Prop String)
term =  parens expr
    <|> variable
    <?> "expression"

expr :: Parser (Prop String)
expr = buildExpressionParser operators term
    <?> "compound expression"

prop :: Parser (Prop String)
prop =     do  spaces
               x <- expr
               spaces
               eof
               return x  

unary :: (Prop String -> Prop String)
      -> String
      -> Operator String u Identity (Prop String)
unary c n = Prefix . chainl1 (string n >> spaces >> return c) $ return (.)

binary :: (Prop String -> Prop String -> Prop String)
       -> String
       -> Operator String u Identity (Prop String)
binary c n = Infix (string n >> spaces >> return c) AssocRight

unaries :: (Prop String -> Prop String)
        -> [String]
        -> [Operator String u Identity (Prop String)]
unaries c = map (unary c)


binaries :: (Prop String -> Prop String -> Prop String)
         -> [String]
         -> [Operator String u Identity (Prop String)]
binaries c = map (binary c)

parse :: String -> Prop String
parse raw = case Parsec.parse prop "haskell" raw of
  Right p -> p
  Left x -> error . show $ x
