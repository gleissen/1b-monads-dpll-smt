{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Prop
  (Prop (..)
  , (-->)
  , (<->)
  , toNNF
  , distribute
  , cnf
  , assocl
  ) where

-- Define operator precedence for binary
-- operators. :&: binds before :|:, similar
-- to how * binds before + in arithmetic.
infixl 7 :&:  -- and
infixl 6 :|:  -- or
infixl 5 -->  -- implies
infixl 4 <->  -- iff

-- Propositional logic format
-- We define the data constructors of the minimal
-- language where (and = :&:, or = :|:)
data Prop a
  = Lit a
  | Neg (Prop a)
  | Prop a :&: Prop a
  | Prop a :|: Prop a
  deriving (Eq, Functor, Foldable, Traversable)

-- Show with operator precedence. This reduces
-- parentheses on prints, improving readability
-- considerably!
instance Show a => Show (Prop a) where
  showsPrec prec (Lit x)   = showsPrec prec x
  showsPrec prec (Neg p)   = showParen (prec >= 8) $ showString "¬" . showsPrec 7 p 
  showsPrec prec (p :&: q) = showParen (prec >= 7) $ showsPrec 6 p . showString " ∧ " . showsPrec 7 q
  showsPrec prec (p :|: q) = showParen (prec >= 6) $ showsPrec 5 p . showString " ∨ " . showsPrec 6 q

-- Left associate a propositional expression.
-- Since :&: and :|: are commutative, meaning
-- is preserved. This is usefull for printing
-- to improve readability by reducing parentheses.
assocl :: Prop a -> Prop a
assocl (p :&: (q :&: r)) = assocl $ p :&: q :&: r
assocl (p :|: (q :|: r)) = assocl $ p :|: q :|: r
assocl (p :&: q)         = assocl p :&: assocl q
assocl (p :|: q)         = assocl p :|: assocl q
assocl (Neg p)           = Neg $ assocl p
assocl p                 = p


-- ^ TODO: Implement the functions below.

-- Implication
(-->) :: Prop a -> Prop a -> Prop a
(-->) = undefined

-- Bi-Implication
(<->) :: Prop a -> Prop a -> Prop a
(<->) =  undefined

-- This function turns a formula into negation normal form.
-- That is, it ensures that it does not contain negation 
-- outside of literals. For this, it applies the transformation 
-- that turns Neg (p :&: q) into the equivalent formula 
-- (Neg p) :|: (Neg q), and similarly, the transformation 
-- that turns Neg (p :|: q) into (Neg p) :&: (Neg q).
toNNF :: Prop a -> Prop a
toNNF = undefined

-- This function implements the distribution 
-- of disjunction over conjunction, i.e., turns
-- (p :|: (q :&: r)) 
-- into the equivalent formula
-- (p :|: q) :&: (p :|: r)
--
-- The function takes as arguments two 
-- propositions p and q, representing formula p :|: q, 
-- and exhaustively applies the distribution operation.
--
-- The function also needs to handle the symmetric case that turns
-- ((p :&: q) :|: r) into (p :|: r) :&: (q :|: r).
-- You can assume that the formulas are in NNF.

distribute :: Prop a -> Prop a -> Prop a
distribute = undefined

-- Recursively transform a propositional formula into Conjunct Normal Form. You
-- can use the two functions defined above. You should use the naive algorithm,
-- not Tseitin's transformation.
--
cnf :: Prop a -> Prop a
cnf = undefined