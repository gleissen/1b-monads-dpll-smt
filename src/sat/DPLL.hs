{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module DPLL
  ( satisfiable
  , dpll
  , resolve
  , bcp
  , ple
  , lits
  , isPure
  , hasPureLit
  , neg
  , satAssign
  , satAssignStr
  ) where

import Data.Maybe
import Control.Applicative
import Data.List
import Control.Monad.Writer
--import Control.Monad.State
import CNF
import Parser
import qualified Tseitin
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State


-- ^ Helper code: Negate a literal. Usefull for `resolve` and branches in `dpll`.
neg :: Lit a -> Lit a
neg (Lit x) = Neg x
neg (Neg x) = Lit x

-- Unit Resolution
--
-- Given a literal and a CNF, remove all occurences of it in the CNF according
-- to Unit Resolution. This implementation can be useful in `bcp` as well as
-- when branching in `dpll`.
--
-- e.g. resolve (Lit p) (And [Or [Lit p], Or [q], Or [Neg p]] ) == And [Or [q],
--   Or []]
--
-- Notice how an occurence of the literal resolves the whole Or while occurence
-- of its negation removes the literal. You may assume that any literal occurs
-- in any given Or at most once.

resolve :: Eq a => Lit a -> CNF a -> CNF a
resolve = undefined

-- Boolean Constraint Propagation (BCP)
--
-- Remove all occurences of single variables according
-- to Boolean Constraint Propagation.

bcp :: Eq a => CNF a -> CNF a
bcp = undefined

-- Pure Literal Elimination (PLE)
--
-- Resolve variables if they only occur positively or
-- negatively (but not both), according to Pure Literal
-- Elimination

ple :: Eq a => CNF a -> CNF a
ple = undefined


-- The DPLL procedure. 
--
-- Finds whether a given CNF is satisfiable.
--
-- A CNF is satisfiable when all of its conjuncts are resolved. In our case,
-- when we have And [] :: CNF a
--
-- A CNF is unsatisfiable when it contains an Or that contains no literals.
-- e.g., And [Or [], ...] is unsat
--
-- You can start by implementing this procedure without doing BCP. This means
-- that we just naively pick any element to branch on. Once this works, you can
-- add BCP and finally, PLE. To get full points, you should use both BCP and PLE.
--
-- We don't expect any heuristics to pick branches in this implementation. Thus,
-- you may just pick any literal.
--
-- Return type: You may return `pure ()` when it is satifiable and `empty`
-- otherwise. This will also allow you to compose branches via the (<|>)
-- operator. More precisely, for two functions f1 and f2, f1 <|> f2 will first
-- try if f1 succeeds (in our case, succeeding means proving satisfiability),
-- and if not, try f2. This will be useful when assigning tentative values to
-- Boolean variables.

dpll :: (Alternative f, Eq a) => CNF a -> f ()
dpll = undefined

-- Next, we want to extend our DPLL implementation. For now, it only tells us
-- wether a formula is satisfiable or not, that is, wether or not there exists a
-- satisfying assignment. But, sometimes we might want to know *what* assignment
-- satisfies the formula. 
-- 
-- Your next task is to write a modified version of dpll that returns the
-- satisfying assignment. We'll store the assignment as list of literals that
-- occurr positively in the formula. For example for the formula `!Lit 0 & (Lit
-- 0 | Lit 1)`, [Lit 1] represents a satisfying assignment that maps `Lit 0` to
-- `false` and `Lit 1` to `true`. Note that both `[]` and `[Lit 0]` are not
-- satisfying assignments.
-- 

-- Your first task is to reimplement BCP and PLE so they keep a list of
-- positively assigned literals. You can see that bcpM and pleM now have return
-- type `State [Lit a] (CNF a)`, that is, they return a value of type (CNF a),
-- while keeping an internal state of type [Lit a]. Your implementation should
-- use this state to maintain the list of positive literals. As before, you can
-- use `put` to udpate the state and `get` to read the state.


bcpM :: Eq a => CNF a -> State [Lit a] (CNF a)
bcpM = undefined

pleM :: Eq a => CNF a -> State [Lit a] (CNF a)
pleM = undefiend

-- You can now use these two functions to implement function `dpllM`, which
-- returns the satisfying assignment. The return type of the function `dpllM`
-- below combines `Maybe`, which allows you to use operator <|> to compose
-- branches, as before, and state monad `State [Lit a]` to keep track of the
-- satifying assignment. You can use `return ()` to indiciate that the formula
-- is satisfiable, and `MaybeT $ return Nothing` to indicate that it's not. The
-- satisfying assignment itself is stored in the state, and you do not need to
-- return it explicitly. Since the state monad is wrapped inside the Maybe
-- monad, you can call `bcpM` and `pleM` using lift, i.e., `do phi' <- lift $
-- bcpM phi`. Similarly, you need to use lift to access the sate, e.g., do
-- `posLits <- lift $ get`.
-- 

dpllM :: (Eq a) => CNF a ->  MaybeT (State [Lit a]) ()
dpllM  = undefined 


-- Scaffolding: check a given CNF for satisfiability. Returning True if the cnf
-- is satisfiable.
satisfiable :: Eq a => CNF a -> Bool
satisfiable = isJust . dpll

-- ^ Scaffolding: Check if a formula is satisfiable, and return satisfying
-- assignment.
satAssign :: (Eq a) => CNF a -> ( Maybe (), [Lit a])
satAssign phi = runState (runMaybeT (dpllM phi)) []

-- ^ Scaffolding: Return satisfying assignment for a satisfiable formula.
satAssignStr :: String -> [Lit Tseitin.ID]
satAssignStr = snd . satAssign . Tseitin.equisat . parse

-- ^ Next, we'll turn this into an SMT solver. For this, open `src/sat/LRA.hs`.