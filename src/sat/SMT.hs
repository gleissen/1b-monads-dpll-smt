{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module SMT
  ( dpllT
  , conflictClause
  , satSMT
  ) where

import Data.Maybe
import Control.Applicative
import Control.Monad.State
import Data.List
import Control.Monad.Writer
import LRA
import Tseitin (ID)
import DPLL (neg, lits, satAssign)
import CNF
import qualified GHC.TypeLits as assignment


-- Your last task is to turn the DPLL solver into a SMT solver over the theory
-- of linear rational arithmetic (LRA). We already implemented the theory solver
-- that can tell us whether a conjuction of arithmetics constraints is
-- satisfiable. This is implemented in function `solve` in `LRA.hs` which takes
-- a list(=conjunction) of literals, annotated with LRA constraints.  
-- We'll now combine this theory solver with our DPLL solver. We'll do this in
-- two steps. 

--(1) First, we forget about the LRA constraints and think of the formula as a
-- simple propositional logic formula. This is called `Boolean abstraction`.
-- Note the input type of our SMT solver `DPLLT` is `CNF (Exp String)` that is,
-- it's just regular CNF formulas annotated with LRA constraints. Luckily the
-- DPLL solver you wrote in the previous part of the assignment can solve
-- formulas for *any• type of annotation, which means, you can simply use
-- `satAssign` to get an assignment to the Boolean abstraction of the formula. 

--(2) Next, we need to check that the assignment also satisfies the theory
-- constraints using solve. If the assignment does satisfy the theory
-- constraints, we're done and have found a satisfying assignment for the
-- overall formula. Thats' great! But what if the assignment doesn't satisfy the
-- theory constraints? In that case, we're faced with a dilemma: the Boolean
-- abstraction allows the assignment, however, it's ruled out by the theory
-- constraints. If we just naively call the DPLL solver again, it might produce
-- the same solution and we'll end up in an infinite loop. Instead, we need to
-- modify the Boolean abstraction to rule out the offending assignment. We can
-- do this by adding a *conflict clause*. The conflict clause is a negation of
-- the offending assignment, which ensures we won't encounter it again.

-- Your task is now to implement the SMT solver following the stategy above.
-- Start by implementing `conflictClause` which turns an assignment asn
-- (conjunction of literals) into a formula conf, such that the formula (phi &
-- conf) no longer allows the assignment asn.

-- Next, implement `dpllT`, which implements the overall SMT solver.


conflictClause :: [CNF.Lit a] -> CNF.CNF a
conflictClause = undefined

dpllT :: (Alternative f) => CNF (Exp String) -> f ()
dpllT = undefined

-- Check a given CNF for satisfiability. Returning
-- True of the cnf is satisfiable.
satSMT :: CNF (Exp String) -> Bool
satSMT = isJust . dpllT
