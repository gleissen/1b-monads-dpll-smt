{-# LANGUAGE FlexibleContexts #-}

module Tseitin
  ( equisat
  , fresh
  , addID
  , ID
  ) where

import Prelude hiding (id)

import Numeric.Natural
import Control.Monad.State
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as Map

import Prop
import CNF (CNF)
import qualified CNF as CNF

-- An identifier for variables for which we can easily generate fresh variables.
type ID = Natural

-- Use this to get a fresh propositional literal.
fresh :: MonadState ID m => m (Prop ID)
fresh = do
  id <- get
  put $ id + 1
  return $ Lit id

-- Tseitins transformation: Transforms a propositional formula into an
-- equisatisfiable CNF formula while avoiding the exponential space blow-up that
-- happens with the normal CNF transformation.
--
-- NOTICE: In Tseitin's transformation, we traverse the propositional formula
-- and introduce new names and definitions for the subexpressions. Your task is
-- to implement this transformation here. Please revisit the lecture slides and
-- make sure you thoroughly understand Tseitin's transformation before
-- attempting to implement it.
--
-- Why does `tseitin` return CNF instead of Prop?
-- - CNF is a rigid form of Prop; it can only ever represent formulas in CNF.
--   Prop may represent formulas in CNF, but also other formulas. Using the
--   rigid form allows us to avoid runtime checks to see whether a Prop is in
--   CNF, as needed for DPLL.
-- 
-- What are the Monad typeclasses used in the definition of tseitin?
-- - `tseitin` uses both the State and Writer interfaces. This means that we can
--   use `put`, `get` and `tell`.
--
-- What do we need these for?
-- - State: You don't need to modify the state (using put/get) directly. The
--   state is used to implement the `fresh` helper function, above. The `fresh`
--   helper function returns a fresh literal. You can use this to create new
--   names and definitions for subformulas.
-- - Writer: The writer is used to *accumulate* the final outcome of the tseitin
--   transformation. That is, whenver you created a definition for a subformula,
--   you can use `tell` to add it to the final CNF. Use `CNF.rigid` to transform
--   a general propositional formula into a rigid CNF formula. 
--
-- Warning: This step uses your implementation of cnf, it may crash if your
--   implementation is incorrect.
--
-- What is the return value of tseitin?
-- - The return value is a (Prop ID), which should always be the propositional
--   variable used to represent the current formula.
-- - We don't introduce new names for literals, so you may directly return a
--   literal when encountered.

-- TODO: Implement the tseitin function below.

tseitin :: (MonadState ID m, MonadWriter (CNF ID) m) 
        => Prop ID -> m (Prop ID)
tseitin = undefined




-- ^ Scaffolding code not needed for your implementation. Get an equisatisfiable
-- CNF by using tseitins transformation.
equisat :: Ord a => Prop a -> CNF ID
equisat p = flip evalState 0 $ do
  (q, _) <- addIDs p 
  (expr, ands) <- runWriterT $ tseitin q
  return $ ands <> CNF.rigid expr

-- ^ Scaffolding code not needed for your implementation. It adds IDs for
-- literals. Literals with the same name get the same IDs.
addID :: (MonadState (ID, Map a ID) m, Ord a) => a -> m ID
addID e = do
  (id, m) <- get
  case Map.lookup e m of
    Just id' -> return id'
    Nothing -> do 
      let id' = id + 1
      let m' = Map.insert e id m
      put (id', m')
      return id

-- This is scaffolding code not needed for your implementation. It adds IDs for
-- all literals. 
addIDs :: (MonadState ID m, Ord a) => Prop a -> m (Prop ID, Map ID a)
addIDs p = do
  id <- get
  let (p', (id', m)) = runState (mapM addID p) (id, Map.empty)
  put id'
  let inverse = Map.foldrWithKey (flip Map.insert) Map.empty m
  return (p', inverse)