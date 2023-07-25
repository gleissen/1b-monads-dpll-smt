{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module LRA
  (Exp(..),
    solve
  , isLinCons
  , isConstraint
  , isLinear
  , addIDs
  , toSimplex
  , fromProp
  , solveString
  ) where

import Linear.Simplex.Types as Sim
import Linear.Simplex.Types
import Linear.Simplex.Simplex (findFeasibleSolution)
--import Prop
import Tseitin (addID, ID)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Numeric.Natural
import CNF (CNF)
import CNF as CNF
import Prop (Prop)
import qualified Prop
import qualified GHC.TypeLits as VarConstMap

infixl 7 :*:  -- times
infixl 6 :+:  -- plus
infixl 5 :<=: -- less or equal
infixl 5 :>=: -- less or equal
infixl 5 :=:  -- equal

-- ^ Our data-type for arithmetic expressions.
data Exp a = Var a
    | Const Int
    | Exp a :*: Exp a
    | Exp a :+: Exp a
    | Minus (Exp a)
    | Exp a :<=: Exp a
    | Exp a :>=: Exp a
    | Exp a :=: Exp a
  deriving (Eq, Functor, Foldable, Traversable, Show)

-- Scaffolding: Our grammar allows us to express non-linear arithmetic
-- constraints. Our solver can only handle linear constraints, though. Here, we
-- check whether a given formula is linear. 
isConst :: Exp a -> Bool
isConst (Const _) = True
isConst (Minus e) = isConst e
isConst _ = False

isVar :: Exp a -> Bool
isVar (Var _) = True
isVar (Minus v) = isVar v
isVar _ = False

isFactor :: Exp a -> Bool
isFactor (Var x)     = True
isFactor (e1 :*: e2) = (isConst e1) && (isVar e2)
isFactor _           = False

isLinear :: Exp a -> Bool
isLinear (Var x)   = True
isLinear (Const c) = True
isLinear e@(_ :*: _) = isFactor e
isLinear (e1 :+: e2) = (isFactor e1) && (isLinear e2)
isLinear (Minus e) =  (isLinear e)
isLinear (e1 :<=: e2) = (isLinear e1) && isConst e2
isLinear (e1 :>=: e2) = (isLinear e1) && isConst e2
isLinear (e1 :=: e2) = (isLinear e1) && isConst e2

isConstraint :: Exp a -> Bool
isConstraint (_ :<=: _) = True
isConstraint (_ :>=: _) = True
isConstraint (_ :=: _) = True
isConstraint _          = False

isLinCons :: Exp a -> Bool
isLinCons e = isConstraint e && isLinear e

-- Transform a formula into the solver format, if it's linear.
-- You will implement function toSimCons.

toSimplex :: Exp ID -> Maybe PolyConstraint
toSimplex e
    | not (isLinCons e) = Nothing
    | otherwise           = Just $ toSimCons e


-- Next, we want to turn our SAT solver into an SMT solver over the theory of
-- linear rational arithmetics (LRA). For this, we first need a theory solver.
-- As we didn't cover this in the lecture, we won't implement it ourselves.
-- Instead, we'll use the following existing solver, using the Simplex
-- algorithm.
--
-- https://hackage.haskell.org/package/simplex-method-0.1.0.0/docs/Linear-Simplex-Simplex.html
--
-- The main method we'll use is `findFeasibleSolution` which takes a set of
-- constraints and determines whether they are satisfiable. Constraints are
-- represented using the data-type PolyConstraint:
-- https://hackage.haskell.org/package/simplex-method-0.1.0.0/docs/Linear-Simplex-Types.html#t:PolyConstraint
-- Your task is to implement a function toSimCons, which takes a linear
-- arithmetic constraint, i.e., an expression e that satisfies isConstraint e,
-- and turn it into a PolyConstraint. In particular, this involves transforming
-- an arithmetic expression consisting of variables and their coefficients,
-- e.g., 3*x+y+z, into a VarConstMap.
-- https://hackage.haskell.org/package/simplex-method-0.1.0.0/docs/Linear-Simplex-Types.html#t:VarConstMap.(*)
-- Functions fromIntegral and toRational might help you with this. Look them up using Hoogle!


-- ^ TODO: Implement this. 
toSimCons :: Exp ID -> PolyConstraint
toSimCons = undefined


-----------------------
-- ^ Scaffolding code.
-----------------------

fromProp :: Prop (Exp a) -> [Exp a]
fromProp (e1 Prop.:&: e2) = (fromProp e1) ++ (fromProp e2)
fromProp (Prop.Lit e) = [e]


fromLit :: CNF.Lit (Exp a) -> Exp a
fromLit (CNF.Lit e) = e
fromLit (CNF.Neg e) = e


-- ^ Variables are over strings, but we need numbers.

addIDExp :: ID -> Map String ID -> Exp String -> (Map String ID, Exp ID, ID)
addIDExp id m e = (m', e', id')
    where
      (e', (id', m')) = runState (mapM addID e) (id, m)

addIDProps :: [Exp String] -> State (Map String ID, ID) [Exp ID]
addIDProps [] = return []
addIDProps (e:es) = do
              (m, id) <- get
              let (m', e', id') = addIDExp id m e
              put (m', id')
              es' <- addIDProps es
              return $ (e':es')

addIDs :: [Exp String] -> [Exp ID]
addIDs es = evalState (addIDProps es) (Map.empty, 0)

solveExp :: [Exp ID] -> Maybe ()
solveExp es = do
    cs <- mapM toSimplex es
    res <- findFeasibleSolution cs
    return $ const () res

solveString :: [Exp String] -> Maybe ()
solveString es = solveExp esID
  where
    esID = addIDs es

solve :: [Lit (Exp String)] ->  Maybe ()
solve ls = solveExp esID
  where
    es = map fromLit ls
    esID = addIDs es