module DPLLSpec
  ( tests
  , checkAssign
  , satAssign
  , chkValidAsn
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import CNF

import qualified DPLL
import Parser (parse)
import qualified Tseitin

sat :: String -> Bool
sat = DPLL.satisfiable . Tseitin.equisat . parse

satAssign :: String -> [Lit Tseitin.ID]
satAssign = snd . DPLL.satAssign . Tseitin.equisat . parse

-- ^ Check if a formula is satisfiable under the assignment.
checkAssign :: (Eq a) => CNF a -> [Lit a] -> Bool
checkAssign (And os) asn = and $ map (checkAssignOr asn) os

checkAssignOr :: (Eq a) => [Lit a] -> (Or a) -> Bool
checkAssignOr asn (Or ls) = any (isValid asn) ls

isValid :: (Eq a) => [Lit a] -> Lit a ->  Bool
isValid ls l@(Lit _) = l `elem` ls
isValid ls l@(Neg _) = not (l `elem` ls)

chkValidAsn :: String -> [Lit Tseitin.ID] -> Bool
chkValidAsn s asn = checkAssign phi asn  
  where
    phi = Tseitin.equisat $ parse s

chkValid :: String -> Bool
chkValid s = checkAssign phi asn  
    where
      phi = Tseitin.equisat $ parse s
      asn = DPLL.satAssignStr s

tests :: Rubric
tests = do
  criterion "resolve" 0.2 . passOrFail $ do
    it "removes Or if it contained the literal" $ do
      DPLL.resolve (Lit 0) (And []) @?= And [] 
      DPLL.resolve (Lit 0) (And [Or [Lit 0]]) @?= And [] 
      DPLL.resolve (Lit 0) (And [Or [], Or [Lit 1, Lit 0, Lit 2], Or [Lit 2]]) @?= And [Or [], Or [Lit 2]]
    it "removes literal from Or if negation was contained" $ do
      DPLL.resolve (Lit 0) (And [Or [Neg 0]]) @?= And [Or []] 
      DPLL.resolve (Neg 2) (And [Or [], Or [Lit 1, Lit 0, Lit 2], Or [Lit 2]]) @?= And [Or [], Or [Lit 1, Lit 0], Or []]
  criterion "bcp" 0.15 . passOrFail $ do
    it "resolves all occurences of single literals" $ do
      DPLL.bcp (And [Or [Neg 0], Or [Lit 0, Lit 1, Lit 2]]) @?= And [Or [Lit 1, Lit 2]]
      DPLL.bcp (And [Or [Lit 0, Lit 1, Lit 2], Or [Lit 0]]) @?= And []
      DPLL.bcp (And [Or [Neg 0, Lit 1, Lit 2, Neg 3], Or [Lit 0], Or [Lit 3]]) @?= And [Or [Lit 1, Lit 2]]    
  criterion "ple" 0.15 . passOrFail $ do
    it "resolves all occurences of positive only literals" $ do
      DPLL.ple (And [Or [Neg 0], Or [Lit 0, Lit 1, Lit 2]]) @?= And []
      DPLL.ple (And [Or [Neg 0], Or [Lit 0], Or [Lit 2]]) @?= And [Or [Neg 0], Or [Lit 0]]
      DPLL.ple (And [Or [Neg 0], Or [Lit 0], Or [Lit 2, Lit 0], Or [Lit 1]]) @?= And [Or [Neg 0], Or [Lit 0]]
    it "resolves all occurences of negative only literals" $ do
      DPLL.ple (And [Or [Neg 0], Or [Lit 0, Lit 1, Neg 2]]) @?= And []
      DPLL.ple (And [Or [Neg 0], Or [Lit 0], Or [Neg 2]]) @?= And [Or [Neg 0], Or [Lit 0]]
      DPLL.ple (And [Or [Neg 0], Or [Lit 0], Or [Neg 2, Lit 0], Or [Neg 1]]) @?= And [Or [Neg 0], Or [Lit 0]]
  criterion "dpll" 0.35 . passOrFail $ do
    it "computes satisfiability" $ do
      sat "a | b" @?= True
      sat "a & -a" @?= False
      sat "b & (-b | a) & -a" @?= False
      sat "b & (b | a) & -a" @?= True
      sat "a & b & c & d & e & f & g & (-a | -b)" @?= False
      sat "a & -b & c & d & e & f & g & (-a | -b)" @?= True
      sat "(a | b | c) & (-a | -b | -c)" @?= True
      sat "(a | b | c) & (-a & (-b | -(a & c)) | -c)" @?= True
      sat "(a | b | c | d | e) & -a & -b & -c & -d & -e" @?= False
      sat "(x | y | z) & (x | y | -z) & (x | -y | z) & (x | -y | -z) & (-x | y | z) & (-x | y | -z) & (-x | -y | z) & (-x | -y | -z)" @?= False
      sat "(¬p ∨ q ∨ r ) ∧ (¬q ∨ r ) ∧ (¬q ∨ ¬r ) ∧ (p ∨ ¬q ∨ ¬r )" @?= True
      sat "(p ∨ ¬q)∧ (q ∨ ¬r) ∧ (r ∨ ¬p)" @?= True
      sat "(p ∧ ¬p)" @?= False
      sat "(p → (q → r )) ∧ ¬((p ∧ q) → r )" @?= False
      sat "(-b|a|-c)&(b|a|-c)&(-b|-a|-c)&(b)&(c)" @?= False
      sat " (x ∨ y) ∧ (¬x ∨ y) ∧ (¬y)" @?= False
      sat "(a ∨ b ∨ ¬c) ∧ (a ∨ c) ∧ (a ∨ ¬b) ∧ (¬a)" @?= False
      sat "x ∨ y → ¬x ∧ z" @?= True
      sat "(𝑥∨𝑦∨𝑧)∧(𝑥∨𝑦∨¬𝑧)∧(𝑥∨¬𝑦∨𝑧)∧(𝑥∨¬𝑦∨¬𝑧)∧(¬𝑥∨𝑦∨𝑧)∧(¬𝑥∨𝑦∨¬𝑧)∧(¬𝑥∨¬𝑦∨𝑧)∧(¬𝑥∨¬𝑦∨¬𝑧)" @?= False
      sat "(x ∨ y)&(¬y∨z∨a)&(¬x∨a)" @?= True
  criterion "dpll: return assignment" 0.15 . passOrFail $ do
    it "returns a valid assignment" $ do
      chkValid "a | b" @?= True
      chkValid "b & (-b | a) & -a" @?= True
      chkValid "b & (b | a) & -a" @?= True
      chkValid "a & -b & c & d & e & f & g & (-a | -b)" @?= True
      chkValid "(a | b | c) & (-a | -b | -c)" @?= True
      chkValid "(a | b | c) & (-a & (-b | -(a & c)) | -c)" @?= True
      chkValid "(¬p ∨ q ∨ r ) ∧ (¬q ∨ r ) ∧ (¬q ∨ ¬r ) ∧ (p ∨ ¬q ∨ ¬r )" @?= True
      chkValid "(p ∨ ¬q)∧ (q ∨ ¬r) ∧ (r ∨ ¬p)" @?= True
      chkValid "x ∨ y → ¬x ∧ z" @?= True
      chkValid "(x ∨ y)&(¬y∨z∨a)&(¬x∨a)" @?= True
      
