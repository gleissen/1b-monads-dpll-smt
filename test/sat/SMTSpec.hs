module SMTSpec
  ( tests
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import CNF

import qualified SMT
import qualified LRA
import Data.Maybe
import LRAParser (parseLRA)

satSMT :: String -> Bool
satSMT = SMT.satSMT . CNF.rigid . parseLRA

solveLRA :: String -> Bool
solveLRA = isJust . LRA.solveString . LRA.fromProp . parseLRA

tests :: Rubric
tests = do
  criterion "solveExp" 0.4 . passOrFail $ do
    it "handles smaller or equal" $ do
      solveLRA "1*x + 1*y <= 3" @?= True
      solveLRA "x + y <= 3" @?= True
      solveLRA "x + y <= 3 & y <= 4" @?= True
    it "handles larger or equal" $ do
      solveLRA "x >= 1" @?= True
      solveLRA "x >= 1 & y>=1" @?= True
      solveLRA "x>=1" @?= True
      solveLRA "x+y>=0" @?= True
    it "handles equal" $ do
      solveLRA "x + (-z) = 0" @?= True
      solveLRA "x + (-z) = 0 & z+(-x)>=1" @?= False
    it "handles complex constaints" $ do  
      solveLRA "x + (-z) = 0 & z+(-x)<=1" @?= True
      solveLRA "x + (-z) = 0 & z+(-x)<= (-1)" @?= False
      solveLRA "x+y<=1 & x>=1 & y>=1" @?= False   
  criterion "dpllT" 0.6 . passOrFail $ do
    it "solves satisfiable constrants without blocking" $ do  
      satSMT "x>=1 & y>=1" @?= True
      satSMT "(x>=1 | y>=1)" @?= True     
      satSMT "x=1 | y=2" @?= True
      satSMT "x=1 & y=2 & z=3 | w=4" @?= True
      satSMT "(x>=1 & y<=2) | (z<=3 & w>=4)" @?= True
      satSMT "(x+y>=1 & z+w<=2) | (a+(-b)>=3 & c+d<=4)" @?= True
    it "solves satisfiable constrants with blocking" $ do    
      satSMT "x+(-y)=0 & (x+(-y)>=1 | y=0)" @?= True
      satSMT "x+(-y)=0 & (y=0 | x+(-y)>=1)" @?= True 
      satSMT "(x>=1 & y<=1) | (x<=1 & y>=1)" @?= True
      satSMT "(x>=1 & y<=1) & !(x<=1 & y>=1)" @?= True
      satSMT "(x=1 | y=1) & !(x=1 & y=1)" @?= True
    it "handles implications correctly" $ do  
      satSMT "(x>=1) → y>=1" @?= True
      satSMT "(x<=0) → (y<=0 & x>=0 & y>=1)" @?= True
    it "recognizes unsatisfiable constrants" $ do
      satSMT "(x>=1 & x<=0)" @?= False
      satSMT "(x>=3 & (x<=0 | x<=1))" @?= False
      satSMT "(x>=1 & y<=0) & (y>=1 & x<=0)" @?= False
    it "solves complex constaints" $ do  
      satSMT "x+(-z)=0 ∧ ((y+(-z)=0 ∧ x+(-z)<= (-1)) ∨ ¬(x+(-z)=0))" @?= False
      satSMT "(x+y=1 | z+w=2) & !(x+y=2 & z+w=1)" @?= True
      satSMT "((x = 0) ∨ ¬(x = 1) ∨ z=1) ∧ ((x = 0) ∨ (x = 1) ∨ (a=1)) ∧ (¬(x = 0) ∨ (x = 1) ∨ (a=1)) ∧ (¬(a=1) ∨ (y = 1)) ∧ (¬(z=1) ∨ (x + y >= 4)) ∧ (y <= (-1)) ∧ ((a=1) ∨ (x+(-y)=4)) ∧ ((y = 2) ∨ ¬(z=1)) ∧ (x >= 0)" @?= False
      
