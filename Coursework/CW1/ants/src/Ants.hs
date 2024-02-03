module Ants where

--------------------------------------------------------------------------------
-- This file should contain your complete work for the first coursework of 
-- CS141 Functional Programming.
-- 
-- USER ID: u5508224
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------

import Ants.Types
import Hatch


-- | Uncomment these two lines once you get to Exercise 6.
-- import Data.Set (Set)
-- import qualified Data.Set as Set



{-| 
  Ex. 1: Implement a Show instance for LineState.

  (The pipe character and full block character are | and █)

  The function is implemented using a combination of string concatenation and the map function. The pipe symbols are concatenated before and after the 
  call to the map function. The map function will apply the stateToChar function to every element within the list and return a list with the outcomes.
  I have defined the stateToChar function which will take a CellState as a parameter and it will output the correct character depending on what the 
  CellState is. If the CellState is On then it will return '█', else if it is Off, it returns ' '. I used an if..then..else statement here as I think 
  that because there is only a possibilty of 2 outcomes, it is better than using a case..of or guard statement. I believe that a case..of statement is 
  better when there are multiple outcomes, and a guard statement is better when multiple conditions are being checked. I also think that using this 
  if..then..else statement is quite readable in this context. I also tried and solved this problem using a list comprehension. Instead of the 'map' 
  function, I had a list comprehension which embedded the if..then.else statement, and applied it to every element in the LineState list. This was 
  essentially the map function. This alternative approach can be seen commented below the code. I could have also used the stateToChar function 
  within the list comprehension. This can be seen commented below the code as the second approach. I also think that implementing the stateToChar 
  function explicitly is good, as it means that the function could be reused again at another point in time.
-}

stateToChar :: CellState -> Char
stateToChar cell = if cell == On then '█' else ' '

instance Show LineState where
  show :: LineState -> String
  show (LS lineState) = "|" ++ map stateToChar lineState ++"|"
  
  --show (LS lineState) = "|" ++ [if x == On then '█' else ' ' | x <- lineState] ++ "|"  -- ALTERNATIVE APPROACH
  --show (LS lineState) = "|" ++ [stateToChar x | x <- lineState] ++ "|"  -- ALTERNATIVE APPROACH 2


{-|
  Ex. 2: Implement ruleX, which turns a cell on if it has exactly one neighbour which was on in the previous step. The three arguments are the states of the left neighbour, current cell, and right neighbour respectively.

  This function is implemented using top-level pattern matching. I figured that this would be the most simple and readable approach compared to other appraches such as 
  case..of statements or any other type of branching statement. This is because it would mean that I have to store the 3 CellState paramaters in 3 separate
  variables, to then use within some branching statement. I think that this would be a waste of computation resources, and would lead to a potentially slower 
  output. In my implementation, I directly pattern match on the 3 CellStates. If the values are passed as On _ Off or Off _ On, where _ pattern matched to any value,
  The output is On. This is because we are only looking at the neighbours, and checking when either ONE of them are On. I then pattern match to _ _ _, (which is
  essentially any combination) and then output Off. I am able to do this as Haskell is an INTERPRETED language which means that it will execute the code in a top-down approach.
-}

ruleX :: CellState -> CellState -> CellState -> CellState
ruleX On _ Off = On
ruleX Off _ On = On
ruleX _ _ _ = Off

{-|
  Ex. 3: Implement applyRule, which, given a rule and a line state, applies the rule to each cell in the state and returns the updated state.

  This function is implemented using explicit recursion. 
-}

applyRule :: (CellState -> CellState -> CellState -> CellState) 
          -> LineState 
          -> LineState
applyRule ruleFunction (LS lineState) = LS (applyRuleToCellStates ([Off] ++ lineState ++ [Off]))
  where
    applyRuleToCellStates :: [CellState] -> [CellState]
    applyRuleToCellStates [] = []
    applyRuleToCellStates (x0:x1:x2:xs) = ruleFunction x0 x1 x2 : applyRuleToCellStates (x1:x2:xs)
    applyRuleToCellStates _ = []

{-|
  Ex. 4: Implement the loopedAt function, which takes a rule and a starting configuration, and returns the number of the iteration at which the automaton first revisits a state.

  [JUSTIFY]
-}
loopedAt :: (CellState -> CellState -> CellState -> CellState)
  -> LineState
  -> Int
loopedAt = error "Not implemented"



{-|
  Ex. 5: Implement allRules, which returns all 256 possible rules.

  [JUSTIFY]
-}
allRules :: [ CellState -> CellState -> CellState -> CellState ]
allRules = error "Not implemented"

{-|
  Ex. 6: Implement initialState, which returns the initial configuration of Langton's Ant.
-}
initialState :: AntState
initialState = error "Not implemented"



{-|
  Ex. 7: Define the functions leftOf and rightOf, which return the direction to the left and right of the given direction, respectively. Follow the additional constraints given in the specification, and answer the written question here.

  [JUSTIFY]

-}
leftOf :: Direction -> Direction
leftOf = error "Not implemented"

rightOf :: Direction -> Direction
rightOf = error "Not implemented"



{-|
  Ex. 8: Implement the step function, which takes the ant state and applies the logic given in the specification to return the new ant state.

  [JUSTIFY]
-}
step :: AntState -> AntState
step = error "Not implemented"



{-|
  Ex. 9: Visualise the behaviour of Langton's Ant by implementing the "animation" function. It takes a number (the step) and you must return a picture that represents the state of the automaton at that step.

  There are no tests for this. You can use `stack run` to test your implementation.

  [JUSTIFY]
-}
animation :: Int -> Image
animation = error "Not implemented"