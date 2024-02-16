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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bits -- Added this for Ex5

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
stateToChar cell = if cell == On
                   then '█'
                   else ' '

instance Show LineState where
  show :: LineState -> String
  show (LS lineState) = "|" ++ map stateToChar lineState ++"|"

  --show (LS lineState) = "|" ++ [if x == On then '█' else ' ' | x <- lineState] ++ "|"  -- ALTERNATIVE APPROACH
  --show (LS lineState) = "|" ++ [stateToChar x | x <- lineState] ++ "|"  -- ALTERNATIVE APPROACH 2

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-|
  Ex. 3: Implement applyRule, which, given a rule and a line state, applies the rule to each cell in the state and returns the updated state.

  This function is implemented using explicit recursion. One of the difficulties of this particular problem was evaluating the edge cases. This is because the type of the
  input "ruleFunction" tells us that it needs 3 input parameters of the type "CellState" and it will output a "CellState". This meant that when evaluating the first and last
  elements of the "LineState", we would not have one of the neighboring cells. As stated in the coursework sheet, it can be assumed that all "CellStates" which are outside the
  automaton can be assumed to be "OFF". Using this information, I could resolve the issue by concatenating [Off] to the start and the end of the input "lineState". I then 
  applied this expression to a function called "applyRuleToCellStates", which applies the given "ruleFunction" to every ADJACENT 3 elements in the list. As we now are dealing
  with the CellStates directly, the type of "applyRuleToCellStates" is [CellState] -> [CellState] and NOT "LineState". Using the cons operator ":", I take the first 3 elements
  of the list "x0,x1,x2" and aaply "ruleFunction" to these 3 elements. I then use the cons opertator ":" again to add append the output to the front of the output list. The rest
  of the list is then generated recursivly by applying "applyRuleToCellStates" to x2,x3 and x3 (the remaining part of the list). Even though I used xs (of type [CellState]) as 
  the 3rd paramter of this function call (instead of just CellState), Haskell is able to recognise this and automatically take another value from "xs" and pass it to "x2". The 
  output of this recursion is then parsed from [CellState] -> LineState by using the LS keyword, and then outputted. I have also written a base case for the function which 
  patterm matches anything to an empty list []. Also, I noticed that the function has a nested bracket, and so it could be simplified further using the $ operator (also given 
  that the function is RIGHT associative). 
-}

applyRule :: (CellState -> CellState -> CellState -> CellState) -> LineState -> LineState

applyRule ruleFunction (LS lineState) = LS $ applyRuleToCellStates ([Off] ++ lineState ++ [Off])
--applyRule ruleFunction (LS lineState) = LS (applyRuleToCellStates ([Off] ++ lineState ++ [Off])) --Without using $ operator.
  where
    applyRuleToCellStates :: [CellState] -> [CellState]
    applyRuleToCellStates (x0:x1:x2:xs) = ruleFunction x0 x1 x2 : applyRuleToCellStates (x1:x2:xs) --"xs will get converted to x2:xs in next call"
    applyRuleToCellStates _ = []

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-|
  Ex. 4: Implement the loopedAt function, which takes a rule and a starting configuration, and returns the number of the iterations at which the automaton first revisits a state.

  This function has been implemented using implicit recursion. This means that it is utilising a helper function "g" to compute the number of iteratioms at which the 
  automaton first revisits a state. The function calls this function "g", but with the input LineState now in a LIST of LineStates. It also takes in an extra integer
  parameter which is the count, and is initally called with this being 1. The function "g" then takes this list of LineStates, and safely takes the latest added lineState
  in the list using "safeHead" and applies the rule it. This new linestate which is generated as a result is then checked against every linestate currently present in the list, and if it
  matches to any lineState, it will return the current count value. I do this by using the "elem" function. If it does not match, then no state has been revisited, and so 
  it will recursively call "g" with the new state added to the current list and the count is incremented. The reason I store every state in a list, is because it is not 
  always the case that a state loops "consectutively", as it can also be the case that the states loop with a period of 3. It means we cannot only check the previously 
  generated state with the current one and we must check against all of the stored values. I had initally assumed this, and have the code for this below commented out. The 
  only reason that code passes the tests, is because it is tested with a rule which produces a short, consectutive loop however it would not work for longer loops. Also, 
  when I remove a lineState from the list using the "safeHead" function I could have also just used the built in "head" function, as it can be guaranteed that the function 
  would not be called with an empty list. This is because even if the input lineState is empty, it will be added to another list giving [[]], and calling head on this gives []
  so it would not give an error. However, I still implemented "safeHead", as it is better code practice and it makes the code resillient to future changes. 
-}

safeHead :: [LineState] -> LineState
safeHead [] = LS [Off]
safeHead list = head list

loopedAt :: (CellState -> CellState -> CellState -> CellState) -> LineState -> Int

loopedAt ruleFunction (LS lineState) = g [LS lineState] 1
  where
    g :: [LineState] -> Int -> Int
    g states count =
      let
        newState = applyRule ruleFunction (safeHead states)
      in
        if newState `elem` states
        then count
        else g (newState:states) (count+1)

{-
loopedAt ruleFunction (LS lineState) = f (LS lineState) 1  --Old implementation (only works for short, consectutive loops.)
  where
    f :: LineState -> Int -> Int
    f state count =
      let
        newState = applyRule ruleFunction state
      in
        if newState == state
        then count
        else f newState (count + 1)
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-|
  Ex. 5: Implement allRules, which returns all 256 possible rules.

  This function has been implemented using bitwise operations and a list comprehension. Initially, my first implementation (commented below as IMPLEMENTATION 1), used a combination
  of pattern matching and modular arithmetic to return all of the 256 possible rules. I first made a list comprehension, which will call a helper function a total of 256 times. Of course,
  it does not only call the function, but it also stores its returned output in the list which I am building. This return type is a function which is our rule. The helper function which 
  the list comprehension calls was called "getAllStates", which took an input integer, which represents the number of the rule we wish to generate. This value is passed in from the list 
  comprehension. I then built the output function using PATTERN MATCHING. This means that I wrote out each of the 8 cases individually that the rule can take as its input 3 Cellstates, ie 
  (Off, Off, Off), (Off, Off, On),..., (On, On, On). Each state has 8 cases, thus why when each case has 2 possible outcomes we have 2^8 = 256 total possible rules. Each of the 8 pattern 
  matching statements then map each case in a state to a binary value - this is the easiest way to iterate through all of the outcomes systematically by enumerating all the binary values. 
  For example look at the following table which demonstrates how each rule is constructed:

                            128             64                32               16               8                  4                2                  1              (Binary Place Value)
      Rule:              On On On        On On Off        On Off On        On Off Off        Off On On        Off On Off        Off Off On        Off Off Off         (8 Possible cases)

        0                  Off              Off              Off              Off              Off                Off              Off                Off          
        1                  Off              Off              Off              Off              Off                Off              Off                On     
       ...                 ...              ...              ...              ...              ...                ...              ...                ...
       255                 On               On               On               On                On                On               On                 On        
  
  Where On would be 1 and Off is 0 in our binary conversion. To tackle this binary conversion, I used a second helper function called getOnOff which took the ruleNumber and the particular 
  binary place value, and it would calculate whether that particular case outputs On or Off where On = 1 and Off = 0. The method of this calculation is using MODULAR ARITHMETIC. I first take
  the whole number division of the ruleNumber and the binary place value for that case. I then MOD this with 2, which gives me either 0 for Off or 1 for On. Finally, I utilised an IF..THEN..ELSE
  statement (as there are only 2 outcomes), to evaluate this value and return either On or Off. This was my first implementation. My second implementation (IMPLEMENTATION 2) then aimed to improve 
  this first implementation by removing the 8 pattern matching statements, as this is in my opinion not a very good way of returning a particular function. This is because, suppose instead of 8 
  cases we had 100 - we would be writing 100 pattern matching cases out manually! To improve this aspect of the implementation, I took advantage of the fact that Haskell curries functions and 
  allows partial application. This means that I have a sort of "lambda" function where the "getAllStates" helper function takes the ruleNumber AND "some" state1, state2, state3. This clever 
  syntax allowed me to remove having to pattern match all the cases which makes my code much more resilient to any future code changes such as a change in the number of cases. However, the removal
  of this pattern matching meant that I no longer had my hard-coded binary place values and needed to calculate this by itself. To do this, I first manually convert the case sequence into its decimal
  equivalent - For example, if evaluating the case On Off On => This is 1 0 1 => Which is (2^2)*1 + (2^1)*0 + (2^0)*1 => Leading to 5. I then take this value and raise it onto the power of 2. This 
  leads to 2^5 = 32. I now know that the binary place value of On Off On is 32. I also use a new small helper function called "cellStateToInt" which converts the CellState into its Int format, so I 
  can do the calculation. Now, instead of using the MODULAR ARITHMETIC method, I used the (.&.) from Data.Bits which computes the BITWISE AND of both values. I realised that if the bitwase AND of 
  the ruleNumber and the binary place value outputs a value which is equal to 0, then the CellState is Off, else it is On. This concludes my second implementation. In my third implementation 
  (IMPLEMENTATION 3), I realised that I can use the "shiftL" function from the Data.Bits to compute the binary place value. Previously, we did 2^(the decimal equivalent of case) to retrieve this value.
  The "shiftL" function shifts the digit 1 a number of times to the left, where this number is the decimal equivalent of the cases. This, in my opinion is faster as bitwise operations tend to be quicker
  than an arithmetic operation, having to multiply 2 many times. For example 2^5 is now the binary shift of 1.0, 5 spaces to the left which gives 100000 = 32. This concludes my third implementation. My
  fouth and final implementation, further refines this, by using an (nearly) equivalent function "testBit" from Data.Bits which has the same function as evaluating (a .&. bit b) /= 0. The slight difference
  here is that the "testBit" function has a /= comparator and it converts the value into a bit automatically. This means I can remove the "shiftL" property, as this is done automatically. I also need to 
  switch the output 2 values as shown below as previously I used == whereas "testBit" uses /=.
-}

allRules :: [CellState -> CellState -> CellState -> CellState]
allRules = [getAllStates x | x <- [0..255]]
  where
    getAllStates :: Int -> (CellState -> CellState -> CellState -> CellState)
    getAllStates x state1 state2 state3 =
      if testBit x (4*cellStateToInt state1 + 2*cellStateToInt state2 + cellStateToInt state3)
      then On     --These 2 values flipped.
      else Off

    cellStateToInt :: CellState -> Int
    cellStateToInt On  = 1
    cellStateToInt Off = 0


{-}
allRules :: [CellState -> CellState -> CellState -> CellState]                                            --IMPLEMENTATION 3, FURTHER OPTIMISATION
allRules = [getAllStates x | x <- [0..255]]
  where
    getAllStates :: Int -> (CellState -> CellState -> CellState -> CellState)
    getAllStates x state1 state2 state3 =
      if x .&. shiftL 1 (4*cellStateToInt state1 + 2*cellStateToInt state2  + cellStateToInt state3) == 0 
      then Off
      else On

    cellStateToInt :: CellState -> Int
    cellStateToInt On  = 1
    cellStateToInt Off = 0
-}

{-
allRules :: [CellState -> CellState -> CellState -> CellState]                                            --IMPLEMENTATION 2 REMOVES PATTERN MATCHING and MODULAR ARITHMETIC method
allRules = [getAllStates x | x <- [0..255]]
  where
    getAllStates :: Int -> (CellState -> CellState -> CellState -> CellState)
    getAllStates x state1 state2 state3 =
      if x .&. 2^(4*cellStateToInt state1 + 2*cellStateToInt state2  + 1*cellStateToInt state3) == 0     -- Use bitwise AND for calculation.
      then Off
      else On

    cellStateToInt :: CellState -> Int
    cellStateToInt On  = 1
    cellStateToInt Off = 0
-}

{-}
allRules :: [CellState -> CellState -> CellState -> CellState]                                            --IMPLEMENTATION 1 USING PATTERN MATCHING + MODULAR ARITHMETIC
allRules = [ getAllStates x | x <- [0..255]] --List comprehension to build up all of the rules.
  where
    getAllStates :: Int -> (CellState -> CellState -> CellState -> CellState)
    getAllStates x Off Off Off  = getOnOff x 1 --Pattern match to all 8 cases.
    getAllStates x Off Off On   = getOnOff x 2
    getAllStates x Off On Off   = getOnOff x 4
    getAllStates x Off On On    = getOnOff x 8
    getAllStates x On Off Off   = getOnOff x 16
    getAllStates x On Off On    = getOnOff x 32
    getAllStates x On On Off    = getOnOff x 64
    getAllStates x On On On     = getOnOff x 128

    getOnOff :: Int -> Int -> CellState
    getOnOff x divisor =
      if ((x `div` divisor) `mod` 2) == 1   --Convert to binary and then back to a CellState
      then On
      else Off
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-|
  Ex. 6: Implement initialState, which returns the initial configuration of Langton's Ant.

  This expression initialises the AntState by using the structure defined in the Types.hs file. The direction is set to West, the position is set to (0,0) and the set is defined
  to be empty using the Set.empty function. I could have also have used (Set.fromList []) instead of the Set.empty. I found this out using Hoogle. 
-}

initialState :: AntState
initialState = AS West (0,0) Set.empty
--initialState = AS West (0,0) (Set.fromList [])  --Alternative

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-|
  Ex. 7: Define the functions leftOf and rightOf, which return the direction to the left and right of the given direction, respectively. Follow the additional constraints given in the specification, and answer the written question here.

  For the "leftOf" function, my implementation is to use a list of mappings using a list. The filter function is first used to filter the list for the pairs in which the first
  element matches the input direction. This pair output is then applied to "head" which returns the pair itself as the output of the filter function is [(x,y)] not (x,y). I 
  assume that it is safe to use "head" here as we have covered every case of the "Direction" data type and will not get an empty list. Now that I have the correct pair, this is
  then applied to the "snd" function which returns the second element in the pair, returning the value we need. In my implementation for rightOf, I use top-level pattern matching 
  which outputs the correct direction given an input. In mt opinion, in this case, I belive that the top-level pattern matching method is better, as it is readable and simple
  to understand and code. However, if there were to be a wider range of inputs and outputs, this method may not be very practical, as top level pattern matching for lots of cases 
  can be quite long. In such cases, this list method may be better.
-}

leftOf :: Direction -> Direction
leftOf dir = snd $  head $ filter (\(d, _) -> d == dir) directionMappings
  where
    directionMappings = [(North, West),(East, North),(South, East),(West, South)]

rightOf :: Direction -> Direction
rightOf North = East
rightOf East  = South
rightOf South = West
rightOf West  = North

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-|
  Ex. 8: Implement the step function, which takes the ant state and applies the logic given in the specification to return the new ant state.

As there are only two outcomes - either it is on a black or white square - I used an If..Then..Else statement to solve this problem. I first needed to check whtether the current
position is black or white. To do this, I implemented a checkIfMember which takes the current position and set, and then utilises the Set.member function which returns a boolean
value for whether the given position is in the set or not. I then needed to calculate the new position given the new direction that the ant will walk in, after calling either leftOf
or rightOf with its previous direction. I created a function called getNewPosition to achieve this. As we are comparing multiple outcomes here, I utilise a case..of statement which 
will increment/decrement the x and y values depending on the new direction. I could have also merged both of these functions into one, however I did not do this as I think that by
creating separate functions, it makes my code more readable, modular and robust. It also means that the function could be used else where. The final step was to then change the 
color of the square that it was on. I did this by using the Set.delete and Set.insert methods which removes a position from the set if it needs to be changed to white, or adds a 
position to the set if it needs to be changed to black. Also, as we return an AnstState, we have AS at the beginning of the expression. 
-}

checkIfMember :: (Int, Int) -> Set(Int, Int) -> Bool        --Returns true if position is in the Set and false otherwise
checkIfMember = Set.member

getNewPosition :: (Int, Int) -> Direction -> (Int, Int)     --Returns new position after moviing in the given direction.
getNewPosition (x,y) direction =
  case direction of
    North -> (x,y+1)
    East -> (x+1,y)
    South -> (x,y-1)
    West -> (x-1,y)

step :: AntState -> AntState
step (AS direction position set) =
  if checkIfMember position set   --check if the current position is in the coordinations.
  then AS (leftOf direction) (getNewPosition position (leftOf direction)) (Set.delete position set)   --Delete the position from the set as we turn it white
  else AS (rightOf direction) (getNewPosition position (rightOf direction)) (Set.insert position set) --Add the position to the set as we turn it black

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-|
  Ex. 9: Visualise the behaviour of Langton's Ant by implementing the "animation" function. It takes a number (the step) and you must return a picture that represents the state of the automaton at that step.

  There are no tests for this. You can use `stack run` to test your implementation.

  In my implementation for Ex9, I have illustrated Langtons Ant. To achieve this, I used a combination of implicit recursion, pattern matching and function chaining. 
  I first call a helper function which will call the render function n times as it means that n black squares need to be drawn. The iterateFrames helper function uses
  implicit recursion and has a base case of the 0th frame, for when no more squares need to be drawn for that iteration. If it has not reached the base case, it recursively 
  calls it self with the frames value being decremented, and the new state which is generated from calling the "step" function above with the current state. The new state is
  then passed as a parameter to the "renderAntState" function which will draw the blackSqrares of the cellular automaton. To do this, I take the set which contains the 
  coordinates of the black squares and recursively add each one on starting from a blank screen. The <@> operator appends the new square to the existing screen. When there are
  no more coordinates in the set left to draw, the base case is reached and the current screen is returned. When a coordinate is being added to the screen a rectangle of length 
  and width 1 is created and is translated/offsetted by the respective x,y coordinates from the set. This "offset" function is used from its definition in Transforms.hs. The downside
  to this function implementation is that to compute a particular frame, it must recompute all of the frames before it. For example, if we were to compute the 1000th frame, 
  the function will compute the last 999 frames in order to get to the 1000th frame - which is EXTREMELY space inefficient. I considered using a recursive method to pass in the 
  previous frame as a parameter so it will only need to compute once per frame, however I soon realised that I could not do this as whenever the animation function is called, 
  the function loses all the information from its previous frame. For example, when doing animation 10, we do not have the AntState at frame 9 as the function is called 
  independently, which means I would need to recompute all of that information. This concludes ths first implementation.

  In my second, implementation I eleminate using the "iterateFrames" method from the first method by utilising a combination of the "iterate" function and the (!!) index
  operator. The iterate function will return an infinite list of recursive applications of the step function to the initial state. Something like [step(initialState), step(step(initialState))...].
  This infinite list is stored as "allAntStates". In the "animation" function I then use the (!!) index operator to select the antstate at the index "frame". Now, we only are only
  having to compute the value of a particular AntState once, and this is stored in allAntStates. Although this may be memory intensive, as we store the values of every AntState, we still are saving 
  computation power in the sense that we do not have to RECOMPUTE EVERY AntState EVERY frame as we are just accessing the AntState using an index instead. I also altered the "renderAntState" 
  function to now use the "foldr" function to recursively render all of the black squares onto the screen. In other words foldl will do something like this: renderAnt(renderAnt(blank, set[0]), set[1])...
  Of course, we cannot access set elements just by doing set[0], however this was for demonstrational purposes. Making this change makes the code shorter, cleaner and more robust. The helper function
  "renderAnt" is responsible for rendering the squares onto the screen. This concludes my second implementation.

  Overall, both implementations are memory intensive, the first implementation is speed and memory inefficient, as we recompute every AntState every time the animation function is called. However, 
  once recomputed, the call stack of the AntStates is cleared. For the second implementation, it computes an infinite list of every AntState in one go, so we do not have to recompute it however the 
  downside to this is that we now are storing EVERY AntState in memory untill the program is killed. An advantage however, could be that this implementation is more speed effecient. The best 
  implementation to use depends on what resources we have, and what rescources we are prioritising out of speed/time and memory. I have also included an upgraded version of my first implementation, 
  however it uses foldl instead of its recursive function. 

-}

allAntStates :: [AntState]                                            --SECOND IMPLEMENTATION which is MEMORY INTENSIVE.
allAntStates = iterate step initialState

animation :: Int -> Image
animation frame = renderAntState (allAntStates !! frame)

renderAntState :: AntState -> Image
renderAntState (AS _ _ set) = foldl renderAnt blank (Set.toList set)
  where
    renderAnt img (a, b) = offset a b (rect 1 1) <@> img


{-

iterateFrames :: Int -> AntState -> AntState
iterateFrames 0 state = state
iterateFrames frame state = iterateFrames (frame - 1) (step state)   --ORIGINAL IMPLEMENTATION EQUIVALENT WITH FOLDL - TIME INTENSIVE

animation :: Int -> Image
animation frame = renderAntState (iterateFrames frame initialState)

renderAntState :: AntState -> Image
renderAntState (AS _ _ set) = foldl renderAnt blank (Set.toList set)
  where
    renderAnt img (a, b) = offset a b (rect 1 1) <@> img

-}

{- 

animation :: Int -> Image
animation frame = renderAntState (iterateFrames frame initialState)  -- ORIGINAL IMPLEMENTATION

iterateFrames :: Int -> AntState -> AntState
iterateFrames 0 state = state
iterateFrames frame state = iterateFrames (frame - 1) (step state)

renderAntState :: AntState -> Image
renderAntState (AS _ _ set) = renderAntState' (Set.toList set) blank --Start of with a blank screen. 
  where
    renderAntState' [] img = img
    renderAntState' ((a, b):xs) img = renderAntState' xs (offset a b (rect 1 1) <@> img) --Render each of the black squared from the set onto the screen.

-}
