import Data.List
import Data.Char (chr, ord)
import Data.Maybe (fromJust)
import Data.Function (on)
import System.Exit

testCase = "C3 F1 F3"

-- | Main code to test Proj2 implementations within Grok. This will be run with
-- no command line arguments, so there's no way to specify the target to search
-- for. Therefore, I've hardwired one test, but students will need to do further
-- testing on their own.
main :: IO ()
main = do
  case mapM toLocation $ words testCase of
    Just target@[_,_,_] ->
      proj2test target
    _ -> do
      putStrLn $ "toLocation Failed to convert one of " ++ testCase
                 ++ " to a Location"
      exitFailure


-- | Guess the given target, counting and showing the guesses.
proj2test :: [Location] -> IO ()
proj2test target = do
  putStrLn $ "Searching for target " ++ showLocations target
  let (guess,other) = initialGuess
  loop target guess other 1


-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: [Location] -> [Location] -> GameState -> Int -> IO ()
loop target guess other guesses = do
  putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ showLocations guess
  let answer = feedback target guess
  putStrLn $ "    My answer:  " ++ show answer
  if answer == (3,0,0)
    then do
      putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
    else do
      let (guess',other') = nextGuess (guess,other) answer
     -- putStrLn $ "    My remaining:  " ++ show other'
      loop target guess' other' (guesses+1)

showLocations :: [Location] -> String
showLocations = unwords . (fromLocation <$>)


-- 49 - 52
rowRange = [ord '1' .. ord '4'] :: [Int]
-- 65 - 72
colRange = [ord 'A' .. ord 'H'] :: [Int]


-- data type Location to store locations in two Int for easier calculations
data Location = Location Int Int
  deriving (Eq)
  
-- Game state to stores all remaining combination of locations
type GameState = [[Location]]

{- 
  str : input lcoation string
  toLocation function takes an input location string, if it's a valid location
  convert String to a Just Location type, otherwise return Nothing
-}
toLocation :: String -> Maybe Location
toLocation str
  | length str == 2 && validLoc = Just $ Location col row
  | otherwise = Nothing
  where
    validLoc = elem col colRange && elem row rowRange
    col = (ord $ head str)
    row = (ord $ last str)

{- 
  fromLocation function takes one argument of type 'Location'
  convert the input Location to a location String
-}
fromLocation :: Location -> String
fromLocation (Location col row) = [chr col, chr row]


{-
  target : a location list in 2 trated as target
  guess : a location list in 2 trated as guess
  feedback function takes in a target and a guess and returns a feedback in 
  a three Int tuple which specifies number of guesses 
  were (0,1,2) spaces away from the target ships
-}
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback target guess = (zero, one, two)
  where
    [zero, one, two] = countRange target guess 0


{-
  target : a location list in 2 trated as target
  guess : a location list in 2 trated as guess
  d : specified distance from the target we want to check
  countRange is a recursive function that generates a feedback in List version
-}
countRange :: [Location] -> [Location] -> Int -> [Int]
countRange target guess d
  | d > 2 = [] 
  | null guess = 0 :  countRange target [] (d+1)
  | otherwise = length range : countRange target (guess\\range) (d+1)
  where
    range = filter (inRange d target) guess

{-
  d : specified distance from the target we want to check
  target : a location list trated as target
  loc : Location to check
  inRange function checks wether a Location is in the range of the 
  specified distance d from any of the other locations in the location list 
  and returns a Boolean
-}
inRange :: Int -> [Location] -> Location -> Bool
inRange d locs loc
  = not . null . filter inRange_ $ locs
  where
    inRange_ (Location c1 r1) = abs(c1 - c2) <= d && abs(r1 - r2) <= d
    (Location c2 r2) = loc


{-
  initialGuess takes no input arguments
  it returns an '[Location]' type combination as an initial guess 
  and a 'GameState' which contains all 4960 posible three length combinations 
  of the 32 locations
-}
initialGuess :: ([Location],GameState)
initialGuess = (initGuess, state)
  where 
    initGuess = fromJust . toLocation <$> ["A2", "D2", "H2"]
    state = [[Location c1 r1, Location c2 r2, Location c3 r3]
              | c1 <- colRange, r1 <- rowRange,
                c2 <- colRange, r2 <- rowRange,
                c3 <- colRange, r3 <- rowRange,
                (c1, r1) < (c2, r2),
                (c2, r2) < (c3, r3)]


{-
  (prevGuess, prevRem) : guess and remaining combinations(game state) 
  from last term
  prevFb : the feedback acoording to the prevGuess
  nextGuess function takes in the guess ,game state, and the corresponding
  feedback from last term and returns the new guess and gamestate for next term
-}
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (prevGuess, prevRem) prevFb
  = (newGuess, newRem)
  where
    -- ref : https://stackoverflow.com/q/62430071/17044452
    newRem = filter ((==prevFb). flip feedback prevGuess) prevRem 
    newGuess = removeSymm $ newRem \\ [prevGuess]


{-
  comb : current remaining posible location combinations
  removeSymm function chose one of the location combination in the 
  combination list which have the least expected number as the new guess 
  for the next term
-}
removeSymm :: [[Location]] -> [Location]
removeSymm comb
 = fst . last $ expectLst
  where 
    -- a list contains tuples of each combination and it's coresponding
    -- expected number sorted via expected number in a desending order
    expectLst = sortBy cmp . map (\t -> (t, expect t comb)) $ comb
    cmp = flip compare `on` snd


{-
  target : the target combination
  guesses : guess combination list
  expect function calculate the feedbacks between the target and each guess 
  in the guess list, count the appearance for each feedbacks 
  and return the sum of the count squres
-}
expect :: [Location] -> [[Location]] -> Int
expect target guesses = sigma
  where
    fbLst = map (feedback target) guesses
    count fb = length . filter (==fb) $ fbLst
    sigma = sum . map ((^2) . count) $ (nub fbLst)


