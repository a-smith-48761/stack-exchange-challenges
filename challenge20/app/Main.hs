module Main (main) where

{-

Stack Overflow Challenge 20

-}

import qualified Data.Text as Text -- efficient strings
import Data.List.Split             -- split lists on delimeters
import Control.Arrow               -- used for easy-to-read function composition
import qualified Data.Map as Map   -- maps
import Data.Maybe (catMaybes)      -- function for flattening lists of Maybes
import Data.Ratio                  -- rational types, for calculations involving scoops and tubs
import System.FilePath             -- filename manipulation
import Control.Monad               -- for looping constructs in monad contexts

-- use a specific type for support names and ice cream flavors to help detect errors
type Name = Text.Text
type Flavor = Text.Text

-- structure to store a supporter's details as supplied in scenario input
data Supporter =
    Supporter {
        name :: Name,       -- supporter's name (1st field on line)
        guests :: Integer,  -- number of guests the supporter has (2nd)
        flavor :: Flavor    -- supporter's preferred flavor of ice cream (3rd)
    }
    deriving (Show, Eq)
    
supporterFromString :: String -> Maybe Supporter
supporterFromString = splitFields >>> supporterFromFieldList
    where
        -- split incoming strings on ":", and don't keep the delimeters in the list
        splitFields :: String -> [String]
        splitFields = split (dropDelims $ oneOf ":")
        
        supporterFromFieldList :: [String] -> Maybe Supporter
        -- supporterFromFieldList returns valid results for lists of 3 items
        -- we also strip leading and trailing whitespace from the name and flavor here
        supporterFromFieldList (n:g:f:[]) = 
            Just (Supporter 
                (Text.strip $ Text.pack n)       -- name converted to text, with whitespace removed
                (read g)                         -- guests as an Integer
                (Text.strip $ Text.pack f))      -- flavor as text, whitespace removed
        -- other sizes are invalid, so return nothing
        supporterFromFieldList _ = Nothing
    
loadSupporters :: FilePath -> IO ([Supporter])
loadSupporters fn = 
    readFile fn >>= ( -- read file and process the results through a pure processing pipeline:
                    lines >>>                    -- break the string's content into lines
                    fmap supporterFromString >>> -- convert each line into a Maybe Supporter
                    catMaybes >>>                -- remove the Maybe, discarding any Nothings
                    return)                      -- encapsulate in an IO monad
                    
-- we use a list of (flavor, [Supporter]) items to build a map of flavors to supporters
-- use pattern matching in a list comprehension to extract the data to build this
buildFlavorList :: [Supporter] -> [(Flavor, [Supporter])]
buildFlavorList supporters = [(f, [supporter]) | supporter@(Supporter _ _ f) <- supporters]

-- count the number of people eating each flavor
countPeople :: [Supporter] -> Integer
countPeople = foldr addGuestsPlus1 0
    where
        addGuestsPlus1 :: Supporter -> Integer -> Integer
        addGuestsPlus1 (Supporter _ g _) current = current + 1 + g
        
-- people to tubs calculation
peopleToTubs :: Integer -> Integer
peopleToTubs people = ceiling (tubs)
    where
        scoops = (people * 3) % 2
        tubs = scoops / 9

formatReport :: String -> Map.Map Flavor Integer -> String
formatReport scenario flavorCounts =
    scenario ++ ":\n" ++ (
        Map.foldrWithKey addLineForEntry "" flavorCounts)
    where
        addLineForEntry flavorName count existingLines = 
            " * " ++ (show flavorName) ++ " " ++ (show count) ++ "\n" ++ existingLines

produceScenarioReport :: FilePath -> String -> IO String
produceScenarioReport filename scenario =
    loadSupporters filename
        >>= ( -- once we've got a supporters list we need to process it through a pure pipeline, which we'll define
              -- here using arrows for convenient composition of functions:
              buildFlavorList >>>           -- first extract the flavors and format each supporter as a list ready for conversion to a map
                Map.fromListWith (++) >>>   -- then convert to a map, concatenating the lists of supporters with the same flavor
                Map.map (countPeople >>>    --   for each value in the map, count the number of people eating each flavor
                         peopleToTubs) >>>  --   and calculate how many tubs are needed
                formatReport scenario >>>   -- finally convert to a string
                return                      -- and then encapsulate the resulting string in an IO monad
            )

filePathForScenario :: Integer -> FilePath
filePathForScenario n = "data" </> "challenge_20_s" ++ (show n) <.> "txt"

-- produce names for the final report; there doesn't seem to be a neater way of doing it than this
-- there is a package "numerals" that can produce it, but it would need more code to use it, so
-- this is simpler:
nameForScenario :: Integer -> String
nameForScenario 1 = "Scenario one"
nameForScenario 2 = "Scenario two"
nameForScenario 3 = "Scenario three"
nameForScenario n = "Scenario " ++ (show n) -- default fallback for unexpected numbers

main :: IO ()
main = do
    -- print a report for each scenario, and ignore the collected results
    _ <- forM 
        [ (filePathForScenario n, nameForScenario n) | n <- [1..3] ] 
        (\ (path, scenario) -> produceScenarioReport path scenario >>= putStrLn)
    -- and return nothing
    return ()
