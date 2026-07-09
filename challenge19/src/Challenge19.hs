{-# LANGUAGE OverloadedStrings  #-}

module Challenge19 (
    POS(..), toPOS, 
    WordData(..), createWordData,
    loadWords,
    WordTree(..),
    buildFrequencyMap,
    wordIsPossible,
    buildWordTree,

    -- utility functions are exported for testing, but not expected to be useful to the main program
    bsToListOfWordLists, bsSplitWords, bsSplitLines, bsRemoveCR, stringToWords
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Word8 as W8
import Data.Maybe (catMaybes)
import Control.Arrow ((>>>))                   -- we use Arrows only for left-to-right function composition.
import Data.IntMap.Strict (IntMap)             -- IntMap (used for storing available character counts)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function                            --  function combinators, e.g. fix

-- ---------------------------------------------------------------------------
--         Data structures and functions for handling dictionary data
-- ---------------------------------------------------------------------------

-- Part of Speech tags. We're only interested in certain classes, so we only define those.
data POS = 
    Noun | -- NoC, NoP, or NoP- in the word list
    Verb | -- Verb, or VMod in the word list
    Adj  | -- just Adj in the word list
    OtherPOS 
    deriving (Show, Eq)

data WordData = 
    WordData {
        wordAsString :: String,
        wordPos      :: POS,
        wordFreq     :: Int
    }
    deriving (Show, Eq)


toPOS :: BS.ByteString -> POS
toPOS s 
    | "No" `BS.isPrefixOf` s     = Noun
    | s == "Verb" || s == "VMod" = Verb
    | s == "Adj"                 = Adj
    | otherwise                  = OtherPOS

createWordData :: [BS.ByteString] -> Maybe WordData
-- we only care about lines with three values
createWordData [w1, w2, w3] = 
    makeResult <$> BSC.readInt w3    -- readInt returns a maybe, so we can use a map function to change the result if successful and ignore failure:
    where 
        makeResult (freq, _) = WordData (BSC.unpack w1) (toPOS w2) freq
-- other list lengths are errors which we ignore
createWordData _            = Nothing

stringToWords :: BS.ByteString -> [WordData]
stringToWords = 
    bsToListOfWordLists  >>>   -- create a list of list of strings, each outer element corresponding to a single word
    filter (not . null)  >>>   -- skip empty inner lists 
    fmap createWordData  >>>   -- convert each inner list of strings to (maybe) word data 
    catMaybes                  -- and finally remove any "Nothing" results, which represent processing errors

loadWords :: IO [WordData]
loadWords = do
    _:wordDefinitions <- (   -- ignore first element (which is the file header) from:
        stringToWords <$>                      -- convert lines to WordData using the content from 
          BS.readFile "data/1_2_all_freq.txt") --   our dictionary file

    return wordDefinitions 


-- utility string handling functions, exported for easier testing:

-- function to break a string into a list of lists, each one containing the words in a line
bsToListOfWordLists :: BS.ByteString -> [[BS.ByteString]]
bsToListOfWordLists = fmap bsSplitWords . bsSplitLines

-- remove final CR if present
bsRemoveCR :: BS.ByteString -> BS.ByteString
bsRemoveCR = BS.dropWhileEnd (== W8._cr) 

-- break into substrings at LF, dropping CRs
bsSplitLines :: BS.ByteString -> [BS.ByteString]
bsSplitLines = fmap bsRemoveCR . (BS.split W8._lf)

-- break a bytestring into words
bsSplitWords :: BS.ByteString -> [BS.ByteString]
bsSplitWords = filter (not . BS.null) . BS.splitWith W8.isSpace


-- ---------------------------------------------------------------------------
--      Data structures and functions for creating a tree of word choices
-- ---------------------------------------------------------------------------

data WordTree =
    WordTree {
        wtSumLogFreq       :: Float,                       -- sum of the log frequencies to get here (which is proportional to log 
                                                           -- probability of this phrase occurring naturally given uniform distribution)
        wtDepth            :: Int,                         -- depth of this node
        wtAvailableLetters :: IntMap Int,                  -- uses upper case ASCII codes as keys, i.e. 65-92
        wtCompletions      :: [(WordData, WordTree)]       -- possible completions from here, which is lazily instantiated (as this is the default
                                                           -- for Haskell values) so we don't generate more of the tree than we need.
    }
    deriving (Eq, Show)

buildFrequencyMap :: String -> IntMap Int
buildFrequencyMap = foldl' addLetterToMap emptyLetterMap 
    where
        addLetterToMap :: IntMap Int -> Char -> IntMap Int
        addLetterToMap cur c = IntMap.adjust (+1) (keyForChar c) cur -- does nothing if the key isn't in the map, which is fine as we're preinitializing.
        
        emptyLetterMap :: IntMap Int
        emptyLetterMap = IntMap.fromList [(keyForChar c, 0) | c <- ['A' .. 'Z']]

        keyForChar :: Char -> Int
        keyForChar c = Char.ord (Char.toUpper c)


wordIsPossible :: IntMap Int -> String -> Bool
wordIsPossible freqs string = frequenciesAreAtLeast (buildFrequencyMap string) freqs

frequenciesAreAtLeast :: IntMap Int -> IntMap Int -> Bool
frequenciesAreAtLeast mins = IntMap.foldrWithKey checkFreqAndAccumulate True
    where
        checkFreqAndAccumulate :: IntMap.Key -> Int -> Bool -> Bool
        checkFreqAndAccumulate _ _ False = False  -- don't need to check anything if we've already failed
        checkFreqAndAccumulate letter freq True = 
            case IntMap.lookup letter mins of
                Nothing -> False
                Just minFreq -> freq >= minFreq

subtractFrequencies :: IntMap Int -> IntMap Int -> IntMap Int
subtractFrequencies = IntMap.differenceWith (\ a b -> Just $ a-b) 

buildWordTree :: IntMap Int -> [WordData] -> Set String -> WordTree
buildWordTree freqs dict ignore = fix buildRoot -- fix is used here to enable passing the parent node to the function that builds the children
    where
        -- because we're using "fix" to call this, the parameter "root" is given a value which is a reference to the object that will contain the result of
        -- evaluating the function call once that is done, i.e. the parameter is also the return value of the function. Because the value is evaluated lazily
        -- (i.e. only once it is actually used) this is not self-contradictory.
        buildRoot root = WordTree 0.0 0 freqs (completionsFrom root freqs)

        -- build a list of completions from a given node by running through the dictionary and skipping items that don't have enough letters
        completionsFrom node remainingFreqs = catMaybes         -- removes "Nothing" from list, converts "Just x" to "x", i.e. this is what skips the failures
            $ fmap (buildCompletion node remainingFreqs) dict   -- builds either Nothing or Just (word, newnode)

        buildCompletion node remainingFreqs word 
            | wordIsPossible remainingFreqs $ wordAsString word    = Just (word, fix $ newNodeFrom node word remainingFreqs)
            | otherwise                                            = Nothing
        
        -- recursively build a new node starting from a given parent with the specified word; we use 'fix' to get the new node
        -- passed back to us so we can use it in recursive calls:
        newNodeFrom parent word frequenciesBeforeWord newNode = 
            let 
                newFrequencies = subtractFrequencies frequenciesBeforeWord (buildFrequencyMap $ wordAsString word) 
            in WordTree 
                (wtSumLogFreq parent + logBase 10 (fromIntegral $ wordFreq word))
                (wtDepth parent + 1)
                newFrequencies
                (completionsFrom newNode newFrequencies)

