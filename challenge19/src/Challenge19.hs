{-# LANGUAGE OverloadedStrings  #-}

module Challenge19 (
    POS(..), toPOS, 
    WordData(..), createWordData,
    loadWords,
    buildFrequencyMap,

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
        wtParent           :: Maybe (WordTree, WordData),  -- parent node and word used to reach this node, Nothing for the root node.
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
