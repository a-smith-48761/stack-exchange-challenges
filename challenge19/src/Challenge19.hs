module Challenge19 (
    POS,
    WordData,
    wordAsString, wordPos, wordFreq,
    loadWords,


    -- utility functions are exported for testing, but not expected to be useful to the main program
    bsToListOfWordLists, bsSplitWords, bsSplitLines, bsRemoveCR
) where

import qualified Data.ByteString as BS
import qualified Data.Word8 as W8

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
        wordFreq     :: Integer
    }
    deriving (Show, Eq)


loadWords :: IO [Word]
loadWords = do
    headerFields:wordDefinitions <- (bsToListOfWordLists <$> BS.readFile "data/1_2_all_freq.txt")
    putStrLn (show $ last wordDefinitions)
    return []


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


