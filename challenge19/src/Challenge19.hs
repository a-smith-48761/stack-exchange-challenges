{-# LANGUAGE OverloadedStrings  #-}

module Challenge19 (
    POS, toPOS,
    WordData, createWordData,
    wordAsString, wordPos, wordFreq,
    loadWords,


    -- utility functions are exported for testing, but not expected to be useful to the main program
    bsToListOfWordLists, bsSplitWords, bsSplitLines, bsRemoveCR
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Word8 as W8
import Data.Maybe (catMaybes)

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
createWordData [w1, w2, w3] = makeResult <$> BSC.readInt w3    -- readInt returns a maybe, so we can use a map function to change the result if successful and ignore failure:
                              where 
                                makeResult (freq, _) = WordData (BSC.unpack w1) (toPOS w2) freq
createWordData _            = Nothing

loadWords :: IO [WordData]
loadWords = do
    _:wordDefinitions <- (   -- ignore first element (which is the file header) from:
        filter (not . null) <$>  -- skip empty elements in
         bsToListOfWordLists <$> -- list of word lists retrieved from
          BS.readFile "data/1_2_all_freq.txt") -- our dictionary file
    return $ catMaybes $ fmap createWordData wordDefinitions


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


