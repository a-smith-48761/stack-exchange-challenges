{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import Challenge19
import Data.String
import Data.IntMap.Strict (IntMap)             -- IntMap (used for storing available character counts)
import qualified Data.IntMap.Strict as IntMap
import Data.Char(ord)
import Data.Set (Set)
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit 

bytestringTests :: TestTree
bytestringTests = testGroup "ByteString utilities" 
    [
        testCase "bsRemoveCR removes terminal CR" $ do
            bsRemoveCR (fromString "hello\r") @?= (fromString "hello"),
        testCase "bsRemoveCR does not affect other terminal characters" $ do
            bsRemoveCR (fromString "hello.") @?= (fromString "hello."),

        testCase "bsSplitLines splits lines correctly" $ do
            bsSplitLines (fromString "first line\nsecond line\nthird line") @?= 
                [
                    fromString "first line",
                    fromString "second line",
                    fromString "third line"
                ],
        testCase "bsSplitLines removes CRs" $ do
            bsSplitLines (fromString "first line\r\nsecond line\nthird line") @?= 
                [
                    fromString "first line",
                    fromString "second line",
                    fromString "third line"
                ],

        testCase "bsSplitWords splits on space" $ do
            bsSplitWords (fromString "first second") @?= 
                [ fromString "first", fromString "second" ],
        testCase "bsSplitWords splits on tab" $ do 
            bsSplitWords (fromString "first\tsecond") @?=
                [ fromString "first", fromString "second" ],
        testCase "bsSplitWords ignores repeated separators" $ do
            bsSplitWords (fromString "first  second") @?=
                [ fromString "first", fromString "second" ],

        testCase "bsToListOfWordLists works appropriately" $ do
            bsToListOfWordLists (fromString "first second\nthird  fourth\tfifth") @?=
                [ [ fromString "first", fromString "second" ],
                  [ fromString "third", fromString "fourth", fromString "fifth" ]]

    ]

dictionaryReadingTests :: TestTree
dictionaryReadingTests = testGroup "Reading dictionary items"
    [
        testCase "POS reading: nouns" $ do
            toPOS "NoC" @?= Noun
            toPOS "NoP" @?= Noun
            toPOS "NoP-" @?= Noun,
        testCase "POS reading: verbs" $ do
            toPOS "Verb" @?= Verb
            toPOS "VMod" @?= Verb,
        testCase "POS reading: adjectives" $ do
            toPOS "Adj" @?= Adj,
        testCase "POS reading: others" $ do
            toPOS "Any other string" @?= OtherPOS,

        testCase "Reading dictionary lines works" $ do
            stringToWords "  word1\t NoP 1234\nword2   Adj   515" @?=
                [ WordData "word1" Noun 1234, WordData "word2" Adj 515 ]
    ]

treeBuildingTests :: TestTree
treeBuildingTests = testGroup "Tree building"
    [
        testCase "Frequency counting works" $ 
            let freqCounts = buildFrequencyMap "The quick brown fox juMPED over the LAZY DOGS" in do
                IntMap.lookup (ord 'E') freqCounts @?= Just 4
                IntMap.lookup (ord 'F') freqCounts @?= Just 1,
        testCase "Frequency counts are zero for not-present letters" $
            IntMap.lookup (ord 'S') (buildFrequencyMap "abcdef") @?= Just 0,
        testCase "Frequency counts missing for non-letter characters" $
            IntMap.lookup (ord '@') (buildFrequencyMap "@abc") @?= Nothing,
        
        testCase "Cannot use words we have insufficient letters for" $ do
            wordIsPossible (buildFrequencyMap "abc") "bad" @?= False
            wordIsPossible (buildFrequencyMap "ther") "there" @?= False,
            
        testCase "Can use words where we do have sufficient letters" $ do
            wordIsPossible (buildFrequencyMap "hello") "hello" @?= True
            wordIsPossible (buildFrequencyMap "antidisestablishmentarianism") "antiestablishment" @?= True,

        testCase "Root of tree is correctly filled in" $ do
            let root = buildWordTree 
                        (buildFrequencyMap "lorem ipsum dolor sit amet consectetuer adipiscing elit")           -- available letters
                        (stringToWords "azeotrope NoC 141\n capsicum NoC 121 \n tame Verb 113\n elite Adj 100") -- dictionary
                        (Set.empty)                                                                             -- words that must be avoided

            wtSumLogProb root @?= 0.0
            wtDepth root @?= 0
            wtAvailableLetters root @?= buildFrequencyMap "lorem ipsum dolor sit amet consectetuer adipiscing elit",

        testCase "First level of tree is correctly filled in" $ do
            let root = buildWordTree 
                        (buildFrequencyMap "lorem ipsum dolor sit amet consectetuer adipiscing elit")           -- available letters
                        (stringToWords "azeotrope NoC 141\n capsicum NoC 121 \n tame Verb 113\n elite Adj 100") -- dictionary
                        (Set.empty)                                                                             -- words that must be avoided

            -- break 3 expected items from completions list, but we won't check the new nodes for the second and third, just the words used to get there
            -- note only expecting 3 items because "azeotrope" isn't possible from our starting frequencies
            let (w1, n1):(w2, _):(w3, _):t = wtCompletions root 

            t @?= []  -- check length is correct

            -- check word and new node for the first entry
            wordAsString w1 @?= "capsicum"
            wtSumLogProb n1 @?= logBase 10 121 - 6
            wtDepth n1 @?= 1
            wtAvailableLetters n1 @?=  buildFrequencyMap "lorem dolor sit met onsectetuer adipising elit"

            -- check remaining words
            wordAsString w2 @?= "tame"
            wordAsString w3 @?= "elite",

        testCase "Second level of tree is correctly filled in" $ do
            let root = buildWordTree 
                        (buildFrequencyMap "lorem ipsum dolor sit amet consectetuer adipiscing elit")           -- available letters
                        (stringToWords "azeotrope NoC 141\n capsicum NoC 121 \n tame Verb 113\n elite Adj 100") -- dictionary
                        (Set.empty)                                                                             -- words that must be avoided

            let (_, firstLevel):_ = wtCompletions root -- get the first completion from the root, which should be "capsicum"
            -- because there's only one 'c' left, we now can't have capsicum a second time, but tame and elite are both still possible:
            let (w1, n1):(w2, _):t = wtCompletions firstLevel

            t @?= []  -- check length is correct

            -- check word and new node for the first entry
            wordAsString w1 @?= "tame"
            wtSumLogProb n1 @?= (logBase 10 121) + (logBase 10 113) - 12
            wtDepth n1 @?= 2
            wtAvailableLetters n1 @?=  buildFrequencyMap "lorem dolor sit onsectetuer dipising elit"

            -- check remaining words
            wordAsString w2 @?= "elite"



    ]

tests :: TestTree
tests = testGroup "Utility functions" 
    [
        bytestringTests,
        dictionaryReadingTests,
        treeBuildingTests
    
    ]

main :: IO ()
main = defaultMain tests

