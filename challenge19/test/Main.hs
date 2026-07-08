{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import Challenge19
import Data.String
import Data.IntMap.Strict (IntMap)             -- IntMap (used for storing available character counts)
import qualified Data.IntMap.Strict as IntMap
import Data.Char(ord)

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
            wordIsPossible (buildFrequencyMap "antidisestablishmentarianism") "antiestablishment" @?= True
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

