{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import Challenge19
import Data.String

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

tests :: TestTree
tests = testGroup "Utility functions" 
    [
        bytestringTests
    ]

main :: IO ()
main = defaultMain tests

