# StackOverflow Challenges

This repository contains a collection of attempts at answering the [Stack Overflow Challenges](https://stackoverflow.com/beta/challenges) in Haskell. The primary motivation for this is to push me into using Haskell for applications outside of the application area I learned it for (parsing and language-design related exploration) and into small examples of more traditional programming tasks. Also because nobody else seemed to be using Haskell to answer these challenges.

In chronological order, the challenges I have attempted are:

* [Challenge 20](./challenge20/), which is a relatively simple application that reads text files and produces a simple summary of the data in them. The implementation highlights the use of the Arrows package for composition of functions in a more readable way than regular Haskell function compostion.
* [Challenge 19](./challenge19/) (unfinished), which is a more interesting problem involving searching a large tree of possible solutions to find the answer. The implementation demonstrates the advantages of Haskell's lazy evaluation by providing a definition that constructs a complete tree and searching through it, which would be prohibitively expensive using eager evaluation
  
