module Main where

import Lib (createExpression, deriveBy, substitute, evaluate, beautify, Tree)


-- 1. Custom example from PDF

-- Test expression "2 * ( x ^ 2 ) + sin ( y + x )"
testTree :: Tree
testTree = createExpression "2 * ( x ^ 2 ) + sin ( y + x )"

-- Derive expression by variable x
testDeriveByX :: Tree
testDeriveByX = deriveBy testTree "x"

-- Derive expression by variable y
testDeriveByY :: Tree
testDeriveByY = deriveBy testTree "y"

-- Substitute variable x with value 0
testSubsTreeX :: Tree
testSubsTreeX = substitute "x" testTree 0

-- Substitute variable x with value 0 and then y with value pi/2
testSubsTreeXY :: Tree
testSubsTreeXY = substitute "y" testSubsTreeX $ pi / 2

-- Print all those expressions
main1 :: IO ()
main1 = do
    putStrLn "1. Custom example:"
    print $ testTree
    print $ testDeriveByX
    print $ testDeriveByY
    print $ testSubsTreeX
    print $ testSubsTreeXY
    print $ evaluate testSubsTreeXY

-- 2. Print all previous expressions using beautify function
main1Beautify :: IO ()
main1Beautify = do
    putStrLn "\n2. Custom example using beautify function:"
    print $ beautify $ testTree
    print $ beautify $ testDeriveByX
    print $ beautify $ testDeriveByY
    print $ beautify $ testSubsTreeX
    print $ beautify $ testSubsTreeXY
    print $ evaluate testSubsTreeXY


-- 3. Example where input expression is entered by user and then evaluated 
main2 :: IO ()
main2 = do
    putStrLn "\n3. Example with entered expression:"
    putStrLn "Please enter your expression:"
    number <- getLine
    print $ evaluate $ createExpression number


-- 4. Main function which calls functions main1, main1Beautify and main2
main :: IO ()
main = do
    main1
    main1Beautify
    main2
