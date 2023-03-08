-- Exercise set 4b: folds

module Set4b where

import Mooc.Todo

{-
review foldr:
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr f b [] = b
foldr f b (x:xs) = f x (foldr f b xs)

function f: 
    argument: a, b
-}

------------------------------------------------------------------------------
-- Ex 1: countNothings with a fold. The function countNothings from
-- the course material can be implemented using foldr. Your task is to
-- define countHelper so that the following definition of countNothings
-- works.
--
-- Hint: You can start by trying to add a type signature for countHelper.
--
-- Challenge: look up the maybe function and use it in countHelper.
--
-- Examples:
--   countNothings []  ==>  0
--   countNothings [Just 1, Nothing, Just 3, Nothing]  ==>  2

countNothings :: [Maybe a] -> Int
countNothings = foldr countHelper 0

countHelper :: Num b => Maybe a -> b -> b
countHelper Nothing cnt = cnt + 1
countHelper _ cnt = cnt

------------------------------------------------------------------------------
-- Ex 2: myMaximum with a fold. Just like in the previous exercise,
-- define maxHelper so that the given definition of myMaximum works.
--
-- Examples:
--   myMaximum []  ==>  0
--   myMaximum [1,3,2]  ==>  3

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = foldr maxHelper x xs

maxHelper :: Int -> Int -> Int
maxHelper = max

------------------------------------------------------------------------------
-- Ex 3: compute the sum and length of a list with a fold. Define
-- slHelper and slStart so that the given definition of sumAndLength
-- works. This could be used to compute the average of a list.
--
-- Start by giving slStart and slHelper types.
--
-- Examples:
--   sumAndLength []             ==>  (0.0,0)
--   sumAndLength [1.0,2.0,4.0]  ==>  (7.0,3)


sumAndLength :: [Double] -> (Double,Int)
sumAndLength xs = foldr slHelper slStart xs

slStart :: (Double, Int)
slStart = (0, 0)
slHelper :: Double -> (Double, Int) -> (Double, Int)
slHelper x (sum, cnt) = (sum + x, cnt + 1)

------------------------------------------------------------------------------
-- Ex 4: implement concat with a fold. Define concatHelper and
-- concatStart so that the given definition of myConcat joins inner
-- lists of a list.
--
-- Examples:
--   myConcat [[]]                ==> []
--   myConcat [[1,2,3],[4,5],[6]] ==> [1,2,3,4,5,6]

myConcat :: [[a]] -> [a]
myConcat xs = foldr concatHelper concatStart xs

concatStart :: [a]
concatStart = []
concatHelper :: [a] -> [a] -> [a]
concatHelper xs result = xs ++ result

------------------------------------------------------------------------------
-- Ex 5: get all occurrences of the largest number in a list with a
-- fold. Implement largestHelper so that the given definition of largest works.
--
-- Examples:
--   largest [] ==> []
--   largest [1,3,2] ==> [3]
--   largest [1,3,2,3] ==> [3,3]

largest :: [Int] -> [Int]
largest xs = foldr largestHelper [] xs

largestHelper :: Int -> [Int] -> [Int]
largestHelper val result 
    | null result || val > head result = [val]
    | val == head result = val:result
    | otherwise = result
            


------------------------------------------------------------------------------
-- Ex 6: get the first element of a list with a fold. Define
-- headHelper so that the given definition of myHead works.
--
-- Start by giving headHelper a type.
-- Examples:
--   myHead []  ==>  Nothing
--   myHead [1,2,3]  ==>  Just 1

myHead :: [a] -> Maybe a
myHead xs = foldr headHelper Nothing xs

headHelper :: a -> Maybe a -> Maybe a
headHelper x t = Just x
-- ! understand that elements are accessed backward
-- headHelper x Nothing = Just x
-- headHelper x t = Just t


------------------------------------------------------------------------------
-- Ex 7: get the last element of a list with a fold. Define lasthelper
-- so that the given definition of myLast works.
-- Start by giving lastHelper a type.
--
-- Examples:
--   myLast [] ==> Nothing
--   myLast [1,2,3] ==> Just 3

myLast :: [a] -> Maybe a
myLast xs = foldr lastHelper Nothing xs

lastHelper x Nothing = Just x
lastHelper x t = t
