-- See https://wiki.haskell.org/Template_Haskell
{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

--------------------------------------------------------------------------
-- Question 1
--------------------------------------------------------------------------
myLast :: [a] -> a 
myLast [] = error "Empty list can't have a last element"
myLast [x] = x
myLast (x:xs) = myLast xs

-- Test solution on concrete examples
testMyLast :: Bool
testMyLast = myLast [1,2,3,4] == 4 && myLast  ['x', 'y', 'z'] == 'z' 

-- Prove solution: Last element in non-empty list must equal first element in reversed list
prop_myLast :: Eq a => NonEmptyList a -> Bool
prop_myLast (NonEmpty xs) = myLast (xs) == head (reverse xs)  

--------------------------------------------------------------------------
-- Question 2
--------------------------------------------------------------------------
myButLast :: [a] -> a 
myButLast [x] = error "Single element list can't have a second last element"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- Test solution on concrete examples
testMyButLast = myButLast [1,2,3,4] == 3 && myButLast ['a'..'z'] == 'y'

-- Prove solution: Second last element on a (miinum 2) element list must equal the element indexed at list's second last position
prop_myButLast :: Eq a => NonEmptyList a -> Property
prop_myButLast (NonEmpty xs) = (length xs > 1) ==> (myButLast xs ==  xs !! (length xs - 2))

--------------------------------------------------------------------------
-- Question 3
--------------------------------------------------------------------------
elementAt :: [a] -> Int -> a 
elementAt [] k = error "Can't look for an element in an empty list"
elementAt xs k
    | k < 1          = error "Can't use a negative index"
    | k > length xs  = error "Can't use an index larger than the size of list"
    | otherwise      = xs !! (k-1)

-- Test solution on concrete examples
testElementAt :: Bool
testElementAt = elementAt [1,2,3] 2 == 2 && elementAt "haskell" 5 == 'e'

-- Prove solution: For k > 1 and no larger than the size of the list the kth 
-- element is equivalent to last element after collecting the first k elements
prop_elementAt :: Eq a => NonEmptyList a -> Int -> Property
prop_elementAt (NonEmpty xs) k = (k > 1 && k <= length xs) ==> ((elementAt xs k) == (last (take k xs)))

--------------------------------------------------------------------------
-- Question 4
--------------------------------------------------------------------------
myLength :: [a] -> Int 
myLength [] = 0
myLength [x] = 1 
myLength (x:xs) = 1 + myLength xs

-- Test solution on concrete examples
testMyLength = myLength [123, 456, 789] == 3 && myLength "Hello, world!" == 13

-- Prove solution: Dropping myLength elements from list should yield empty list (other list is bigger than myLenght)
-- and custom defined length should be equivalent to built in length function
prop_myLength :: Eq a => [a] -> Bool 
prop_myLength xs = (drop (myLength xs) xs) == [] && myLength xs == length xs

--------------------------------------------------------------------------
-- Question 5
--------------------------------------------------------------------------
myReverse :: [a] -> [a] 
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Test solution on concrete examples
testMyReverse = myReverse "A man, a plan, a canal, panama!" == "!amanap ,lanac a ,nalp a ,nam A" && myReverse [1..4] == [1,2,3,4]

-- Prove solution: Reversing a reversed list should yield the original list and
-- custom reverse should be equivalent to built in reverse function
prop_myReverse :: Eq a => [a] -> Bool
prop_myReverse xs = (myReverse (myReverse xs)) == xs && myReverse xs == reverse xs  

--------------------------------------------------------------------------
-- Question 6
--------------------------------------------------------------------------
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = False
isPalindrome xs = xs == (reverse xs)

-- Test solution on conrete examples
testIsPalindrome = isPalindrome [1,2,3] == False && isPalindrome "madamimadam" == True && isPalindrome [1,2,4,8,16,8,4,2,1]

-- Prove solution: Should reliably detect even, odd length and non palindromes
prop_isPalindrome :: Eq a => NonEmptyList a -> Bool
prop_isPalindrome (NonEmpty xs) = isPalindrome (evenLengthPalindrome xs) && isPalindrome (oddLengthPalindrome xs) &&
    (not (firstHalf xs == reverse (secondHalf xs)) == not (isPalindrome xs))
    where 
        evenLengthPalindrome xs = xs ++ reverse xs                          -- For list of length k creates a new list of size 2k
        oddLengthPalindrome xs = xs ++ (head xs:[]) ++ reverse xs           -- For list of length k creates a new list of size 2k + 1
        firstHalf xs = take ((quot (length xs) 2) + (rem (length xs) 2)) xs -- e.g. for [1,2,1] and [1,2] returns [1,2] and [1] respectively
        secondHalf xs = drop (quot (length xs) 2) xs                        -- e.g. for [1,2,1] and [1,2] returns [2,1] and [2] respectively
          

-- Add ability to run quickCheck props 
-- See http://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-All.html
return []
runTests = $quickCheckAll