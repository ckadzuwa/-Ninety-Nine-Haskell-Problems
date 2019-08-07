--
-- Question 1
-- 
myLast :: [a] -> a 
myLast [x] = x
myLast (x:xs) = myLast xs

-- 
-- Question 2
--
myButLast :: [a] -> a 
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- 
-- Question 3
--
elementAt :: [a] -> Int -> a 
elementAt xs k = xs !! (k-1)

-- 
-- Question 4
--
myLength :: [a] -> Int 
myLength [] = 0
myLength [x] = 1 
myLength (x:xs) = 1 + myLength xs

-- 
-- Question 5
--
myReverse :: [a] -> [a] 
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]