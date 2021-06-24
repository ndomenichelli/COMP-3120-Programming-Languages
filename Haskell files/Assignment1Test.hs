--Assignment 1 
--try everything
-- find all possible solutions for given out numbers
-- https://github.com/noperative/kingdom-of-zed

import Data.List

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

tests = permutations [1..4]

--letters are merchants
--   a b c d
-- p		 e
-- o		 f
-- n 		 g
-- m		 h
--   l k j i
--zed ([a,b,c,d], [e,f,g,h], [i,j,k,l], [m,n,o,p])
--zed :: ([x]) -> [[x]]

--zed ([a,b,c,d], [e,f,g,h], [i,j,k,l], [m,n,o,p])

--giveslist to test
test n = tests !! n

-- check if row elements are equal to x -> and check if row backwards is equal to y <-
check x y [row] = 
	row !! 1  <= x
		then row !! 2 <= x
 
 

zed ([a,b,c,d], [e,f,g,h], [i,j,k,l], [m,n,o,p])
 | m1 = a
 | t1 = head $ head tests
 | ms1 = 0
 | m2 = l
 | t2 = last $ head tests
 | ms2 = 0