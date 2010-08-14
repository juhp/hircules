module Data.Ord.QuickSort where

quickSort :: Ord a => [a] -> [a]
quickSort [] =  []
quickSort (x:xs) =  quickSort [y | y <- xs, y<x ]
		    ++ [x]
		    ++ quickSort [y | y <- xs, y>=x]
