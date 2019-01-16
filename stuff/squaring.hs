sqr :: Int -> Int
sqr x = (2 * x -1) + sum (take (x-1) [1,3..])
