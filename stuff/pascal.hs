pascal [] = [1]
pascal a = pascal' $ [0] ++ a ++ [0]

pascal' [] = []
pascal' (x:[]) = []
pascal' (x:xs) = (x+y):(pascal' xs) where y = head xs




pascalI = flip take $ iterate pascal [1]
pascalIterate = iterate pascal [1]

pascalIterate' = []:[ pascal (pascalIterate' !! i) | i <- [0..] ]


--- you do need to use . to merge functions! I can optimize the lazy evaluation
--- in places where you use $ right now...
