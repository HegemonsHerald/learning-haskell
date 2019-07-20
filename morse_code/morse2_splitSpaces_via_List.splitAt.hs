import qualified Data.List as List

-- splits a string into words (= strings of non-space chars) and spaces (= " ")
splitSpaces "" = []     -- if there's nothing left in the string, there can't be anything more to split on
splitSpaces s
        | head s == ' '  = " "  : splitSpaces $ tail s               -- if the beginning of string is a space, add that space to the list and continue splitting on the rest
        | otherwise      = word : splitSpaces rest
        where (word, rest) = List.splitAt (findSpace s) s
         -- word is everything up to the next space or the end of the string, if there is no next space
         -- rest is everything from the next space to the end or "" if there is no next space

-- finds the index of the next space in a string
-- if there is no space, returns the length of the string
findSpace s = case List.findIndex (==' ') s of (Just a) -> a
                                               (Nothing) -> length s
