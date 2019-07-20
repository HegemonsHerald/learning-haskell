import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.List as List

regulars = [' ', 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','0','1','2','3','4','5','6','7','8','9']
codes    = [" ", ".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--..","-----",".----","..---","...--","....-",".....","-....","--...","---..","----."]
encodeDict = Map.fromList $ zip regulars codes
decodeDict = Map.fromList $ zip codes regulars

-- encodes a string of alphanums and spaces into morse code, where the morse code codes are separated by spaces
-- filter (/=""): if illegal Chars are in string, map encodeChar turns these into "", which would be intercalated with spaces, which we don't want!
encode :: [Char] -> [Char]
encode string = List.intercalate " " $ filter (/="") $ map encodeChar string

-- encodes a single character
-- if no matching encoding is found, this returns ""
encodeChar :: Char -> [Char]
encodeChar c = case r of (Just a) -> a
                         (Nothing) -> ""
                         where r = Map.lookup (Char.toUpper c) encodeDict

-- decodes properly encoded morse code
--  - splitSpaces: split morse code into morse code codes and spaces
--  - takeSeconds: remove the spaces, that are intercalated between the morse code codes
decode :: [Char] -> [Char]
decode string = foldMaybe $ map (\s -> Map.lookup s decodeDict) $ takeSeconds $ splitSpaces string

-- merges a list of Maybe Chars into a String
foldMaybe :: [Maybe Char] -> [Char]
foldMaybe = foldr (\maybe acc -> case maybe of (Just a) -> a:acc
                                               (Nothing) -> acc) ""

-- -- splits a string into words (= strings of non-space chars) and spaces (= " ")
splitSpaces :: [Char] -> [[Char]]
splitSpaces "" = []
splitSpaces s
        | head s == ' ' = " "  : splitSpaces (tail s)
        | otherwise     = word : splitSpaces rest
        where word = takeWhile (/=' ') s
              rest = dropWhile (/=' ') s

-- removes every second element of a list
takeSeconds :: [a] -> [a]
takeSeconds [] = []
takeSeconds [a] = [a]
takeSeconds (a:_:xs) = a:(takeSeconds xs)
