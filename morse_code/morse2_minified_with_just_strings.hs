import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.List as List
regulars = [' ', 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','0','1','2','3','4','5','6','7','8','9']
codes    = [" ", ".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--..","-----",".----","..---","...--","....-",".....","-....","--...","---..","----."]
encodeDict = Map.fromList $ zip regulars codes
decodeDict = Map.fromList $ zip codes regulars
encode string = List.intercalate " " $ filter (/="") $ map encodeChar string
encodeChar c = case r of (Just a) -> a
                         (Nothing) -> ""
                         where r = Map.lookup (Char.toUpper c) encodeDict
decode string = foldMaybe $ map (\s -> Map.lookup s decodeDict) $ takeSeconds $ splitSpaces string
foldMaybe = foldr (\maybe acc -> case maybe of (Just a) -> a:acc
                                               (Nothing) -> acc) ""
splitSpaces "" = []
splitSpaces s
        | head s == ' ' = " "  : splitSpaces (tail s)
        | otherwise     = word : splitSpaces rest
        where word = takeWhile (/=' ') s
              rest = dropWhile (/=' ') s
takeSeconds [] = []
takeSeconds [a] = [a]
takeSeconds (a:_:xs) = a:(takeSeconds xs)
