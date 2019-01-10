import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.List as List

-- Morse Data type, represents a morse-encoded string as a list of morse code codes
data Morse = Plain String | Code [String]

-- Rendering the Morse type with spaces to separate the morse code codes
instance Show Morse where
  show (Plain a) = a
  show (Code  a) = show $ List.unwords a       -- conjoin codes with spaces; show because you get back a string here, show it as such

-- Encode into morse code
--   map (\a -> [a]):    split string into a list of Strings, where each String contains just one Char
--                       ... codeToken expects a String to lookup, so we provide it with one!
--   map (codeToken...): replace the Strings with their corresponding codes
--   filter (/=""):      remove elements, where the lookup of the code failed
encode :: Morse -> Morse
encode (Plain string) = Code $ filter (/="") $ map (codeToken encodeDict) $ map (\a -> [a]) string

-- Decode from morse code
--   foldl over the codes, replacing each code with its decoded counterpart and accumulating that
--   into a String.
decode :: Morse -> Morse
decode (Code tokens) = Plain $ foldl (\acc token -> acc ++ codeToken decodeDict token) "" tokens

-- Substitute valid tokens for their code, invalid ones for an emtpy string
codeToken :: Map.Map String String -> String -> String
codeToken dict tokenString = case r of (Just a) -> a
                                       (Nothing) -> ""
                                       where r = Map.lookup tokenString dict

-- Code definition as a mapping, note that the code contains variants for upper and lower case characters
regulars = [" ", "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","0","1","2","3","4","5","6","7","8","9"]
codes    = [" ", ".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--..",".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--..","-----",".----","..---","...--","....-",".....","-....","--...","---..","----."]
encodeDict = Map.fromList $ zip regulars codes
decodeDict = Map.fromList $ zip codes regulars



-- ALTERNATE VERSION USING DATA.CHAR.TOUPPER
-- This Code def. doesn't contain the lower case letters
regulars' = [" ", "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","0","1","2","3","4","5","6","7","8","9"]
codes'    = [" ", ".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--..","-----",".----","..---","...--","....-",".....","-....","--...","---..","----."]
encodeDict' = Map.fromList $ zip regulars' codes'
decodeDict' = Map.fromList $ zip codes' regulars'

-- encode' works like encode, but uppercases in between, and it uses the other code dictionairy
encode' (Plain string) = Code $ filter (/="") $ map (codeToken encodeDict') $ map (\a -> [a]) $ map Char.toUpper string

-- identical to decode, just with the other code dictionairy
decode' (Code tokens) = Plain $ foldl (\acc token -> acc ++ codeToken decodeDict' token) "" tokens
