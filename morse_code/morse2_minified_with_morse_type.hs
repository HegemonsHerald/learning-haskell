import qualified Data.Map as Map
import qualified Data.List as List
data Morse = Plain String | Code [String]
instance Show Morse where
  show (Plain a) = a
  show (Code  a) = show $ List.unwords a
encode (Plain string) = Code $ filter (/="") $ map (codeToken encodeDict) $ map (\a -> [a]) string
decode (Code tokens) = foldl (\acc token -> acc ++ codeToken decodeDict token) "" tokens
codeToken dict tokenString = case r of (Just a) -> a
                                       (Nothing) -> ""
                                       where r = Map.lookup tokenString dict
regulars = [" ", "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","0","1","2","3","4","5","6","7","8","9"]
codes    = [" ", ".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--..",".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--..","-----",".----","..---","...--","....-",".....","-....","--...","---..","----."]
encodeDict = Map.fromList $ zip regulars codes
decodeDict = Map.fromList $ zip codes regulars
