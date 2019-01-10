import qualified Data.Map as Map
import qualified Data.Char as Char
import Data.List

-- What this stuff does:
--
--  - You can create a TokenList; that's just a list of strings, each string is a token.
--  - Using tokenize you can then match a string against that TokenList
--    tokenize will return a list of tokens, in order, that matched literally against the string.
--    When the matching fails or the string has ended, tokenize stops adding tokens to its list.
--    It will always find the longest matching token.
--
--  - This is useful when you want to literally substitute patterns of characters -- tokens -- as
--    e.g. with my current InfProgOO HW, where I am to make a Morse-code encoder and decoder.
--    That is also why tokenize (unlike a regex engine) returns, when a match fails: in that
--    case the input string must have been mal-formed.


type Token  = String
type TokenList = [Token]


-- zips while f holds true
zipWhile :: (a -> a -> Bool) -> [a] -> [a] -> [(a,a)]
zipWhile f x y = takeWhile (\(a,b) -> a `f` b) $ zip x y


-- gets the first elements of all the pairs from zipWhile
zipWhileA :: (a -> a -> Bool) -> [a] -> [a] -> [a]
zipWhileA  f x y = foldr (\(a,_) acc -> a:acc) [] $ zipWhile f x y

zipWhileA' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
zipWhileA' f x y = foldr (\a acc -> (fst a):acc) [] $ zipWhile f x y


-- takes elements from the beginning of x and y while the elements are equal
takeWhileSame :: Eq a => [a] -> [a] -> [a]
takeWhileSame x y = foldr (\(a,b) acc -> if a == b then a:acc else acc) [] $ zip x y


-- yields the longest of a list of strings
longestString :: [String] -> String
longestString strings = foldl (\acc a -> if length a > length acc then a else acc) "" strings


-- yields a list of tokens that fully matched the beginning of string
match :: TokenList -> String -> TokenList
match tokens string = foldr (\token acc ->
                let taken = takeWhileSame string token in
                if taken == token then token:acc else acc
        ) [] tokens


-- returns a list of fully matched tokens against the entire string
tokenize :: TokenList -> String -> TokenList
tokenize _ [] = []      -- without tokens, no token can match
tokenize [] _ = []      -- if there's no string, no token can match; this is the base-case pattern for the recursion of the following line
tokenize tokens string
        | x  == ""  = []                                        -- if you didn't find a matching token, stop
        | otherwise = x:(tokenize tokens xs)                    -- if you found sth, try finding more!
        where x  = longestString $ match tokens string        -- the longest matching token that's in string
              xs = drop (length x) string                     -- what's left of the string without the token



validChars = lowers ++ regulars ++ ["-", "."]
lowers   = [" ", "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","0","1","2","3","4","5","6","7","8","9"]

-- turns the string uppercase...
-- This works basically like encode and decode, with two caveats:
--   - toUpper uses a list of all alphanumerics in upper and lower case variants, not just upper
--   - it also uses a dictionairy, that maps both the lower and upper cases to the upper cases
toUpper string = catMaybes $ map (\key -> Map.lookup key lowerDict) $ tokenize validChars string
        where lowerDict = Map.fromList $ zip validChars (regulars ++ regulars ++ ["-", ".", " "])

--regulars = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]
--codes    = ["!","@","#","$","%","^","&","*","(",")","Ф","[","]","`","~","-","=","{","}","_","+",".","/","á","ś","¯","ŉ","ú","ű","ä","ő","ó","ƣ","ぽ","…",",","‥","∙","⊙","Θ","Μ","Ψ","λ","ρ","μ","υ","ϋ","ω","π","η","ψ","β"]

regulars = [" ", "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","0","1","2","3","4","5","6","7","8","9"]
codes    = [" ", ".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--..","-----",".----","..---","...--","....-",".....","-....","--...","---..","----."]
encodeDict = Map.fromList $ zip regulars codes
decodeDict = Map.fromList $ zip codes regulars

-- encode string into morse code
-- order of function application:
--   - convert to upper case for easier lookup <- I could also just use larger dictionairies for the encoding and decoding but nah
--   - intersperse resulting string with spaces, so we can tell apart, where the morse code letters start and end
--   - tokenize the characters
--   - substitute tokens for morse code equivs
--   - destructure Maybe output from Map.lookup
-- the resulting string is the source string in morse code, with each full morse code character separated by spaces
encode string = catMaybes $ map (\key -> Map.lookup key encodeDict) $ tokenize regulars $ intersperse ' ' $ toUpper string

-- decode string from morse code
-- order of function application:
--   - tokenize the characters
--   - remove every second token; those are the spaces that were interspersed during encoding
--   - substitute remaining tokens for regular code equivs
--   - destructure Maybe output from Map.lookup
-- the resulting string is the source string in morse code, with each full morse code character separated by spaces
decode string = catMaybes $ map (\key -> Map.lookup key decodeDict) $ takeSeconds $ tokenize codes string

-- turns the output of a token-dictionairy lookup into a string by catting the as
catMaybes = foldl (\acc m -> case m of (Just a) -> acc ++ a
                                       (Nothing) -> acc) ""

-- removes every second element of a list
takeSeconds [] = []
takeSeconds [a] = [a]
takeSeconds (a:_:xs) = a:(takeSeconds xs)


-- I'd like to point out, that these are 30 lines for just the morse-encoder-decoder stuff and this could still be made shorter.
-- I SHALL COUNT MY JAVA LINES, WHEN I MAKE THEM!
