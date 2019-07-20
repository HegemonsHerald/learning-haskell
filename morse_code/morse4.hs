import Data.Maybe
import Data.Char
import qualified Data.Map as Map

regulars = [' ', 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','0','1','2','3','4','5','6','7','8','9']
codes    = [" ", ".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--..","-----",".----","..---","...--","....-",".....","-....","--...","---..","----."]

encodeDict = Map.fromList $ zip regulars codes
decodeDict = Map.fromList $ zip codes regulars

{- alternate method of creating the dictionairies, that makes this a line shorter...

morseCode = [(' '," "),('A',".-"),('B',"-..."),('C',"-.-."),('D',"-.."),('E',"."),('F',"..-."),('G',"--."),('H',"...."),('I',".."),('J',".---"),('K',"-.-"),('L',".-.."),('M',"--"),('N',"-."),('O',"---"),('P',".--."),('Q',"--.-"),('R',".-."),('S',"..."),('T',"-"),('U',"..-"),('V',"...-"),('W',".--"),('X',"-..-"),('Y',"-.--"),('Z',"--.."),('0',"-----"),('1',".----"),('2',"..---"),('3',"...--"),('4',"....-"),('5',"....."),('6',"-...."),('7',"--..."),('8',"---.."),('9',"----.")]

encodeDict = Map.fromList morseCode
decodeDict = Map.fromList $ map (\(a,b) -> (b,a)) morseCode

-- though you really should use Data.Tuple.swap instead of `(\(a,b) -> (b,a))`

-}

-- (!?) is the lookup infix operator, gets me around a flip on Map.lookup
-- `catMaybes :: [Maybe a] -> [a]` by ignoring the `Nothing`s

transcode :: Ord k => (Map.Map k v) -> [k] -> [v]
transcode d = catMaybes . map (d Map.!?)

encode :: String -> String
encode = unlines . map (unwords . transcode encodeDict) . words . map toUpper

decode :: String -> String
decode = unwords . map (transcode decodeDict . words) . lines







-- Neat reimplementation of catMaybes
--  catMaybes, fromMaybe and isJust are from Data.Maybe
--  sequence is from Control.Monad
--
--  fromMaybe unwraps a Just and replaces a Nothing with a default value
--  `sequence :: [Maybe a] -> Maybe [a]` ie collect the results of running a list of monadic actions
myCatMaybes :: [Maybe a] -> [a]
myCatMaybes = fromMaybe [] . sequence . filter isJust
