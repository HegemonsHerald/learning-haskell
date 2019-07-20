import Data.Maybe
import Data.Char
import qualified Data.Map as Map
morseCode = [(' '," "),('A',".-"),('B',"-..."),('C',"-.-."),('D',"-.."),('E',"."),('F',"..-."),('G',"--."),('H',"...."),('I',".."),('J',".---"),('K',"-.-"),('L',".-.."),('M',"--"),('N',"-."),('O',"---"),('P',".--."),('Q',"--.-"),('R',".-."),('S',"..."),('T',"-"),('U',"..-"),('V',"...-"),('W',".--"),('X',"-..-"),('Y',"-.--"),('Z',"--.."),('0',"-----"),('1',".----"),('2',"..---"),('3',"...--"),('4',"....-"),('5',"....."),('6',"-...."),('7',"--..."),('8',"---.."),('9',"----.")]
encodeDict = Map.fromList morseCode
decodeDict = Map.fromList $ map (\(a,b) -> (b,a)) morseCode       -- I would use Data.Tuple.swap, but that'd add another import defeating the point
encode = unlines . map (unwords . catMaybes . map (encodeDict Map.!?)) . words . map Data.Char.toUpper
decode = unwords . map (          catMaybes . map (decodeDict Map.!?) . words) . lines

-- these are 7 lines in total!
-- note, that I inlined the transcode function, so that's a tiny bit of duplicate code...
-- also this uses chars for the chars now, which is good
-- also can I just point out: THESE ARE 5 LINES OF CODE ON 3 LINES OF IMPORTS!
--
-- so really:
-- these are 5 lines! (6 if you use transcode, which you should)
