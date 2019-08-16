import Data.Char
import Control.Monad

main :: IO ()
main = forever $ putStr "λ " >> handler <$> getLine >>= putStrLn

startsWith :: String -> String -> Bool
startsWith t s = length t == (length $ takeWhile (\(a,b) -> a == b) $ zip t s)

handler :: String -> String
handler s
  | startsWith "uc " s = map toUpper $ drop 3 s
  | startsWith "lc " s = map toLower $ drop 3 s
  | startsWith "rv " s = reverse     $ drop 3 s
  | otherwise = s

{- handler, but with pattern matching. This isn't easy to read, so I'm not using it.
handler ('u':'c':' ':xs) = map toUpper xs
handler ('l':'c':' ':xs) = map toLower xs
handler ('r':'v':' ':xs) = reverse xs
handler s = s
-}



-- on a different note:
import Data.Complex

mandelbrot :: Num a => Complex a -> Complex a -> Complex a
mandelbrot z c = z * z + c

-- all of these do the same...
orbit :: Num a => Complex a -> [ Complex a ]
orbit = scanl mandelbrot (0 :+ 0) . repeat
orbit c = (0 :+ 0) : [ mandelbrot z c | z <- orbit c ]
orbit c = (0 :+ 0) : map (flip mandelbrot c) (orbit c)
orbit c = iterate (flip mandelbrot c) (0 :+ 0)
orbit = flip iterate (0 :+ 0) . flip mandelbrot
