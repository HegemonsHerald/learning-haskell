import Data.Complex
isMandelbrot c          = (<=2) $ realPart $ abs $ last $ take 400 $ iterate (\z -> z * z + c) (0.0 :+ 0.0)
mandelField tl br z     = [ [ isMandelbrot (r :+ i) | r <- [startR, startR+step .. endR] ] | i <- [startI, startI-step .. endI] ]
           where startI = imagPart tl
                 startR = realPart tl
                 endI   = startI + imagPart br - imagPart tl
                 endR   = startR + realPart br - realPart tl
                 step   = 1.0 / z
printMandelbrot tl br z = putStrLn $ unlines . map (unwords . map (\c -> if c then " #" else "  ")) $ mandelField tl br z
main = printMandelbrot ((-1.5) :+ ( 1.5)) (( 1.5) :+ (-1.5)) 20

-- 10 LINES!!!
-- And even though I inlined the one-time variables and the mandelSeq code, as
-- well as the mandelFieldToString function this is still a readable piece of
-- code. Bloody awesome!
-- Note: 180 is the largest working zoom for iTerm
