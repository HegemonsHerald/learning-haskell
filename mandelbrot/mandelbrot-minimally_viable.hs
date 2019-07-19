import Data.Complex
precision		= 400
origin		     	= (0.0 :+ 0.0)
mandelbrot c	     	= z * z + c
mandelSeq c	     	= iterate (mandelbrot c) origin
isMandelbrot c	     	= (<=2) $ realPart $ abs $ last $ take precision $ mandelSeq c
mandelField tl br z  	= [ [ isMandelbrot (r :+ i) | r <- [startR, startR+step .. endR] ] | i <- [startI, startI-step .. endI] ]
           where startI = imagPart tl
                 startR = realPart tl
                 endI   = startI + imagPart br - imagPart tl
                 endR   = startR + realPart br - realPart tl
                 step   = 1.0 / z
mandelFieldToString m	= unlines $ map (unwords . map (\c -> if c then " #" else "  ")) m
printMandelbrot tl br z = putStrLn $ mandelFieldToString $ mandelField tl br z
main = printMandelbrot ((-1.5) :+ ( 1.5)) (( 1.5) :+ (-1.5)) 20.0
