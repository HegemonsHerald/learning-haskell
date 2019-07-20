
-- type system: what do them ! in the def of Complex mean? What does any of this mean?
-- I HAVE A LOT STILL TO LEARN

-- For all the syntax abstractions you should check out the haskell wiki entries, to see how they desugar.
-- That will be insightul with regards to the list comprehension and exactly what you can put into them
-- as constructor expressions... anything really, and the values from outer comprehensions are bound with
-- >>= so they'll be available in the expression, so that's that!



import Data.Complex

{- Constants -}

origin = 0.0 :+ 0.0     -- center of cood-sys
precision = 100         -- how many iterations to check isMandelbrot


{- Mandelbrot Functions -}

mandelbrot :: Complex Double -> Complex Double -> Complex Double
mandelbrot c z = z * z + c

mandelSeq :: Complex Double -> [Complex Double]
mandelSeq c = iterate (mandelbrot c) origin

isMandelbrot :: Complex Double -> Bool
isMandelbrot = (<=2) . realPart . abs . last . take precision . mandelSeq

-- Takes top left corner and bottom right corner of rectangle, as well as a
-- zoom factor and computes which points in the resulting row-major field are
-- in the mandelbrot set.
--
-- Note that r walks left-to-right so positive steps, but
--           i walks top-to-bottom so negative steps
mandelField :: Complex Double -> Complex Double -> Double -> [[Bool]]
mandelField tl br z = [ [ isMandelbrot (r :+ i) | r <- [startR, startR+step .. endR] ] | i <- [startI, startI-step .. endI] ]
   where height = imagPart br - imagPart tl
         width  = realPart br - realPart tl
         startI = imagPart tl
         startR = realPart tl
         endI   = startI + height
         endR   = startR + width
         step   = 1 / z




{- Output as String -}

mandelFieldToString :: [[Bool]] -> String
mandelFieldToString = unlines . map (unwords . map (\c -> if c then " #" else "  "))

printMandelbrot :: Complex Double -> Complex Double -> Double -> IO ()
printMandelbrot tl br z = putStrLn $ mandelFieldToString $ mandelField tl br z

printMandelbrot' :: Complex Double -> Double -> IO ()
printMandelbrot' c z = putStrLn $ mandelFieldToString $ mandelField' c z




{- Nice outputs with mandelField -}

-- Used as input for doubly uncurried printMandelbrot:
--   f a b c              :: a -> b -> c   -> d
--   uncurry f            ::  (a,b) -> c   -> d
--   uncurry $ uncurry f  :: ((a,b), c)    -> d
-- That's ((tl, br), z)
type View = ((Complex Double, Complex Double), Double)


-- full mandelbrot
view1 :: View
view1 = ((((-1.5) :+ ( 1.5))
         ,(( 1.5) :+ (-1.5))), 20)

-- segment with first two islands
view2 :: View
view2 = ((((-1.00) :+ (-0.15))
         ,((-0.45) :+ (-0.70))), 80)

-- same segment but more zoom
view3 :: View
view3 = ((((-1.00) :+ (-0.15))
         ,((-0.45) :+ (-0.70))), 700)

-- one of the fibonacci bobbles of the main island, the large one at the bottom
-- of the segment from the last two
view4 :: View
view4 = ((((-0.57) :+ (-0.5))
         ,((-0.45) :+ (-0.63))), 1000)

-- baby brot
-- in the previous this shows up as 4 '#' in the bottom left corner
view5 :: View
view5 = ((((-0.552) :+ (-0.625))
         ,((-0.547) :+ (-0.628))), 30000)

-- baby brot but way larger
view6 :: View
view6 = ((((-0.552) :+ (-0.625))
         ,((-0.547) :+ (-0.628))), 100000)

main = (uncurry $ uncurry printMandelbrot) view6

-- TIP: play with the precision variable at the top. At 100 the babybrot looks funky, at 10000 it looks perfect!
