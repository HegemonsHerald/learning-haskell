module Iterator ( makeIter )where

import Prelude hiding ( (>>=), fmap, return )
import Data.Complex

-- Time for a useless Monad


data Iterator a = Iterator { getValue :: a,
                             getRest  :: [a],
                             getOp    :: (a -> a) }

makeIter :: (a -> a) -> [a] -> Iterator a
makeIter op baseValues = Iterator { getValue = head baseValues,
                                    getRest  = tail baseValues,
                                    getOp    = op }

next :: Iterator a -> Iterator a
next it = Iterator { getValue = value',
                     getRest  = rest',
                     getOp    = op }
   where value  = getValue it
         rest   = getRest  it
         op     = getOp    it
         value' = if null rest then op value else head rest
         rest'  = if null rest then []       else tail rest ++ [ op value ]

instance (Show a) => Show (Iterator a) where
   show it = "Iterator(" ++ show (getValue it) ++ " : " ++ show (getRest it) ++ ")"



-- This is a Monad with the constraint that all operations happen only on ONE
-- polymorphic type a. That's because we don't really have a way to map the op
-- from (a -> a) to (b -> b)...
-- That's also the reason we don't have an implementation of flatten.

-- Also, technically this is a Monad over the polymorphic type [a].

fmap :: (a -> a) -> Iterator a -> Iterator a
fmap f it = Iterator { getValue = value',
                       getRest  = rest',
                       getOp    = getOp it }
   where value' = f $ getValue it
         rest'  = map f $ getRest it

-- For an iterator to work it needs at least two starting values

return :: [a] -> Iterator a
return baseV = makeIter id baseV

(>>=) :: Iterator a -> ([a] -> Iterator a) -> Iterator a
m >>= f = Iterator { getValue = value',
                     getRest  = rest',
                     getOp    = (op' . op) }
   where value  = getValue m
         rest   = getRest m
         op     = getOp m
         m'     = f $ value : rest
         value' = getValue m'
         rest'  = getRest m'
         op'    = getOp m'



mandelOrbit c = return [ (0 :+ 0) ] >>= (\v -> makeIter (\z -> z * z + c) v)
mo1 = mandelOrbit (0.25:+0.25)

-- sadly this doesn't work, as you can't overwrite fibs on every iteration of calling next...
fibs = makeIter (\n -> n + (getValue $ next fibs)) [1, 1] 
