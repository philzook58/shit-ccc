{-# LANGUAGE FunctionalDependencies, 
FlexibleInstances, GADTs, DataKinds, TypeOperators, KindSignatures, PolyKinds,
FlexibleContexts, UndecidableInstances, ScopedTypeVariables, NoImplicitPrelude #-}
module Main where

import GHC.TypeLits
import Data.Proxy
import Prelude hiding (id, fst, snd, (.))
import CCC
import Cat



main :: IO ()
main = putStrLn "Hi"

{-
example6 = ccc (\x -> x) 'a'


example7 = ccc (\(x, y) -> x)  -- ('a','b')

example8 = ccc (\(x, y) -> y) --  ('a','b')
example9 = ccc (\(x, y) -> (y,x)) --  ('a','b')
example10 = ccc (\(( x, z),  y) -> (y,x)) -- ((1,'b'),'c')
swappo = ccc $ \(( x, z),  y) -> (x,(z,y))

-}




{-
data App f a = App f a

f $$ x = App f x

plus :: (Int, Int) -> Int
plus (x,y) = x + y
plus' (x,y) = x + y

inc :: Int -> Int
inc = (+ 1)
--example11 = ccc (\(x,y) -> App plus (x,y))

example11 = arrccc (\(V x) -> App inc x) --  $ (1 :: Int)
example12 = arrccc (\(V x,V y) -> plus $$ (x,y))
example13 = arrccc (\(V x,V y) -> inc $$ (plus $$ (x,y)))
example14 :: Num a => (a,a) -> a -- Without this annotation it inferred Integer? Monomorphization?
example14 = ccc (\(V x,V y) -> plus' $$ (x,y))
-}


