{-# LANGUAGE FunctionalDependencies, 
FlexibleInstances, GADTs, DataKinds, TypeOperators, KindSignatures, PolyKinds,
FlexibleContexts, UndecidableInstances, ScopedTypeVariables, NoImplicitPrelude,
TypeApplications #-}
module Main where

import GHC.TypeLits
import Data.Proxy
import Prelude hiding (id, fst, snd, (.))
import CCC
import Cat



main :: IO ()
main = putStrLn "Hi"


example6 = toCcc (\x -> x) 'a'


example7 = toCcc @FreeCat (\(x, y) -> x)  -- ('a','b')

example8 = toCcc @FreeCat (\(x, y) -> y) --  ('a','b')
example8andahalf = toCcc' (Proxy :: Proxy FreeCat) (\(x, y) -> y)
example9 = toCcc @FreeCat (\(x, y) -> (y,x)) --  ('a','b')
example10 = toCcc @FreeCat (\(( x, z),  y) -> (y,x)) -- ((1,'b'),'c')
swappo = toCcc @FreeCat $ \((x, z),  y) -> (x,(z,y))

example11 = toCcc @(->) $ \(x,y) -> binApp addC x y
example12 = toCcc @(->) $ \(x,y) -> App negateC x

-- infix synonyms
--(+++) = BinApp addC
-- (***) = BinApp mulC
(+++) = binApp addC
(***) = binApp mulC
example13 = toCcc @(->) $ \(x,(y,z)) -> x +++ (y *** z)


{-
data App f a = App f a

f $$ x = App f x

plus :: (Int, Int) -> Int
plus (x,y) = x + y
plus' (x,y) = x + y

inc :: Int -> Int
inc = (+ 1)
--example11 = toCcc (\(x,y) -> App plus (x,y))

example11 = arrtoCcc (\(V x) -> App inc x) --  $ (1 :: Int)
example12 = arrtoCcc (\(V x,V y) -> plus $$ (x,y))
example13 = arrtoCcc (\(V x,V y) -> inc $$ (plus $$ (x,y)))
example14 :: Num a => (a,a) -> a -- Without this annotation it inferred Integer? Monomorphization?
example14 = toCcc (\(V x,V y) -> plus' $$ (x,y))
-}


