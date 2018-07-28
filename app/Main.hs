{-# LANGUAGE FunctionalDependencies, 
FlexibleInstances, GADTs, DataKinds, TypeOperators, KindSignatures, PolyKinds,
FlexibleContexts, UndecidableInstances, ScopedTypeVariables, NoImplicitPrelude #-}
module Main where

import Lib
import GHC.TypeLits
import Data.Proxy
import Prelude hiding (id, fst, snd, (.))
main :: IO ()
main = someFunc


-- We will use these wrappers to know when we've hit polymorphism

newtype V a = V a
--data Apply f a = Apply f a


example = \(V x, V y) -> x
example2 = \((V x, V y), V z) -> (y,z)
-- example3 = \(F f) -> \(V x, V y) -> f x

{-
-- Simple typelevel lambda calc
class Apply a b r | a b -> r

instance (a ~ c) => Apply (a->b) c b
-}

data Z
data S a

{-
class Tag a b c | a b -> c
instance (Tag b (S n) c, a ~ n) => Tag (a -> b) n c
-}
-- What if I tagged (a ~ (S n, c))

class Test a where
    test :: Proxy a
instance (Z,c) ~ a => Test a where
    test = Proxy

-- Can I rebuild a polymorphic function? It seems I can by including the new variables in
-- the typeclass sig

{-
-- Yeah, I can do it if I don't make the variables functions of the inputs.
-- Sure. Makes sense. They aren't. Duh.
class Test a c d where 
    fun :: Proxy a -> c -> d
instance Test (S Z, Z) (a,b) (b,a) where 
    fun _ = \(x,y) -> (y,x)  
instance Test (V Z) a a where
    fun _ = id
-}

{-
class BuildPoly n d r where
    fun :: Proxy n -> Proxy d -> r
instance BuildPoly n d r => BuildPoly (S n) d (a->r) where
    fun _ p = \x -> fun (Proxy :: Proxy n) p
instance BuildPoly Z () ()  where
    fun _ _ = ()
-}

{-
class Tag a b c d | a b -> c d
instance (Tag a n n'' r, Tag b n'' n' d) => Tag (a, b) n n' (r,d)
instance (a ~ n) => Tag (V a) n (S n) (V n)

instance  (Tag a Z tot labelled,  ) =>   (a -> b)
-}

data Leaf n a = Leaf
data Node n a b = Node a b
-- maybe if I move d back from the outputs It could infer?
    -- not so much
    -- 
    {-
class Tag t a b c d | t a b -> c d
instance (Tag t a n n'' r1, Tag t b n'' n' r2) => Tag t (a, b) n n' (Node n'' r1 r2)
instance (a ~ Leaf n c, c ~ t) => Tag t (V a) n (S n) (Leaf n c)
-- I could perhaps use b to infer the type of c.
-- b will be a matching structure to the input of a.
-- faiing that will fail to find an instance
-- yes that might work
class Top t a b c | t a -> b c where
   ccc :: Proxy t -> a -> b -> c
instance (Tag t a Z n labels,
         Build labels b c d)
    => Top t (a->b) c d where
        ccc _ _ = build (Proxy :: Proxy labels) (Proxy :: Proxy b)
-}
ccc' :: Top a b c k => Proxy k -> a -> k b c
ccc' _ f = ccc f
{-
ccc'' :: (Top a b c, CartesianCategory k) => a -> k b c
ccc'' f = ccc f
-}
class Tag a b c d mono | a b mono -> d c where
    val :: Proxy a -> Proxy b -> Proxy mono -> a
instance (Tag a n n'' r1 a', 
    Tag b n'' n' r2 b', (a', b') ~ q) => Tag (a, b) n n' (Node n'' r1 r2) q where
    val _ _ _ = (val (Proxy :: Proxy a) (Proxy :: Proxy n) (Proxy :: Proxy a'), val (Proxy :: Proxy b) (Proxy :: Proxy n'') (Proxy :: Proxy b'))
instance (a ~ Leaf n a') => Tag (V a) n (S n) (Leaf n a') a' where
    val _ _ _ = V Leaf

class CartesianCategory k => Top a b c k | a b -> c where
   ccc :: a -> k b c
instance (Tag a Z n labels c,
         Build labels b c d k,
         CartesianCategory k)
    => Top (a->b) c d k where
        ccc f = build (Proxy :: Proxy labels) (Proxy :: Proxy b) res where
                res = f (val (Proxy :: Proxy a) (Proxy :: Proxy Z) (Proxy :: Proxy c))

--fan f g = \x -> (f x, g x) --- &&&
fan f g = (par f g) . dup
class CartesianCategory k => Build labels b c d k | labels b -> c d where
    build :: Proxy labels -> Proxy b -> b -> k c d
instance (Build labels b i o1 k, Build labels c i o2 k) => Build labels (b,c) i (o1,o2) k where
    build pl pbc (x,y) = fan (build pl (Proxy :: Proxy b) x) (build pl (Proxy :: Proxy c) y)
instance (Extract labels n a b, CartesianCategory k) => Build labels (Leaf n c) a b k where
    build pl pb _ = extract pl (Proxy :: Proxy n)
instance (Build labels c a b k, CartesianCategory k) => Build labels (App (k b d) c) a d k where
    build pl pb (App f x) = f . (build pl (Proxy :: Proxy c) x)


class StripN a b | a -> b 
instance (StripN a a', StripN b b') => StripN (Node n a b) (a',b')
instance StripN (Leaf n a) a

class Extract a n d r | a n -> d r where
    extract :: CartesianCategory k => Proxy a -> Proxy n -> k d r
instance (LT n n' gt, -- which one is greater
          StripN (Node n' a b) ab,
          FstSnd gt ab r1, -- get value level rep of this
          ITE gt a b c, -- Select to go down branch
          Extract c n r1 r) -- recurse
            => Extract (Node n' a b) n ab r where
    extract _ p = (extract (Proxy :: Proxy c) p) . (fstsnd (Proxy :: Proxy gt))
instance Extract (Leaf n a) n a a where
    extract _ _ = id

-- Wait. In actual use, will it be able to deduce which instance to use
-- Uhhhh probably not. Shit. That is why I need tht functional dependency
-- Is it ok? 
{-
class CatExtract a n d r | a n d -> r where
    extract :: Proxy a -> Proxy n -> d -> r
instance (GTE n n1 gt, -- which one is greater
          FstSnd gt (a, b) r1, -- get value level rep of this
          ITE gt n1 n2 n3, -- Select to go down branch
          CatExtract n3 n r1 r) -- recurse
            => CatExtract (n1,n2) n (a,b) r where
    extract _ p = (extract (Proxy :: Proxy n3) p) . (fstsnd (Proxy :: Proxy gt))
instance CatExtract n n a a where
    extract _ _ = id
-}
{-
example4 :: Extract (Z, S Z) Z (a,b) b => (a,b) -> b
example4 = extract (Proxy :: Proxy (Z, S Z)) (Proxy :: Proxy Z)

example5 :: Extract (Leaf Z Int) Z Int Int => Int -> Int
example5 = extract (Proxy :: Proxy (Leaf Z Int)) (Proxy :: Proxy Z)
-}
-- example6 :: Int -> Int
arrccc :: (Top a b c (->)) => a -> b -> c
arrccc = ccc' (Proxy :: Proxy (->))

-- applying the category let's us imply arrow
example6 = ccc (\(V x) -> x) 'a'

--example7 ::  -> Int

--example7 :: CartesianCategory k => k _ _
example7 = arrccc (\(V x, V y) -> x)  -- ('a','b')

example8 = arrccc (\(V x, V y) -> y) --  ('a','b')
example9 = arrccc (\(V x, V y) -> (y,x)) --  ('a','b')
example10 = arrccc (\((V x,V z), V y) -> (y,x)) -- ((1,'b'),'c')
swappo = arrccc $ \((V x,V z), V y) -> (x,(z,y))

{-
instance CatExtract (FstSnd (n1,n2) (Greater n n1)) n => CatExtract (n1,n2) n d r
    extract = extract . snd

instance CatExtract n2 (GreaterThan n) n => CatExtract (n1,n2) False n
    extract = extract . fst

instance CatExtract (V n) n where
    extract = id
-}


class FstSnd a d r | a d -> r where
    fstsnd :: CartesianCategory k => Proxy a -> k d r 

instance FstSnd 'True (a,b) a where
    fstsnd _ = fst

instance FstSnd 'False (a,b) b where
    fstsnd _ = snd



class Fst a b | a -> b
instance Fst (a,b) a

class Snd a b | a -> b
instance Snd (a,b) b

class ITE a b c d | a b c -> d
instance ITE 'True a b a
instance ITE 'False a b b

class GT a b c | a b -> c
instance GT a b d => GT (S a) (S b) d
instance GT Z (S a) 'False
instance GT (S a) Z 'True
instance GT Z Z 'False

class LT a b c | a b -> c
instance LT a b d => LT (S a) (S b) d
instance LT Z (S a) 'True
instance LT (S a) Z 'False
instance LT Z Z 'False

-- of questionable utility
class Any a
instance Any a

-- demarcate a function
--data F a b = F (a->b)
-- Top (F f -> b)
-- Can I catch usage of Additive? Additive a =>
-- No not so much


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

-- instance Build

-- No this is clearly a problem
class Vify a b | b -> a 
instance (Vify a c, Vify b d) => Vify (a,b) (c,d)
instance Vify a (V a)

-- Now we can use Generic1 to get a handle on typeparameter locations
-- but can we use that without destroying the fully polyorphic nature needed for labelling?

class CartesianCategory k where
    (.) :: k b c -> k a b -> k a c
    id :: k a a
    fst :: k (a,b) a
    snd :: k (a,b) b
    dup :: k a (a,a)
    par :: k a c -> k b d -> k (a,b) (c,d)

instance CartesianCategory (->) where
    id = \x -> x
    fst (x,y) = x
    snd (x,y) = y
    dup x =(x,x)
    f . g = \x -> f (g x)
    par f g = \(x,y) -> (f x, g y)  

-- could carry around a seperate polymorphism pump? to generate new polymohpc variables
-- a~ (a',Int) => a

