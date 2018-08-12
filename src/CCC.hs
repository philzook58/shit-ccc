{-# LANGUAGE DataKinds, 
    AllowAmbiguousTypes, 
    TypeFamilies, 
    TypeOperators, 
    MultiParamTypeClasses, 
    FunctionalDependencies, 
    PolyKinds, 
    FlexibleInstances, 
    UndecidableInstances,
    TypeApplications,
    NoImplicitPrelude,
    ScopedTypeVariables #-}
module CCC where

import Cat
import Data.Proxy 
import Data.Type.Equality (type (==))
import Prelude (Bool(..))
import Control.Category


-- The unfortunate Incoherent instances I need to force polymorphic values
class IsTup a b | a -> b
instance {-# INCOHERENT #-} (c ~ 'True) => IsTup (a,b) c
instance {-# INCOHERENT #-} (b ~ 'False) => IsTup a b

data Leaf n = Leaf


data Z
data S a


data App f a = App f a

f $$ x = App f x


class Cartesian k => CCC a k a' b' | a a' -> b' where
   ccc :: a -> k a' b'
instance (Tag a n,
         Build a b a' b',
         Cartesian k)
    => CCC (a->b) k a' b' where
        ccc f = build @a @b @a' @b' res where  -- build (Proxy :: Proxy labels) (Proxy :: Proxy b) res where
                res = f val 

class Tag a totaln | a -> totaln where
    val :: a

instance (IsTup a flag, Tag' a Z flag n) => Tag a n where
    val = val' @a @Z @flag

class Tag' a n (flag :: Bool) n'| a n flag -> n' where
    val' :: a

instance (IsTup a flaga,
          IsTup b flagb,
          Tag' a n flaga n'',
          Tag' b n'' flagb n') => Tag' (a,b) n 'True n'  where
    val' = (val' @a @n @flaga, val' @b @n'' @flagb)

instance (a ~ Leaf n) => Tag' a n 'False (S n)  where
    val' = Leaf


type family Or a b where
    Or 'True b = 'True
    Or a 'True = 'True
    Or 'False 'False = 'False

class In a b flag | a b -> flag where
instance (
    ((a,b) == c) ~ isc, 
    flag' ~ Or flaga flagb, 
    flag ~ Or flag' isc,
    In a c flaga, 
    In b c flagb) => In (a,b) c flag
instance ((Leaf n == c) ~ flag) => In (Leaf n) c flag


class Build input key a' b' | input key a' -> b' where
   build :: Cartesian k => key -> k a' b'

instance ( 
    iseq ~ ((a,b) == key),
    In a key isinleft,
    In b key isinright,
    Cond iseq isinleft isinright (a,b) key a' b'
    ) => Build (a,b) key a' b' where
    build key = cond @iseq @isinleft @isinright @(a,b) @key @a' key

instance (Leaf n ~ b, a' ~ b') => Build (Leaf n) b a' b' where
    build _ = id


class Cond iseq isinleft isinright input key a b | iseq isinleft isinright input key a -> b where
    cond :: Cartesian k => key -> k a b
instance (a ~ b) => Cond 'True x x input key a b where
    cond _ = id
instance (Build a key a' c', (a',b') ~ ab) => Cond 'False 'True x (a,b) key ab c' where -- get those input types inferred baby!
    cond key = (build @a @key @a' key) . fst
instance (Build b key b' c', (a',b') ~ ab) => Cond 'False 'False 'True (a,b) key ab c' where
    cond key = (build @b @key @b' key) . snd
instance (Build input key1 a' c', 
          Build input key2 a' d',
          key ~ (key1,key2)) => Cond 'False 'False 'False input key a' (c',d') where
    cond (key1,key2) = fan (build @input @key1 @a' key1) (build @input @key2 @a' key2)
{-
instance (Build input key a' b') => Cond 'False 'False 'False input (App f key) a' b' where
    cond (App f key) = f . (build @input @key @a' key)
-}
{-
type family (StripLeaf a) where
    StripLeaf (a,b) = (StripLeaf a, StripLeaf b)
    StripLeaf (Leaf n a) = a 
-}





