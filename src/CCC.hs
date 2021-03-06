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
module CCC (
      CCC
    , App (App)
    , binApp
    , toCcc' 
    , toCcc )where

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
binApp f a b = App f (a,b)

class CCC k a a' b' | a a' -> b' where
   toCcc :: a -> k a' b'
instance (Tag a,
         Build k a b a' b')
    => CCC k (a->b) a' b' where
        toCcc f = build @k @a @b @a' @b' res where  -- build (Proxy :: Proxy labels) (Proxy :: Proxy b) res where
                res = f val 

toCcc' :: CCC k f a' b' => Proxy k -> f -> k a' b' 
toCcc' _ f = toCcc f

class Tag a where
    val :: a

instance (IsTup a flag, Tag' a Z flag n) => Tag a where
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


class Build k input key a' b' | input key a' -> b' where
   build :: key -> k a' b'

instance ( 
    iseq ~ ((a,b) == key),
    In a key isinleft,
    In b key isinright,
    Cond k iseq isinleft isinright (a,b) key a' b'
    ) => Build k (a,b) key a' b' where
    build key = cond @k @iseq @isinleft @isinright @(a,b) @key @a' key

instance (Leaf n ~ b, a' ~ b', Category k) => Build k (Leaf n) b a' b' where
    build _ = id


class Cond k iseq isinleft isinright input key a b | iseq isinleft isinright input key a -> b where
    cond :: key -> k a b
-- Find the key is in the input
instance (a ~ b, Category k) => Cond k 'True x x input key a b where
    cond _ = id
instance (Build k a key a' c', 
    (a',b') ~ ab,
    Cartesian k
    ) => Cond k 'False 'True x (a,b) key ab c' where -- get those input types inferred baby!
    cond key = (build @k @a @key @a' key) . fst
instance (Build k b key b' c', 
    (a',b') ~ ab,
    Cartesian k
    ) => Cond k 'False 'False 'True (a,b) key ab c' where
    cond key = (build @k @b @key @b' key) . snd

-- Otherwise destruct on the key
instance (Build k input key1 a' c', 
          Build k input key2 a' d',
          Cartesian k) => Cond k 'False 'False 'False input (key1,key2) a' (c',d') where
    cond (key1,key2) = fan (build @k @input @key1 @a' key1) (build @k @input @key2 @a' key2)

instance (Build k input key a' b',
    f ~ k b' c',
    Category k)
 => Cond k 'False 'False 'False input (App f key) a' c' where
    cond (App f key) = f . (build @k @input @key @a' key)

-- Could I replace almost everything with App? A very powerful construct
-- This is a of some relation to defunctionalization like in HList
-- Maybe I should build a typelevel FreeCat and then do compilation passes on it

{-
type family (StripLeaf a) where
    StripLeaf (a,b) = (StripLeaf a, StripLeaf b)
    StripLeaf (Leaf n a) = a 
-}





