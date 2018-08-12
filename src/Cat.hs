{-# LANGUAGE GADTs, StandaloneDeriving, NoImplicitPrelude  #-}

module Cat where
import Control.Category
import Prelude hiding ((.))


class Category k => Monoidal k where
    par :: k a c -> k b d -> k (a,b) (c,d)

class Monoidal k => Cartesian k where
    fst :: k (a,b) a 
    snd :: k (a,b) b 
    dup :: k a (a,a) 

fan f g = (par f g) . dup

{-
data FreeCat k a b where
    Comp :: FreeCat k b c -> FreeCat k a b -> FreeCat k a c
    Id :: FreeCat k a a
    Morph :: k a b -> FreeCat k a b

data FreeMon k a b where
    Par :: FreeCat a b -> FreeCat c d -> FreeCat (a,c) (b,d)

data FreeNumCat k a b where
    AddC :: FreeNumCat k (a,a) a
    Morph :: k a b -> FreeNumCat k a b

data FreeNum a b = Add a b | Mul a b | Neg

data Add a b = Add a b
instance Addable a b c where
   (+) :: a -> b -> c
instance Num a => Addable a a a -- Not a valid instance set
    (+) = Prelude.(+)
instance Addable a b (Add a b) where
    (+) = Add

-}

data FreeCat a b where
    Comp :: FreeCat b c -> FreeCat a b -> FreeCat a c
    Id :: FreeCat a a
    Fst :: FreeCat (a,b) a
    Snd :: FreeCat (a,b) b
    Dup :: FreeCat a (a,a)
    Par :: FreeCat a b -> FreeCat c d -> FreeCat (a,c) (b,d)

deriving instance Show (FreeCat a b)

instance Category FreeCat where
    (.) = Comp
    id = Id

instance Monoidal FreeCat where
    par = Par

instance Cartesian FreeCat where
    fst = Fst
    snd = Snd
    dup = Dup

instance Monoidal (->) where
    par f g = \(x,y) -> (f x, g y)  

instance Cartesian (->) where
    fst (x,y) = x
    snd (x,y) = y
    dup x = (x,x)

class NumCat k where
    mulC :: Num a => k (a,a) a
    negateC :: Num a => k a a
    addC :: Num a => k (a,a) a

instance NumCat (->) where
    mulC = uncurry (*)
    negateC = negate
    addC = uncurry (+)

