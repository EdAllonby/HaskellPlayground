{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr

-- Part 1

-- A functor cannot be created for this, because its kind is * -> *
data Bool = False | True

data BoolAndSomethingElse a = False' a | True' a

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' $ f a
    fmap f (True' a) = True' $ f a

data BoolAndMaybeSomethingElse a = Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
    fmap f Falsish = Falsish
    fmap f (Truish a) = Truish $ f a

-- Looks like it should.. But can't get it to work. Todo.
newtype Mu f = InF { outF :: f (Mu f) }

-- Has kind * as it's fully applied, won't work.
data D = D (Array Word Word) Int Int

-- Part 2
data Sum a b = First a | Second b

instance Functor (Sum e) where
    fmap f (First a) = First a
    fmap f (Second b) = Second $ f b

data Company a b c = DeepBlue a c | Something b

instance Functor (Company e e') where
    fmap _ (Something b) = Something b
    fmap f (DeepBlue a c) = DeepBlue a $ f c

data More a b = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L a (f b) a'
    fmap f (R b a b') = R (f b) a (f b')

-- Part 3
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = (Desk a)
    fmap f (Bloor b) = Bloor $ f b

data K a b = K a

-- This is just a Constant we have seen earlier on
instance Functor (K a) where
    fmap _ (K a) = K a 

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip $ K (f a) 

data EvilGoateeConst a b =
    GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a = LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga) 

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa $ fmap f gb

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga $ fmap f gt

data List a = Nil | Cons a (List a)
    
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)

data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a)
                (GoatLord a)
                (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats g h i) = MoreGoats (fmap f g) (fmap f h) (fmap f i)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Show a => Show (TalkToMe a) where
    show Halt = "halt"
    show (Print s a) = concat ["Print ", s, " ", show a]
    show (Read g) = "Read"

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g) = Read (fmap f g)