import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) Nada (Only a) = Only a
    (<>) (Only a) Nada = Only a
    (<>) (Only a) (Only b) = Only $ a <> b

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

check1 = Only (Sum 1) `mappend` Only (Sum 1) == Only (Sum {getSum = 2})
check2 = Only (Product 4) `mappend` Only (Product 2) == Only (Product {getProduct = 8})
check3 = Only (Sum 1) `mappend` Nada == Only (Sum {getSum = 1})
check4 = Only [1] `mappend` Nada == Only [1]
check5 = Nada `mappend` Only (Sum 1) == Only (Sum {getSum = 1})