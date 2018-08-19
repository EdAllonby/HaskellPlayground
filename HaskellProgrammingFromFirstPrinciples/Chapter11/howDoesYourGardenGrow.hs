data FlowerType = 
    Gardenia
  | Daisy
  | Rose
  | Lilac deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType deriving Show

-- The following is the above in Normal Form.
data GardenNormalForm =
    Gardenia' Gardener
  | Daisy' Gardener
  | Rose' Gardener
  | Lilac' Gardener deriving Show