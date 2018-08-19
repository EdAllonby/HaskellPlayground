-- PugType is the type constructor. It takes no arguments so it is a type constant.
-- PugData is the data constructor, is also a constant. For any function that requires a value
-- of type PugType, you know that value will be PugData.
data PugType = PugData

-- HuskyType is the type constructor. It takes one polymorphic type variable as an argument.
-- HuskyData is the data constructor. 'a' does not occur, therefore the type argument is 'phantom'.
data HuskyType a = HuskyData

-- DogueDeBordeaux is the type constructor. It takes one polymorphic type variable as an argument.
-- DogueDeBordeaux is also the data constructor. It takes the variable as one of its parameters.
-- Although both the type and data constructor share the same name, they are not the same thing.
data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

-- Because the type argument is never used in the data constructor (it is phantom), we can give it whatever we want.
myOtherOtherHusky :: HuskyType [[[[[[[[Int]]]]]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

-- ':k Doggies' has the kind of '* -> *'
data Doggies a = Husky a | Mastiff a deriving (Eq, Show)