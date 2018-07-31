data TrafficLight = Red | Yellow | Green

-- We only need to define == (not /=) because it is the minimal complete definition.
-- This is because the Eq class is defined by mutual recursion, by implementing ==, we have also implicitely implemented /= (or: not (==)).
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where 
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"