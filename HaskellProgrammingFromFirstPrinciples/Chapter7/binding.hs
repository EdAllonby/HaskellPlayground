addOne :: Integer -> Integer
addOne x = x + 1

bindExp :: Integer -> String
bindExp x =
    let y = 5 in
        "the integer was: "  ++ show x ++ " and y was: " ++ show y

bindExpShadowed :: Integer -> String
bindExpShadowed x =
    let x = 10; y = 5 in -- the input x is shadowed with the let x. x will always be 10.
        "the integer was: " ++ show x ++ " and y was: " ++ show y