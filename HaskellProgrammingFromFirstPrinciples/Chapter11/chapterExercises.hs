data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

-- 1) a) Weekday is a type with 5 data constructors

f Friday = "Miller Time"

-- 2) c) f :: Weekday -> String

-- 3) b) must begin with a capital letter

g xs = xs !! (length xs - 1)

-- 4) c) delivers the final element of xs