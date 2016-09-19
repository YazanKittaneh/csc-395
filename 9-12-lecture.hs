id1 :: a -> a
id1 x = x

bad :: a
bad = (\x -> bad x)
