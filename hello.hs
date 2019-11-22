takeAway :: (Int, Int) -> Int
takeAway (x,y) = y - x

subtract' :: Int -> Int -> Int
subtract' x y = y - x

apply :: (a -> b) -> a -> b
apply f x = f x


square :: (Num a)=> a -> a
square x = x * x

