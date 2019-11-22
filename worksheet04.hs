--2
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:y) = x + sum y

fromJusts :: [Maybe a] -> [a]
fromJusts [Nothing] = undefined
fromJusts [(Just xs)] = [xs]

merge :: [Int] -> [Int] -> [Int]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) | x > y  = y : merge (x:xs) ys
merge (x:xs) (y:ys) | otherwise = x : merge xs (y:ys)


--5

data List a = Empty
            |Cons a (List a)
  deriving Eq

toList :: [a] -> List a 
toList[] = Empty
toList(x:xs) = Cons x (toList xs)


repeat' :: a -> [a]
repeat' x = x: repeat' x
