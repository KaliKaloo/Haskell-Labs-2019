sum'' :: [Integer] -> Integer
sum'' [] = 0
sum'' (x:xs) = x + sum'' xs

foldr' :: (a->b->b) -> b -> [a] -> b
foldr' op z [] = z
foldr' op z (x:xs) = 
  x `op` foldr' op z xs

--3.a
sum' :: [Integer] -> Integer
sum' = foldr' (+) 0

--3.b
product' :: [Integer] -> Integer
product' = foldr' (*) 1

--3.c
and' :: [Bool] -> Bool
and' = foldr' (&&) True

--3.d
or' :: [Bool] -> Bool
or' = foldr' (||) False

--3.e
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr((&&) . p) True

--3.f
any' :: (a -> Bool) ->[a] -> Bool
any' p = foldr((||) . p) False

--3.g
length' :: [a] -> Int
length' =
  foldr(\xs -> (+) 1) 0
 

--4
reverse' ::[a] ->[a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

snoc::[a] -> a -> [a]
snoc xs x = xs ++ [x]

reverse'' :: [a] ->[a]
reverse'' = foldr(flip snoc) []

--5
filter' ::(a->Bool) ->[a]->[a]
filter' p xs = foldr(pred p) [] xs --pred p  is function with parameter p
  where
    pred p x xs  --define the funcction and what it does in where
      | p x = x:xs  --use guards to say each case. if p x is true then return x as list
      | otherwise = xs  -- if x doesnt satisfy then print out the rest of the list
--filter' p xs = [x | x <- xs, p x]

--6
group ::Eq a => [a] -> [[a]]
group = foldr f []
      where
        f x [] = [[x]]
        f x ((y:ys):yss)
          | x == y = ((x:y:ys):yss)
          | otherwise = [x]:((y:ys):yss)

--7
transpose:: [[a]]->[[a]]
transpose=foldr f[ ]
  where 
    f xs[ ] = map(:[ ])xs-- creates an empty list per item 
    f xs xss = zipWith(:)xs xss