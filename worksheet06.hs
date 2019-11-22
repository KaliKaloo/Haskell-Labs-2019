--1.1.a
nats:: [Integer]
nats = [1..]

squares :: [Integer]
squares = [ x*x | x <- nats ]

--1.1.c
filter'' :: (a->Bool) -> [a] ->[a]
filter'' p [] = []
filter'' p (x:ys) | p x = x : filter'' p ys 
                  | otherwise = filter'' p ys

  --1.1d
cartesian :: [a] -> [b] -> [(a,b)]
cartesian xs ys =
  [ (x,y)| x <- xs, y <- ys]

--2.1.a
class Pretty a where
  pretty :: a -> String

data Reaction = Happy|Sad|Excited|Angry|Indifferent

instance Pretty Reaction where
  pretty Happy = ":)"
  pretty Sad = ":c"
  pretty Excited = ":D"
  pretty Angry = ">:c"
  pretty Indifferent = ":-|"