--1
data Expr = Num Int
        | Add Expr Expr
        | Mul Expr Expr
        | Sub Expr Expr
        | Div Expr Expr

size :: Expr -> Int --counts the number of operators in expression
size (Num n) = 0
size (Add a b) = 1 + size a + size b
size (Mul a b) = 1 + size a + size b
size (Sub a b) = 1 + size a + size b
size (Div a b) = 1 + size a + size b

eval :: Expr -> Int
eval (Num n)     = n
--eval (Div _ 0)   = error "you divide by 0"
eval (Add a b)   = eval a + eval b
eval (Mul a b)   = eval a * eval b
eval (Sub a b)   = eval a - eval b
eval (Div a b)   = eval a `div` eval b

showExpr :: Expr -> String
showExpr (Num n)   = show n
showExpr (Add a b) =
    showExpr a ++ "+" ++ showExpr b
showExpr (Mul a b) =
    showFactor a ++ "*" ++ showFactor b
showExpr (Sub a b) =
    showExpr a ++ "-" ++ showExpr b
showExpr (Div a b) =
    showFactor a ++ "/" ++ showFactor b

showFactor :: Expr -> String
showFactor (Add a b) =
  "("++showExpr (Add a b)++")"
showFactor e = showExpr e

addd:: Maybe Int -> Maybe Int -> Maybe Int
addd (Just a) (Just b) = Just (a + b)
addd  l        r        = Nothing

mult:: Maybe Int -> Maybe Int -> Maybe Int
mult (Just a) (Just b) = Just (a * b)
mult  l        r        = Nothing

subt:: Maybe Int -> Maybe Int -> Maybe Int
subt (Just a) (Just b) = Just (a - b)
subt  l        r        = Nothing

divi:: Maybe Int -> Maybe Int -> Maybe Int
divi (Just a) (Just b) = Just (a `div` b)
divi  l        r        = Nothing

evall :: Expr -> Maybe Int
evall (Num n)     = Just n
evall (Add a b)   = addd (evall a) (evall b)
evall (Mul a b)   = mult (evall a) (evall b)
evall (Sub a b)   = subt (evall a) (evall b)
evall (Div a b)   = divi (evall a) (evall b)


--2
data Tree a = Leaf  a
            |Split (Tree a) (Tree a)
    deriving (Eq, Show)


collapse :: Tree Int -> [Int]
collapse (Leaf n) = [n]
collapse (Split a b) = collapse a ++ collapse b


mirror :: Tree a -> Tree a
mirror (Leaf n) = Leaf n
mirror (Split a b) = Split (mirror(b)) (mirror(a))


foldTree :: (a->b)->(b->b->b)-> Tree a -> b
foldTree f g (Leaf n) = f(n)
foldTree f g (Split x y) = g(foldTree f g x) (foldTree f g y)

collapse' :: Tree Int -> [Int]
collapse' x = foldTree f g x where
    --f ::  Int -> [Int]
    f a = [a]
   -- f = :[] 
    --g :: [Int] -> [Int] -> [Int]
    g = (++)


--3


--instance Arbitrary a => Arbitrary (Tree a) where
     

prop_MirrorTree :: Eq a => Tree a -> Bool
prop_MirrorTree tree = 
    tree == mirror(mirror tree)

prop_check :: Tree Int -> Bool
prop_check tree = 
    collapse(mirror tree) == reverse(collapse tree)