-- 1
-- class Monad where
--     return :: a->ma 
--     (>>=)  :: ma->(a->mb)->mb
--     (>>)   :: ma->mb->mb    

app::Monad m => m(a->b) -> m a -> m b
app mf mx = do f <- mf
               x <- mx
               return (f x)   

ap::Monad m => m(a->b) -> m a -> m b
ap mf mx = mf >>= (\f ->
              mx >>= (\x -> 
                return (f x)))


--2
-- instance Monad Maybe where
--     return x          = Just x
--     Nothing >>= _    = Nothing
--     (Just x) >>= f    = f x
--     fail _            = Nothing

safeDiv:: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just (x `div` y)

div2 :: Int -> Int -> Int -> Maybe Int
div2 _ _ 0 = Nothing
div2 _ 0 _ = Nothing
div2 x y z = Just ((x `div` y ) `div` z)



--3
map' :: (a->b) ->[a]->[b]
map' _ [] = []
map' f(x:xs) = f x : map' f xs

liftM :: Monad m => (a->b) -> m a -> m b
liftM f xs = do x <- xs 
                return (f x)
-- lifeM takes a monadic value and a function and maps that function over the monadiv value. 

join :: Monad m => m (m a) -> m a
join mm = do
    m <- mm
    m
--if we have more than one monad value nested inside eachother, you can flatten it out to just one. e.g. Just(Just 9) == Just 9



--4: LAWS valid instances of Monad typeclass should satisfy
-- 1)monad left unit         return x >>= f = f x
-- 2)monad right unit        mx >> return = mx
-- 3)monad associativity     mx >>= f >> g = ms >>= (\x -> f x  >>= y)

-- return :: a -> m a for Maybe
-- a function that takes a value type a and returns a value type Maybe a
-- Just x and Nothing both have the same type as return; both take a value type a and return a value type Maybe a

-- Law 1) return x >>= f = fx
--   - return x = Just x
--     Just x >>= f  should equal f x

--     case x of
--         Nothing -> Nothing
--         Just n -> f n
    
--     True for return x = Just x

--     Nothing >>= _ returns Nothing. Law does not work for Nothing


-- Law 2) mx >>= return = mx
--      mx = Just x
--      Just x >>= return    should equal return x 
--      return is Just x because of the definiation of return

    
--     True for mx == Just x
 
--     mx = Nothing

--     Nothing >>= return   should equal Nothing


Law 3) (m x >>= f) >>= g = m x >>= (\x -> f x >>= g)
        mx = Just x
        (Just x >>= f) >>= g = Just x >>= (\x -> f x >>= g)
        (Just x >>= f) >>= g                                 ==                Just x >>= (\x -> f x >>= g)
                                                                               \x -> f x.. is a function that applies on x
        Just x >>= f  == f x   according to defintion of instance Maybe        Just x >>= (function) ==  f x
        f x >>= g                                                              f x >>= g
        Both sides are equal, therefore the law is true for Just x

        Nothing >>= f >>= g
        Nothing >>= g  == Nothing
        Nothing = Nothing
        So true for Nothing as well


Do Notation Transformations matched with Laws 
Left unit law: return x >>= f = f x
  do y <- return x
     f y
  =
  do f x


-- Right unit law: mx >> return = mx
--   do x <- m x
--     return x
--   =
--   do m x

-- Associative Law: mx >>= f >> g = ms >>= (\x -> f x  >>= y)
--   do x <- m x
--     do y <- f x
--         g y
--   =
--     do y <- do x <- m x
--                f x
--        g y