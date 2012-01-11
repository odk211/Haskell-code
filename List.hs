data List a = Nil | Cons a (List a)
 deriving Show

pop :: List a -> List a
pop Nil = Nil
pop (Cons x xs) = xs

push :: a -> List a -> List a
push x xs = Cons x (xs)

peek :: List a -> a
peek Nil = error "Error!!!"
peek (Cons x xs) = x

llength :: List a -> Int
llength Nil = 0
llength (Cons x xs) = 1 + llength xs

get :: Int -> List a -> a
get 0 (Cons x xs) = x
get y (Cons x xs) = get (y-1) xs
get y Nil = error "Error dayo!!"

set :: a -> Int -> List a -> List a
set _ _ Nil = error "set Error"
set y 0 (Cons x xs) = Cons y xs
set y n (Cons x xs) = Cons x (set y (n-1) xs) 

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

lmap :: (a -> b) -> List a -> List b
lmap _ Nil = Nil
lmap f (Cons x xs) = Cons (f x) (lmap f xs)

lfilter :: (a -> Bool) -> List a -> List a
lfilter f Nil = Nil
lfilter f (Cons x xs) = if f x then Cons x remains  else  remains
 where
  remains = lfilter f xs

sort :: Ord a => List a -> List a
sort Nil = Nil
sort (Cons x xs) = append (sort small) (Cons x (sort large))
 where
  small = lfilter ( <= x) xs
  large = lfilter (x < ) xs


lfoldr :: (a -> b -> b) -> b -> List a -> b
--lfoldr f y Nil = y
lfoldr f y (Cons x Nil) = f x y
lfoldr f y (Cons x xs) = f x (lfoldr f y xs)

suffixes ::  [a] -> [[a]]
suffixes [] = [[]]
suffixes ((:) x xs) = (:) ((:) x xs) (suffixes xs)
