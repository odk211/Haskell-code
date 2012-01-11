data Ord a => BSTree a = Emp | Node (BSTree a) a (BSTree a)
 deriving Show

contains :: Ord a => a -> BSTree a -> Bool
contains x Emp = False
--contains x (Node yl y yr) = if (x == y) then True else if (x < y) then (contains x yl) else (contains x yr)   
contains x (Node yl y yr) = f (compare x y)
 where
  f EQ = True
  f LT = contains x yl
  f GT = contains x yr 

insert :: Ord a => a -> BSTree a -> BSTree a
insert x Emp = Node Emp x Emp
insert x (Node yl y yr) = f (compare x y)
 where
  f EQ = Node yl x yr
  f LT = Node (insert x yl) y yr
  f GT = Node yl y (insert x yr) 

complete :: Ord a => a -> Int -> BSTree a
complete _ 0 = Emp
complete x n = Node child x child
 where
  child = complete x (n-1)


complete2 :: Ord a => a -> Int -> BSTree a
complete2 _ 0 = Emp
complete2 x n =  fst $ create2 x n
 where
  create2 :: Ord a => a -> Int -> (BSTree a, BSTree a)
  create2 x 0 = (Emp , Node Emp x Emp)
  create2 x n = f (mod (n - 1) 2)
   where
    f 0 = ((Node sub_0 x sub_0) , (Node sub_1 x sub_0))
    f 1 = ((Node sub_1 x sub_0) , (Node sub_1 x sub_1))
    (sub_0, sub_1) = create2 x (div (n-1) 2)

size :: Ord a => BSTree a -> Int
size Emp = 0
size (Node xl x xr) = (size xl) + (size xr) + 1

balance :: Ord a => BSTree a -> Bool
balance Emp = True
balance (Node xl x xr) = nearlySize && balance xr && balance xl
 where
  nearlySize = ((-1 <= diff) && (diff <= 1))
  diff = (size xl) - (size xr)
