data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a)
 deriving Show

rank E = 0
rank (T r _ _ _) = r

makeT x a b = if rank a >= rank b then T (rank b + 1) x a b
              else T (rank a + 1) x b a

