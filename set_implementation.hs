-- 1) you may change this to your own data type
data Colour = Red | Black
    deriving (Show)

data Set a = Empty | Node Colour (Set a) a (Set a)
    deriving (Show)

-- 2) toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Set a -> [a]
toList xs = setfoldr (:) xs []


-- 3) fromList [2,1,1,4,5] => {2,1,4,5}
fromList :: Ord a => [a] -> Set a
fromList xs = foldr insert empty xs

-- 4) test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where
  s1 == s2 = toList s1 == toList s2


-- 5) the empty set
empty :: Set a
empty = Empty


-- 6) Set with one element
singleton :: a -> Set a
singleton x = Node Black Empty x Empty


-- 7) insert an element of type a into a Set
-- make sure there are no duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert x xs = makeBlack (insert' xs)
    where
        insert' Empty = Node Red Empty x Empty
        insert' (Node colour left y right)
            | x < y = balance colour (insert' left) y right
            | x > y = balance colour left y (insert' right)
            | x == y = balance colour left y right
        
makeBlack (Node _ a y b) = Node Black a y b

balance Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance colour a x b = Node colour a x b

-- Referenced from a paper about red black trees implementation in a functional programming language
-- Link to it: https://www.cs.tufts.edu/comp/150FP/archive/chris-okasaki/redblack99.pdf

-- 8) join two Sets together
-- be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union xs ys = setfoldr insert xs ys


-- 9) return the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
--intersection xs ys = setfoldr (\x acc -> if member x ys then insert x acc else acc) xs Empty
intersection xs ys = setfoldr ifsame xs Empty
   where
      ifsame x acc
         | member x ys = insert x acc
         | otherwise = acc


-- 10) all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference xs ys = setfoldr ifdiff xs Empty
   where
      ifdiff x acc
         | member x ys = acc
         | otherwise = insert x acc


-- 11) is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
member x (Empty) = False
member x (Node _ left num right)
   | x == num = True
   | x < num = member x left
   | x > num = member x right
      


-- 12) how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality xs = setfoldr count xs 0
   where
      count x acc = acc + 1

-- 13) map
setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap func xs = setfoldr (insert . func) xs Empty


-- 14) foldr
setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr func (Empty) acc = acc
setfoldr func (Node _ left num right) acc = setfoldr func left $ func num $ setfoldr func right acc


unOrdInsert :: a -> Set a -> Set a
unOrdInsert x xs = makeBlack (insert' xs)
    where
        insert' Empty = Node Red Empty x Empty
        insert' (Node colour left y right) = balance colour (insert' left) y right
-- Use makeBlack and balance from insert

-- 15) powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: Set a -> Set (Set a)
powerSet xs = foldr unOrdInsert empty [foldr unOrdInsert empty x | x <- (powerList (toList xs))]
powerList :: [a] -> [[a]]
powerList xs = []: foldr go [] xs
    where go x acc = [x]: map (x:) acc ++ acc


-- 16) cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian xs ys = foldr unOrdInsert empty [(x, y) | x <- toList xs, y <- toList ys]


-- 17) partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition f xs = (yes, no)
   where
      yes = setfoldr (\x acc -> if f x then unOrdInsert x acc else acc) xs Empty
      no = setfoldr (\x acc -> if not (f x) then unOrdInsert x acc else acc) xs Empty
