-- Question 1; implement these function following Data.List library
-- length
length' [] = 0
length' (x:xs) = 1 + length' xs

-- group
group' [] = []
group' [a] = [[a]]
group' [a, b]
    | a == b = [[a,b]]
    | otherwise = [[a], [b]]
group' (x:xs)
    | x == head xs = _mergeSubArray x (group' xs)
    | otherwise = [[x]] ++ group' xs

_mergeSubArray e (x:xs) = [e : x] ++ xs

-- nub

-- filter
filter' p [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

-- head
head' [] = error "empty list"
head' (x:xs) = x

-- tail
tail' [] = error "empty list"
tail' (x:xs) = xs

-- init
init' [] = error "empty list"
init' [a] = []
init' (x:xs) = x : init' xs

-- last
last' [] = error "empty list"
last' [a] = a
last' (x:xs) = last' xs

-- reverse
-- NOTE: it's worst performance cause ++ operater take O(n)
--  the list is linked list structure when we take ++. the cost will take O(n)
--  when n is number of list that is appending
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- concat
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- map
map' p [] = []
map' p (x:xs) = p x : map' p xs

-- concatMap
concatMap' p [] = []
concatMap' p (x:xs) = p x ++ concatMap' p xs

-- any
any' p [] = False
any' p (x:xs)
    | p x = True
    | otherwise = any' p xs

-- all
all' p [] = True
all' p (x:xs)
    | p x = all' p xs
    | otherwise = False

-- and
and' l = all' (== True) l

-- or
or' l = any' (== True) l

-- sum
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- product
product' [] = 1
product' (x:xs) = x * product' xs

-- maximum
-- NOTE: it's worst performance cause recursion
--  idk, how to optimize
-- TODO: can we combine some func with a > b predicate?
maximum' [] = error "empty list"
maximum' [a] = a
maximum' (x:xs)
    | x > maximum' xs = x
    | otherwise = maximum' xs

-- minimum
minimum' [] = error "empty list"
minimum' [a] = a
minimum' (x:xs)
    | x < minimum' xs = x
    | otherwise = minimum' xs

-- iterate
-- NOTE: iterate' is already existing in Data.List
iterate'' p x = x: iterate'' p (p x)

-- repeat
repeat' a = a : repeat' a

-- replicate
replicate' n x = take' n $ repeat' x

-- cycle
cycle' [] = error "empty list"
cycle' l = concat' $ repeat' l

-- take
take' 0 l = []
take' n [a] = [a]
take' n (x:xs) = x : take' (n - 1) xs

-- drop
drop' 0 l = l
drop' n [] = []
drop' n (x:xs) = drop' (n - 1) xs

-- takeWhile
takeWhile' p [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

-- dropWhile
dropWhile' p [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = x : xs

-- span
span' p [] = ([], [])
span' p (x:xs)
    | p x = _mergeSpanResult ([x], []) (span' p xs)
    | otherwise = ([], x:xs)

-- TODO: Rename this
_mergeSpanResult ([], []) ([], []) = ([], [])
_mergeSpanResult (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)

-- elem
elem' x l = any' (== x) l

-- zip
zip' [] l = []
zip' l [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- unzip
unzip' [] = ([], [])
unzip' [(a, b)] = ([a], [b])
unzip' (x:xs) = _mergeSpanResult (_unzipElem x) (unzip' xs)

_unzipElem (a, b) = ([a], [b])

-- delete
delete' w [] = []
delete' w (x:xs)
    | w == x = xs
    | otherwise = x : delete' w xs

-- intersect
intersect' [] l = []
intersect' l [] = []
intersect' (x:xs) yl
    | elem' x yl = x : intersect' xs yl
    | otherwise = intersect' xs yl

-- intersperse
intersperse' i [] = []
intersperse' i [a] = [a]
intersperse' i (x:xs) = x : i : intersperse' i xs

-- intercalate
intercalate' i l = concat' $ intersperse' i l

-- permutations


-- Question 2; implement insert and merge for insertionsort and mergesort

-- insertion sort
insertionsort :: (Ord a) => [a] -> [a]
insertionsort [] = []
insertionsort (x:xs) = insert' x (insertionsort xs)

-- implement insert
insert' :: (Ord a) => a -> [a] -> [a]
insert' a [] = [a]
insert' a [b]
    | a < b = [a, b]
    | otherwise = [b, a]
insert' a (x:xs)
    | a < x = a : x : xs
    | otherwise = x : insert' a xs

-- merge sort
mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
    where
        half = div (length xs) 2
        left = take half xs
        right = drop half xs

-- implement merge
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
