-- comenzile din comentarii care încep cu -- \> sunt de scris la consolă

-- :help (sau :?)
-- :set prompt "λ> "
-- :r (:reload) pentru reîncărcarea sursei
-- :! com -- pentru rularea unei comenzi a sistemului de operare
--   (e.g. :! clear / :! cls  sau  :! ls / :! dir)
-- :t add1  -- (:type) -- află tipul oricărei expresii
-- :t (+) -- pentru operatori trebuie dați în forma prefixată

-- de asemenea:
-- :i add1 -- (:info)
-- funcționează și pentru operatori, e.g. :i +


-- Tipuri de date

-- \> :type 1
-- 1 :: Num a => a		-- 1 este de un tip oarecare, cu condiția să fie un tip numeric

-- \> :t 1.2
-- 1.2 :: Fractional a => a		-- 1.2 este de un tip oarecare, cu condiția să fie fracțional
-- \> :t True
-- True :: Bool
-- \> :t False
-- False :: Bool

-- \> :t [1,2,3]
-- [1,2,3] :: Num t => [t]   -- este o listă de elemente de tip t, iar t este un tip numeric
-- \> :t (1, 'a')
-- (1, 'a') :: Num t => (t, Char)  -- o pereche
-- \> :t (1, 'a', True, [5..9])   -- un tuplu
-- (1, 'a', True, [5..9]) :: (Num t1, Num t, Enum t1) => (t, Char, Bool, [t1])
	
-- definire directă a funcției
add1 :: Num t => t -> t -> t -- necesar pentru că sinteza de tip determină că add1 este pe întregi
add1 = \x y -> ((+) x y)
-- (add1 1 2)
-- ((add1 1) 2)
-- add1 1 2
-- \> :t add1

add2 = \x -> \y -> (((+) x) y)  -- cu toate parantezele și în formă curry, dar ne-necesar

-- pattern-matching
add3 = \x y -> x + y

add4 x y = x + y

-- transformare operator -> funcție (formă infixată -> formă prefixată
add5 = (+)
-- transformarea inersă: 1 `add6` 3

-- p-m incomplet (non-exhaustiv)
addX 1 2 = 3
addX 1 x = x + 1
addX 5 6 = 11

-- aplicare parițială
-- \> map (add1 1) [1..5]


-- Perechi (acces cu fst și snd)
-- \> (1, 'a')
-- Tupluri
-- \> ('a', [1,2,3], False)

-- Liste, acces cu head și tail, funcții utile: (:), elem, (!!), (++)
-- \> [1, 2, 3]
-- \> [1..10]
-- \> [1, -1..-20] -- setează pasul listei (rația progresiei aritmetice)
-- \> ['a'..'z']

len1 l = if l == [] then 0
		else 1 + len1 (tail l)

-- pattern-matching
len2 [] = 0
len2 (_:t) = 1 + len2 t
-- _ = nu ma intereseaza sa leg valoarea la un nume

-- case
len3 l = case l of
		[] -> 0
		otherwise -> 1 + len3 (tail l)
len3b l = case l of
	[] -> 0
	_:t -> 1 + len3b t
--inline:
--len3c l = case l of []->0; _:t->1+len3c t
	
-- patterns non-exhaustive
len4 [] = 0
len4 [_] = 1

-- gărzi (vedeți https://wiki.haskell.org/Pattern_guard )
len5 l
	| l == [] = 0
	| otherwise = 1 + len5 (tail l)
	
-- gărzi pe aceeași linie, și where
len6 l | l == [] = 0 | otherwise = 1+lt
	where lt = len6 (tail l)

-- ... și cu let
len7 l | l==[] = 0 | True = 
	let lt = len7 (tail l) in 1 + lt

-- late și inline case
len8 l = let t = tail l in
	case l of [] -> 0 ; otherwise -> 1 + len8 t

len9 [] = 0
len9 [_] = 1
len9 (_:rest@(_:_)) = 1 + len8 rest


sumList [] = 0
sumList (h:t) = h + sumList t

maxList [x] = x
maxList (h:t) = max h (maxList t)

-- altă variantă
maxList2 [] = undefined
maxList2 [x] = x
maxList2 (h:rest@(_:_)) = max h $ maxList2 rest

-- descompunere structuri cu pattern-matching
fst3 (x, _, _) = x
snd3 (_, x, _) = x
thr3 (_, _, x) = x

-- legare folosind pattern-matching
fst2 p = let (x, _) = p in x


-- List comprehensions
evens = [x | x <- [0..], mod x 2 == 0]
multiples n = [x | x <- [1..], mod x n == 0]
cart l1 l2 = [(x, y) | x <- l1, y <- l2]
squareMult d = [(n, n * n) | n <- [0..], n `mod` d == 0]


primes = sieve [2..]
	where sieve (p:xs) = 
		p : sieve [x | x <- xs, x `mod` p /= 0]
		-- sau
		--p : sieve (filter (\x -> mod x p /= 0) xs)
		-- sau
		--p : sieve (filter ((/= 0) . (flip mod $ p)) xs)

mapLC f l = [f x | x <- l]
filterLC f l = [x | x <- l, f x] -- sau f x == True


-- Funcționale

lenF l = foldl (\lenPart _ -> lenPart + 1) 0 l
sumF = foldl (\sumPart e -> sumPart + e) 0


inWhichList x ll = map (elem x) ll
-- \> inWhichList 2 [[1..4], [5..6], [0,2..6], [1,3..7]]

isInAnyList x ll = 
	not $ null $ filter (elem x) ll
-- \> isInAnyList 2 [[1..4], [5..6], [0,2..6], [1,3..7]]
	
-- \> filter ((flip elem) [1,3..100]) [2,1,5,3,4,8,7]



-- Fluxuri

-- în Racket: (define ones (stream-cons 1 ones))  -- dar în Haskell evaluarea este lazy oricum, deci
ones			=	1 : ones

naturals = let natFrom start = start : natFrom (start+1) in natFrom 0
naturals2 = 0 : (zipWith (+) ones naturals2)
naturalsLC		=	0 : [n + 1 | n <- naturalsLC]

evenNaturals1 = filter even naturals2
evenNaturals2 = zipWith (+) naturals2 naturals2

powersOf2 = 1 : (zipWith (+) powersOf2 powersOf2)
powersOf2A = 1 : (zipWith (*) (zipWith (+) ones ones) powersOf2)

fibo = 0 : 1 : (zipWith (+) fibo (tail fibo))




-- căutare în spațiul stărilor

symbols = ['a'..'c']
expand s = map (flip (:) $ s) symbols
palindrome s = s == reverse s

bfs init expand isGoal =
	let search frontiera
			| frontiera == [] = []
			| isGoal state = state : other_sols
			| otherwise = other_sols
		where -- atenție la indentare, where este indentat între let și gărzi
			state = head frontiera -- va fi evaluat doar dacă state este necesar
									-- (când frontiera este nevidă)
			other_sols = search $ tail frontiera ++ expand state -- la fel
	in search [init]

-- verificați tipul cu  :t bfs

palindromes = bfs "" expand palindrome





