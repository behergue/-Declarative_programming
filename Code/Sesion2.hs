-- Beatriz Herguedas

-- 1. Definid funciones recursivas en Haskell para calcular las siguientes
--    expresiones:

--a) La lista de los cuadrados de los números naturales entre 0 y n, o sea:
--   [0,1,4,9,...,n^2]
cuadrados :: (Num a, Eq a) => a -> [a]
cuadrados n = if n == 0 then [0] else cuadrados (n-1) ++ [n^2]

--b) La lista anterior, pero con cada número emparejado con su cuadrado y
--   en orden inverso: [(n,n^2),...,(2,4),(1,1),(0,0)]
cuadrados2 :: (Num a, Eq a) => a -> [(a, a)]
cuadrados2 n = if n == 0 then [(0, 0)] else ([(n, n^2)] ++ cuadrados2 (n-1))

--c) La suma Sum_(i=1)^(n){i * |sin(i)|}
sumatorio :: (Eq a, Floatig a) => a -> a 
sumatorio n = if n == 1 then abs (sin 1) else n * (abs (sin n)) + sumatorio (n - 1)

--d) El número de potencias de 3 menores que n y que acaben en 67
pot3 :: Integral a => a -> [a]
pot3 n = filter (\y -> mod y 100 == 67) (filter (\x-> mod x 3 == 0) [1..n])

--e) La suma de los números menores que n, que sean múltiplos
--   de 3 ó 5
mult35 :: Integral a => a -> a
mult35 n = sum (filter (\x -> mod x 3 == 0 || mod x 5 == 0) [1..n])


-- 2. Programa, utilizando funciones de orden superior predefinidas, las
--    siguientes funciones de orden superior. No olvides declarar sus tipos

--a) 'filter2 xs p q = (us, vs)' donde 'us' son los elementos de 'xs'
--   que cumplen 'p' y 'vs' los que cumplen 'q'
filter2 :: [a] -> (a -> Bool) -> (a -> Bool) -> ([a], [a])
filter2 xs p q  = (filter p xs, filter q xs)

--b) 'filters xs ps = [xs1,...,xsn]', donde 'xsi' son los elementos de
--   'xs' que cumplen 'pi', supuesto que 'ps = [p1,...,pn]'
filters :: [a] -> [a -> Bool] -> [[a]]
filters xs ps = [filter p xs | p <- ps]

--c) 'mapx x [f0,...,fn] = [f0 x,..., fn x]'
mapx :: a -> [a -> b] -> [[b]]
mapx x fs= [map f x| f <- fs]

--d) 'iguales f g n m <-> f x = g x', para todo n <= x <= m
iguales :: (Enum a, Eq b) => (a -> b) -> (a -> b) -> a -> a -> Bool
iguales f g n m = if map f [n..m] == map g [n..m] then True else False

--e) 'cuantos p xs' = número de elementos de la lista 'xs' que cumplen
--   la propiedad p
cuantos :: (a -> Bool) -> [a] -> Int
cuantos p xs = length(filter p xs)

--f) 'menorA n m p' = menor x con n <= x <= m que verifica p
menorA :: Enum a => a -> a -> (a -> Bool) -> a 
menorA n m p = head(filter p [n..m])

--g) 'mayor n p' = mayor x <= n que  verifica p
mayor :: (Ord a, Num a, Enum a) => a -> (a -> Bool) -> a
mayor n p = foldr1 max (filter p [0..n])

--h) 'ex n m p' <-> existe x con n <= x <= m que verifica p
ex :: Enum a => a -> a -> (a -> Bool) -> Bool
ex n m p = any p [n..m]


--3. Define mediante 'foldr' o 'foldl', en lugar de mediante recursión explícita,
--   las siguientes funciones: 

--   PS: Expresar mediante lambda-expresiones el primer argumento de la función
--   'fold' que utilices

--a) last
lastx :: Foldable t => t a -> a
lastx xs = foldl (\x y -> y) undefined xs

--b) reverse
reversex :: Foldable t => t a -> [a]
reversex xs = foldl (\ys y-> y:ys) [] xs

--c) all
allx :: Foldable t => (a -> Bool) -> t a -> Bool
allx p xs = foldl (\xs y -> xs && p y) True xs

--d) minimum
minimumx :: Ord a => [a] -> a
minimumx xs = foldr (\x ys -> min x ys) (head xs) xs

--e) map
mapxb :: Foldable t => (b -> a) -> t b -> [a]
mapxb f xs = foldr (\x ys-> (f x):ys) [] xs

--f) filter
filterx :: Foldable t => (a -> Bool) -> t a -> [a]
filterx p xs = foldr (\x ys -> (if p x then x:ys else ys)) [] xs

--g) takeWhile
takeWhilex :: Foldable t => (a -> Bool) -> t a -> [a]
takeWhilex p xs = foldr (\x ys -> (if p x then x:ys else [])) [] xs

--h) (++)
(+++) :: Foldable t => t a -> [a] -> [a]
(+++) xs ys = foldr (\x y -> x:y) ys xs

-- 4. Programa, indicando los tipos, las siguientes variantes de 'foldl'
--    y 'foldr', que operan con listas no vacías y no usan valor acumulado
--    inicial:

--a) foldr (asociando por la derecha)
foldr2 :: (a -> b -> b) -> [a] -> b
foldr2 f [x]    = x
foldr2 f (x:xs) = f x (foldr2 f xs)

--b) foldl (asociando por la izquierda)
foldl2 :: (a -> a -> a) -> [a] -> a
foldl2 f [x]    = x
foldl2 f (x:xs) = foldl f x xs

-- 5. Programa al menos tres de los apartados del primer ejercicio utilizando
--    funciones de orden superior en lugar de recursión explícita

--a) La lista de los cuadrados de los números naturales entre 0 y n, o sea:
--   [0,1,4,9,...,n^2]
cuadradosb :: (Num a, Enum a) -> a -> [a] 
cuadradosb n = map (^2) [0..n]

--b) La lista anterior, pero con cada número emparejado con su cuadrado y
--   en orden inverso: [(n,n^2),...,(2,4),(1,1),(0,0)]
cuadradosc :: (Num a, Enum a) => a -> [(a, a)]
cuadradosc n = reverse (zip ([0..n]) (map (^2) [0..n]))

--d) El número de potencias de 3 menores que n y que acaben en 67
pot3b :: Integral a => a -> Int
pot3b n = length (filter (\y -> mod y 100 == 67) (filter (\x-> mod x 3 == 0)[1..n]))