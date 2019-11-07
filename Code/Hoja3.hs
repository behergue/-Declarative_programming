-- Beatriz Herguedas Pinedo

-- 1. Utiliza listas intensionales para representar las siguientes listas:

--    a) [[1; 2; 3; 4; : : : ; 20]; [1; 4; 9; 16; : : : ; 400]; [1; 8; 27; : : : ; 8000]; :::; [1; 210; 310; : : : ; 2010]]

[[x ^ n | x <- [1..20]] | n <- [1..10]]

--    b) [[1; 1; 1; : : : ; 1]; [2; 4; 8; 16; : : : ; 210]; [3; 9; 27; : : : ; 310]; : : : ; [20; 202; 203; : : : ; 2010]]

[[x ^ n | n <- [1..10]] | x <- [1..20]]


-- 2. Elimina, reemplazándolas por funciones auxiliares no locales, las definiciones locales y
--    la lambda-abstracción de la definición siguiente:

f x y = map (\u -> (g u,g (u+1))) y
    where z = x * last y
    g u = (x+z)*u


z :: Num a => a -> [a] -> a
z x y = x * last y

g :: Num a => a -> a -> [a] -> a
g u x y = (x + z x y) * u

lambda :: Num b => b -> [b] -> b -> (b, b)
lambda x y u = (g u x y, g (u + 1) x y)

f :: Num b => b -> [b] -> [(b, b)]
f x y = map (lambda x y) y

-- 3. Elimina las listas intensionales de las siguientes definiciones, usando map, filter y concat:

f :: Integral a => a -> [a]
f n = [x*x | x <- [1..n], mod x 2 == 0]
f n = map (^ 2) (filter even [1..n])

g :: (Num a, Enum a) => a -> a -> [a]
g n m = [x+y | x <- [1..n], y <- [x..m]]
g n m = concat (map (\x -> map (\y -> x + y) [x..m]) [1..n])

h :: (Num a, Enum a) => (a -> Bool) -> a -> a -> [a]
h p n m = [x+y | x <- [1..n], p (n-x), y <- [x..m]]
h p n m = concat (map (\x -> map (\y -> y + x) [x..m]) [1..n]) filter p (n - x) [1..n]

-- 4. Programa usando listas intensionales las siguientes expresiones:

--    a) La lista con los números entre 19 y 50 emparejados cada uno con la lista de sus
--    divisores (excluido el propio número), es decir, la lista:
--    [(19; [1]); (20; [1; 2; 4; 5; 10]); (21; [1; 3; 7]); : : : ; (50; [1; 2; 5; 10; 25])]

[(x, [y | y <- [1..x - 1], mod x y == 0]) | x <- [19..50]]

--    b) La lista de los números perfectos menores que 1000. Un número es perfecto si es
--    igual a la suma de sus divisores (excluido él mismo). Por ejemplo, 6 es perfecto,
--    pues 6=1+2+3.

map fst (filter (\(x, xs) -> sum xs == x)) [(x, [y | y <- [1..x - 1], mod x y == 0]) | x <- [19..50]]

--    c) Generaliza los dos apartados anteriores definiendo funciones, para que no dependan
--    de números naturales concretos sino de los argumentos de la función que definas en
--    cada caso.

listDiv n m = [(x, [y | y <- [1..x - 1], mod x y == 0]) | x <- [n..m]]

perfectos n = map fst (filter (\(x, xs) -> sum xs == x)) [(x, [y | y <- [1..x - 1], mod x y == 0]) | x <- [1..n]]

-- 5. Sea minimoDesde p n una función que devuelve el menor natural mayor o igual que n
--    que satisface la propiedad p. Programa esta función usando funciones de OS y/o listas
--    intensionales. Utilízala para encontrar el primer primo a partir de 692.

minimoDesde :: (Num a, Enum a, Ord a) => (a -> Bool) -> a -> a
minimoDesde p n = head [x | x <- [1..], x > n, p x]

primo :: Integral a => a -> Bool
primo x =
    if (x, [y | y <- [1..x - 1], mod x y == 0]) == (x, [1]) then True
    else False