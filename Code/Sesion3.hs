-- Beatriz Herguedas Pinedo

-- 1. Definid expresiones Haskell usando funciones de orden superior y/o listas
--    intensionales para representar:

-- a) La lista [1,-1,2,-2,3,-3,...]
lista1 :: (Num a, Enum a) => [a]
lista1 = [x * (-1)^y | x <-[1..], y <-[2,3]]

-- b) Una lista infinita [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),...],
--    que sirva como enumeración de todas las parejas de números naturales
lista2 :: (Num a, Enum a) => [(a,a)]
lista2 = [(y,x - y) | x <- [0..], y <- [0..x]]


-- 2. Programa las siguientes funciones, usando orden superior 
--    y listas intensionales

--a) 'sufijos xs' devuelve la lista de todos los sufijos de xs
sufijos :: [a] -> [[a]]
sufijos xs = [reverse(take x (reverse xs))| x<-[0..length xs]]

--b)'sublista xs' devuelve la lista de todas las sublistas de xs
sublista :: Eq a => [a] -> [[a]]
sublista xs = if xs == [] then [] else [take x xs| x<-[1..length xs]] ++ sublista (drop 1 xs) 

--c) 'perms xs' devuelve la lista de todas las permutaciones de xs
perms :: Eq a => [a] -> [[a]]
permsx [x] = [[x]]
permsx xs = [x:y | x <- xs, y <- permsx(takeWhile (/= x) xs ++ tail(dropWhile (/= x) xs))]

--d) 'sumandos n' devuelve la lista de todas las descomposiciones
--   en sumandos de n
sumandos :: (Num a, Enum a) => a -> [[a]]
sumandos 0 = [[]]
sumandos n = [y | x <- [1..n], y <- (map (x:) (sumandos (n-x))) ]