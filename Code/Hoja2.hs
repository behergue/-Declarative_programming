-- Beatriz Herguedas Pinedo

-- 1. Determina razonadamente cuál es el tipo (cualificado si es necesario) de las funciones
--    definidas por las siguientes ecuaciones:

--    a)
f1 :: Ord a => a -> a -> a
f1 x y = if x < y then x else y

--    b)
f2 :: Num a => (a -> b) -> a -> b
f2 x y = x (y + 1)

--    c)
f3 :: Num a => (b -> a) -> b -> a
f3 x y = (x y) + 1

--    d)
f4 :: ((a -> b) -> b -> c) -> (a -> b) -> a -> c
f4 x y z = x y (y z)


-- 2. Supongamos una representación de los números racionales por medio de pares de enteros.
--    Es decir, n / m se representa como (n, m).

--    a) Escribe una función simplifica que dada una fracción en forma de par, devuelva
--    otra equivalente lo más simplificada posibles. Por ejemplo:
--    simplifica (15,9) = (5,3).

simplifica :: Integral b => (b, b) -> (b, b)
simplifica (x, y) = let mcd = gcd x y in 
    if mcd /= 1 then (div x mcd, div y mcd)
    else (x, y)

--    b) Escribe una función para calcular el máximo común divisor de dos enteros positivos.

-- Es el algoritmo de Euclides
mcd :: (Eq a, Num a, Num b) => a -> a -> b
mcd x 0 = abs x
mcd x y = mcd y (mod x y)

--    c) Escribe una función para calcular el mínimo común múltiplo de dos enteros positi-
--    vos.

mcm :: (Eq a, Num a, Num b) => a -> a -> b
mcm 0 0 = 0
mcm x y = div (x * y) (mcd x y)

--    d) Utilizando las funciones anteriores define una función para cada una de las opera-
--    ciones de abajo. Todas ellas deben devolver la expresión simplificada de la fracción
--    resultado. No olvides anotar las funciones con su tipo.

--    Suma de dos números racionales.
suma :: Integral a => (a, a) -> (a, a) -> (a, a)
suma (x, y) (z, t) = simplifica (x * t + z * y, y * t)

--    Resta de dos números racionales.
resta :: Integral a => (a, a) -> (a, a) -> (a, a)
resta (x, y) (z, t) = simplifica (x * t - z * y, y * t)

--    Multiplicación de dos números racionales.
mult :: Integral a => (a, a) -> (a, a) -> (a, a)
mult (x, y) (z, t) = simplifica (x * z, y * t)

--    División de dos números racionales.
div :: Integral a => (a, a) -> (a, a) -> (a, a)
div (x, y) (z, t) = mult (x, y) (t, z)

--    Elevar un número racional a una potencia entera (incluye negativos).
elev  :: (Integral a, Integral b) => (a, a) -> b -> (a, a)
elev = 
    if a > 0 then simplifica (x ^ a, y ^ a)
    else
        if a < 0 then elev (x, y) (negate a)
        else (1,1)


-- 3. Simplifica las siguientes expresiones siempre que estén bien tipadas:
--    a)
(\x y -> y x) 2
--  Da error

--    b)
(\x y -> y x) 2 (\x -> x + 1) = 3

--    c)
(\x -> \y -> x y) (\z -> z + 1) 2 = 3

--    d)
(\x -> \y -> y/x) 2
-- Da error

--    e)
(\x y -> y * x) 2 (\x -> x + 1)
-- Da error

--    f )
(\x y z -> y x (z x)) 2 (\x y -> y * x)
-- Da error

--    g)
(\x y z -> y x (z x)) 2 (\x y -> y * x) (\x -> x + 1) = 6

--    h)
let y = (\x -> x + 1) in y 2 = 3

--    i )
(\x -> x + 1) (let y = \x -> x + 1 in y 2) = 4


-- 4. Indica razonadamente cuál es el tipo (cualificado si es necesario) de las siguientes lambda-
-- expresiones:

--    a) 
\x -> \y -> y/x :: Fractional a => a -> a -> a

--    b) 
\z -> y/x - z :: Fractional a => a -> a

--    c)
\z w -> w (y/x - z) :: Fractional a => a -> (a -> b) -> a


-- 5. Programa en Haskell las siguientes funciones sin utilizar definiciones recurivas, sino lla-
--    madas a funciones de orden superior predefinidas:

--    a) Escribe una función zip3 :: [a] ->[b] ->[c] ->[(a,b,c)], análoga a zip, pero
--    que "empareje" tres listas en lugar de dos. El número de elementos de la lista
--    resultante coincidirá con el de la lista más corta.

zip3 :: [a] ->[b] ->[c] ->[(a,b,c)]
zip3 xs ys zs = map f (zip (zip xs ys) zs)
        where f = \((a, b), c) -> (a, b, c)

--    b) imparesEn xs = lista de los números impares en la lista xs. Por ejemplo:
--    imparesEn [1..6] = [1,3,5]

imparesEn :: Integral a => [a] -> [a]
imparesEn xs = filter (\x -> mod x 2 /= 0) xs

--    c) escalar xs ys = producto escalar de las listas de igual longitud xs e ys. Por
--    ejemplo: escalar [1,3,5] [2,4,6] = 1 * 2 + 3 * 4 + 5 * 6

escalar :: Num a => [a] -> [a] -> a
escalar xs ys = sum (zipWith (*) xs ys)

--    d) mcdList xs = máximo común divisor de los elementos de la lista xs.

mcdList :: (Foldable t, Integral b) => t b -> b
mcdList xs = foldr gcd 0 xs

-- 6. Utilizando listas intensionales escribe definiciones de las siguientes expresiones y funciones:

--    a) [(0,0),(1,2),(3,6),(7,14),(15,30),...]

[(2 ^ x - 1, (2 ^ x - 1) * 2) | x <- [0..]]

--    b) [1,-2,3,-4,5,-6,...]

[x * (-1) ^ (x + 1) | x <- [1..]]

--    c) paresHasta n = lista de los números naturales pares menores o iguales que n.

paresHasta n = [x | x <- [1..n], even x]

--    d) listpares n = lista de los n primeros números naturales pares.

listpares n = [x | x <- [1..2 * n], even x]

--    e) mezclaParImpar xs ys = lista de todos los los pares posibles (x,y) tales que x es
--    par y está en la lista xs, y es impar y está en la lista ys.

mezclaParImpar xs ys = [(x, y) | x <- filter even xs, y <- filter odd ys]

--    f ) prefijos xs = lista de las listas que son prefijo de xs. Por ejemplo:
--    prefijos [1,2,3] = [[],[1],[1,2],[1,2,3]].

prefijos xs = [take n xs | n <- [0.. length xs]]