-- Beatriz Herguedas Pinedo

-- 1. Escribe el tipo de las siguientes expresiones, siempre que sea posible. Escribe las que
--    sean sintácticamente correctas en notación simplificada, sin utilizar la constructora de
--    listas (:).

--    a. 
[ True : [ ]] :: [[Bool]]

--    b. 
[ ] : [True ]
-- Da error de tipos al intentar meter una lista vacía en otra de tipo Bool

--    c. 
[ True ]: [ ] :: [[Bool]]

--    d. 
True : [ True ] :: [Bool]

--    e. 
1 : (2 : 3 : [ ]) :: Num a => [a]

--    f. 
[1 : [2]] : [ [ ] ] :: Num a => [[[a]]]

--    g. 
[1, 1] : (2 : [ ])
-- Da error de tipos al intentar meter una lista de Num en otra de Num

--    h.
[ ] : [ [ ] ] : [ ] :: [[[a]]]


-- 2. Escribe el tipo de las siguientes expresiones, siempre que sea posible. Indica cuáles están
--    mal tipadas y por qué.

--    a) 
head ['a', 'f'] :: Char

--    b) 
tail ['a', 'f'] :: [Char]

--    c) 
tail head ''af''
-- Da error al intentar coger el tail de una lista que solo tiene 1 elemento

--    d) 
head (tail ''af'') :: Char

--    e) 
splitAt 4 ['a' .. 'f'] :: ([Char], [Char])

--    f )
zip [3 + 2, 0] [''af'']
-- Da error pues "af" ya es una lista de Char en si misma

--    g) 
drop (+2) [1,2,3]
-- Da error por el + del 2

--    h) 
drop (div 2 0) [1,2,3] :: Num a => [a]

--    i1) 
'ab' ++ 'bc' 
-- Da error pues 'ab' y 'bc' no son listas

--    i2)
''ab'' ++ ''bc'' :: [Char]

--    i3)
''ab'' + ''bc'' 
-- Da error al intentar sumar dos listas

--    i4)
''ab'' ++ 'c'
-- Da error pues 'c' no es una lista


-- 3. Determina el valor de las expresiones evaluables del ejercicio anterior.

--    a) 
head ['a', 'f'] = a

--    b) 
tail ['a', 'f'] = [f]

--    c) 
tail head ''af''
-- Da error al intentar coger el tail de una lista que solo tiene 1 elemento

--    d) 
head (tail ''af'') = f

--    e) 
splitAt 4 ['a' .. 'f'] = ([a, b, c, d], [e, f])

--    f )
zip [3 + 2, 0] [''af'']
-- Da error pues "af" ya es una lista de Char en si misma

--    g) 
drop (+2) [1,2,3]
-- Da error por el + del 2

--    h) 
drop (div 2 0) [1,2,3]
-- Da exception al dividir por cero

--    i1) 
'ab' ++ 'bc' 
-- Da error pues 'ab' y 'bc' no son listas

--    i2)
''ab'' ++ ''bc'' = [a, b, c, d]

--    i3)
''ab'' + ''bc'' 
-- Da error al intentar sumar dos listas

--    i4)
''ab'' ++ 'c'
-- Da error pues 'c' no es una lista


-- 4. Empareja cada una de las expresiones de la columna izquierda con su equivalente de la
--    derecha:
--    1. 0:2:[4]                 ->           d. 0:(2:(4:[])) 
--    2. [0]:([2:(4:[])])        ->           c. [0]:([2:(4:[])])  
--    3. [[0]:(2:[4]):[]]        ->           a. [[[0],[2,4]]]
--    4. [0]:(2:4:[]):[[]]       ->           b. [0]:(2:[4]):[[]]



--    5. Encuentra si es posible el valor de las siguientes expresiones y explica por qué no es
--    posible en las que no se pueda.

--    a) 
let x = y + 1 in let z = x ^ 2 in z
-- Da error porque la vble. y no está definida

--    b)
let y = let x = 2 in (let z = x ^ 2 in z) in y = 4

--    c)
let y = let x = 2 in (let z = x ^ 2 in z) in z + y
-- Da error porque la vble. z no está definida

--    d)
let {x = 5; y = 4} in if x < y then x else y = 4

--    e)
let {x = 5; y = 4} in if x < y then z = x else z = y
-- Da error porque la vble. z no está definida

--    f)
if [1] !! 1 == 1 then [1] else [ ]
-- Da exception porque se sale de rango

--    g)
let x = elem 1 [1] in if x then [1] else [ ] = [1]

--    h)
let x = elem 1 [ ] in if x then [1] !! 1 else [1] !! 0 = 1

--    i )
let x = elem 1 [ ] in if x then 1 else [ ]
-- Da error porque el if y el else devuelven cosas de distinto tipo


-- 6. Indica razonadamente cuáles de los siguientes tipos son equivalentes:
--    t1 = (a ->b) ->(a ->a ->b)
--    t2 = a ->b ->((a ->a) ->b)
--    t3 = a ->b ->(a ->(a ->b))
--    t4 = a ->(b ->(a ->a ->b))

--    RESPUESTA: Solo son equivalentes t3 y t4 porque la flecha asocia por la derecha

-- 7. Supuesto que ! es un operador que se ha declarado como infijo y que asocia por la izquierda
--    (infixl 4 !) 
--    ¿Cuáles de las siguientes expresiones son sintácticamente correctas? Usa
--    paréntesis para comprobarlo. Transforma e1 en una expresión equivalente en notación prefifija.

--    e1 = f x y ! g x ! h y = (!) ((!) (f x y) (g x)) (h y)
--    Es correcta

--    e2 = ((!) (f x y) g x) ! h y
--    Es incorrecta porque le faltan los paréntesis a la g

--    e3 = (!) ((!) (f x y) (g x)) h y
--    Es correcta


-- 8. La función f está definida mediante la ecuación:
f (x, y, z) = let m = min (min x y) z in m

--    a) ¿Qué calcula f (x, y, z)?
--    f calcula el mínim de x, y, z

--    b) Halla el tipo de f.
f :: Ord a => (a, a, a) -> a

--    c) Redefine la función f en notación currificada y escribe su tipo.
f :: Ord a => a -> a -> a -> a
f x y z = let m = min (min x y) z in m


-- 9. Define una función (sin olvidar declarar su tipo cualificado) que dados tres argumentos,
--    que admitan un orden entre ellos, devuelva una terna compuesta por los tres argumentos
--    en orden creciente.

f :: Ord a => a -> a -> a -> (a, a, a)

--    a) Usando expresiones if.

f x y z = 
    if x <= y && y <= z then (x, y, z)
    else
        if x <= z && z <= y then (x, z, y)
        else
            if y <= x && x <= z then (y, x, z)
            else
                if y <= z && z <= x then (y, z, x)
                else
                    if z <= x && x <= y then (z, x, y)
                    else (z, y, x)
          

--    b) Usando guardas.

f x y z = 
    | (x <= y && y <= z) = (x, y, z)
    | (x <= z && z <= y) = (x, z, y)
    | (y <= x && x <= z) = (y, x, z)
    | (y <= z && z <= x) = (y, z, x)
    | (z <= x && x <= y) = (z, x, y)
    | (z <= y && y <= x) = (z, y, x)

-- 10. Define una función, usando ajuste de patrones, que aplicada a una lista cualquiera dé
--     como resultado True si la lista tiene exactamente dos elementos y False en caso contrario.

f :: [a] -> Bool
f [x, y] = True
f x = False

-- 11. Considera el siguiente programa:
p :: Int ->Bool
p n = if n == 0 then True else i (n-1)
i :: Int ->Bool
i n = if n == 0 then False else p (n-1)

-- p comprueba si una función es par
-- i comprueba si una función es impar

-- Explica el significado de las funciones i y p. Indica el valor de la expresión i 4 || p 4.
i 4 || p 4 = True