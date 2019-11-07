-- Beatriz Herguedas Pinedo

-- 1. Supongamos que definimos el tipo data Pila = P [a] para representar pilas. Define fun-
--    ciones crea Pila para crear una pila vacía, esPilaVacia para determinar si una pila dada
--    está vacía o no, apilar para apilar un elemento, cima para consultar la cima de una pila
--    no vacía y desapilar para eliminar la cima de una pila no vacía. 

data Pila a = P [a]

creaPila :: Pila a
creaPila = P []

esPilaVacia :: Eq a => Pila a -> Bool
esPilaVacia (P pila) = (pila == [])

apilar :: Pila a -> a -> Pila a
apilar (P pila) x = P (x:pila)

--cima :: Pila a -> a 
--cima (P pila) = head pila
-- Mejor con ajuste de patrones:
cima :: Pila a -> a 
cima (P []) = error "Pila vacía"
cima (P x:xs) = x

desapilar :: Pila a -> Pila a
desapilar (P []) = error "Pila vacía"
desapilar (P x:xs) = xs

-- Determina el significado de la siguiente definición:
--    r :: [a] -> [a]
--    r xs = ys
--    where P ys = foldl (\p x -> apilar x p) creaPila xs

-- Respuesta: la definición hace lo mismo que la función reverse


-- 2. Define una función primeroQueCumple :: (a -> Bool) ->[a] -> Maybe a, que dada una
--    propiedad y una lista devuelva el primer elemento de la lista que cumple la propiedad. De-
--    vuelve Nothing en el caso de que ninguno la cumpla.

-- Recordemos: data Maybe a = Nothing | Just a (predefinido)

primeroQueCumple _ [] = Nothing
primeroQueCumple p (x:xs) = 
  if (p x == True) then Just a
  else primeroQueCumple p xs

-- Ahora sin recursión (con funciones de orden superior)

primeroQueCumple p xs = 
  if (null ys) then Nothing
  else Just (head ys) 
    where ys = filter p xs 

-- 3. Define un tipo de datos Cj para representar conjuntos de elementos del mismo tipo. Define
--    funciones para crear un conjunto vacío, para determinar si un conjunto dado está vacío o
--    no, para determinar si un elemento pertenece o no a un conjunto y para devolver la lista
--    con todos los elementos que pertenecen a un conjunto. Recuerda que en un conjunto no
--    puede haber elementos repetidos y que el orden de los elementos no importa.

data Cj a = Set [a]

listaElems :: Eq a => Cj a -> [a]
listaElems (Set xs) = foldr incElem [] xs
  where incElem x ys = 
    if elem x ys then ys
    else (x: ys)

creaCjto :: Eq a => Cj a -> Cj a
creaCjto (Set xs) = Set (listaElems (Set xs))

cjtoVacio :: Cj a
cjtoVacio = Set []

esVacio :: Cj a -> Bool
esVacio (Set []) = True
esVacio _ = False

pertenece :: Eq a => a -> Cj a -> Bool
pertenece x (Set xs) = elem x xs

contenido :: Eq a => Cj a -> Cj a -> Bool
contenido (Set xs) (Set ys) = [x | x <- xs, elem x ys] == xs

instance Eq a => Ord (Cj a) where c1 <= c2 = contenido c1 c2

union :: Eq a => Cj a -> Cj a -> Cj a
union (Set xs) (Set ys) = creaCjto (Set (xs ++ ys))

interseccion :: Eq a => Cj a -> Cj a -> Cj a
interseccion (Set xs) (Set ys) = creaCjto (Set [x | x <- xs, elem x ys])


-- 4. Define un tipo para representar matrices de números reales. Escribe una función que cal-
--    cule la transpuesta de una matriz rectangular dada. Escribe una función para calcular la
--    operación de suma de matrices.

type Vector = [Float]
type Matrix = [Vector]

add :: Matrix -> Matrix -> Matrix
add xss yss = zipWith addVector xss yss where addVector = zipWith (+)

traspuesta :: Matrix -> Matrix
traspuesta [] = []
traspuesta xss = foldr (\xs rows -> zipWith (\x row -> x:row) xs rows) (map (const []) xs xss) [[]]

-- 5. Dada la declaración:
--    data Temp = Kelvin Float | Celsius Float | Fahrenheit Float
--    para representar temperaturas en diferentes escalas, escribe una función para realizar con-
--    versiones de una escala a otra y otra para determinar la escala en la que está representada
--    una temperatura. El nuevo tipo tiene que ser instancia de las clases Ord y Eq. Define ade-
--    cuadamente los métodos == y compare para la nueva estructura de datos.

data Temp = Kelvin Float | Celsius Float | Fahrenheit Float

algoACelsius :: Temp -> Temp 
algoACelsius (Kelvin n) = Celsius (n - 273.15)
algoACelsius (Fahrenheit n) = Celsius ((n - 32) / 1.8)

algoAKelvin :: Temp -> Temp
algoAKelvin (Celsius n) = Kelvin (n + 273.15)
algoAKelvin (Fahrenheit n) = Kelvin ((n - 32) * 5 / 9 + 273.15)

algoAFarenheit :: Temp -> Temp 
algoAFarenheit (Celsius n) = Fahrenheit (n * 9 / 5 + 32)
algoAFarenheit (Kelvin n) = Fahrenheit  ((n - 273.15) * 9 / 5 + 32)

escalaMedida :: Temp -> Temp 
escalaMedida (Kelvin _) = "Kelvin"
escalaMedida (Celsius _) = "Celsius"
escalaMedida (Fahrenheit _) = "Farenheit"

instance Eq Temp where t1 == t2 = x1 == x2
    where Celsius x1 = algoACelsius t1
          Celsius x2 = algoACelsius t2

instance Ord Temp where compare t1 t2 = compare x1 x2
    where Celsius x1 = algoACelsius t1
          Celsius x2 = algoACelsius t2


-- 6. Declara adecuadamente un tipo de datos para representar árboles binarios de búsqueda
--    con valores en los nodos pero no en las hojas. Programa en Haskell la ordenación de una
--    lista por el algoritmo treeSort, consistente en ir colocando uno a uno los elementos de la
--    lista en un árbol binario de búsqueda inicialmente vacío. A continuación devuelve la lista
--    resultante de recorrer el árbol en inOrden.

data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a)

treeSort :: Ord a => [a] -> [a]
treeSort xs = inOrden (listaArbolOrd xs)
    where listaArbolOrd (x:xs) = Nodo x (listaArbol (<)) (listaArbol (>))


-- 7. Escribe una función adivina n para jugar a adivinar un número. Debe pedir que el usuario
--    introduzca un número hasta que acierte con el valor de n. Devuelve mensajes de ayuda
--    indicando si el número introducido es menor o mayor que el número n a adivinar. Observa
--    que el tipo de la función será adivina :: Int -> IO ().

getInt :: IO Int
getInt = do
          line <- getLine
          return (read line::Int)

adivina :: Int -> IO ()
adivina n = do 
              putStr ("Introduce un número: ")
              x <- getInt
              if x == n then
                putStrLn ("Has adivinado el número")
              else
                if x < n then
                    do
                        putStrLn ("El número que buscas es mayor")
                        adivina n
                else 
                    do 
                        putStrLn ("El número que buscas es menor")
                        adivina n

-- 8. Escribe un programa que lea una línea introducida por teclado y muestre el número de
--    palabras que contiene.


-- Elimina los espacios duplicados seguidos de un String para luego contar el número de palabras en función de los espacios
elim :: [Char] -> [Char]
elim []                = []
elim (s:ss) | s == ' ' = s : (elim $ dropWhile (== ' ') ss)
                  | s /= ' ' = s : (elim ss)

palabras :: IO Int
palabras = do 
            xs <- getLine 
            let 
              ss = elim xs
              s1 = length (filter (== ' ') ss) 
              s2 = if head ss == ' ' then s1 - 1 else s1
              s3 = if last ss /= ' ' then s2 + 1 else s2 
                in return s3