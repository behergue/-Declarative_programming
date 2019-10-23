-- Beatriz Herguedas Pinedo

-- 1. Denid expresiones Haskell para los siguientes apartados:

-- a) Calculad cuántos a~nos hay en 10^10 segundos (supón que todos los a~nos tienen 365
--    días; en otro momento puedes hacerlo teniendo en cuenta bisiestos)
añosEn :: Fractional a => a
añosEn = 10^10 / (60 * 60 * 24 * 365)
 
--b) Calculad cuántos a~nos enteros, días restantes enteros, horas restantes enteras, minutos
--   restantes enteros y segundos restantes hay en 10^10 segundos
añosEnteros :: Integral a => (a,a,a,a,a)
añosEnteros = (mod años 365, mod dias 24, mod horas 60, mod mins 60, mod (10^10) 60)
        where mins = div (10^10) 60
              horas = div mins 60
              dias = div horas 24
              años = div dias 365 

--c) Generalizad los dos apartados anteriores, convirtiendo el número 
--   de segundos en argumento de una función
apartadoa :: Fractional a => a -> a
apartadoa n = n / (60 * 60 * 24 * 365)
    
apartadob ::  Integral a => a -> (a, a, a, a, a)
apartadob n = (mod anos 365, mod dias 24, mod horas 60, mod mins 60, mod n 60)
        where mins = div n 60
              horas = div mins 60
              dias = div horas 24
              anos = div dias 365
        
-- 2. Pensad cuáles de las siguientes expresiones tardarán poco (digamos centésimas o milésimas
--    de segundos), regular (digamos décimas o segundos) o mucho (digamos toda una
--    vida) en ser evaluadas. Probadlo en el intérprete, pero no esperéis toda la vida, interrumpid
--    el cómputo cuando sea necesario

a = last [1..10^5] 
-- Resultado 100000
-- Estadísticas: (0.12 secs, 7,255,296 bytes)
-- Tarda REGULAR

b = last [1..10^7]
-- Resultado 10000000
-- Estadísticas: (0.11 secs, 720,054,704 bytes)
-- Tarda REGULAR

c = last [1..10^20]
-- Hubo que hacer Interrupted con Ctrl+C
-- Tarda MUCHO

d = head [1..10^20]
-- Resultado: 1
-- Estadísticas: (0.00 secs, 51,144 bytes)
-- Tarda POCO

e = last [10^20..1]
-- *** Exception: Prelude.last: empty list

f = head (tail [1..10^20])
-- Resultado: 2
-- Estadísticas: (0.01 secs, 51,216 bytes)
-- Tarda POCO

g = length [1..10^20]
-- Hubo que hacer Interrupted con Ctrl+C
-- Tarda MUCHO

h = last (take (10^7) [1..10^20])
-- Resultado: 10000000
-- Estadísticas: (0.17 secs, 1,280,055,376 bytes)
-- Tarda REGULAR

i = head (take (10^7) ([1..100] ++ [1..10^20]))
-- Resultado: 1
-- Estadísticas: (0.01 secs, 51,376 bytes)
-- Tarda POCO

j = last (take 100 ([1..10^20] ++ [1..100]))
-- Resultado: 100
-- Estadísticas: (0.00 secs, 71,600 bytes)
-- Tarda POCO

k = last (drop 100 ([1..10^20] ++ [1..100]))
-- Hubo que hacer Interrupted con Ctrl+C
-- Tarda MUCHO

l = head (drop (10^7) ([1..10^20] ++ [1..100]))
-- Resultado: 10000001
-- Estadísticas: (0.18 secs, 1,280,055,528 bytes)
-- Tarda REGULAR

m = [1..10^7]==[1..10^7]
-- Resultado: True
-- Estadísticas: (0.28 secs, 1,440,054,088 bytes)
-- Tarda REGULAR

n = [1..10^20]==[1..10^20]
-- Hubo que hacer Interrupted con Ctrl+C
-- Tarda MUCHO

o = [1..10^20]==[1..10^20+1] 
-- Hubo que hacer Interrupted con Ctrl+C
-- Tarda MUCHO

p = [1..10^20]==[2..10^20]
-- Resultado: False
-- Estadísticas: (0.01 secs, 55,296 bytes)
-- Tarda POCO

q = head (reverse [1..10^7])
-- Estadísticas: (0.91 secs, 960,054,760 bytes)
-- Tarda REGULAR

r = last (reverse [1..10^7])
-- Resultado 1
-- Estadísticas: (0.92 secs, 960,050,736 bytes)
-- Tarda REGULAR

s = reverse [1..10^20] == reverse [1..10^20+1]
-- Hubo que hacer Interrupted con Ctrl+C
-- Tarda MUCHO

-- 3. Programad la función media que calcula la media aritmética de una lista de números,
--    usando para ello la función length que calcula el número de elementos de una lista.
--    ¿Surge algún problema con los tipos? Utilizad la función de conversión fromIntegral
--    para resolverlo
media :: Integral a => [a] -> Fractional
media xs = fromIntegral(sum xs) / fromIntegral (length xs)

-- 4. Programad las siguientes funciones, declarando sus tipos:

--a) digitos x = número de dígitos del número entero x
digitos :: (Ord a, Num b, Fractional a) => a -> b
digitos x = if abs x < 10 then 1 else 1 + digitos (x / 10)

--b) reduccion x = resultado del proceso de sumar los dígitos del entero x, sumar los
--   dígitos del resultado obtenido, y así sucesivamente hasta obtener un número menor
--   que 10. La reducción de un entero negativo es la de su valor absoluto
listaDigitos :: Integral a => a -> [a]
listaDigitos x = if x < 10 then [x] else listaDigitos (div x 10) ++ [mod x 10] 

reduction :: Integral a => a -> a
reduction x = if x < 10 then x else reduction (sum (listaDigitos x))

--c) perm n = número de permutaciones de n elementos
factorial :: (Eq a, Num a) => a -> a
factorial n = if n == 2 then 2 else n * factorial (n-1)

perm :: (Eq a, Num a) => a -> a
perm n = factorial n

--d) var n m = número de variaciones de n elementos tomados de m en m
var :: (Fractional a, Eq a) => a -> a -> a
var n m = factorial n / factorial (n-m)

--e) comb n m = número de combinaciones de n elementos tomados m en m
comb :: (Fractional a, Eq a) => a -> a -> a
comb n m = (factorial(n)) / ((factorial (n-m))*(factorial(m)))

-- 5. Denid la conjunción booleana por ajuste de patrones, pero de cuatro o cinco formas
--    diferentes, cambiando el número de ecuaciones, o las combinaciones de patrones True,
--    False, x, ... en cada ecuación, o el orden de ecuaciones, etc. y de manera que al
--    menos haya una versión estricta en el primer argumento y otra estricta en el segundo,
--    pero no en el primero. Para que coexistan todas deniciones en el mismo programa,
--    dadles nombres (o usa operadores) diferentes.

-- Conjunción Booleana por Patrones (CGP)
-- Implementación por descripción exhaustiva (casos innecesarios)
cbp1 :: Bool -> Bool -> Bool
cbp1 True True = True
cbp1 True False = False
cbp1 False True = False
cbp1 False False = False

-- Estricto en el segundo argumento, no en el primero
cbp2 :: Bool -> Bool -> Bool
cbp2 _ False = False
cbp2 False True = False
cbp2 True True = True

-- Estricto en ambos argumentos
cbp3 :: Bool -> Bool -> Bool
cbp3 _ False = False
cbp3 False _ = False
cbp3 True True = True

-- Estricto en el primer argumento, no en el segundo
cbp4 :: Bool -> Bool -> Bool 
cbp4 False _ = False
cbp4 True True = True
cbp4 True False = False

-- Definición del AND por igualdad de valor con dos variables
cbp5 :: Bool -> Bool -> Bool
cbp5 x y = if x == y then x else False 

-- Definición del AND por desigualdad de valor con dos variables
cbp6 :: Bool -> Bool -> Bool
cbp6 x y = if x /= y then False else x 

-- Definición del AND por igualdad de valor con una variable como primer argumento
cbp7 :: Bool -> Bool -> Bool
cbp7 a True = a == True
cbp7 _ _ = False

-- Definición del AND por igualdad de valor con una variable como segundo argumento
cbp8 :: Bool -> Bool -> Bool
cbp8 True a = True == a
cbp8 _ _ = False