-- Beatriz Herguedas Pinedo

-- 1. Definir un tipo Nat para representar números naturales con la aritmética de Peano. Es
--    decir, toda expresión de tipo Nat sería Cero o el sucesor de un elemento de Nat (expresión
--    de la forma Suc e con e:: Nat. Declarar el tipo como instancia de Eq y Ord usando
--    deriving

data Nat = Cero | Suc Nat deriving (Eq, Ord)

-- a) Definir operadores infijos para calcular la suma y el producto de elementos de Nat

instance Num Nat where 
	Cero + x = x
	Suc x + y = x + Suc y
	
	Cero * x = Cero
	x * Cero = Cero
	Suc Cero * x = x
	Suc x * y = x * y + x

-- b) Definir una función natToInt que convierta una expresión de tipo Nat en su equi-
--    valente en el tipo Int

natToInt :: Nat -> Int
natToInt Cero = 0
natToInt (Suc x) = (natToInt x) + 1 


-- c) Utilizar natToInt para declarar Nat como instancia de Show. (Para declarar un tipo
--    T como instancia de Show basta con definir la función show::T->String, no hace
--    falta definir las otras funciones de la clase Show)

instance Show Nat where show x = show (natToInt (x))


-- 2. Definir un tipo para representar números complejos y declararlo como instancia de las
--    clases Eq, Num, y Show usando deriving solo cuando sea conveniente. Por ejemplo, show
--    del término Haskell que represente al complejo 2 + 3i sería el string ''2+3i'' (análoga-
--    mente ''2-3i'' para 2 - 3i). Tendrás que redefinir los métodos de Num para expresar
--    las operaciones aritméticas entre números complejos, redefine al menos (+), (-) y (*)


data Complex a = C a a deriving (Eq, Ord)

instance Num a => Num (Complex a) where
	(C a b) + (C c d) = C (a + c) (b + d)

	(C a b) - (C c d) = C (a - c) (b - d)

	(C a b) * (C c d) = C (a * c - b * d) (a * d + b * c)

instance Show a => Show (Complex a) where
		show (C x y) = concat [(show x), "+",  (show y), "i"]

-- 3. Definir una clase de tipos Medible que disponga de un método tamanyo::a ->Int que
--    se pueda aplicar a cada tipo a de dicha clase. Declara algunos tipos como instancia de
--    la clase Medible, por ejemplo Bool, [a], (a,b), definiendo la función tamayo para cada
--    uno de ellos

class Medible a where tamanyo :: a -> Int

instance Medible Bool where tamanyo a = 1

instance Medible [a] where tamanyo [a] = length [a]

instance Medible (a, b) where tamanyo (a, b) = 2


-- 4. Definir un tipo enumerado Direccion con cuatro valores que representen movimientos
--    (arriba, abajo, izquierda, derecha) por una cuadrícula en el plano con coordenadas ente-
--    ras. Convertirlo en instancia de Eq, Ord, Show usando deriving. Definir una función
--    destino que al aplicarse a un punto del plano y una lista de movimientos, devuelva el
--    punto final al que se llega

data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Eq, Ord, Show)
type Coordenada = Int
type Punto = (Coordenada, Coordenada)

mov :: Punto -> Direccion -> Punto
mov (x, y) Arriba = (x, y + 1)
mov (x, y) Abajo = (x, y - 1)
mov (x, y) Izquierda = (x - 1, y)
mov (x, y) Derecha = (x + 1, y)

destino :: Punto -> [Direccion] -> Punto
destino p [] = p
destino p (x:xs) = destino (mov p x) xs


-- Opcional: Definir una función trayectoria que al aplicarse a un punto del plano y una
-- lista de movimientos, devuelva la lista de puntos por los que se pasa al aplicar movs a
-- punto

trayectoria :: Punto -> [Direccion] -> [Punto]
trayectoria p [] = [p]
trayectoria p (x:xs) = p : trayectoria (mov p x) xs

-- 5. Definir un tipo de datos polimórfico para representar árboles generales, en los que cada
--    nodo tiene una información y n hijos (n >= 0, y puede variar con cada nodo). No se
--    consideran árboles vacíos

data Arbol a = Nodo a [Arbol a]

-- a) Programar las siguientes funciones:

--  listaHojas t, que obtiene la lista de las informaciones de todas las hojas del
--  árbol t

listaHojas :: (Arbol a) -> [a]
listaHojas (Nodo x []) = [x]
listaHojas (Nodo x ys) = foldr (\a bs -> listaHojas a ++ bs) [] ys


-- listaNodos t, que obtiene la lista de las informaciones de todos los nodos del
-- árbol t

listaNodos :: (Arbol a) -> [a]
listaNodos (Nodo x []) = [x]
listaNodos (Nodo x ys) = [x] ++ foldr (\a bs -> listaHojas a ++ bs) [] ys

-- repMax t, que devuelve el árbol resultante de poner como información de todos
-- los nodos del árbol t la información más grande que aparece en t

repMaxAux :: a -> (Arbol a) -> (Arbol a)
repMaxAux x (Nodo _ []) = Nodo x []
repMaxAux x (Nodo _ xs) = Nodo x (foldr (\y ys -> (repMaxAux x y):ys) [] xs)


-- b) Declarar explícitamente el tipo de los árboles como instancia de la clase Ord
--    (usando instance), de manera que el orden definido sea el mismo que resultaría
--    de usar deriving Ord

instance Eq a => Eq (Arbol a) where Nodo x xs == Nodo y ys = (x == y) && (xs == ys)

instance Ord a => Ord (Arbol a) where Nodo x xs <= Nodo y ys = if x /= y then x <= y else xs <= ys

-- c) Declarar el tipo de los árboles como instancia de la clase Show, de manera que la
--    vista en pantalla de un árbol sea visualmente más atractiva que lo que nos da el
--    poner simplemente deriving Show

instance Show a => Show (Arbol a) where
    show (Nodo x []) = show x
    show (Nodo x xs) = " " ++ show x ++ ":[" ++ (foldr (\n ns -> show n ++ " " ++ ns) "" xs) ++ "]"