-- Beatriz Herguedas Pinedo

-- Definimos vértice, arista y grafo
data Vertice = A | B | C | D | E | F deriving (Read, Eq, Show)
type Arista = (Vertice, Vertice)
data Grafo = G [Vertice] [Arista] deriving (Read, Show)

-- Creamos 6 grafos
-- Los 3 primeros son los proporcionados por el enunciado
g1::Grafo
g1 = G [B,D,E,C] [(D,E),(E,B),(C,B),(E,C)]

g2::Grafo
g2 = G [D,F,E] [(D,F),(E,D),(D,E),(F,E)]

g3::Grafo
g3 = G [A,C,D] [(A,C),(C,D),(A,D)]

-- g4 es isomorfo a g2
g4::Grafo
g4 = G [B,C,A] [(C,B),(A,C),(C,A),(B,A)]

-- g5 es igual que g4 pero sin una arista (por lo que no son isomorfos)
g5::Grafo
g5 = G [B,C,A] [(C,B),(A,C),(B,A)]

-- g6 es isomorfo a g1 (es una permutación de sus vértices)
g6::Grafo
g6 = G [D,B,C,E] [(D,E),(E,B),(C,B),(E,C)]

-- Función que comprueba si los dos extremos de cada arco están en la lista de vértices
arcos_en_vertices:: [Vertice] -> [Arista] -> Bool
arcos_en_vertices vlist alist = and (map (\(v1,v2) -> elem v1 vlist && elem v2 vlist) alist)

-- Función que informa de si hay vértices repetidos
hay_repetidos:: [Vertice] -> Bool
hay_repetidos vlist = any (\x -> length (filter (== x) vlist) > 1) vlist

-- 	Función que comprueba si lo que le pasamos es realmente un grafo
es_grafo:: Grafo -> Bool
es_grafo (G vlist alist) = not (null vlist) && arcos_en_vertices vlist alist && not (hay_repetidos vlist)

-- Matriz de adyacencia del grafo
mat_ady:: Grafo -> [[Integer]]
mat_ady (G vlist alist) = [[if elem (v1,v2) alist then 1 else 0 | v2 <- vlist] | v1 <- vlist]

-- Lista de los grados positivos de los vértices del grafo
grados_pos:: Grafo -> [Integer]
grados_pos grafo = map sum (mat_ady grafo)

-- Función que traspone una matriz cuadrada. Asumir que la matriz es cuadrada
-- nos permite usar que el número de columnas es el mismo que el de filas
-- para no tener que acceder a una fila para obtener el número de columnas
trasponeCuadrada:: [[a]] -> [[a]]
trasponeCuadrada matriz = [[(matriz !! i) !! j | i <- [0..(long-1)]]|j <- [0..(long-1)]]
                          where long = length matriz

-- Lista de los grados negativos de los vértices del grafo
grados_neg:: Grafo -> [Integer]
grados_neg grafo = map sum (trasponeCuadrada (mat_ady grafo))

-- Devuelve la lista de aristas que salen desde el vértice v
aristasDesde:: Vertice -> [Arista] -> [Arista]
aristasDesde v alist = filter (\(x,y) -> x == v) alist

-- Función que comprueba si un camino tiene ciclos
-- Sabemos que la lista con que se llama a la función tiene al menos un elemento
noCiclosCamino:: [Vertice] -> Bool
noCiclosCamino [x] = True
noCiclosCamino (x:xs) = not (hay_repetidos xs) && not (elem x (init xs))

-- Lista de los caminos en el grafo con origen en v y longitud n
camino_lng:: Grafo -> Vertice -> Int -> [[Vertice]]
camino_lng (G vlist alist) v n
 | n == 0 = [[v]]
 | n > 0 = [v:camino| (a1,a2) <- aristasDesde v alist, camino <- camino_lng (G vlist alist) a2 (n-1), noCiclosCamino (v:camino) ]
 | otherwise = error "El tamaño del camino introducido debe ser un número natural"

-- Devuelve una matriz de booleanos True o False que indica los vértices alcanzables desde otro en n pasos
alcanzables::Grafo -> Int -> [[Bool]]
alcanzables (G vlist alist) n
 | n == 0 = [[v1 == v2|v2 <- vlist]|v1 <- vlist]
 | n > 0 = let {esta_en_paso_anterior orig dest = or [paso_anterior !! i !! j| i <-ind, j <-ind, elem (vlist !! i) orig, vlist !! j == dest]; 
                ind = [0..length vlist-1];
                paso_anterior = alcanzables (G vlist alist) (n-1)}
           in [[esta_en_paso_anterior ( v1:[w| w <- vlist, elem (v1,w) alist] ) v2 | v2 <- vlist]| v1 <- vlist]

-- Comprueba si el grafo es conexo
conexo:: Grafo -> Bool
conexo (G vlist alist) = any and (alcanzables (G vlist alist) (length vlist))

-- Calcula las permutaciones de la lista de vértices de un grafo (de cara a ver si dos grafos son isomorfos)
permutaciones:: Eq a => [a]-> [[a]]
permutaciones [x] = [[x]]
permutaciones xs = [x:y | x <- xs, y <- permutaciones( takeWhile (/= x) xs ++ tail (dropWhile(/= x) xs)) ]

-- Sobreescribimos el método "==" de la clase Eq para que nos permita saber si dos grafos son isomorfos
-- recorriendo la lista de permutaciones de uno y comprobado si alguna de ellas es igual al otro
instance Eq Grafo where
 (G vlist_1 alist_1) == (G vlist_2 alist_2) = any coincide_perm (permutaciones vlist_2)
  where coincide_perm p = mat_ady (G vlist_1 alist_1) == mat_ady (G p alist_2)

-- Función auxiliar que lee n vértices introducidos por el usuario
leerVertices::Integer -> IO [Vertice]
leerVertices 0 = return []
leerVertices n = do putStr "Introduzca el vértice\n"
                    linea <- getLine
                    verticesAnt <- leerVertices (n-1)
                    return ((read linea::Vertice) : verticesAnt)

-- Función auxiliar que lee n aristas introducidas por el usuario
leerAristas::Integer -> IO [Arista]
leerAristas 0 = return []
leerAristas n = do putStr "Introduzca la arista\n"
                   linea <- getLine
                   aristasAnt <- leerAristas (n-1)
                   return ((read linea::Arista) : aristasAnt)

-- Lee un grafo introducido por el usuario y comprueba que efectivamente lo es
leegrafo:: IO Grafo
leegrafo = do putStr "Introduzca el número de vértices\n"
              linea <- getLine
              vlista <- leerVertices (read linea::Integer)
              putStr "Introduzca el número de aristas\n"
              linea <- getLine
              alista <- leerAristas (read linea::Integer)
              if es_grafo (G vlista alista) then return (G vlista alista)
              else do putStr "Lo que ha introducido no es un grafo\n"
                      leegrafo

-- Función auxiliar para mostrar la matriz de adyacencia
muestra_aux:: [[Integer]] -> IO ()
muestra_aux [] = return ()
muestra_aux (x:xs) = do print x
                        muestra_aux xs

-- Muestra la matriz de adyacencia de un grafo leído
muestra_matriz:: IO ()
muestra_matriz = do g <- leegrafo
                    putStr "Matriz:\n"
                    muestra_aux (mat_ady g)

-- Función que muestra un camino
muestra_un_camino::[Vertice] -> IO()
muestra_un_camino [v] = do print v
muestra_un_camino (v:vs) = do putStr ((show v) ++ "-")
                              muestra_un_camino vs

-- Función auxiliar para mostrar los caminos desde un vértice
-- Siempre debería ser llamada con al menos un vértice cuyos caminos mostrar
muestra_justifica_conexo:: Grafo -> [Vertice] -> IO()
muestra_justifica_conexo (G vlist alist) (v:vs) = if all (\x-> not (null (caminos_entre v x))) [v2 | v2 <- vlist, v2 /= v]
                                                  then do aux_muestra_caminos $ (map head (map (caminos_entre v) [v2 | v2 <- vlist, v2 /= v]))
                                                  else muestra_justifica_conexo (G vlist alist) vs                        
 where caminos_entre v1 v2 = filter (\c-> last c == v2) [c|i <- [0..length vlist - 1], c <- camino_lng (G vlist alist) v1 i ]
       aux_muestra_caminos (c:cs)
        | cs == [] = muestra_un_camino c
        | otherwise = do muestra_un_camino c
                         aux_muestra_caminos cs
       aux_muestra_caminos [] = putStr "El grafo es trivialmente conexo"

-- Comprueba si el grafo es conexo y muestra para un vértice, un camino desde
-- él hasta el resto de vértices del grafo usando la función auxiliar anterior
muestra_caminos:: IO ()
muestra_caminos = do (G vlist alist) <- leegrafo
                     if conexo (G vlist alist) then muestra_justifica_conexo (G vlist alist) vlist
                     else putStr "El grafo no es conexo\n"