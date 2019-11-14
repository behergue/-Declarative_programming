-- Pablo Hernández Aguado
-- Beatriz Herguedas Pinedo

-- 1. Escribe una función adivina n para jugar a adivinar un número. Debe pedir que el
--    usuario introduzca un número hasta que acierte con el valor de n. Devuelve mensajes de
--    ayuda indicando si el número introducido es menor o mayor que el número n a adivinar.
--    Observa que el tipo de la función será adivina :: Int -> IO ()

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


-- 2. Escribe un programa que lea una línea introducida por teclado y muestre el número de
--    palabras que contiene

numPalabras :: IO Int
numPalabras = do 
                xs <- getLine
                let n = (length . words) xs
                  in return n

-- 3. Programar funciones que realicen los siguientes procesos interactivos

-- a) palabras:: String -> IO Int
--    palabras fileIn obtiene como valor asociado el número de palabras de un fichero
--    fileIn

palabras :: String -> IO Int
palabras fileIn = do 
                    fs <- readFile fileIn
                    let 
                      n = (length . words) fs
                        in return n

-- b) palabras':: IO ()
--    Como la anterior, pero leyendo el nombre del fichero de la entrada y mostrando el
--    resultado con un mensaje de la forma: El fichero Fichero tiene n palabras

palabras' :: IO ()
palabras' = do 
              putStr ("Introduce el nombre del fichero: ")
              f <- getLine
              fs <- readFile f
              n <- palabras f
              putStrLn ("El fichero " ++ f ++ " tiene " ++ show n ++ " palabras.")

-- c) promedia :: IO ()
--    Proceso que va leyendo un entero de cada línea de la entrada y mostrando la suma
--    y el promedio de los valores leídos hasta ese momento. El proceso se detiene al leer
--    -1 en la entrada

promedia :: IO ()
promedia = promediaReal 0 0

promediaReal :: Int -> Int -> IO ()
promediaReal sum num = do
                        x <- getInt
                        if x /= -1 then 
                          let
                            s = sum + x
                            n = num + 1
                            m = (fromIntegral s) / (fromIntegral n)
                              in do
                                putStrLn ("Suma: " ++ show s)
                                putStrLn ("Promedio: " ++ show m)
                                promediaReal s n
                        else 
                          putStrLn ("Proceso finalizado.")


-- d) formatea:: String -> String -> Int -> IO ()
--    formatea fileIn fileOut n formatea a n columnas de ancho cada línea de fileIn
--    y escribe el resultado en fileOut. Para formatear se meten espacios intermedios
--    repartidos de manera uniforme entre palabras de modo que la línea quede justifi-
--    cada a izquierda y derecha. Puede suceder que, por su longitud, la línea quede con
--    más de n columnas

formatea :: String -> String -> Int -> IO ()
formatea fileIn fileOut n = do
                              fs <- readFile fileIn
                              let
                                fss = lines fs
                                fss' = format fss n
                                oss = unlines fss'
                                in
                                  writeFile fileOut oss

format :: [String] -> Int -> [String]
format lss n = let
                  wss = map words lss
                  ls = map (length . concat) wss
                  wss' = [putSpaces n l ws | (l, ws) <- zip ls wss]
                  in
                    map concat wss'

putSpaces :: Int -> Int -> [String] -> [String]
putSpaces n l ws = if l > n || null ws then
                    ws
                   else
                    let
                      holeNum = length ws - 1
                      nLeft = n - l
                      d = nLeft `div` holeNum
                      r = nLeft `mod` holeNum
                      rest = \i -> if i > r then [] else " "
                      in
                        [w ++ replicate d ' ' ++ rest i | (w, i) <- zip ws [1..], i <= holeNum] ++ [last ws]

-- -- e) Si te animas: calculadora :: IO () (Intérprete de expresiones aritméticas)
-- --    Proceso que lee de la entrada una expresión aritmética (formada por números y
-- --    operadores infijos +,*,-,/) y la evalúa
