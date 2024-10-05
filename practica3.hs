data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0
longitud (Node x xs) = 1 + longitud xs

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void a = False
estaContenido (Node x xs) a = x == a || estaContenido xs a

convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node x xs) = x : convertirALista xs

conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node x xs) = 
    if not (estaContenido xs x) 
        then Node x (conjunto xs)
        else conjunto xs

eliminarIndice :: List a -> Int -> List a
eliminarIndice Void _ = error "No se puede eliminar un elemento de una lista vacía"
eliminarIndice (Node x xs) i = 
    if i >= 0 && i < longitud (Node x xs)
        then if i == 0
            then xs
            else Node x (eliminarIndice xs (i - 1))
        else error "Índice fuera del rango permitido"

insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void i a = if i == 0 then Node a Void else error "Índice fuera del rango permitido"
insertarIndice (Node x xs) i a = 
    if i >= 0 && i <= longitud (Node x xs)
        then if i == 0
            then Node a (Node x xs)
            else Node x (insertarIndice xs (i - 1) a)
        else error "Índice fuera del rango permitido"

recorrerLista :: List a -> Int -> List a
recorrerLista Void i = Void
recorrerLista (Node x xs) i = 
    if i == 0
        then Node x xs
        else recorrerLista (insertarIndice xs (longitud xs) x) (i - 1)