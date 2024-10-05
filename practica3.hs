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

