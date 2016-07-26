import Data.List
import Test.QuickCheck
import GrafoConListaDeAristas
import GeneradorGrafos            

-- (caminos g x y) es la lista de todos los caminos simples en el grafo
-- g desde x hasta y. Por ejemplo,
--    λ> let ej1 = creaGrafo [1..3] [(1,2),(2,2),(1,3),(2,3)]
--    λ> caminos ej1 1 3
--    [[1,3],[1,2,3]]
--    λ> caminos ej1 3 1
--    [[3,1],[3,2,1]]
--    λ> caminos ej1 1 1
--    [[1]]
caminos :: Ord a => Grafo a -> a -> a -> [[a]]
caminos g x y = aux [[y]]
  where aux []       = []
        aux ([]:zss) = aux zss
        aux ((z:zs):zss)
          | z == x    = (z:zs) : aux zss
          | otherwise = aux (zss ++ [v:z:zs | v <- adyacentes g' z \\ zs])
        g' = eliminaLazos g

-- (eliminaLazos g) es el grafo obtenido eliminando los lazos de g. Por
-- ejemplo, 
--    λ> let ej1 = creaGrafo [1..3] [(1,2),(2,2),(1,3),(2,3)]
--    λ> eliminaLazos ej1
--    G [1,2,3] [(1,2),(1,3),(2,3)]
eliminaLazos :: Ord a => Grafo a -> Grafo a
eliminaLazos g =
  creaGrafo (vertices g)
            [(x,y) | (x,y) <- aristas g, x /= y]

-- Prop. : El primer elemento de (caminos g x y) es de mínima longitud.
prop_caminos :: Grafo Int -> Property
prop_caminos g =
  not (null (vertices g)) ==>
  forAll (parDeVertices g)
         (\(x,y) -> let zss = caminos g x y
                    in null zss || length (head zss) == minimum (map length zss))

-- (parDeVertices g) es un generador de pares de vértices del grafo no
-- nulo g. Por ejemplo, 
--    λ> sample (parDeVertices (creaGrafo [1..9] [(6,9),(3,5)]))
--    (3,4)
--    (1,5)
--    (5,7)
--    (9,9)
--    (7,4)
--    (2,2)
--    (2,8)
--    (8,7)
--    (3,6)
--    (7,1)
--    (1,7)
parDeVertices :: Grafo Int -> Gen (Int,Int)
parDeVertices g = do
  x <- elements vs
  y <- elements vs
  return (x,y)
  where vs = vertices g

-- La comprobación es  
--    λ> quickCheckWith (stdArgs {maxSize=14}) prop_caminos
--    +++ OK, passed 100 tests.
--    (37.78 secs, 95,567,391,584 bytes)

-- Para comprobar el orden de los grafos de los ejemplos
prop_caminos2 :: Grafo Int -> Property
prop_caminos2 g =
  not (null vs) ==>
  collect (length vs) $
  forAll (parDeVertices g)
         (\(x,y) -> let zss = caminos g x y
                    in null zss || length (head zss) == minimum (map length zss))
  where vs = vertices g

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=14}) prop_caminos2
--    +++ OK, passed 100 tests:
--    26% 1
--    17% 2
--    13% 4
--    11% 5
--    11% 3
--     5% 7
--     5% 6
--     5% 12
--     2% 9
--     2% 8
--     2% 10
--     1% 11
--    (244.90 secs, 280,583,186,352 bytes)

-- Se puede hacer la comprobación sólo con grafos pequeños. Por ejemplo,
--    λ> quickCheckWith (stdArgs {maxSize=10}) prop_caminos2
--    +++ OK, passed 100 tests:
--    41% 1
--    13% 3
--    13% 2
--     9% 5
--     8% 6
--     7% 4
--     5% 7
--     2% 9
--     2% 8
--    (0.02 secs, 0 bytes)
