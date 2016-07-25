import Data.List
import Test.QuickCheck
import GrafoConListaDeAristas
import GeneradorGrafos            

-- (caminos g x y) es la lista de todos los caminos simples en el grafo
-- g desde x hasta y. Por ejemplo,
--    λ> let ej1 = creaGrafo [1..3] [(1,2),(1,3),(2,3)]
--    λ> caminos ej1 1 3
--    [[1,3],[1,2,3]]
--    λ> caminos ej1 3 1
--    [[3,1],[3,2,1]]
--    λ> caminos ej1 1 1
--    [[1]]
caminos :: Eq a => Grafo a -> a -> a -> [[a]]
caminos g x y = aux [[y]]
  where aux []       = []
        aux ([]:zss) = aux zss
        aux ((z:zs):zss)
          | z == x    = (z:zs) : aux zss
          | otherwise = aux (zss ++ [v:z:zs | v <- adyacentes g z \\ zs])

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
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_caminos
--    +++ OK, passed 100 tests.
--    (0.58 secs, 1,822,906,496 bytes)

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
--    λ> quickCheck prop_caminos2
--    +++ OK, passed 100 tests:
--     5% 6
--     5% 2
--     5% 1
--     4% 18
--     4% 17
--     4% 10
--     3% 8
--     3% 52
--     3% 5
--     3% 48
--     3% 36
--     3% 3
--     3% 27
--     3% 21
--     3% 19
--     3% 15
--     2% 9
--     2% 88
--     2% 44
--     2% 4
--     2% 39
--     2% 38
--     2% 37
--     2% 30
--     2% 29
--     2% 28
--     2% 24
--     2% 23
--     1% 79
--     1% 78
--     1% 70
--     1% 69
--     1% 61
--     1% 49
--     1% 45
--     1% 41
--     1% 35
--     1% 34
--     1% 32
--     1% 31
--     1% 26
--     1% 25
--     1% 22
--     1% 16
--     1% 13
--     1% 12
--     1% 11

-- Se observa que se han generado grafos grandes (con 88 vértices).

-- Se puede ver el porcentaje de grafos grandes (con más de 10 vértices)
prop_caminos3 :: Grafo Int -> Property
prop_caminos3 g =
  not (null vs) ==>
  classify (length vs > 10) "grande" $
  forAll (parDeVertices g)
         (\(x,y) -> let zss = caminos g x y
                    in null zss || length (head zss) == minimum (map length zss))
  where vs = vertices g

-- La comprobación es
--    λ> quickCheck prop_caminos3
--    +++ OK, passed 100 tests (68% grande).
--    λ> quickCheck prop_caminos3
--    +++ OK, passed 100 tests (69% grande).

-- Se puede hacer la comprobación sólo con grafos pequeños. Por ejemplo,
--    λ> quickCheckWith (stdArgs {maxSize=10}) prop_caminos2
--    +++ OK, passed 100 tests:
--    36% 1
--    19% 2
--    14% 3
--    10% 5
--     8% 4
--     5% 7
--     4% 6
--     2% 9
--     2% 8
