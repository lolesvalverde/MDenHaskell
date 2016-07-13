module Morfismos where
  
import GrafoConListaDeAristas
import EjemplosGrafosLista
import GeneradorGrafosListas
import Test.QuickCheck
import Data.List 
import DefinicionesYPropiedadesLista 
import Rel_26_sol (variacionesR)

-- (productoCartesiano xs ys) es el producto cartesiano de xs e ys. Por
-- ejemplo, 
--    λ> productoCartesiano [3,1] [2,4,7]
--    [(3,2),(3,4),(3,7),(1,2),(1,4),(1,7)]
productoCartesiano :: [a] -> [b] -> [(a,b)]
productoCartesiano xs ys =
  [(x,y) | x <- xs, y <- ys]

-- (esRelacion xs ys r) se verifica si r es una relación binaria de xs
-- en ys. Por ejemplo,
--    esRelacion [3,1] [2,4,7] [(3,4),(1,2)]  ==  True
--    esRelacion [3,1] [2,4,7] [(3,1),(1,2)]  ==  False
esRelacion :: (Eq a, Eq b) => [a] -> [b] -> [(a,b)] -> Bool
esRelacion xs ys r =
  r `esSubconjunto` productoCartesiano xs ys

-- (imagenRelacion r x) es la imagen de x en la relación r. Por ejemplo, 
--    imagenRelacion [(1,3),(2,5),(1,4)] 1  ==  [3,4]
--    imagenRelacion [(1,3),(2,5),(1,4)] 2  ==  [5]
--    imagenRelacion [(1,3),(2,5),(1,4)] 3  ==  []
imagenRelacion :: Eq a => [(a,b)] -> a -> [b]
imagenRelacion r x =
  [y | (z,y) <- r, z == x] 

-- (unitario xs) se verifica si el conjunto xs es unitario. Por ejemplo, 
--    unitario [5]    ==  True
--    unitario [5,3]  ==  False
--    unitario [5,5]  ==  True
unitario :: Eq a => [a] -> Bool
unitario xs =
  length (nub xs) == 1

-- (dominio r) es el dominio de la relación r. Por ejemplo,
--    dominio [(3,2),(5,1),(3,4)]  ==  [3,5]
dominio :: Eq a => [(a,b)] -> [a]
dominio r =
  nub (map fst r)

-- Una relación es funcional si todos los elementos de su dominio tiene
-- una única imagen

-- (esFuncional r) se verifica si la relación r es funcional. Por
-- ejemplo,
--    esFuncional [(3,2),(5,1),(7,9)]  ==  True
--    esFuncional [(3,2),(5,1),(3,4)]  ==  False
--    esFuncional [(3,2),(5,1),(3,2)]  ==  True
esFuncional :: (Eq a, Eq b) => [(a,b)] -> Bool
esFuncional r =
  and [unitario (imagenRelacion r x) | x <- dominio r] 

-- (esFuncion xs ys f) se verifica si f es una función de xs en ys. Por
-- ejemplo, 
--    esFuncion [3,1] [2,4,7] [(1,7),(3,2)]        ==  True
--    esFuncion [3,1] [2,4,7] [(1,7)]              ==  False
--    esFuncion [3,1] [2,4,7] [(1,7),(3,2),(1,4)]  ==  False
esFuncion :: (Eq a, Eq b) => [a] -> [b] -> [(a,b)] -> Bool
esFuncion xs ys f =
  esRelacion xs ys f &&
  xs `esSubconjunto` dominio f &&
  esFuncional f 

           

-- Las funciones se representarán como listas de pares.
type Funcion a b = [(a,b)]

-- (imagen f x) es la imagen del elemento x en la función f. Por
-- ejemplo, 
--    imagen [(1,7),(3,2)] 1  ==  7
--    imagen [(1,7),(3,2)] 3  ==  2
imagen :: Eq a => Funcion a b -> a -> b
imagen f x =
  head (imagenRelacion f x)

-- (conservaAdyacencia g1 g2 f) se verifica si la función f conserva las
-- adyacencias. Por ejemplo.
--    λ> let g1 = creaGrafo [1,2,3] [(1,2),(2,3)]
--    λ> let g2 = creaGrafo [4,5,6] [(4,6),(5,6)]
--    λ> conservaAdyacencia g1 g2 [(1,4),(2,6),(3,5)]
--    True
--    λ> conservaAdyacencia g1 g2 [(1,4),(2,5),(3,6)]
--    False
conservaAdyacencia
  :: (Eq a,Ord b) => Grafo a -> Grafo b -> Funcion a b -> Bool
conservaAdyacencia g1 g2 f =
  and [(imagen f x,imagen f y) `aristaEn` g2
      | (x,y) <- aristas g1]

--    λ> let g1 = creaGrafo [1,2,3] [(1,2),(2,3)]
--    λ> let g2 = creaGrafo [4,5,6] [(4,6),(5,6)]
--    λ> esMorfismo g1 g2 [(1,4),(2,6),(3,5)]
--    True
--    λ> esMorfismo g1 g2 [(1,4),(2,5),(3,6)]
--    False
--    λ> esMorfismo g1 g2 [(1,4),(2,6),(3,5),(7,9)]
--    False
esMorfismo :: (Eq a,Ord b) => Grafo a -> Grafo b -> Funcion a b -> Bool
esMorfismo g1 g2 f =
  esFuncion (vertices g1) (vertices g2) f &&
  conservaAdyacencia g1 g2 f
