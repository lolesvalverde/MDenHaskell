-- Comparación de definiciones de biyecciones

module Biyecciones where

import ConjuntosRelacionesYFunciones
import Data.List (permutations)
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import DefinicionesYPropiedades 
import Morfismos

biyecciones1 :: (Eq a, Eq b) => [a] -> [b] -> [Funcion a b]
biyecciones1 xs ys =
  [f | f <- funciones xs ys
     , esBiyectiva xs ys f]

--    λ> length (biyecciones1 [1..7] [1..7])
--    5040
--    (5.93 secs, 4,281,955,136 bytes)

biyecciones2 :: (Eq a, Eq b) => [a] -> [b] -> [Funcion a b]
biyecciones2 xs ys
  | length xs /= length ys = []
  | otherwise              = [zip xs zs | zs <- permutations ys]

--    λ> length (biyecciones2 [1..7] [1..7])
--    5040
--    (0.01 secs, 0 bytes)

isomorfismos1 :: (Ord a,Ord b) => Grafo a -> Grafo b -> [Funcion a b]
isomorfismos1 g h =
  [f | f <- funciones vs1 vs2 , esIsomorfismo g h f]
  where vs1 = vertices g
        vs2 = vertices h

isomorfismos3 :: (Ord a,Ord b) => Grafo a -> Grafo b -> [Funcion a b]
isomorfismos3 g h =
  [f | f <- biyecciones2 vs1 vs2
     , esMorfismo g h f
     , esMorfismo h g (inversa f)]
  where vs1 = vertices g
        vs2 = vertices h

--    λ> head (isomorfismos1 (completo 8) (completo 8))
--    [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)]
--    (18.22 secs, 9,943,749,352 bytes)
--    λ> head (isomorfismos3 (completo 8) (completo 8))
--    [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)]
--    (0.00 secs, 0 bytes)

