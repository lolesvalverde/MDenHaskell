import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import ConjuntosRelacionesYFunciones
import DefinicionesYPropiedades 
import Morfismos
import Biyecciones
       
import Test.QuickCheck
import Data.List

isomorfos3 ::
    (Ord a, Ord b) => Grafo a -> Grafo b -> Bool
isomorfos3 g =
    not. null . isomorfismos3 g

-- Definición de implicación como operador
(-->) :: Bool -> Bool -> Bool
(-->) = (<=)

esInvariantePorIsomorfismos ::
  Eq a => (Grafo Int -> a) -> Grafo Int -> Grafo Int -> Bool
esInvariantePorIsomorfismos p g h = 
  isomorfos3 g h --> (p g == p h)

-- λ> quickCheckWith (stdArgs {maxSize=10}) (esInvariantePorIsomorfismos orden)
-- +++ OK, passed 100 tests.
-- (2.20 secs, 1,178,725,984 bytes)
-- λ> quickCheckWith (stdArgs {maxSize=10}) (esInvariantePorIsomorfismos tamaño)
-- +++ OK, passed 100 tests.
-- (0.02 secs, 0 bytes)
-- λ> quickCheckWith (stdArgs {maxSize=10}) (esInvariantePorIsomorfismos secuenciaGrados)
-- +++ OK, passed 100 tests.
-- (0.03 secs, 0 bytes)
  
