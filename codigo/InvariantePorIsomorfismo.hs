import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
-- import Conjuntos
-- import Relaciones
-- import Funciones
import DefinicionesYPropiedades 
import Morfismos hiding (esInvariantePorIsomorfismos)
--import Biyecciones
       
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

-- | Ejemplos
-- >>> quickCheck (esInvariantePorIsomorfismos orden)
-- +++ OK, passed 100 tests.
-- >>> quickCheck (esInvariantePorIsomorfismos tamaño)
-- +++ OK, passed 100 tests.
-- >>> quickCheck (esInvariantePorIsomorfismos secuenciaGrados)
-- +++ OK, passed 100 tests.
  
