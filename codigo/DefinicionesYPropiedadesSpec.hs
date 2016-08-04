module DefinicionesYPropiedadesSpec where

import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import DefinicionesYPropiedades

import Test.Hspec
import Test.QuickCheck

ejemplosDefinicionesYPropiedades :: Spec
ejemplosDefinicionesYPropiedades =
  describe "DefinicionesYPropiedades" $ do
  describe "orden" $ do
    it "e1" $
      orden (grafoCiclo 4) `shouldBe`
      4
    it "e2" $
      orden (grafoEstrella 4) `shouldBe`
      5
    it "e3" $
      orden grafoPetersen `shouldBe`
      10
    it "e4" $
      orden (grafoPetersenGen 2 5) `shouldBe`
      4
    it "e5" $
      orden (completo 3) `shouldBe`
      3

  describe "tamaño" $ do
    it "e1" $
      tamaño (grafoCiclo 4) `shouldBe`
      4
    it "e2" $
      tamaño (grafoEstrella 4) `shouldBe` 
      4
    it "e3" $
      tamaño grafoPetersen `shouldBe`
      15
    it "e4" $
      tamaño (grafoPetersenGen 2 5) `shouldBe`
      4
    it "e5" $
      tamaño (completo 3) `shouldBe`
      3

  describe "sonIncidentes" $ do
    it "e1" $
      sonIncidentes (1,2) (2,4) `shouldBe`
      True
    it "e2" $
      sonIncidentes (1,2) (3,4) `shouldBe`
      False

  describe "esLazo" $ do
    it "e1" $
      esLazo (1,2) `shouldBe`
      False
    it "e2" $
      esLazo (4,4) `shouldBe`
      True

  describe "entorno" $ do
    it "e1" $
      entorno (grafoEstrella 5) 1 `shouldBe`
      [2,3,4,5,6]
    it "e2" $
      entorno (grafoEstrella 5) 2 `shouldBe`
      [1]
    it "e3" $
      entorno (bipartitoCompleto 2 4) 5 `shouldBe`
      [1,2]
    it "e4" $
      entorno grafoPetersen 4 `shouldBe`
      [1,2,9]

  describe "grado" $ do
    it "e1" $
      grado (grafoEstrella 5) 1 `shouldBe`
      5
    it "e2" $
      grado (grafoEstrella 5) 2 `shouldBe`
      1
    it "e3" $
      grado grafoThomson 6 `shouldBe`
      3
    it "e4" $
      grado (grafoAmistad 2) 4 `shouldBe`
      2

  describe "esAislado" $ do
    it "e1" $
      esAislado (grafoEstrella 5) 1 `shouldBe`
      False
    it "e2" $
      esAislado (bipartitoCompleto 1 5) 4 `shouldBe`
      False
    it "e3" $
      esAislado (creaGrafo [1..4] [(1,2),(1,4),(2,4)]) 2 `shouldBe`
      False
    it "e4" $
      esAislado (creaGrafo [1..4] [(1,2),(1,4),(2,4)]) 3  `shouldBe`
      True

  describe "esRegular" $ do
    it "e1" $
      esRegular (grafoCiclo 5) `shouldBe`
      True
    it "e2" $
      esRegular (bipartitoCompleto 2 2) `shouldBe`
      True
    it "e3" $
      esRegular (grafoEstrella 4) `shouldBe`
      False
    it "e4" $
      esRegular (grafoRueda 7) `shouldBe`
      False

  describe "valenciaMin" $ do
    it "e1" $
      valenciaMin (grafoEstrella 6) `shouldBe`
      1
    it "e2" $
      valenciaMin (grafoCiclo 4) `shouldBe`
      2
    it "e3" $
      valenciaMin grafoPetersen `shouldBe`
      3
    it "e4" $
      valenciaMin (creaGrafo [1..4] [(1,2),(1,4),(2,4)]) `shouldBe`
      0

  describe "valenciaMax" $ do
    it "e1" $
      valenciaMax (grafoEstrella 6) `shouldBe`
      6
    it "e2" $
      valenciaMax (grafoCiclo 4) `shouldBe`
      2
    it "e3" $
      valenciaMax grafoPetersen `shouldBe`
      3

  describe "esSimple" $ do
    it "e1" $
      esSimple (bipartitoCompleto 3 4) `shouldBe`
      True
    it "e2" $
      esSimple (creaGrafo [1..3] [(1,2),(1,3),(2,3)]) `shouldBe`
      True
    it "e3" $
      esSimple (creaGrafo [1..3] [(1,1),(1,2),(2,3)]) `shouldBe`
      False

  describe "secuenciaGrados" $ do
    it "e1" $
      secuenciaGrados (grafoEstrella 6) `shouldBe`
      [6,1,1,1,1,1,1]
    it "e2" $
      secuenciaGrados (grafoCiclo 4) `shouldBe`
      [2,2,2,2]
    it "e3" $
      secuenciaGrados grafoPetersen `shouldBe`
      [3,3,3,3,3,3,3,3,3,3]

  describe "secuenciaGrafica" $ do
    it "e1" $
      secuenciaGrafica [2,2,2,2,2,2] `shouldBe`
      True
    it "e2" $
      secuenciaGrafica [6,1,1,1,1,1,1] `shouldBe`
      True
    it "e3" $
      secuenciaGrafica [6,1,1,1,1,1] `shouldBe`
      False
    it "e4" $
      secuenciaGrafica [5,4..1] `shouldBe`
      False

  describe "esSubgrafo" $ do
    it "e1" $
      esSubgrafo (bipartitoCompleto 3 2) (bipartitoCompleto 3 3) `shouldBe`
      True
    it "e2" $
      esSubgrafo (grafoEstrella 4) (grafoEstrella 5) `shouldBe`
      True
    it "e3" $
      esSubgrafo (completo 5) (completo 4) `shouldBe`
      False
    it "e4" $
      esSubgrafo (completo 3) (completo 4) `shouldBe`
      True

  describe "esSubgrafoMax" $ do
    it "e1" $
      esSubgrafoMax (grafoRueda 3) (grafoRueda 4) `shouldBe`
      False
    it "e2" $
      esSubgrafoMax (grafoCiclo 4) (grafoRueda 4) `shouldBe`
      True
    it "e3" $
      esSubgrafoMax (creaGrafo [1..3] [(1,2)]) (grafoCiclo 3) `shouldBe`
      True
    it "e4" $
      esSubgrafoMax (creaGrafo [1..2] [(1,2)]) (grafoCiclo 3) `shouldBe`
      False

  describe "esSubgrafoPropio" $ do
    it "e1" $
      esSubgrafoPropio (grafoRueda 3) (grafoRueda 4) `shouldBe`
      True
    it "e2" $
      esSubgrafoPropio (grafoRueda 4) (grafoCiclo 5) `shouldBe`
      False
    it "e3" $
      esSubgrafoPropio (creaGrafo [1..3] [(1,2)]) (grafoCiclo 3) `shouldBe`
      True
    it "e4" $
      esSubgrafoPropio (creaGrafo [1..2] [(1,2)]) (grafoCiclo 3) `shouldBe`
      True

  describe "Propiedades" $ do
    it "Lema del apretón de manos" $ 
      property $ prop_LemaApretonDeManos
    it "Teorema de Havel-Hakimi" $
      property $ prop_HavelHakimi

  describe "eliminaArista" $ do
    it "e1" $
      eliminaArista grafoThomson (3,4) `shouldBe`
      creaGrafo [1..6] [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,5),(3,6)]
    it "e2" $
      eliminaArista grafoThomson (4,3) `shouldBe`
      creaGrafo [1..6] [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,5),(3,6)]
    it "e3" $
      eliminaArista grafoThomson (1,3) `shouldBe`
      creaGrafo [1..6] [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

  describe "eliminaVertice" $ do
    it "e1" $
      eliminaVertice grafoThomson 3 `shouldBe`
      creaGrafo [1,2,4,5,6] [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6)]
    it "e2" $
      eliminaVertice grafoThomson 2 `shouldBe`
      creaGrafo [1,3,4,5,6] [(1,4),(1,5),(1,6),(3,4),(3,5),(3,6)]
    it "e3" $
      eliminaVertice grafoThomson 8 `shouldBe` 
      creaGrafo [1..6] [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
    
  describe "sumaArista" $ do
    it "e1" $
      sumaArista (grafoCiclo 5) (1,3) `shouldBe`
      creaGrafo [1..5] [(1,2),(1,3),(1,5),(2,3),(3,4),(4,5)]
    it "e2" $
      sumaArista (grafoEstrella 5) (4,5) `shouldBe`
      creaGrafo [1..6] [(1,2),(1,3),(1,4),(1,5),(1,6),(4,5)]

  describe "sumaVertice" $ do
    it "e1" $
      sumaVertice (completo 5) 6 `shouldBe`
      creaGrafo [1..6] [(1,2),(1,3),(1,4),(1,5),(1,6),
                        (2,3),(2,4),(2,5),(2,6),(3,4),
                        (3,5),(3,6),(4,5),(4,6),(5,6)]
    it "e2" $
      sumaVertice (creaGrafo [2..5] []) 1 `shouldBe`
      creaGrafo [1,2,3,4,5] [(1,2),(1,3),(1,4),(1,5)]

  describe "Suma de vértices de grafos completos" $ do
    it "prop_completos" $ 
      property $ prop_completos

  describe "sumaGrafos" $ do
    it "e1" $
      sumaGrafos (grafoCiclo 3) (grafoCiclo 3) `shouldBe`
      creaGrafo [1,2,3] [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]
    it "e2" $
      sumaGrafos (grafoRueda 3) (grafoEstrella 4) `shouldBe`
      creaGrafo [1..5] [(1,1),(1,2),(1,3),(1,4),(1,5),(2,2),
                        (2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]

  describe "unionGrafos" $ do
    it "e1" $
      unionGrafos (grafoCiclo 3) (grafoCiclo 3) `shouldBe`
      creaGrafo [1,2,3] [(1,2),(1,3),(2,3)]
    it "e2" $
      unionGrafos (grafoRueda 3) (grafoEstrella 4) `shouldBe`
      creaGrafo [1..5] [(1,2),(1,3),(1,4),(1,5),(2,3),(2,3)]
  
  describe "complementario" $ do
    it "e1" $
      complementario (grafoEstrella 5) `shouldBe`
      creaGrafo [1..6] [(1,1),(2,2),(2,3),(2,4),(2,5),(2,6),
                        (3,3),(3,4),(3,5),(3,6),(4,4),(4,5),
                        (4,6),(5,5),(5,6),(6,6)]
    it "e2" $
      complementario (completo 4) `shouldBe`
      creaGrafo [1..4] [(1,1),(2,2),(3,3),(4,4)]

verificaDefinicionesYPropiedades :: IO ()
verificaDefinicionesYPropiedades = 
  hspec ejemplosDefinicionesYPropiedades

-- La validación es
--    ghci> verificaDefinicionesYPropiedades
--    
--    DefinicionesYPropiedades
--      orden
--        e1
--        e2
--        e3
--        e4
--        e5
--      tamaño
--        e1
--        e2
--        e3
--        e4
--        e5
--      sonIncidentes
--        e1
--        e2
--      esLazo
--        e1
--        e2
--      entorno
--        e1
--        e2
--        e3
--        e4
--      grado
--        e1
--        e2
--        e3
--        e4
--      esAislado
--        e1
--        e2
--        e3
--        e4
--      esRegular
--        e1
--        e2
--        e3
--        e4
--      valenciaMin
--        e1
--        e2
--        e3
--        e4
--      valenciaMax
--        e1
--        e2
--        e3
--      esSimple
--        e1
--        e2
--        e3
--      secuenciaGrados
--        e1
--        e2
--        e3
--      secuenciaGrafica
--        e1
--        e2
--        e3
--        e4
--      esSubgrafo
--        e1
--        e2
--        e3
--        e4
--      esSubgrafoMax
--        e1
--        e2
--        e3
--        e4
--      esSubgrafoPropio
--        e1
--        e2
--        e3
--        e4
--      Propiedades
--        Lema del apretón de manos
--        Teorema de Havel-Hakimi
--      eliminaArista
--        e1
--        e2
--        e3
--      eliminaVertice
--        e1
--        e2
--        e3
--      sumaArista
--        e1
--        e2
--      sumaVertice
--        e1
--        e2
--      Suma de vértices de grafos completos
--        prop_completos
--      sumaGrafos
--        e1
--        e2
--      unionGrafos
--        e1
--        e2
--      complementario
--        e1
--        e2
--    
--    Finished in 4.8628 seconds
--    78 examples, 0 failures
