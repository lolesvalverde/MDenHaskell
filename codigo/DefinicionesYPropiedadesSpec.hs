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
      comprueba prop_LemaApretonDeManos
    it "Teorema de Havel-Hakimi" $
      comprueba prop_HavelHakimi

comprueba :: Testable prop => prop -> Expectation
comprueba p = 
  quickCheckWith (stdArgs {chatty=False}) p `shouldReturn` ()
  
verificaDefinicionesYPropiedades :: IO ()
verificaDefinicionesYPropiedades = 
  hspec ejemplosDefinicionesYPropiedades
