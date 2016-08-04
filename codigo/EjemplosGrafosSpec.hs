module EjemplosGrafosSpec where

import Test.Hspec
import GrafoConListaDeAristas
import EjemplosGrafos

ejemplosEjemplosGrafos :: Spec
ejemplosEjemplosGrafos =
  describe "EjemplosGrafos" $ do
  describe "esGrafoNulo" $ do
    it "e1" $
      esGrafoNulo (grafoNulo :: Grafo Int) `shouldBe` True
    it "e2" $
      esGrafoNulo (creaGrafo [] [(1,2)]) `shouldBe` False
    it "e3" $
      esGrafoNulo (creaGrafo [1,2] [(1,2)]) `shouldBe`  False

  describe "grafoCiclo" $ do
    it "e1" $
      grafoCiclo 5 `shouldBe`
      creaGrafo [1..5] [(1,2),(1,5),(2,3),(3,4),(4,5)]

  describe "grafoAmistad" $ do
    it "e1" $
      grafoAmistad 2 `shouldBe` 
      creaGrafo [1..5] [(1,2),(1,3),(1,4),(1,5),(2,3),(4,5)]
    it "e2" $
      grafoAmistad 3 `shouldBe`
      creaGrafo [1..7] [(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(2,3),(4,5),(6,7)]

  describe "completo" $ do
    it "e1" $
      completo 4 `shouldBe`
      creaGrafo [1..4] [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

  describe "bipartitoCompleto" $ do
    it "e1" $
      bipartitoCompleto 2 3 `shouldBe`
      creaGrafo [1..5] [(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)]

  describe "grafoEstrella" $ do
    it "e1" $
      grafoEstrella 5 `shouldBe`
      creaGrafo [1..6] [(1,2),(1,3),(1,4),(1,5),(1,6)]

  describe "grafoRueda" $ do
    it "e1" $
      grafoRueda 6 `shouldBe`
      creaGrafo [1..6] [(1,2),(1,3),(1,4),(1,5),(1,6),
                        (2,3),(2,6),(3,4),(4,5),(5,6)]

  describe "grafoCirculante" $ do
    it "e1" $
      grafoCirculante 6 [1,2] `shouldBe`
      creaGrafo [1..6] [(1,2),(1,3),(1,5),(1,6),(2,3),(2,4),
                        (2,6),(3,4),(3,5),(4,5),(4,6),(5,6)]

  describe "grafoPetersenGen" $ do
    it "e1 1" $
      grafoPetersenGen 4 2 `shouldBe`
      creaGrafo [1..8] [(1,3),(1,5),(2,4),(2,6),(3,7),
                        (4,8),(5,6),(5,8),(6,7),(7,8)]

  describe "grafoThomson" $ do
    it "e1" $
      grafoThomson `shouldBe`
      creaGrafo [1..6] [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

  describe "grafoPetersen" $ do
    it "e1" $
      grafoPetersen `shouldBe` 
      creaGrafo [1..10]
                [(1,3),(1,4),(1,6),(2,4),(2,5),(2,7),(3,5),(3,8),
                 (4,9),(5,10),(6,7),(6,10),(7,8),(8,9),(9,10)]

  describe "grafoMoebiusCantor" $ do
    it "e1" $
      grafoMoebiusCantor `shouldBe` 
      creaGrafo [1..16]
                [(1,4),(1,6),(1,9),(2,5),(2,7),(2,10),(3,6),(3,8),(3,11),(4,7),
                 (4,12),(5,8),(5,13),(6,14),(7,15),(8,16),(9,10),(9,16),(10,11),
                 (11,12),(12,13),(13,14),(14,15),(15,16)]
      
verificaEjemplosGrafos :: IO ()
verificaEjemplosGrafos = 
  hspec ejemplosEjemplosGrafos

-- La validación es
--    ghci> verificaEjemplosGrafos
--    
--    EjemplosGrafos
--      esGrafoNulo
--        e1
--        e2
--        e3
--      grafoCiclo
--        e1
--      grafoAmistad
--        e1
--        e2
--      completo
--        e1
--      bipartitoCompleto
--        e1
--      grafoEstrella
--        e1
--      grafoRueda
--        e1
--      grafoCirculante
--        e1
--      grafoPetersenGen
--        e1 1
--      grafoThomson
--        e1
--      grafoPetersen
--        e1
--      grafoMoebiusCantor
--        e1
--    
--    Finished in 0.0045 seconds
--    15 examples, 0 failures
