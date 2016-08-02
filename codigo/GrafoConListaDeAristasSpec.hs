import Test.Hspec
import GrafoConListaDeAristas

ejGrafo :: Grafo Int
ejGrafo = creaGrafo [1..5]
                    [(1,2),(1,4),(1,5),(2,3),(2,5),(3,4),(3,5),(4,5)]

ejemplosGrafoConListaDeAristas :: Spec
ejemplosGrafoConListaDeAristas =
  describe "GrafoConListaDeAristas" $ do
    describe "vertices" $ do
      it "e1" $
        vertices ejGrafo `shouldBe`
        [1,2,3,4,5]

    describe "adyacentes" $ do
      it "e1" $
        adyacentes ejGrafo 4 `shouldBe` [1,3,5]
      it "e2" $
        adyacentes ejGrafo 3 `shouldBe` [2,4,5]

    describe "aristaEn" $ do
      it "e1" $
        (5,1) `aristaEn` ejGrafo `shouldBe` True
      it "e2" $
        (3,1) `aristaEn` ejGrafo `shouldBe` False

    describe "aristas" $ do
      it "e1" $
        aristas ejGrafo `shouldBe`
        [(1,2),(1,4),(1,5),(2,3),(2,5),(3,4),(3,5),(4,5)]

verificaGrafoConListaDeAristas :: IO ()
verificaGrafoConListaDeAristas = 
  hspec ejemplosGrafoConListaDeAristas

-- La validación es
--    ghci> verificaGrafoConListaDeAristas
--    
--    GrafoConListaDeAristas
--      vertices
--        e1
--      adyacentes
--        e1
--        e2
--      aristaEn
--        e1
--        e2
--      aristas
--        e1
--    
--    Finished in 0.0005 seconds
--    6 examples, 0 failures
