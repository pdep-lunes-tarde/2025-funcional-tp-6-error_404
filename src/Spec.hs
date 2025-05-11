module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 5" $ do
     describe"Parte 1: Hamburguesas" $ do
         it "Agrandar:agranda una hamburguesa se agrega otro ingrediente base y se elige el ingrediente base a agregar según lo que ya haya en la hamburguesa " $ do
            agrandar cuartoDeLibra `shouldBe ` Hamburguesa 20 [Carne,Pan, Carne, Cheddar, Pan]
         it "agregarIngrediente: recibe un ingrediente y una hambrugesa lo agrega a la hamburguesa. " $ do
            agregarIngrediente Cheddar cuartoDeLibra `shouldBe ` Hamburguesa 20 [Cheddar,Pan, Carne, Cheddar, Pan]
         it "descuento: recibe un % de descuento, y devuelve la hamburguesa con ese descuento aplicado al precio base." $ do
            descuento 20 cuartoDeLibra `shouldBe ` Hamburguesa 16 [Pan, Carne, Cheddar, Pan]
         it "la pdepBurger, que es un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento tiene precio final de  110." $ do
            precioFinal pdepBurger `shouldBe ` 110
     describe"PARTE 2: Algunas hamburguesas más" $ do
        it " dobleCuarto = es un cuarto de libra con carne y cheddar. El precio final deberia ser 84" $ do
            precioFinal dobleCuarto `shouldBe ` 84
        it "bigPdep = es un doble cuarto con curry. El precio final deberia ser 89" $ do
            precioFinal bigPdep `shouldBe ` 89
        it "delDia = es una promo que, dada una hamburguesa, le agrega Papas y un descuento del 30%.una doble cuarto del día deberia valer 88." $ do
            precioFinal (delDia dobleCuarto) `shouldBe ` 88

        