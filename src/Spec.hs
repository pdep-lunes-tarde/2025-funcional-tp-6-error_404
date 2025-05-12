module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)
import GHC.RTS.Flags (ProfFlags(descrSelector))

correrTests :: IO ()
correrTests = hspec $ do
    describe "# TP 6" $ do
        describe"## Parte 1: Hamburguesas" $ do
            it "Agrandar:agranda una hamburguesa se agrega otro ingrediente base y se elige el ingrediente base a agregar según lo que ya haya en la hamburguesa " $ do
                agrandar cuartoDeLibra `shouldBe ` Hamburguesa 20 [Carne,Pan, Carne, Cheddar, Pan]
            it "agregarIngrediente: recibe un ingrediente y una hambrugesa lo agrega a la hamburguesa. " $ do
                agregarIngrediente Cheddar cuartoDeLibra `shouldBe ` Hamburguesa 20 [Cheddar,Pan, Carne, Cheddar, Pan]
            it "descuento: recibe un % de descuento, y devuelve la hamburguesa con ese descuento aplicado al precio base." $ do
                descuento 20 cuartoDeLibra `shouldBe ` Hamburguesa 16 [Pan, Carne, Cheddar, Pan]
            it "la pdepBurger, que es un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento tiene precio final de  110." $ do
                precioFinal pdepBurger `shouldBe ` 110
        describe"## Parte 2: Algunas hamburguesas más" $ do
            it " dobleCuarto = es un cuarto de libra con carne y cheddar. El precio final deberia ser 84" $ do
                precioFinal dobleCuarto `shouldBe ` 84
            it "bigPdep = es un doble cuarto con curry. El precio final deberia ser 89" $ do
                precioFinal bigPdep `shouldBe ` 89
            it "delDia = es una promo que, dada una hamburguesa, le agrega Papas y un descuento del 30%.una doble cuarto del día deberia valer 88." $ do
                precioFinal (delDia dobleCuarto) `shouldBe ` 88
        describe "## Parte 3: Algunos cambios más" $ do
            describe "- hacerVeggie: cambia ingrediente base -> PatiVegano, Cheddar -> QuesoDeAlmendras y Panceta -> BaconDeTofu" $ do
                let superCheeseBurger = Hamburguesa 20 [Pan, Panceta, Cheddar, Carne, Pan]
                let veggieCheeseBurger = Hamburguesa 20 [Pan, BaconDeTofu, QuesoDeAlmendras, PatiVegano, Pan]
                it "    Dada una hamburguesa de Carne con cheddar y panceta devuelve la vegetariana" $ do
                    hacerVeggie superCheeseBurger `shouldBe` veggieCheeseBurger
                it "    Dada una hamburguesa hecha vegetariana devuelve su precio verde" $ do
                    precioFinal veggieCheeseBurger `shouldBe` 61 -- 20 + 2 + 12 + 15 + 10 + 2
            describe "- cambiarPanDePati: cambia el Pan que haya en la hamburguesa por PanIntegral" $ do
                it "    Dada una cuarto de libra devuelve la misma con pan integral" $ do
                    cambiarPanDePati cuartoDeLibra `shouldBe` cuartoDeLibra {ingredientes = [PanIntegral, Carne, Cheddar, PanIntegral]}
                it "    Dada una cuarto de libra con pan integral devuelve su precio actualizado" $ do
                    precioFinal (cambiarPanDePati cuartoDeLibra) `shouldBe` 56 -- 20 + 3 + 20 + 10 + 3
            describe "- dobleCuartoVegano: es un dobleCuarto veggie con pan integral" $ do
                it "    Dado un doble cuarto vegano devuelve su precio verde" $ do
                    precioFinal dobleCuartoVegano `shouldBe` 76 -- 20 + 3 + 10 + 10 + 10 + 10 + 3 

        