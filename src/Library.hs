module Library where
import PdePreludat


data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papa
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papa = 10

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

--Parte 1
precioFinal (Hamburguesa precioBase ingredientes) = precioBase + (sum . map precioIngrediente)ingredientes


agrandar::  Hamburguesa -> Hamburguesa
agrandar hamburguesa
 |any(== Carne) (ingredientes hamburguesa) = agregarIngrediente Carne hamburguesa
 |any(== Pollo) (ingredientes hamburguesa) = agregarIngrediente Pollo hamburguesa
 |otherwise = hamburguesa


agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = hamburguesa{
 ingredientes = ingrediente : ingredientes hamburguesa
}

descuento:: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa = hamburguesa{
    precioBase = precioBase hamburguesa * (1-porcentaje/100)
 }

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]


pdepBurger:: Hamburguesa
pdepBurger=(descuento 20.agregarIngrediente Cheddar . agregarIngrediente Panceta .agrandar.agrandar)cuartoDeLibra

--Parte 2
dobleCuarto:: Hamburguesa
dobleCuarto = (agregarIngrediente Cheddar .agregarIngrediente Carne)cuartoDeLibra

bigPdep:: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia:: Hamburguesa -> Hamburguesa
delDia= descuento 30 . agregarIngrediente Papa