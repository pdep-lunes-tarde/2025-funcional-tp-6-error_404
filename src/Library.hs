module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | BaconDeTofu
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

-- Hamburguesa de ejemplo
cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 ["Pan", "Carne", "Cheddar", "Pan"]

--Precio Final
precioFinal :: Hamburguesa -> Number
precioFinal (Hamburguesa precioBase ingredientes) = precioBase + sum . map precioIngrediente . ingredientes

--Agrandar

agrandar :: Ingrediente -> Hamburguesa -> Hamburguesa
agrandar ingrediente (Hamburguesa unPrecio ingredientes) = Hamburguesa (unPrecio + precioIngrediente ingrediente) ()
     

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingr burga = 

descuento = implementame

pdepBurger = implementame