import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre:: String,
    puntaje:: Int,
    inventario:: [Material]
}

data Material = UnMaterial{
    nombreMaterial :: String,
    cantidad :: Int,
    receta :: Receta
    }
type Receta = (ListaNombreMateriales,Demora)

type NombreMaterial = String
type Cantidad = Int
type ListaNombreMateriales = [String]
type Demora = Int

--Craft
--1. Hacer las funciones necesarias para que un jugador craftee un nuevo objeto
--  .El jugador debe quedar con el nuevo objeto en su inventario
--  .El jugador debe quedar sin los materiales usados para craftear
--  .La cantidad de puntos del jugador se incrementa a razón de 10 puntos por segundo utilizado en el crafteo.
--  .El objeto se craftea sólo si se cuenta con todos los materiales requeridos antes de comenzar la tarea. En caso contrario, no se altera el inventario, pero se pierden 100 puntos.

{-
tienePara :: ListaNombreMateriales -> Personaje -> Bool
tienePara listaMateriales unPersonaje = 

craftear :: String -> Personaje -> Personaje
craftear unMaterial unPersonaje
    |tienePara (map fst . receta $ unMaterial) unPersonaje = unPersonaje {inventario = inventario unPersonaje ++ }
    |otherwise = unPersonaje {puntaje = puntaje unPersonaje - 100}

-}
--tiene, actualizar inventario, restar 100 puntos

{-
-El jugador debe quedar sin los materiales usados para craftear
-La cantidad de puntos del jugador se incrementa a razón de 10 puntos por segundo utilizado en el crafteo.
-El objeto se craftea sólo si se cuenta con todos los materiales requeridos antes de comenzar la tarea. En caso contrario, no se altera el inventario, pero se pierden 100 puntos.
-El jugador debe quedar con el nuevo objeto en su inventario
-}