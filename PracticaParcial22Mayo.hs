import Text.Show.Functions ()

--Parte A

{-
Nuestro programa tendrá participantes que cuentan con nombre, trucos de cocina y un plato que es su especialidad. Los platos, a su vez, tienen una dificultad que va de 0 a 10 y un conjunto de componentes que nos indican sus componentes con sus respectivos pesos en gramos.
-}

data Participante = UnParticipante
    {
        nombre :: String,
        trucos :: [Truco],
        especialidad :: Plato
    } deriving Show


type Truco = Plato -> Plato

data Plato = UnPlato
    {
        dificultad :: Int,
        componentes :: [(String,Int)]
    } deriving (Show,Eq)


agregarComponente :: (String,Int) -> Plato -> Plato
agregarComponente (nombre,gramos) unPlato = unPlato {componentes = componentes unPlato ++ [(nombre, gramos)]}


-- Parte A

--Algunos de los trucos más famosos son:
--  1. endulzar: dada una cantidad de gramos de azúcar, le agrega ese componente a un plato.  

endulzar :: Int -> Truco
endulzar gramosAzucar = agregarComponente ("Azucar",gramosAzucar)

--  2. salar: la vieja y confiable… dada una cantidad de gramos de sal y un plato, nos retorna el mismo con esa cantidad de sal para que quede flama.

eliminarComponente :: String -> Truco
eliminarComponente nombre unPlato = unPlato {componentes = filter (\(ingrediente, _) -> ingrediente /= nombre) (componentes unPlato)}

salar :: Int -> Truco
salar gramosSal unPlato = agregarComponente ("Sal", gramosSal) (eliminarComponente "Sal" unPlato)

--  3. darSabor: dadas una cantidad de sal y una de azúcar sala y endulza un plato.

darSabor :: Int -> Int -> Truco
darSabor gramosSal gramosAzucar = endulzar gramosAzucar . salar gramosSal

--  4. duplicarPorcion: se duplica la cantidad de cada componente de un plato… para más placer.

duplicarPorcion :: Truco
duplicarPorcion unPlato = unPlato {componentes= map (\(ingrediente, gramos) -> (ingrediente, gramos * 2)) (componentes unPlato)}

--  5. simplificar: hay platos que son realmente un bardo. Es por ello que si un plato tiene más de 5 componentes y una dificultad mayor a 7 lo vamos a simplificar, sino lo dejamos igual. Simplificar un plato es dejarlo con 5 de dificultad y quitarle aquellos componentes de los que hayamos agregado menos de 10 gramos.

simplificar :: Plato->Plato
simplificar unPlato
    |esComplejo unPlato = unPlato {dificultad = 5, componentes = filter (\(_, gramos) -> gramos < 10) (componentes unPlato)}
    |otherwise = unPlato

--  5. esVegano: si no tiene carne, huevos o alimentos lácteos.

tiene :: Plato -> String -> Bool
tiene unPlato nombre = any (\(ingrediente, _) -> ingrediente == nombre) (componentes unPlato)


esVegano :: Plato -> Bool
esVegano unPlato = not (tiene unPlato "Carne" || tiene unPlato "Huevos" || tiene unPlato "Alimentos Lacteos")

--  6. esSinTacc: si no tiene harina.

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not (tiene unPlato "Harina")

--  7. esComplejo: cuando tiene más de 5 componentes y una dificultad mayor a 7.

esComplejo :: Plato -> Bool
esComplejo unPlato = length (componentes unPlato) >= 5 && dificultad unPlato >=7

--  8. noAptoHipertension: si tiene más de 2 gramos de sal.

gramosDe :: String -> Plato -> Int
gramosDe nombre unPlato = cantidad . conseguirComponente nombre $ unPlato

cantidad :: (String,Int) -> Int
cantidad (_,gramos) = gramos

conseguirComponente :: String -> Plato -> (String,Int)
conseguirComponente nombre unPlato = head . filter (esDe nombre) $ (componentes unPlato)

esDe :: String -> (String,Int)->Bool
esDe nombre (ingrediente,_) = nombre == ingrediente

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = tiene unPlato "Sal" && gramosDe "Sal" unPlato >= 2


--Parte B
--modelar a pepe


--Parte C


--1. Cocinar

cocinar :: Participante -> Plato
cocinar unParticipante = aplicarTrucos (trucos unParticipante) (especialidad unParticipante)

aplicarTrucos :: [Truco] -> Plato -> Plato
aplicarTrucos unosTrucos unPlato = foldr ($) unPlato unosTrucos --ahh es como un map invertido, toma una lista de funciones (Truco) y aplica todo sobre unPlato

--2. esMejorQue

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = esMasDificil unPlato otroPlato && esMasLigero unPlato otroPlato

esMasDificil :: Plato -> Plato -> Bool
esMasDificil unPlato otroPlato = dificultad unPlato > dificultad otroPlato

esMasLigero :: Plato -> Plato -> Bool
esMasLigero unPlato otroPlato = peso unPlato < peso otroPlato

peso :: Plato -> Int
peso unPlato = sum . map cantidad . componentes $ unPlato

--3. participanteEstrella

participanteEstrella :: [Participante] -> Participante
participanteEstrella unosParticipantes = foldr1 mejorParticipante unosParticipantes
--foldr1 unaFuncion (x:xs) = foldr unaFuncion x xs

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante unParticipante otroParticipante = if esMejorQue (cocinar unParticipante) (cocinar otroParticipante) then unParticipante else otroParticipante

participanteEstrella' :: [Participante] -> Participante
participanteEstrella' [unParticipante] = unParticipante
participanteEstrella' (unParticipante : siguientesParticipantes) = mejorParticipante unParticipante (participanteEstrella' siguientesParticipantes)


--Parte D

platinum :: Plato
platinum = UnPlato 10 unaListaDeComponentesRara

unaListaDeComponentesRara :: [(String,Int)]
unaListaDeComponentesRara = map (\unNumero -> ("Ingrediente " ++ show unNumero, unNumero)) [1..]
--show unNumero: "convierte" unNumero a String