import Text.Show.Functions ()
import Data.List (maximumBy,minimumBy)
import Data.Ord (comparing)

data Carrera = UnaCarrera {
    vueltas :: Int,
    longPista :: Int,
    participantes :: [Corredor],
    publico :: [String]
}

data Corredor = UnCorredor {
    nombre :: String,
    nafta :: Int,
    velocidad :: Int,
    enamorada :: String,
    truco :: Truco
}

type Truco = Corredor -> Corredor

deReversaRocha :: Int -> Truco
deReversaRocha metros unCorredor = unCorredor {nafta = nafta unCorredor + 5*metros}

impresionar :: Carrera -> Truco
impresionar unaCarrera unCorredor
    |elem (enamorada unCorredor) (publico unaCarrera) = unCorredor {velocidad = 2 * velocidad unCorredor}
    |otherwise = unCorredor

nitro :: Truco
nitro unCorredor = unCorredor {velocidad = velocidad unCorredor + 15}

comboLoco :: Carrera -> Truco
comboLoco unaCarrera unCorredor = nitro.impresionar unaCarrera $ unCorredor



n :: Int
n = 100

carrera :: Carrera
carrera = UnaCarrera {
    vueltas = 60,
    longPista = 98,
    participantes = [rochaMcQueen,bainkerr,gushtav,rodra],
    publico = []
}

rochaMcQueen :: Corredor
rochaMcQueen = UnCorredor {
    nombre = "Rocha McQueen",
    nafta = 282,
    velocidad = 0,
    enamorada = "Ronco",
    truco = deReversaRocha n
}

bainkerr :: Corredor
bainkerr = UnCorredor {
    nombre = "Bainkerr",
    nafta = 378,
    velocidad = 0,
    enamorada = "Tincho",
    truco = impresionar carrera
}

gushtav :: Corredor
gushtav = UnCorredor{
    nombre = "Gushtav",
    nafta = 230,
    velocidad = 0,
    enamorada = "Peti",
    truco = nitro
}

rodra :: Corredor
rodra = UnCorredor{
    nombre = "Rodra",
    nafta = 153,
    velocidad = 0,
    enamorada = "Tais",
    truco = comboLoco carrera
}


darVuelta :: Carrera -> Corredor -> Corredor
darVuelta unaCarrera unCorredor = unCorredor{nafta = nafta unCorredor - ((*longPista unaCarrera) . length . nombre $ unCorredor), velocidad = aumentarVelocidadA unCorredor}

aumentarVelocidadA :: Corredor -> Int
aumentarVelocidadA unCorredor
    |(length . nombre $ unCorredor) >= 1 && (length . nombre $ unCorredor) <= 5 = velocidad unCorredor + 15
    |(length . nombre $ unCorredor) >= 6 && (length . nombre $ unCorredor) <= 8 = velocidad unCorredor + 20
    |(length . nombre $ unCorredor) > 8 = velocidad unCorredor + 30
    |otherwise = velocidad unCorredor

--no pude hacer que el ultimo aplique el truco


correrCarrera:: Carrera -> Carrera
correrCarrera unaCarrera = unaCarrera {participantes = iterate (map (darVuelta unaCarrera)) (participantes unaCarrera) !! (vueltas unaCarrera + 1)}--genera lista infinita de "dar vueltas" y toma la vuelta n+1, la ultima, y se queda con eso


ganadorDeCarrera :: Carrera -> Corredor
ganadorDeCarrera unaCarrera = maximumBy (comparing velocidad) (participantes . correrCarrera $ unaCarrera)--chatGPT wtf que éjercicio de mierda

--find :: (a->Bool) -> [a] -> a

-- find (maximum . map velocidad) listaCorredores = corredor

puedenCompetirOtraVez :: Carrera -> [Corredor]
puedenCompetirOtraVez unaCarrera = ganadorDeCarrera unaCarrera : filter ((>=27).nafta) (participantes . correrCarrera $ unaCarrera)--si el ganador tiene +de o 27 litros aparece 2 veces, pero bueno

--a) En mi modelo no, ya que al querer dar la 1era vuelta, no va a terminar nunca de ajustar los datos de c/participante.
--b) Dado que el 1er participante va a tener un determinado puesto, si podemos llegar a conocerlo, pero tras un tiempo indeterminado de que la compu trabaje, dado que no se puede determinar en que posicion perteneciente a [1; +inf) va a estar, pero va a aparecer en cierto punto.
--c) No, y ya lo explique.