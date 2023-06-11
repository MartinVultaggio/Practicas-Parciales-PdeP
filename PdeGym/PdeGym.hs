import Text.Show.Functions ()

--Parte A

data Persona = UnaPersona {
    nombre :: String,
    calorias :: Int,
    indHidratacion :: Int,
    tiempo :: Int,
    equipamiento :: [String]
}deriving Show

type Ejercicio = Persona -> Persona
--1. abdominales
abdominales :: Int -> Ejercicio
abdominales repeticiones unaPersona = unaPersona {calorias = calorias unaPersona - 8*repeticiones}

--2. flexiones
flexiones :: Int -> Ejercicio
flexiones repeticiones unaPersona = unaPersona {calorias = calorias unaPersona - 16*repeticiones, indHidratacion = indHidratacion unaPersona - 2*div repeticiones 10}

--3. levantar pesas
levantarPesas :: Int -> Int -> Ejercicio
levantarPesas repeticiones peso unaPersona
    |elem "pesa" (equipamiento unaPersona) = unaPersona {calorias = calorias unaPersona - 32*repeticiones, indHidratacion = indHidratacion unaPersona - peso * div repeticiones 10}
    |otherwise = unaPersona

--4.laGranHomeroSimpson
laGranHomeroSimpson :: Ejercicio
laGranHomeroSimpson unaPersona = unaPersona
laGranHomeroSimpson' :: Ejercicio
laGranHomeroSimpson' = id


--5.1, renovar equipo
renovarEquipo :: Persona -> Persona
renovarEquipo unaPersona = unaPersona {equipamiento = map ("Nuevo " ++ ) (equipamiento unaPersona)}

matias :: Persona
matias = UnaPersona {
    nombre = "Matias",
    calorias = 1000,
    indHidratacion = 50,
    tiempo = 4000,
    equipamiento = ["Pesa", "Mancuerna"]
}

--5.2. volverse yoguista
volverseYoguista :: Persona -> Persona
volverseYoguista unaPersona = unaPersona {calorias = div (calorias unaPersona) 2,indHidratacion = min (2*indHidratacion unaPersona) 100, equipamiento = ["Colchoneta"]}

--5.3. volverse Body Builder
volverseBodyBuilder :: Persona -> Persona
volverseBodyBuilder unaPersona
    |all (=="Pesa") (equipamiento unaPersona)= unaPersona {nombre = "BB " ++ nombre unaPersona, calorias = calorias unaPersona * 3}
    |otherwise = unaPersona

--5.4. comerUnSandwich
comerUnSandwich :: Persona -> Persona
comerUnSandwich unaPersona = unaPersona {calorias= calorias unaPersona + 500, indHidratacion = 100}


--Parte B

type Rutina = ([Ejercicio],Int)

ejecutarRutina :: Rutina -> Persona -> Persona
ejecutarRutina unaRutina unaPersona = foldl (\acc ejercicio -> ejercicio acc) unaPersona (fst unaRutina)

--1. es peligrosa
esPeligrosa :: Rutina -> Persona -> Bool
esPeligrosa unaRutina = estaAgotada . ejecutarRutina unaRutina
estaAgotada :: Persona -> Bool
estaAgotada unaPersona = ( (<50) . calorias $ unaPersona) && ((<10) . indHidratacion $ unaPersona)

--2. es balanceada
esBalanceada :: Rutina -> Persona -> Bool
esBalanceada unaRutina unaPersona = ( (< div (calorias unaPersona) 2) . calorias . ejecutarRutina unaRutina $ unaPersona) && ((>80) . indHidratacion . ejecutarRutina unaRutina $ unaPersona)

--3.elAbominableAbdominal


--Parte C

--1.seleccionar grupo
seleccionarGrupoDeEjercicio :: Persona -> [Persona] -> [Persona]
seleccionarGrupoDeEjercicio unaPersona listaPersonas = [unaPersona] ++ filter (mismoTiempo unaPersona) listaPersonas
mismoTiempo :: Persona -> Persona -> Bool
mismoTiempo unaPersona otraPersona = tiempo unaPersona == tiempo otraPersona

--2.promedioDeRutina
grupoHaceRutina :: Rutina -> [Persona] -> [Persona]
grupoHaceRutina unaRutina listaPersonas = map (ejecutarRutina unaRutina) listaPersonas
promedioDeRutina :: Rutina -> [Persona] -> (Int,Int)
promedioDeRutina unaRutina listaPersonas = (div (sum . map calorias . grupoHaceRutina unaRutina $ listaPersonas) (length listaPersonas) , div (sum . map indHidratacion . grupoHaceRutina unaRutina $ listaPersonas) (length listaPersonas))

--sum . map . calorias . (grupoHaceRutina unaRutina) $ listaPersonas