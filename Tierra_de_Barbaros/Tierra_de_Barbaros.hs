import Text.Show.Functions ()
import Data.Char (toUpper,isUpper)
import Data.List (intersect)


--Punto 1:

data Barbaro = UnBarbaro{
    nombre :: String,
    fuerza :: Int,
    habilidades :: [Habilidad],
    objetos :: [Objeto]
} deriving Show

type Habilidad = String
type Objeto = Barbaro -> Barbaro

--1.
espada :: Int -> Objeto
espada peso unBarbaro = unBarbaro {fuerza = fuerza unBarbaro + 2*peso}

--2.
amuletoMistico :: Habilidad -> Objeto
amuletoMistico unaHabilidad unBarbaro = unBarbaro {habilidades = habilidades unBarbaro ++ [unaHabilidad]}

--3.
varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = unBarbaro {habilidades = ["Hacer magia"]}

--4.

ardilla :: Objeto
ardilla unBarbaro = unBarbaro

--5.
cuerda :: Objeto
cuerda unBarbaro = unBarbaro {objetos = [(head.(drop 2). objetos $ unBarbaro).(last.(drop 2). objetos $ unBarbaro)] ++ ((drop 2). objetos $ unBarbaro)}--agarra los primeeros 2 objetos, los compone, y le suma el resto de la lista


--Punto 2:

megafono :: Objeto
megafono unBarbaro = unBarbaro {habilidades = [map toUpper.concat.habilidades $ unBarbaro]}

megafonoBarbarico:: Objeto
megafonoBarbarico = megafono.cuerda.ardilla


--Punto 3:
type Aventura = [Evento]
type Evento = Barbaro -> Bool

sabe :: Barbaro -> Habilidad -> Bool
sabe unBarbaro unaHabilidad = elem unaHabilidad . habilidades $ unBarbaro

seLlama :: Barbaro -> String -> Bool
seLlama unBarbaro unNombre = (==unNombre).nombre $ unBarbaro

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes unBarbaro = sabe unBarbaro "Escribir Poesía Atroz"

cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = seLlama unBarbaro "Faffy" || seLlama unBarbaro "Astro"

saqueo :: Evento
saqueo unBarbaro = (elem "Robar" . habilidades $ unBarbaro) && ((>=80).fuerza $ unBarbaro)
gritoDeGuerra :: Evento
gritoDeGuerra unBarbaro = ( (*4) . length . objetos $ unBarbaro) == (sum . map length.habilidades $ unBarbaro)
vocales :: [Char]
vocales = "aAeEiIoOuU"
caligrafia :: Evento
caligrafia unBarbaro = (all (>=3) . map length . map (intersect vocales) . habilidades $ unBarbaro) && (all isUpper . map head . habilidades $ unBarbaro)
ritualDeFechorias :: Evento
ritualDeFechorias unBarbaro = saqueo unBarbaro && gritoDeGuerra unBarbaro && caligrafia unBarbaro

capituloN :: Aventura
capituloN = [invasionDeSuciosDuendes,cremalleraDelTiempo,ritualDeFechorias]

sobrevive :: Barbaro -> Aventura -> Bool
sobrevive unBarbaro unaAventura = and . map (\evento -> evento unBarbaro) $ unaAventura

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes listaDeBarbaros unaAventura = filter (flip sobrevive unaAventura) listaDeBarbaros


--Punto 4:

--A:

sinRepetidos :: [Habilidad] -> [Habilidad]
sinRepetidos [] = []
sinRepetidos (habilidad:sgteHabilidad)
    | elem habilidad sgteHabilidad = sinRepetidos sgteHabilidad -- si esta presente mas adelante, lo "suelta"
    | otherwise =  [habilidad] ++ sinRepetidos sgteHabilidad -- si no esta presente, se lo trae consigo

--B:

usarObjetos :: Barbaro -> Barbaro
usarObjetos unBarbaro = foldl (\acc objeto -> objeto acc) unBarbaro (objetos unBarbaro)

actualizarGeneracion :: Barbaro -> Barbaro
actualizarGeneracion unBarbaro = unBarbaro {nombre = nombre unBarbaro ++ "*", habilidades = sinRepetidos . habilidades $ unBarbaro}

avanzarGeneracion :: Barbaro -> Barbaro
avanzarGeneracion = actualizarGeneracion . usarObjetos

descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = tail . iterate avanzarGeneracion $ unBarbaro

--C: Sobre la lista de objetos no puesto que la lista de objetos es sobre funciones, y sobre el nombre de un barbaron tampoco, puesto que la funcion admite listas de strings y el nombre es solo un String.