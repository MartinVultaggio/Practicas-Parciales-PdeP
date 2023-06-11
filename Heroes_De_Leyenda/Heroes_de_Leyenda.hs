import Text.Show.Functions ()


--1)
data Heroe = UnHeroe {
    nombre:: String,
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
}deriving Show
type Artefacto = (NombreArtefacto, Rareza)
type NombreArtefacto = String
type Rareza = Int


--2)
pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria unHeroe
    |reconocimiento unHeroe > 1000 = unHeroe {epiteto = "El Mitico"}
    |reconocimiento unHeroe >= 500 && reconocimiento unHeroe <= 1000 = unHeroe {epiteto = "El Magnifico", artefactos = artefactos unHeroe ++ [("Lanza del Olimpo", 100)]}
    |reconocimiento unHeroe >= 100 && reconocimiento unHeroe < 500 = unHeroe {epiteto = "Hoplita", artefactos = artefactos unHeroe ++ [("Xiphos", 50)]}
    |otherwise = unHeroe

type Tarea = Heroe -> Heroe

encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto unArtefacto unHeroe = unHeroe {reconocimiento = reconocimiento unHeroe + snd unArtefacto, artefactos = artefactos unHeroe ++ [unArtefacto], tareas = tareas unHeroe ++ [encontrarArtefacto unArtefacto]}

escalarOlimpo :: Tarea
escalarOlimpo unHeroe = unHeroe {reconocimiento = reconocimiento unHeroe + 500, artefactos = filter ((>=1000).snd) (map (\(nombre,rareza) -> (nombre,rareza*3)) (artefactos unHeroe)),tareas = tareas unHeroe ++ [escalarOlimpo]}

ayudarACruzarNCalles :: Int->Tarea
ayudarACruzarNCalles cuadras unHeroe = unHeroe {epiteto = "Gros" ++ (concat . replicate cuadras $ "o"), tareas = tareas unHeroe ++ [ayudarACruzarNCalles cuadras]}

type Bestia = (String,Heroe->Bool)

matarBestia :: Bestia -> Tarea
matarBestia bestia unHeroe
    |(snd bestia) unHeroe = unHeroe {epiteto = "El Asesino de " ++ (fst bestia), tareas = tareas unHeroe ++ [matarBestia bestia]} 
    |otherwise = unHeroe {epiteto = "El Cobarde", artefactos = tail (artefactos unHeroe)}


--3)

aquiles :: Heroe
aquiles = UnHeroe{
    nombre = "Aquiles",
    epiteto = "Heroe de Troya",
    reconocimiento =1000,
    artefactos = [("Dory",100),("Aspis",100),("Xiphos",250),("Thorax", 400), ("Kraneos", 300) ,("Knemides",200)],
    tareas = []
}

ciclope :: Bestia
ciclope = ("Polifemo",(>100).reconocimiento)

--4)
heracles :: Heroe
heracles = UnHeroe{
    nombre = "Heracles",
    epiteto = "Guardian del Olimpo ",
    reconocimiento =700,
    artefactos = [("Pistola",1000),("Relampago de Zeus",2000)],
    tareas = [matarAlLeonDeNemea]
}

--5)

leonDeNemea :: Bestia
leonDeNemea = ("Leon de Nemea",(>=20).length.epiteto)

matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea unHeroe = matarBestia leonDeNemea unHeroe

--6)ya lo hice

--7)

rarezaTotalDe :: Heroe -> Int
rarezaTotalDe unHeroe = sum . map snd . artefactos $ unHeroe

ejecutarTareas :: [Tarea] -> Heroe -> Heroe
ejecutarTareas listaTareas unHeroe = foldl (\acc tarea -> tarea acc) unHeroe listaTareas

presumir :: Heroe -> Heroe -> (Heroe,Heroe)
presumir unHeroe otroHeroe
    |reconocimiento unHeroe > reconocimiento otroHeroe = (unHeroe,otroHeroe)
    |reconocimiento unHeroe < reconocimiento otroHeroe = (otroHeroe,unHeroe)
    |rarezaTotalDe unHeroe > rarezaTotalDe otroHeroe = (unHeroe,otroHeroe)
    |rarezaTotalDe unHeroe < rarezaTotalDe otroHeroe = (otroHeroe,unHeroe)
    |otherwise = presumir (ejecutarTareas (tareas otroHeroe) unHeroe) (ejecutarTareas (tareas unHeroe) otroHeroe)

--8) No para de evaluar, no devuelve nunca nada pero siempre va a estar entrando recursivamente en si misma.

heroe1 :: Heroe
heroe1 = UnHeroe{
    nombre = "Pedro",
    epiteto = "Soldado de Atenas ",
    reconocimiento =100,
    artefactos = [],
    tareas = []
}

heroe2 :: Heroe
heroe2 = UnHeroe{
    nombre = "Juan",
    epiteto = "Soldado de Esparta ",
    reconocimiento =100,
    artefactos = [],
    tareas = []
}

--9)

type Labor = [Tarea]

tareasDeHeracles :: Labor
tareasDeHeracles = [encontrarArtefacto ("Pegaso",1000),escalarOlimpo,ayudarACruzarNCalles 2, matarAlLeonDeNemea]

--10) No, porque nunca terminara de aplicar tareas, entonces nunca devolvera nada.