import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Data.Maybe (mapMaybe)

-- Definición del tipo de datos para representar la información de un Libro
data Libro = Libro {
    nombre  :: String,
    codigo  :: String,
    entrada :: UTCTime,       -- Fecha de registro en biblioteca
    salida  :: Maybe UTCTime, -- Fecha/hora de préstamo (Nothing si está disponible)
    estado  :: String         -- "Disponible" o "Prestado"
} deriving (Show, Read)

-- Función auxiliar para rellenar con ceros a la izquierda
padLeft :: Char -> Int -> String -> String
padLeft ch n str = replicate (n - length str) ch ++ str

-- Registrar un nuevo libro (Check In inicial en la biblioteca)
registrarEntrada :: String -> UTCTime -> [Libro] -> [Libro]
registrarEntrada nombreLibro tiempo biblioteca =
    let nuevoCodigo = "L" ++ padLeft '0' 3 (show (length biblioteca + 1))
    in Libro nombreLibro nuevoCodigo tiempo Nothing "Disponible" : biblioteca

-- Registrar préstamo (Check Out)
prestarLibro :: String -> UTCTime -> [Libro] -> ([Libro], String)
prestarLibro codigoLibro tiempo biblioteca =
    case find (\v -> codigo v == codigoLibro) biblioteca of
        Nothing -> (biblioteca, "Libro no encontrado.")
        Just libro ->
            if estado libro == "Prestado"
            then (biblioteca, "El libro ya fue prestado.")
            else (map (\v -> if codigoLibro == codigo v
                             then v { salida = Just tiempo, estado = "Prestado" }
                             else v) biblioteca,
                  "Libro '" ++ nombre libro ++ "' con código " ++ codigoLibro ++ " ha sido prestado.")

-- Registrar devolución (Check In de préstamo)
devolverLibro :: String -> [Libro] -> ([Libro], String)
devolverLibro codigoLibro biblioteca =
    case find (\v -> codigo v == codigoLibro) biblioteca of
        Nothing -> (biblioteca, "Libro no encontrado.")
        Just libro ->
            if estado libro == "Disponible"
            then (biblioteca, "El libro ya estaba disponible, no se puede devolver.")
            else (map (\v -> if codigoLibro == codigo v
                             then v { salida = Nothing, estado = "Disponible" }
                             else v) biblioteca,
                  "Libro '" ++ nombre libro ++ "' con código " ++ codigoLibro ++ " ha sido devuelto.")

-- Calcular la duración del préstamo en MINUTOS
duracionPrestamo :: Libro -> UTCTime -> Int
duracionPrestamo libro tiempoActual =
    case salida libro of
        Just tiempoPrestamo -> floor (diffUTCTime tiempoActual tiempoPrestamo / 60)
        Nothing -> 0

-- Guardar la información en archivo
guardarBiblioteca :: [Libro] -> IO ()
guardarBiblioteca biblioteca =
    writeFile "biblioteca.txt" (unlines (map show biblioteca))
    >> putStrLn "Biblioteca guardada en el archivo biblioteca.txt."

-- Cargar la información desde archivo de manera segura
cargarBiblioteca :: IO [Libro]
cargarBiblioteca = do
    resultado <- try (readFile "biblioteca.txt") :: IO (Either IOException String)
    case resultado of
        Left _ -> return []  -- Si no existe el archivo, empezamos con lista vacía
        Right contenido -> do
            let lineas = filter (not . null) (lines contenido)
            return (mapMaybe leerSeguro lineas)
  where
    leerSeguro :: String -> Maybe Libro
    leerSeguro linea =
        case reads linea of
            [(libro, "")] -> Just libro
            _             -> Nothing

-- Lista de libros prestados
listarPrestados :: [Libro] -> IO ()
listarPrestados biblioteca = do
    let prestados = filter (\v -> estado v == "Prestado") biblioteca
    if null prestados
        then putStrLn "No hay libros prestados."
        else mapM_ (\v -> putStrLn $ "Nombre: " ++ nombre v
                                 ++ ", Código: " ++ codigo v
                                 ++ ", Prestado desde: " ++ show (salida v)) prestados

-- Ciclo principal del programa
cicloPrincipal :: [Libro] -> IO ()
cicloPrincipal biblioteca = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar un Libro"
    putStrLn "2. Registrar Préstamo"
    putStrLn "3. Registrar Devolución"
    putStrLn "4. Buscar libro por código"
    putStrLn "5. Lista de libros prestados"
    putStrLn "6. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el nombre del Libro:"
            nombreLibro <- getLine
            tiempoActual <- getCurrentTime
            let bibliotecaActualizada = registrarEntrada nombreLibro tiempoActual biblioteca
                nuevoLibro = head bibliotecaActualizada
            putStrLn $ "Libro '" ++ nombre nuevoLibro ++ "' registrado con código " ++ codigo nuevoLibro ++ "."
            guardarBiblioteca bibliotecaActualizada
            cicloPrincipal bibliotecaActualizada

        "2" -> do
            putStrLn "Ingrese el código del libro a prestar:"
            codigoLibro <- getLine
            tiempoActual <- getCurrentTime
            let (bibliotecaActualizada, mensaje) = prestarLibro codigoLibro tiempoActual biblioteca
            putStrLn mensaje
            guardarBiblioteca bibliotecaActualizada
            cicloPrincipal bibliotecaActualizada

        "3" -> do
            putStrLn "Ingrese el código del libro a devolver:"
            codigoLibro <- getLine
            let (bibliotecaActualizada, mensaje) = devolverLibro codigoLibro biblioteca
            putStrLn mensaje
            guardarBiblioteca bibliotecaActualizada
            cicloPrincipal bibliotecaActualizada

        "4" -> do
            putStrLn "Ingrese el código del libro a buscar:"
            codigoLibro <- getLine
            case find (\v -> codigo v == codigoLibro) biblioteca of
                Just libro -> do
                    putStrLn $ "Nombre: " ++ nombre libro
                    putStrLn $ "Código: " ++ codigo libro
                    putStrLn $ "Estado: " ++ estado libro
                    if estado libro == "Prestado"
                        then do
                            tiempoActual <- getCurrentTime
                            let duracion = duracionPrestamo libro tiempoActual
                            putStrLn $ "Tiempo en préstamo: " ++ show duracion ++ " minutos."
                        else return ()
                Nothing -> putStrLn "Libro no encontrado en la biblioteca."
            cicloPrincipal biblioteca

        "5" -> do
            putStrLn "Lista de libros prestados:"
            listarPrestados biblioteca
            cicloPrincipal biblioteca

        "6" -> putStrLn "Hasta luego."

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal biblioteca

-- Función principal del programa
main :: IO ()
main = do
    biblioteca <- cargarBiblioteca
    putStrLn "Bienvenido al Sistema de Gestión de la Biblioteca."
    cicloPrincipal biblioteca
