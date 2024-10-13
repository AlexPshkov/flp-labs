import System.IO
import Data.Char (toUpper, toLower)

-- Получение целого числа от пользователя. 
-- Принимает строку, которая будет выведена как подсказка и возвращает введенное число
readInt :: String -> IO Int
readInt description = do
    putStrLn description
    input <- getLine
    return (read input)

-- Получение строки от пользователя.
-- Принимает строку, которая будет выведена как подсказка и возвращает введенную строка
readString :: String -> IO String
readString description = do
    putStrLn description
    input <- getLine
    return input


-- Функция для отображения содержимого файла
viewFile :: FilePath -> IO ()
viewFile filePath = do
    content <- readFile filePath
    putStrLn "Содержимое файла:"
    putStrLn content


-- Функция для добавления новой строки в файл
addLineToFile :: FilePath -> IO ()
addLineToFile filePath = do
    newLine <- readString "Введите строку для добавления:"
    appendFile filePath ("\n" ++ newLine)
    putStrLn "Строка добавлена."

deleteLineFromFile :: FilePath -> IO ()
deleteLineFromFile filePath = do
    -- Открываем файл для чтения
    withFile filePath ReadMode $ \handle -> do
        lineNumber <- readInt "Введите номер строки для удаления:"

        content <- hGetContents handle
        let linesContent = lines content
        let newContent = unlines $ deleteElementAt (lineNumber - 1) linesContent

        -- Открываем файл для записи
        withFile filePath WriteMode $ \writeHandle -> do
            hPutStr writeHandle newContent
    putStrLn "Строка удалена."

-- Вспомогательная функция для удаления элемента по индексу
deleteElementAt :: Int -> [a] -> [a]
deleteElementAt idx xs = let (ys, zs) = splitAt idx xs in ys ++ drop 1 zs

-- Функция для копирования с фильтрацией
copyWithFilter :: FilePath -> FilePath -> IO ()
copyWithFilter inputFile outputFile = do
    content <- readFile inputFile
    filterMode <- readInt "Выберите фильтр: 1 - В верхний регистр, 2 - В нижний регистр"
    let filteredContent = case filterMode of
            1 -> map toUpper content
            2 -> map toLower content
            _   -> content
    writeFile outputFile filteredContent
    putStrLn "Копирование завершено."


main :: IO ()
main = do
    inputFile <- readString "Введите входной файл:"

    mode <- readInt "Выберите действие: 1 - Просмотр, 2 - Добавление, 3 - Удаление, 4 - Копирование с фильтрацией"

    case mode of
        1 -> viewFile inputFile
        2 -> addLineToFile inputFile
        3 -> deleteLineFromFile inputFile
        4 -> do
            outputFile <- readString "Введите имя выходного файла:"
            copyWithFilter inputFile outputFile
        _ -> putStrLn "Нет такого. Выберите из существующих"