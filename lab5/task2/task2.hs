import Data.Char (isPunctuation)

replacePunctuation :: Char -> String -> String
replacePunctuation replacement = map (\c -> if isPunctuation c then replacement else c)

-- Получение строки от пользователя.
-- Принимает строку, которая будет выведена как подсказка и возвращает введенную строка
readString :: String -> IO String
readString description = do
    putStrLn description
    input <- getLine
    return input

-- Получение строки от пользователя.
-- Принимает строку, которая будет выведена как подсказка и возвращает введенную строка
readChar :: String -> IO Char
readChar description = do
    input <- readString description
    return (head input)

main :: IO ()
main = do
  inputFile <- readString "Введите входной файл:"
  outputFile <- readString "Введите выходной файл:"
  replacementChar <- readChar "Введите символ, которым заменить:"

  fileContent <- readFile inputFile

  let outputFileContent = replacePunctuation replacementChar fileContent

  writeFile outputFile outputFileContent