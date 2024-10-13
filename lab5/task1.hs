import Control.Monad (when)

-- Получение целого числа от пользователя. 
-- Принимает строку, которая будет выведена как подсказка и возвращает введенное число
readInt :: String -> IO Int
readInt description = do
    putStrLn description
    input <- getLine
    return (read input)


main :: IO ()
main = do
    start <- readInt "Введите начальное значение:"
    count <- readInt "Введите количество элементов:"
    multiple <- readInt "Введите кратность:"

    -- Проверяем, чтобы кратность была больше нуля
    when (multiple <= 0) $ error "Кратность должна быть больше нуля"

    -- Проверка на случай, если начальное число не соотвествует указанной кратности
    let first = if start `mod` multiple == 0 then start else start + (multiple - start `mod` multiple)

    print $ take count [first, first + multiple ..]
