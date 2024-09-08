--3. Список нечётных натуральных чисел.  Количество чисел в списке = 20. (не менее 3 разных способа)

oddNumbersRecursive :: Int -> [Int]
oddNumbersRecursive 0 = [] -- Если 0, то возвращаем пустой список
oddNumbersRecursive n = 1 : map (+2) (oddNumbersRecursive (n - 1)) -- Через конс оператор добавляем каждый раз 1 в начало списка, а остальные значения списка увеличиваем на два

oddNumbersFilter :: [Int]
oddNumbersFilter = filter odd [1..]  -- Фильтрация нечётных чисел из списка натуральных чисел

oddNumbersIterator :: [Int]
oddNumbersIterator = iterate (+2) 1  -- Начинаем с 1 и добавляем 2 для получения следующего нечётного числа

main :: IO ()
main = do
    print (oddNumbersRecursive 20)
    print (take 20 oddNumbersFilter)
    print (take 20 oddNumbersIterator)