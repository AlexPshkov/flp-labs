module CustomFunctions where 

-- 1. oddEven(L) -  функция перестановки местами соседних элементов списка L  (Например, список [2,5,7,9,1,8]  после преобразования будет иметь вид [5,2,9,7,8,1])
-- 
-- Описание сигнатуры:
-- 	Вход: Параметром является список элементов типа a. 
--  Выход: Результатом функции будет список элементов с тем же типом a
-- 
-- Например:
-- 	oddEven [1,2,3,4]  вернет [2,1,4,3]
-- 	oddEven [1]  вернет [1]
-- 	oddEven ["a","b","c"]  вернет ["b", "a", "c"] Последний элемент остается без изменений
oddEven :: [a]-> [a]
oddEven [] = []
oddEven [first] = [first]
oddEven (first:second:another) = second : first : oddEven another  -- Через шаблон списка меняем местами два первых элемента в списке и т.д через рекурсию

oddEvenTests :: IO()
oddEvenTests = do
    putStrLn $ "OddEven tests:"
    print $ oddEven [1,2,3,4] -- вернет [2,1,4,3]
    print $ oddEven [1] -- вернет [1]
    print $ oddEven ["a","b","c"] -- вернет ["b", "a", "c"]
    putStrLn $ ""

-- 2. insert (L,A,n) -  функция включения в список L заданного атома А на определенную позицию n.
-- 
-- Описание сигнатуры:
-- 	Вход: Функция принимает три параметра:
-- 		1. Первым параметром фукнции является список из элементов типа a
-- 		2. Вторым параметром является новый элемент типа a, который нужно вставить. 
-- 		3. Третим и последним параметром является целое число, являющееся позицией элемента, который нужно вставить (куда вставить). 
--  Выход: Результатом функции является список элементов типа a 
-- 
-- Например:
-- 	insert ["a","b","c"],"d",1  вернет ["a","d","b","c"]
-- 	insert [True,False],False,0  вернет [False,True,False]
-- 	insert ["world"], "hello", 0  вернет ["hello","world"]
-- 	insert ["test"], "large", 99  вернет ["test","large"]
insert :: [a] -> a -> Int -> [a]
insert l a n = take n l ++ [a] ++ drop n l

insertTests :: IO()
insertTests = do
    putStrLn $ "Insert tests:"
    print $ insert ["a","b","c"] "d" 1 -- вернет ["a","d","b","c"]
    print $ insert [True,False] False 0 -- вернет [False,True,False]
    print $ insert ["world"] "hello" 0 -- вернет ["hello","world"]
    print $ insert ["test"] "large" 99 -- вернет ["test","large"]
    putStrLn $ ""

-- 3. listSumm(L1,L2) -  функция сложения элементов двух списков. Возвращает список , составленный из сумм элементов списков - параметров L1, L2. 
-- Учесть, что переданные списки могут быть разной длины.
-- 
-- Описание сигнатуры:
--  Вход: Функция принимает два параметра:
--  	1. Первый список целых чисел
-- 		2. Второй список целых чисел
--  Выход: Результатом функции будет новый массив, являющийся суммой двух изначальных. 
--  	Сложение списков происходит через сложение элементов на одинаковых позициях в обоих списках. 
--      Если длины списков разные, то элементы оставшиеся без пары складываются с 0 
-- 
-- Например:
-- 	listSumm [1,2], [3,4] вернет [4,6]
-- 	listSumm [1,2] [3,4,5] вернет [4,6,5]
-- 	listSumm [1,2,3] [4,5] вернет [5,7,3]
listSumm :: [Int] -> [Int] -> [Int]
listSumm [] secondList = secondList
listSumm firstList [] = firstList
listSumm (x:firstList) (y:secondList) = (x + y) : listSumm firstList secondList  -- Через шаблон списков получаем первые элементы скписков и складываем их. Остальные элементы через рекурсию обабатываем так же 

listTests :: IO()
listTests = do
    putStrLn $ "List tests:"
    print $ listSumm [1,2] [3,4] -- вернет [4,6]
    print $ listSumm [1,2] [3,4,5] -- вернет [4,6,5]
    print $ listSumm [1,2,3] [4,5] -- вернет [5,7,3]
    putStrLn $ ""

-- 4. position (L,A)- возвращает номер первого вхождения заданного атома А в список L.
-- 
-- Описание сигнатуры:
--  Вход: Функция принимает два параметра:
--  	1. Первый параметр список элементов типа a
-- 		2. Второй параметр элемент типа a, который нужно найти.
--      Важным уточнением является то, что элементы должно быть такого типа, который можно сравнивать на равенство и неравенство
--  Выход: Результатом функции будет целое число, обозначающее позицию искомого элемента в списке. 
-- 		Если элемент не был найден, то вернется -1 
-- 
-- Например:
-- 	position ['a','b','c','d'], 'b' вернет 1
-- 	position ['a','b','c','d'], 'd' вернет 3
-- 	position [1,2,3,4], 1  вернет 0
-- 	position [1,2,3,4], 10  вернет -1
-- 
-- Описание работы:  
--   Через генератор пробегаемся по всем позициям списка и через индексацию проверяем не равен ли элемент на этой позиции искомому. 
--   Если равен, то записываем его реальную позицию, если нет, то длину массива (как самое большое число). 
--   В итоге чтобы найти номер первого вхождения атома A в список L нужно будет лишь найти минимальное значение из полученного списка.
position :: Eq a => [a] -> a -> Int
position list element = if (element `elem` list)
  then minimum [ if (list !! (i) == element) then i else length list | i <- [0..(length list)-1] ] 
  else -1

positionTests :: IO()
positionTests = do
    putStrLn $ "Postion tests:"
    print $ position ['a','b','c','d'] 'b' -- вернет 1
    print $ position ['a','b','c','d'] 'd' -- вернет 3
    print $ position [1,2,3,4] 1 -- вернет 0
    print $ position [1,2,3,4] 10  -- вернет -1
    putStrLn $ ""



-- 5. F(n)=∑i=1n (i)
-- 
-- Описание сигнатуры:
--   Вход: Функция принимает целое число, до которого нужно вычислить сумму (от 1 до этого числа).
--   Выход: Результатом функции будет целое число, являющееся суммой всех чисел (от 1 до введенного числа)
-- 
-- Например:
-- 	sumNumbersFirst(3) вернет 6, так как 1 + 2 + 3 = 6
-- 	sumNumbersFirst(4) вернет 10, так как 1 + 2 + 3 + 4 = 10
-- 	sumNumbersFirst(5) вернет 15, так как 1 + 2 + 3 + 4 + 5 = 15
sumNumbersFirst :: Int -> Int
sumNumbersFirst n = sum [1..n]

sumNumbersFirstTests :: IO()
sumNumbersFirstTests = do
    putStrLn $ "SumNumbersFirst tests:"
    print $ sumNumbersFirst 3 -- вернет 6
    print $ sumNumbersFirst 4 -- вернет 10
    print $ sumNumbersFirst 5 -- вернет 15
    putStrLn $ ""


-- 6. F(n)=∑i=1n(n−i)
-- 
-- Описание сигнатуры:
--   Вход: Функция принимает целое число, до которого нужно вычислить сумму элементов по формуле (n - i).
--   Выход: Результатом функции будет число, являющееся суммой всех чисел от 1 до n, вычесленных по формуле (n - i)
-- 
-- Например:
-- 	sumNumbersSecond(2) вернет 1, так как (2-1) + (2-2) = 1
-- 	sumNumbersSecond(3) вернет 3, так как (3-1) + (3-2) + (3-3) = 3
-- 	sumNumbersSecond(4) вернет 6, так как (4-1) + (4-2) + (4-3) + (4-4) = 5
sumNumbersSecond :: Int -> Int
sumNumbersSecond n = sum [n - i | i <- [1..n]]

sumNumbersSecondTests :: IO()
sumNumbersSecondTests = do
    putStrLn $ "SumNumbersSecond tests:"
    print $ sumNumbersSecond 2 -- вернет 1
    print $ sumNumbersSecond 3 -- вернет 3
    print $ sumNumbersSecond 4 -- вернет 6
    putStrLn $ ""


main :: IO() 
main = do
    oddEvenTests
    insertTests
    listTests
    positionTests
    sumNumbersFirstTests
    sumNumbersSecondTests