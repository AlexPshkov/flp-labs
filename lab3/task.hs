-- 1. Она берет численный аргумент n и возвращает список всех чисел от n до 1, включительно.
listnums :: Int -> [Int]
listnums n
  | n == 1 = [1]
  | n > 1 = [n] ++ listnums (n-1)
  | otherwise = []

listnumsTests :: IO()
listnumsTests = do
    putStrLn $ "listnums tests:"
    print $ listnums 5 -- вернет [5,4,3,2,1]
    print $ listnums 1 -- вернет [1]
    print $ listnums 0 -- вернет []
    print $ listnums (-1) -- вернет []
    putStrLn $ ""



-- 2. Эта функция берет список  списков и возвращает последние элементы каждого, объединенные  в список.
secondlastlist :: [[a]] -> [a]
secondlastlist [] = []
secondlastlist (firstList:anotherLists)
  | null firstList = secondlastlist anotherLists
  | otherwise = last firstList : secondlastlist anotherLists

secondlastlistTests :: IO()
secondlastlistTests = do
    putStrLn $ "secondlastlist tests:"
    print $ secondlastlist [[1,2,3], [4,5], [6]] -- вернет [3, 5, 6]
    print $ secondlastlist [[10,20], [30], [40,50]] -- вернет [20, 30, 50]
    print $ secondlastlist [['a'], [], ['b']] -- вернет ['a', 'b']
    putStrLn $ ""



-- 3. Функция находит объединение двух  списков. 
-- Объединением двух списков будет список содержащий элементы, которые есть по крайней мере в одном из списков.
myunion :: Eq a => [a] -> [a] -> [a]
myunion firstList [] = firstList
myunion firstList (x:secondList)
  | x `elem` firstList = myunion firstList secondList
  | otherwise = myunion (firstList ++ [x]) secondList

myunionTests :: IO()
myunionTests = do
    putStrLn $ "myunion tests:"
    print $ myunion [1, 2, 3] [2, 3, 4] -- вернет [1, 2, 3, 4]
    print $ myunion [5, 6] [] -- вернет [5, 6]
    print $ myunion [] [7, 8, 9] -- вернет [7, 8, 9]
    print $ myunion ([] :: [Int]) [] -- вернет []
    putStrLn $ ""



-- 4. Получив два списка, она возвращает их разность. 
-- Разность двух списков называется список, состоящий из элементов  первого списка, которые не принадлежат второму списку.
mysubst :: Eq a => [a] -> [a] -> [a]
mysubst [] _ = []
mysubst (x:firstList) secondList
  | x `elem` secondList = mysubst firstList secondList
  | otherwise = x : mysubst firstList secondList

mysubstTests :: IO()
mysubstTests = do
    putStrLn $ "mysubst tests:"
    print $ mysubst [1, 2, 3] [2, 3, 4] -- вернет [1]
    print $ mysubst [5, 6, 7] [5, 8, 9] -- вернет [6, 7]
    print $ mysubst [] [1, 2, 3] -- вернет []
    print $ mysubst [1, 2, 3] [] -- вернет [1, 2, 3]
    putStrLn $ ""



-- 5. Функция, берущая список списков и возвращающая список из N -х элементов подсписков с помощью функций map и (!!)
nposlist :: [[a]] -> Int -> [a]
nposlist lists n
  | n >= 0 = map (\list -> list !! n) (filter (\list -> length list > n) lists)
  | otherwise = []


nposlistTests :: IO()
nposlistTests = do
    putStrLn $ "nposlist tests:"
    print $ nposlist [[1, 2, 3], [4, 5, 6], [7, 8]] 2 -- вернет [3, 6]
    print $ nposlist [[10, 20], [30], [40, 50]] 1 -- вернет [20, 50]
    print $ nposlist [['a'], ['b', 'c']] 0 -- вернет ['a', 'b']
    print $ nposlist [[1, 2], [3, 4, 5]] (-1) -- вернет []
    print $ nposlist ([[]] :: [[Int]]) 0 -- вернет []
    putStrLn $ ""


main :: IO() 
main = do
    listnumsTests
    secondlastlistTests
    myunionTests
    mysubstTests
    nposlistTests