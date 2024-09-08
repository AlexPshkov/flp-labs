--2. Используя функции head и tail  получить элемент b из следующих списков
--    1) ['a', 'b', 'c']
--    2) [['a', 'b'], ['c','d']]
--    3) [['a', 'c', 'd'], ['a','b']]
--    4) [['a','d'], ['b', 'c']]

getB1 :: [Char] -> Char
getB1 list = head (tail list)

getB2 :: [[Char]] -> Char
getB2 list = head (tail (head list))

getB3 :: [[Char]] -> Char
getB3 list = head (tail (head (tail list)))

getB4 :: [[Char]] -> Char
getB4 list = head (head (tail list))

main :: IO ()
main = do
    putStrLn $ "1) " ++ [getB1 ['a', 'b', 'c']]
    putStrLn $ "2) " ++ [getB2 [['a', 'b'], ['c', 'd']]]
    putStrLn $ "3) " ++ [getB3 [['a', 'c', 'd'], ['a', 'b']]]
    putStrLn $ "4) " ++ [getB4 [['a', 'd'], ['b', 'c']]]