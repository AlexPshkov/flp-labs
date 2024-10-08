module LW4 (myDeleteMap, myDeleteSet, myDrop, myToLower, myMaybe, myTest) where

-- myDeleteMap - Удаляет элемент из Map по ключу
data Map k a = EmptyMap | Node k a (Map k a) (Map k a)
  deriving (Show, Eq)

-- Функция для удаления элемента из бинарного дерева поиска
myDeleteMap :: (Ord k) => k -> Map k a -> Map k a

-- Базовый случай: если дерево пустое, возвращаем пустое дерево
myDeleteMap _ EmptyMap = EmptyMap

-- Основной случай: обрабатываем узел дерева
myDeleteMap key (Node k v left right)
  -- Если искомый ключ меньше ключа текущего узла, рекурсивно удаляем из левого поддерева
  | key < k   = Node k v (myDeleteMap key left) right

  -- Если искомый ключ больше ключа текущего узла, рекурсивно удаляем из правого поддерева
  | key > k   = Node k v left (myDeleteMap key right)

  -- Если ключи совпадают, удаляем текущий узел
  | otherwise = case (left, right) of
      -- Если левое поддерево пустое, возвращаем правое поддерево
      (EmptyMap, _) -> right

      -- Если правое поддерево пустое, возвращаем левое поддерево
      (_, EmptyMap) -> left

      -- Если оба поддерева не пустые, ищем минимальный элемент в правом поддереве
      (_, _) -> let (minKey, minVal) = findMin right
                -- Заменяем текущий узел минимальным элементом и удаляем этот элемент из правого поддерева
                in Node minKey minVal left (myDeleteMap minKey right)
  where
    -- Вспомогательная функция для поиска минимального элемента в дереве
    findMin (Node k v EmptyMap _) = (k, v)  -- Если левое поддерево пустое, это минимальный элемент
    findMin (Node _ _ left _)     = findMin left  -- Идем дальше в левое поддерево







-- myDeleteSet - Удаляет элемент из множества
data Set a = EmptySet                   -- Пустое множество
           | Element a (Set a) (Set a)  -- Узел с элементом и двумя поддеревьями
  deriving (Show, Eq)

-- Функция для удаления элемента из множества
myDeleteSet :: (Ord a) => a -> Set a -> Set a

-- Базовый случай: если множество пустое, возвращаем пустое множество
myDeleteSet _ EmptySet = EmptySet

-- Основной случай: обрабатываем узел дерева
myDeleteSet x (Element e left right)
  -- Если удаляемый элемент меньше текущего, рекурсивно удаляем из левого поддерева
  | x < e     = Element e (myDeleteSet x left) right

  -- Если удаляемый элемент больше текущего, рекурсивно удаляем из правого поддерева
  | x > e     = Element e left (myDeleteSet x right)

  -- Если элемент найден (x == e), обрабатываем удаление
  | otherwise = case (left, right) of
      -- Если левое поддерево пустое, возвращаем правое поддерево
      (EmptySet, _) -> right

      -- Если правое поддерево пустое, возвращаем левое поддерево
      (_, EmptySet) -> left

      -- Если оба поддерева не пустые, находим минимальный элемент в правом поддереве
      (_, _) -> let minElem = findMin right
                -- Заменяем текущий элемент минимальным элементом и удаляем его из правого поддерева
                in Element minElem left (myDeleteSet minElem right)
  where
    -- Вспомогательная функция для поиска минимального элемента в правом поддереве
    findMin (Element e EmptySet _) = e  -- Если левое поддерево пустое, это минимальный элемент
    findMin (Element _ left _)     = findMin left  -- Идем дальше в левое поддерево








-- myDrop - Удаляет первые n элементов из спикса
-- Функция для удаления первых n элементов из списка
myDrop :: Int -> [a] -> [a]

-- Реализация функции myDrop
myDrop n xs
  -- Если n меньше или равно 0, возвращаем исходный список, так как нечего удалять
  | n <= 0    = xs

  -- В противном случае, обрабатываем список
  | otherwise = case xs of
                  -- Если список пустой, возвращаем пустой список
                  []   -> []

                  -- Если список не пустой, пропускаем первый элемент и рекурсивно вызываем myDrop для оставшегося списка
                  _:xs' -> myDrop (n-1) xs'









-- mytoLower - Преобразует символ в нижний регистр
myToLower :: Char -> Char
myToLower c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise            = c










-- myMaybe - Применяет функцию к значению Maybe, если оно есть, иначе возвращает значение по умолчанию
myMaybe :: b -> (a -> b) -> Maybe a -> b

-- Реализация функции myMaybe
myMaybe defaultVal f maybeVal = case maybeVal of
  -- Если значение Nothing, возвращаем значение по умолчанию
  Nothing -> defaultVal
  -- Если значение Just x, применяем функцию f к x и возвращаем результат
  Just x  -> f x










myTest :: (Eq a, Show a) => a -> a -> IO ()
myTest expected actual
  | expected == actual = putStrLn $ "Test passed: " ++ show actual
  | otherwise          = putStrLn $ "Test failed: expected " ++ show expected ++ ", but got " ++ show actual

main :: IO ()
main = do
  let m = Node 1 "a" (Node 0 "z" EmptyMap EmptyMap) (Node 2 "b" EmptyMap EmptyMap)
  myTest (Node 1 "a" (Node 0 "z" EmptyMap EmptyMap) EmptyMap) (myDeleteMap 2 m)
  myTest (Node 2 "b" (Node 0 "z" EmptyMap EmptyMap) EmptyMap) (myDeleteMap 1 m)

  let s = Element 2 (Element 1 EmptySet EmptySet) (Element 3 EmptySet EmptySet)
  myTest (Element 2 (Element 1 EmptySet EmptySet) EmptySet) (myDeleteSet 3 s)
  myTest (Element 3 (Element 1 EmptySet EmptySet) EmptySet) (myDeleteSet 2 s)

  myTest [3, 4] (myDrop 2 [1, 2, 3, 4])
  myTest [] (myDrop 5 [1, 2, 3])
  myTest [1, 2, 3] (myDrop 0 [1, 2, 3])

  myTest 'a' (myToLower 'A')
  myTest 'b' (myToLower 'b')
  myTest '1' (myToLower '1')
  myTest '\n' (myToLower '\n' )  

  myTest 6 (myMaybe 0 (+1) (Just 5))
  myTest 0 (myMaybe 0 (+1) Nothing)