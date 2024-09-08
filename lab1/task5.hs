--5. Список пирамидальных чисел Ферма. Колво чисел в списке = 50

-- Функция для вычисления n-го пирамидального числа по формуле n * (n + 1) * (n + 2) / 6
fermatPyramidalNumber :: Int -> Int
fermatPyramidalNumber n = n * (n + 1) * (n + 2) `div` 6

main :: IO ()
main = do
    print [fermatPyramidalNumber n | n <- [1..50]] -- Используя генератор вычисляем чисо Ферма для 1..50