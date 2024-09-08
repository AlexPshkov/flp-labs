--4. Список треугольных чисел Ферма. Колво чисел в списке = 50.

-- Функция для вычисления n-го треугольного числа по формуле n * (n + 1) / 2.
fermatTriangleNumber :: Int -> Int
fermatTriangleNumber n = n * (n + 1) `div` 2

main :: IO ()
main = do
    print [fermatTriangleNumber n | n <- [1..50]] -- Используя генератор вычисляем чисо Ферма для 1..50