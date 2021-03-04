collatz :: Integer -> Integer -> Integer
collatz n iterations 
        | n == 1 = iterations
        | even n = collatz(n `div` 2)(iterations + 1)
        | otherwise = collatz(3*n + 1)(iterations + 1)

findMaxCollatz :: Integer -> [Integer]
findMaxCollatz n = findMaxCollatz' n 1 []
        where
            findMaxCollatz' n' a list
                | a <= n' = findMaxCollatz' n' (a + 1) (list ++ [collatz a 1])
                | otherwise = list
        

maxIndex ::  [Integer] -> Integer
maxIndex list = snd . maximum $ zip list [1 .. ]

main :: IO ()
main = do 
        print "Po podaniu liczby naturalnej n program wypisuje, dla jakiej liczby naturalnej m ≤ n zaczyna się najdłuższy ciąg Collatza."
        print "Podaj n: "
        n <- readLn
        print (maxIndex(findMaxCollatz(n :: Integer)))

