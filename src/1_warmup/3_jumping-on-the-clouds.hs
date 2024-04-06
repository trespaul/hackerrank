solve :: [Int] -> Int
solve [0, 0] = 1
solve [0, _, 0] = 1
solve (a:b:c:xs)
    | c == 0    = 1 + solve (c:xs)
    | otherwise = 1 + solve (b:c:xs)

main :: IO ()
main = interact $ show . solve . parse
    where parse = map (read :: String -> Int)
                . words . last . lines
