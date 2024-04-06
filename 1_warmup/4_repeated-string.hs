solve :: [String] -> Int
solve [s, n] =
    as_in_s * repetitions + (length . filter (=='a') $ take remainder s)
        where as_in_s = length . filter (=='a') $ s
              (repetitions, remainder) = divMod (read n :: Int) $ length s

main :: IO ()
main = interact $ show . solve . lines
    -- where solve [s, n] = length . filter (=='a') . take (read n) $ cycle s

