import qualified Data.Matrix as M

solve :: M.Matrix Int -> Int
solve = maximum . map hourglassSum . hourglasses
    where hourglasses m =
              [ M.toLists . threeByThree (i, j) $ m
              | i <- [1..4], j <- [1..4]
              ]
          threeByThree (i, j) = M.submatrix i (i + 2) j (j + 2)
          hourglassSum [[a, b, c], [_, d, _], [e, f, g]] =
              sum [a, b, c, d, e, f, g]
          hourglassSum _ = 0

main :: IO ()
main = interact $ show . solve . parse
    where parse = M.fromLists
                . map (map (read :: String -> Int) . words)
                . lines
