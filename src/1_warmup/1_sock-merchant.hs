import Data.List

main :: IO ()
main = interact $ show . solve . parse
    where parse = map (read :: String -> Int) . tail . words
          solve = sum . map ((`div` 2) . length) . group . sort
