main :: IO ()
main = interact $ show . solve . parse
    where parse = id
          solve = id
