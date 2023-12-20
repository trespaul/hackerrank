import Data.List
import Data.Function

main :: IO ()
main = interact $ show . solve . parse
    where parse = last . words
          solve = length                       -- count valleys
                . filter (==(-1))
                . map head                   -- take first element (always 1/0/-1)
                . groupBy ((==) `on` signum) -- groups with same sign (-/+)
                . scanl1 (+)                 -- change delta into abs elevation
                . map (\x -> if x == 'U' then 1 else (-1) )

{-- More straightforward solution is to just search for every time a
    valley starts or ends, i.e., when the elevation goes 0 -> -1 or -1 -> 0:
    length . filter (== True) . mapAdjacent (\x y -> x == 0 && y == (-1))
--}
