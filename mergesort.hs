-- Theta(m+n)
merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = 
  if x <= y
  then x : merge xs (y:ys)
  else y : merge (x:xs) ys

-- Theta(n log n)
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let
    halfLen = ((`div` 2) . length) xs
    front = take halfLen xs
    back = drop halfLen xs
  in
    merge (mergeSort front) (mergeSort back)

main :: IO ()
main = putStrLn $ show $ mergeSort [2, 3, 4, 1, 5]
