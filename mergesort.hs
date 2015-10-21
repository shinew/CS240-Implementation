merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = 
  if x <= y
  then x : merge xs (y:ys)
  else y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let
    len = length xs
    first = take (len `div` 2) xs
    second = drop (len `div` 2) xs
  in
    merge (mergeSort first) (mergeSort second)
