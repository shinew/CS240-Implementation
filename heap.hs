{-# LANGUAGE BangPatterns #-}

import qualified Data.Vector as V

type Heap a = V.Vector a

swap :: Int -> Int -> V.Vector a -> V.Vector a
swap i j vec =
  let 
    a = vec V.! i
    b = vec V.! j
  in
    vec V.// [(i, b), (j, a)]

parent :: Int -> Int
parent n = (n - 1) `div` 2

leftChild :: Int -> Int
leftChild n = 2 * n + 1

rightChild :: Int -> Int
rightChild n = 2 * n + 2

bubbleUp :: Ord a => Int -> Heap a -> Heap a
bubbleUp 0 h = h
bubbleUp !i !h =
  if h V.! parent i >= h V.! i
  then h
  else bubbleUp (parent i) (swap (parent i) i h)

bubbleDown :: Ord a => Int -> Heap a -> Heap a
bubbleDown !i !h
  | i >= V.length h = h
  | current >= left && current >= right = h
  | left >= right = swapBubble i (leftChild i) h
  | otherwise = swapBubble i (rightChild i) h
  where
    current = h V.! i
    childValue v = if (v >= V.length h) then h V.! i else h V.! v
    left = childValue $ leftChild i
    right = childValue $ rightChild i
    swapBubble a b h2 = bubbleDown b (swap a b h2)

-- O(log n)
push :: Ord a => a -> Heap a -> Heap a
push v h =
  let
    newHeap = V.snoc h v
  in
    bubbleUp (V.length newHeap - 1) newHeap

-- O(1)
getMax :: Ord a => Heap a -> Maybe a
getMax h =
  if V.null h
  then Nothing
  else Just (V.head h)

-- O(log n)
pop :: Ord a => Heap a -> Heap a
pop h =
  if V.null h
  then h
  else
    let
      short = V.length h - 1
      swapped = swap 0 short h
      reduced = V.take short swapped
    in
      bubbleDown 0 reduced

-- O(n)
heapify :: Ord a => [a] -> Heap a
heapify xs =
  let
    start = V.fromList xs
    n = V.length start - 1
    indices = reverse $ take (n `div` 2 + 1) [0..]
  in
    foldl (\h i -> bubbleDown i h) start indices
