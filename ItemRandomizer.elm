module ItemRandomizer where

import InputModel (Item)
import KittenConstants
import Random
import List

largeInterval : Time
largeInterval = 1000 * 60 * 60 * 24 * 7 * 365 --update every year (non-leap ;) )

initialSeed : Signal Int 
initialSeed = lift floor (every largeInterval)

removeIndex : Int -> [a] -> (a, [a])
removeIndex index list =
  let firstChunk = reverse (take index list)
      lastChunk = drop index list
      item = head firstChunk
      remaining = reverse (tail firstChunk)
  in
  (item, (remaining ++ lastChunk))

reorderList : [Int] -> [a] -> [a]
reorderList numbers sourceList = --numbers because indices is a stdlib function
  if | length sourceList == 0 -> []
     | length sourceList == 1 -> sourceList
     | otherwise ->
        let (item, moreThings) = removeIndex (head numbers) sourceList in
        item :: (reorderList (tail numbers) moreThings)

--given a source of randomness and a list of length k, 
--generate k possible indices, 
--for which the first ranges from 1 to k,
--the second ranges from 1 to k - 1,
--...
--and the last ranges from 1 to (k - (k - 1)), i.e. 1.
randomIndices : Signal Int -> Int -> [Signal Int]
randomIndices gen upperBound =
  if | upperBound <= 0 -> []
     | upperBound == 1 -> [(constant 1)]
     | otherwise ->
       (Random.range 1 upperBound gen) :: (randomIndices gen (upperBound - 1))

randomizeList : Signal Int -> [a] -> Signal [a]
randomizeList gen list =
  --generate a list of indices for the list
  let numbers = combine (randomIndices gen (length list)) in
  reorderList <~ numbers ~ (constant list)

randomColor : Signal Int -> Signal Color
randomColor gen =
  let colorGenerator = Random.range 0 255 gen
  in (rgb <~ (colorGenerator) ~ (colorGenerator) ~ (colorGenerator))

randomLocation : Signal Int -> (Int, Int) -> Signal (Int, Int)
randomLocation gen (w, h) = 
  let xrand = Random.range (-1 * w) w gen
      yrand = Random.range (-1 * h) h gen
      pair = (,) <~ xrand ~ yrand
  in pair

makeNKI : ((Int, Int), Color, Char, String) -> Item {}
makeNKI ((xcoord, ycoord), col, symbol, desc) =
  { char = String.fromList [symbol], description = desc, xd = xcoord, yd = ycoord, cd = col, isKitten = False }

combineN : Int -> [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
combineN n a b c d =
  if | n == 0 -> []
     | otherwise ->
     (head a, head b, head c, head d) :: combineN (n - 1) (tail a) (tail b) (tail c) (tail d)

randomSymbols : Int -> Signal Int -> Signal [Char]
randomSymbols numToMake seed =
  let availableSymbols = foldl (++) [] (repeat ((div numToMake (length KittenConstants.characters)) + 1) KittenConstants.characters) in
  take <~ constant (numToMake + 1) ~ (randomizeList seed availableSymbols)

randomLocations : Signal Int -> (Int, Int) -> Signal Int -> Signal [(Int, Int)]
randomLocations numToMake dim seed = 
  repeat <~ ((+) <~ numToMake ~ (constant 1)) ~ (randomLocation seed dim)

randomColors : Signal Int -> Signal Int -> Signal [Color]
randomColors numToMake seed =
  repeat <~ ((+) <~ numToMake ~ (constant 1)) ~ (randomColor seed)

randomNKIs : Signal Int -> Signal Int -> Signal [String]
randomNKIs numToMake seed = 
  take <~ numToMake ~ (randomizeList seed KittenConstants.rawItemList)
