module ItemRandomizer where

import InputModel (Item)
import TextField 
import KittenConstants

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

generateItem : (Int, Int) -> [String] -> [Int] -> ([String], Item{})
generateItem (w, h) descs randomInts =
  let (randDescIndex::randX::randY::randR::randG::randB::randSymbolIndex::others) = randomInts
      (description, unusedDescs) = removeIndex ((randDescIndex `mod` (length descs)) + 1) descs
      (columnLimit, rowLimit) = TextField.toCartesianLimits <| TextField.makeLimits (w, h)
      location = (randX `rem` columnLimit, randY `rem` rowLimit) --negatives are OK for this value only.
      col = rgb (randR `mod` 256) (randG `mod` 256) (randB `mod` 256)
      (symbol, _) = removeIndex ((randSymbolIndex `mod` (length KittenConstants.characters)) + 1) KittenConstants.characters
  in
  (unusedDescs, { description = description, char = String.fromList [symbol], cd = col, xd = fst location, yd = snd location, isKitten = False } )

generateItems : (Int, Int) -> [String] -> [Int] -> Int -> [Item {}]
generateItems (w, h) descs randomInts howMany =
  if howMany <= 0 || (length descs == 0) || (length randomInts < 8) then []
  else
    let (unusedDescs, item) = generateItem (w, h) descs randomInts in
    item :: (generateItems (w, h) unusedDescs (drop 8 randomInts) (howMany - 1))
