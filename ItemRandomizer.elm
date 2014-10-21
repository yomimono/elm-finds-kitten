module ItemRandomizer where

import GameLogic (collision)
import InputModel (GamePiece, Item)
import TextField 
import KittenConstants

type CommonProperties a = {
  a | char:String, xd:Int, yd:Int, cd:Color
}

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

generateCommonAttributes : (Int, Int) -> [Int] -> GamePiece {}
generateCommonAttributes (w, h) randomInts =
  let (randX::randY::randR::randG::randB::randSymbolIndex::others) = randomInts
      (columnLimit, rowLimit) = TextField.toCartesianLimits <| TextField.makeLimits (w, h)
      location = (randX `rem` columnLimit, randY `rem` rowLimit) --negatives are OK for this value only.
      col = rgb (randR % 256) (randG % 256) (randB % 256)
      (symbol, _) = removeIndex ((randSymbolIndex % (length KittenConstants.characters)) + 1) KittenConstants.characters
  in
  { char = (show symbol), cd = col, xd = fst location, yd = snd location }

makeKitten : (Int, Int) -> [Int] -> Item (GamePiece {})
makeKitten (w, h) randomInts =
  let baseRecord = generateCommonAttributes (w, h) randomInts 
      kittened = { baseRecord | isKitten = True}
  in
  { kittened | description = KittenConstants.kittenDescription }

avoidCollisions : GamePiece a -> [GamePiece b] -> GamePiece a
avoidCollisions newItem existingItems =
  case (collision newItem existingItems) of
      Just x -> avoidCollisions { newItem | xd <- (//) (newItem.xd * -1) 2} existingItems
      Nothing -> newItem

generateItem : (Int, Int) -> [String] -> [Int] -> ([String], Item (GamePiece {}))
generateItem (w, h) descs randomInts =
  let baseRecord = generateCommonAttributes (w, h) randomInts 
      randDescIndex = head (drop 6 randomInts)
      (description, unusedDescs) = removeIndex ((randDescIndex % (length descs)) + 1) descs
      notKitten = { baseRecord | isKitten = False }
  in
  (unusedDescs, { notKitten | description = description })

generateItems : (Int, Int) -> [String] -> [Int] -> Int -> [Item (GamePiece {})]
generateItems (w, h) descs randomInts howMany =
  if howMany <= 0 || (length descs == 0) then [makeKitten (w, h) randomInts]
  else
    let (unusedDescs, item) = generateItem (w, h) descs randomInts 
        otherItems = (generateItems (w, h) unusedDescs (drop 7 randomInts) (howMany - 1)) in
    (avoidCollisions item otherItems) :: otherItems
