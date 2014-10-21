module ItemRandomizer where

import GameLogic (collision)
import InputModel (GamePiece, Item)
import TextField 
import KittenConstants
import String (fromList)

-- a number to use as a seed for a random number generator.
-- using a larger time interval than the game is likely to take 
-- (or a browser instance is likely to survive) provides the 
-- illusion that items can't change location, color, and symbol
-- right in front of the player.
largeInterval : Time
largeInterval = 1000 * 60 * 60 * 24 * 7 * 365 --update every year (non-leap ;) )

-- another possibility for seeding the RNG.
-- using an interval this small means the items will change
-- random attributes every two seconds, making for a challenging 
-- game indeed!
smallInterval : Time
smallInterval = 2

-- process the signal so it can be used as an RNG seed.
initialSeed : Signal Int 
initialSeed = lift floor (every largeInterval)

-- remove the element with the index given from the list;
-- return the element in the index and the list with the 
-- element excised.
removeIndex : Int -> [a] -> (a, [a])
removeIndex index list =
  let firstChunk = reverse (take index list)
      lastChunk = drop index list
      item = head firstChunk
      remaining = reverse (tail firstChunk)
  in
  (item, (remaining ++ lastChunk))

-- given some random numbers and bounds on possible locations,
-- generate locations, colors, and descriptions for items.
generateCommonAttributes : (Int, Int) -> [Int] -> GamePiece {}
generateCommonAttributes (w, h) randomInts =
  let (randX::randY::randR::randG::randB::randSymbolIndex::others) = randomInts
      (columnLimit, rowLimit) = 
              TextField.toCartesianLimits <| 
              TextField.makeLimits (w, h)
      -- negatives are OK for location, hence the unusual use of `rem`
      location = (randX `rem` columnLimit, randY `rem` rowLimit) 
      col = rgb (randR % 256) (randG % 256) (randB % 256)
      (symbol, _) = removeIndex ((randSymbolIndex % (length KittenConstants.characters)) + 1) KittenConstants.characters
  in
  { char = fromList [symbol], cd = col, xd = fst location, yd = snd location }

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
