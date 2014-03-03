module SimpleRandom where

import Random
import Time

type Item a = { a | xd: Int, yd: Int } --2 values to randomize

largeInterval : Time
largeInterval = 1000 * 60 * 60 * 24 * 7 * 365 --update every year (non-leap ;) )

initialSeed : Signal Int 
initialSeed = lift floor (every largeInterval)

makeItem : Int -> Int -> Item {}
makeItem x y = 
  { xd = x, yd = y }

makeSomeRandomItems : Int -> Signal Int -> [Signal (Item {})]
makeSomeRandomItems howMany seed =
  if howMany == 0 then []
  else 
    let x = Random.range 0 200 seed
        y = Random.range 0 200 seed
    in (lift2 makeItem x y) :: (makeSomeRandomItems (howMany - 1) seed)

index : Int -> [a] -> a
index number list = 
  last (take number list)

randomListItem : Signal Int -> [Signal a] -> Signal a
randomListItem gen list =
  if length list == 1 then (head list)
  else 
    let randomIndex = (Random.range 1 (length list) gen)
    in lift2 index randomIndex (combine list)

main =
  let ofInterest = makeSomeRandomItems 10 initialSeed
  in lift asText (randomListItem initialSeed ofInterest)
  --lift asText (constant [1, 2, 3, 4])
  --in lift asText (lift2 (==) (head ofInterest) (last ofInterest))
