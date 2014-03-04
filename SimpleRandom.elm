module SimpleRandom where

import Random (range)
import Time 
import List 

type Item a = { a | char:String, 
     xd:Int, 
     yd:Int, 
     cd:Color, 
     isKitten:Bool }

type Described a = { a | description : String}

largeInterval : Time
largeInterval = 1000 * 60 * 60 * 24 * 7 * 365 --update every year (non-leap ;) )

initialSeed : Signal Int 
initialSeed = lift floor (every largeInterval)

characters : [Char]
characters = (String.toList "$%^&*()qwertyuiop[]{}asdfghjkl;:zxcvbnm,.<>")

stringifySingleChar : Char -> String
stringifySingleChar x = String.fromList [x]

randomColor : Signal Int -> Signal Color
randomColor seed =
  let colorGenerator = range 0 255 seed
  in lift3 rgb colorGenerator colorGenerator colorGenerator

index : [a] -> Int -> a
index list number = 
  last (take number list)

randomListItem : Signal Int -> [a] -> Signal a
randomListItem seed list =
  if length list == 1 then constant (head list)
  else 
    let randomIndex = (range 1 (length list) seed)
    in lift (index list) randomIndex

insert : a -> [a] -> Int -> [a]
insert item list index =
  if | length list < index -> list ++ [item]
     | index <= 0 -> item :: list
     | otherwise -> (take index list) ++ (item :: (drop index list))

randomInsert : Signal Int -> a -> Signal [a] -> Signal [a]
randomInsert seed thing list =
  let randomIndex = (range 1 (length list) seed)
  in (insert thing) <~ list ~ randomIndex

makeItem : Int -> Int -> Color -> String -> Item {}
makeItem x y col repr = 
  { xd = x, yd = y, cd = col, char = repr, 
    isKitten = False }

--make some number of Items based on the random number generator.
makeSomeRandomItems : Int -> Signal Int -> [Signal (Item {})]
makeSomeRandomItems howMany seed =
  if howMany == 0 then []
  else 
    let x = range 0 200 seed
        y = range 0 200 seed
        col = randomColor seed
        char = lift stringifySingleChar (randomListItem seed characters)
    in (makeItem <~ x ~ y ~ col ~ char)
     :: (makeSomeRandomItems (howMany - 1) seed)

--randomize the order of the items in list, based on input from signal int.
randomizeList : ([a], [Signal a], Signal Int) -> ([a], [Signal a], Signal Int) 
randomizeList (list, random, seed) =
  if | length list < 1 -> ([], random, seed)
     | length random < 1 -> (tail list, [constant (head list)], seed)
     | otherwise -> 
       let nextElement = head list
           nextList = tail list
       in (nextList, randomInsert seed nextElement random, seed)