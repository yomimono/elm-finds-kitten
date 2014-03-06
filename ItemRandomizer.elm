module ItemRandomizer where

import Generator
import Generator.Standard
import KittenConstants
import List

type Item a = { a | char:String, description:String, xd:Int, yd:Int, cd:Color, isKitten:Bool }

makeGen : Int -> Generator.Generator Generator.Standard.Standard
makeGen x = Generator.Standard.generator x

largeInterval : Time
largeInterval = 1000 * 60 * 60 * 24 * 7 * 365 --update every year (non-leap ;) )

initialSeed : Signal Int 
initialSeed = lift floor (every largeInterval)

randomListItem : Generator.Generator b -> [a] -> (a, Generator.Generator b)
randomListItem gen list =
  if | length list == 1 -> (head list, gen)
     | otherwise -> 
       let (index, gen') = Generator.int32Range (1, length list) gen
       in (last (take index list), gen')

randomColor : Generator.Generator b -> (Color, Generator.Generator b)
randomColor gen =
  let colorGenerator = Generator.int32Range (0, 255)
      (r, nextGen) = colorGenerator gen
      (g, ds9) = colorGenerator nextGen
      (b, voyager) = colorGenerator ds9
  in (rgb r g b , voyager)

randomizeItem : Generator.Generator a -> (Int, Int) -> String -> (Item {}, Generator.Generator a)
randomizeItem gen (w, h) desc =
  let (xrand, nextGen) = Generator.int32Range (-1 * w, w) gen
      (yrand, ds9) = Generator.int32Range (-1 * h, h) nextGen
      (charColor, voyager) = randomColor ds9
      (representation, enterprise) = randomListItem voyager (String.toList KittenConstants.characters) --randomize symbol
      madeItem = { char = String.fromList [representation], description = desc,
        isKitten = False, xd = xrand, yd = yrand, cd = charColor }
  in (madeItem, enterprise)
itemify : (Generator.Generator a, (Int, Int), [String], [Item {}]) -> (Generator.Generator a, (Int, Int), [String], [Item {}])
itemify (gen, dim, descs, items) =
  if length descs == 0 then (gen, dim, descs, items)
  else
    let (item, gen') = randomizeItem gen dim (head descs)
    in itemify (gen', dim, (tail descs), item :: items)

randomListSubset : ([a], [a], Generator.Generator b, Int) -> ([a], [a],Generator.Generator b, Int) 
randomListSubset (list, random, gen, howManyMore) =
  if (length list < 1 || howManyMore == 0) then ([], random, gen, howManyMore)
  else 
    let (randomElement, gen') = randomListItem gen list
        nextList = List.filter ((/=) randomElement) list
        (_, randomList, gen'', _) = randomListSubset (nextList, random, gen', howManyMore - 1)
    in (nextList, --don't duplicate elements
        randomElement :: randomList, gen', howManyMore - 1)

--pass maximum/minimum to this function
--(should bear some resemblance to the wrapping level, 
--otherwise kitten may be tragically rendered offscreen and unreachable)
makeItems : Int -> (Int, Int) -> Int -> [Item {}]
makeItems p (w, h) numToMake = 
  let (gen', _, _, nonKittenItems) = itemify (makeGen p, (w, h), KittenConstants.rawItemList, [])
      (_, randomizedItems, gen'', _) = randomListSubset (nonKittenItems, [], gen', numToMake)
      (lastGen, _, _, (kitten'::[])) = itemify (gen'', (w, h), KittenConstants.kittenDescription :: [], [])
      kitten = { kitten' | isKitten <- True }
  in kitten :: randomizedItems