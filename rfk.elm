import Window
import Keyboard
import List
import Char
import Generator
import Generator.Standard
import KittenConstants

type Item a = { a | char:String, description:String, xd:Int, yd:Int, cd:Color, isKitten:Bool }
type Colliding b = { b | collidingWith: String }
robot = { char = "@", xd = 0, yd = 0, 
  description = "Robot, sans kitten.", collidingWith = "", 
  isKitten = False, cd = white }
  
fontify : Color -> String -> Text
fontify col x = Text.color col ( monospace ( toText x) )

samePlace : Item a -> Item b -> Bool
samePlace robot item = robot.xd == item.xd && robot.yd == item.yd

--Determine the case where the robot is investigating an object.
--The robot shouldn't be able to collide with multiple things simultaneously,
--but if this does occur, we'll just take the first object.
collision : Item b -> [Item a] -> Maybe (Item a)
collision robot items = 
  let found = (List.filter (samePlace robot) items )
  in if List.isEmpty found then Nothing else Just (head found)

--convenience function for looking up what message ought to be displayed
--based on whether our robot's just investigated something and hasn't yet 
--moved away.
getMessage : Colliding a -> Element
getMessage r = Text.text (fontify white r.collidingWith)

kittenFound : Colliding a -> Bool
kittenFound r = r.collidingWith == KittenConstants.kittenDescription

foundAnimation : (Int, Int) -> Colliding (Item {}) -> Element
foundAnimation (w,h) robot = 
    collage w h [
      filled black (rect (toFloat w) (toFloat h))
      , toForm (flow down [
        (flow right [ 
          KittenConstants.drawRobot, 
          KittenConstants.drawHeart, 
          KittenConstants.drawKitten 
        ]),
        getMessage robot] )
    ]

drawItemForm : Element -> (Int, Int) -> Item a -> Form
drawItemForm roboElem (w, h) item = 
    move (nextPoint (item.xd, item.yd) (w, h) roboElem) 
         (toForm (Text.text (fontify item.cd item.char)))


nextPoint : (Int, Int) -> (Int, Int) -> Element -> (Float, Float)
nextPoint (x, y) (w', h') roboElem =
  let (nextX, nextY) = (toFloat ((widthOf roboElem) * x), toFloat ((heightOf roboElem) * y))
      (w, h) = (toFloat w', toFloat h')
  in 
    if | nextX*2 > w -> (nextX - w, nextY)
       | nextY*2 > h -> (nextX, nextY - h)
       | otherwise -> (nextX, nextY)

render : (Int, Int) -> (Colliding (Item {})) -> [Item a] -> Element
render (w, h) robot items =
  let roboElem = Text.text ( fontify white robot.char )
  in case kittenFound robot of
    False -> collage w h ( (++) ([
      filled black (rect (toFloat w) (toFloat h))
      , move (nextPoint (robot.xd, robot.yd - 1) (w, h) roboElem) (toForm (getMessage robot)) 
      , move (nextPoint (robot.xd, robot.yd) (w, h) roboElem) (toForm roboElem)
    ]) (map (drawItemForm roboElem (w,h)) items))
    True -> foundAnimation (w, h) robot

updatePosition : Item a -> (Int, Int) -> Item a
updatePosition r (x, y) = {r | xd <- r.xd + x, yd <- r.yd + y}

removeCollision : Colliding a -> Colliding a
removeCollision r = { r | collidingWith <- ""}

step : ({x:Int, y:Int}, [Item {}]) -> Colliding(Item {}) -> Colliding(Item {})
step ({x, y}, items) ({xd, yd, collidingWith} as r) = 
  if (not (kittenFound r)) && (x /= 0 || y /= 0) then
    case (collision (updatePosition r (x, y)) items) of
      Just otherItem -> { r | collidingWith <- otherItem.description }
      Nothing -> updatePosition (removeCollision r) (x, y)
  else r

viKeys : Signal { x:Int, y:Int}
viKeys =
  Keyboard.directions (Char.toCode 'K') (Char.toCode 'J') (Char.toCode 'H') (Char.toCode 'L')

input : Signal {x:Int, y:Int}
input = --let delta = lift (\t -> t/20) (fps 25)
        --in sampleOn delta (lift2 (,) delta Keyboard.arrows)
        merges [Keyboard.arrows, Keyboard.wasd, viKeys]

makeGen : Int -> Generator.Generator Generator.Standard.Standard
makeGen x = Generator.Standard.generator x

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
    --in (gen', dim, (tail descs), item :: items)

randomListSubset : ([a], [a], Generator.Generator b, Int) -> ([a], [a],Generator.Generator b, Int) 
randomListSubset (list, random, gen, howManyMore) =
  if (length list < 1 || howManyMore == 0) then ([], random, gen, howManyMore)
  else 
    let (randomElement, gen') = randomListItem gen list
        nextList = List.filter ((/=) randomElement) list
        (_, randomList, gen'', _) = randomListSubset (nextList, random, gen', howManyMore - 1)
    in (nextList, --don't duplicate elements
        randomElement :: randomList, gen', howManyMore - 1)

makeLimits : (Int, Int) -> (Int, Int)
makeLimits (w, h) = --w and h come as the number of pixels.
  --we want to give the largest index in (x,y).
    (10, 10)

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

largeInterval : Time
largeInterval = 1000 * 60 * 60 * 24 * 7 * 365 --update every year (non-leap ;) )

initialSeed : Signal Int 
initialSeed = lift floor (every largeInterval)

main
 =
  let items = makeItems <~ initialSeed ~ (lift makeLimits Window.dimensions) ~ (constant 25)
  in render <~ Window.dimensions ~ (foldp step robot (lift2 (,) input items)) ~ items