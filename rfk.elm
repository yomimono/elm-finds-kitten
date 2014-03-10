import Window
import Keyboard
import List
import Char
import Generator
import Generator.Standard
import KittenConstants
import ItemRandomizer (Item, makeItems, initialSeed)

type Colliding b = { b | collidingWith: String }
robot = { char = "@", xd = 0, yd = 0, 
  description = "Robot, sans kitten.", collidingWith = "", 
  isKitten = False, cd = white }
  
fontify : Color -> String -> Text
fontify col x = Text.color col ( monospace ( toText x) )

samePlace : Item a -> Item b -> Bool
samePlace robot item = robot.xd == item.xd && robot.yd == item.yd

collision : Item b -> [Item a] -> Maybe (Item a)
collision robot items = 
  let found = (List.filter (samePlace robot) items )
  in if List.isEmpty found then Nothing else Just (head found)

getMessage : Colliding a -> Element
getMessage r = centered (bold (fontify white r.collidingWith))

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
  let (nextX, nextY) = (toFloat ((widthOf roboElem) * x), 
                        toFloat ((heightOf roboElem) * y))
      (w, h) = (toFloat w', toFloat h')
  in 
    if | nextX*2 > w -> (nextX - w, nextY)
       | nextY*2 > h -> (nextX, nextY - h)
       | otherwise -> (nextX, nextY)

render : (Int, Int) -> (Colliding (Item {})) -> [Item a] -> Element
render (w, h) robot items =
  let roboElem = Text.text ( fontify white robot.char )
      (limitX, limitY) = makeLimits (w, h)
      upperLeft = (0, limitY + 1)
  in case kittenFound robot of
    False -> collage w h ( (++) ([
      filled black (rect (toFloat w) (toFloat h))
      , move (nextPoint upperLeft (w, h) roboElem) (toForm (getMessage robot)) 
      , move (nextPoint (robot.xd, robot.yd) (w, h) roboElem) (toForm roboElem)
    ]) (map (drawItemForm roboElem (w,h)) items))
    True -> foundAnimation (w, h) robot

updatePosition : Item a -> (Int, Int) -> (Int, Int) -> Item a
updatePosition r (x, y) windowDimensions = 
  let (xMin, yMin) = makeLimits windowDimensions
  in {r | xd <- (clamp ((-1) * xMin) (abs xMin) (r.xd + x))
        , yd <- (clamp ((-1) * yMin) (abs yMin) (r.yd + y))}

removeCollision : Colliding a -> Colliding a
removeCollision r = { r | collidingWith <- ""}

step : ({x:Int, y:Int}, (Int, Int), [Item {}]) -> Colliding(Item {}) -> Colliding(Item {})
step ({x, y}, windowDimensions,  items) ({xd, yd, collidingWith} as r) = 
  if (not (kittenFound r)) && (x /= 0 || y /= 0) then
    case (collision (updatePosition r (x, y) windowDimensions) items) of
      Just otherItem -> { r | collidingWith <- otherItem.description }
      Nothing -> updatePosition (removeCollision r) (x, y) windowDimensions
  else r

viKeys : Signal { x:Int, y:Int}
viKeys =
  Keyboard.directions (Char.toCode 'K') (Char.toCode 'J') (Char.toCode 'H') (Char.toCode 'L')

input : Signal {x:Int, y:Int}
input = merges [Keyboard.arrows, Keyboard.wasd, viKeys]

charDims : (Int, Int)
charDims = 
  let sampleChar = (Text.text (monospace (toText "@")))
  in (widthOf sampleChar, heightOf sampleChar)

--w and h come as the number of pixels.
--we want to give the largest index in (x,y).
makeLimits : (Int, Int) -> (Int, Int)
makeLimits (w, h) = 
  let (charWidth, charHeight) = charDims
  in ((div (div w charWidth) 2) - 1, (div (div h charHeight) 2) - 1)

itemsToMake : (Int, Int) -> Int
itemsToMake (x, y) = max 10 (div (x * y) 20000)

make3tuple : a -> b -> c -> (a, b, c)
make3tuple x y z = (x, y, z)

main : Signal Element
main
 =
  let items = makeItems <~ initialSeed ~ (lift makeLimits Window.dimensions) ~ (lift itemsToMake Window.dimensions)
  in render <~ Window.dimensions ~ (foldp step robot (lift3 make3tuple input Window.dimensions items)) ~ items