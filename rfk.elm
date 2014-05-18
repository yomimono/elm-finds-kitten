import Window
import Keyboard
import List
import Char
import GameLogic (kittenFound)
import KittenConstants
import InputModel (Input, State, Item, Colliding, allDirectionalInputs)
import ItemRandomizer (randomSymbols, randomLocations, randomColors, randomNKIs, initialSeed)
import Render (render)
import TextField (toCartesianLimits, makeLimits)

robot = { char = "#", xd = 0, yd = 0, 
  description = "Robot, sans kitten.", collidingWith = "", 
  isKitten = False, cd = white }

samePlace : Item a -> Item b -> Bool
samePlace robot item = robot.xd == item.xd && robot.yd == item.yd

collision : Item b -> [Item a] -> Maybe (Item a)
collision robot items = 
  let found = (List.filter (samePlace robot) items )
  in if List.isEmpty found then Nothing else Just (head found)

updatePosition : Item a -> (Int, Int) -> (Int, Int) -> Item a
updatePosition r (x, y) windowDimensions = 
  let (xMin, yMin) = toCartesianLimits <| makeLimits windowDimensions
  in {r | xd <- (clamp ((-1) * xMin) (abs xMin) (r.xd + x))
        , yd <- (clamp ((-1) * yMin) (abs yMin) (r.yd + y))}

--step : Input -> Colliding(Item {}) -> Colliding(Item {})
step : Input -> State -> State
step {controls, randomElements} statusQuo =
--step ({playingField, windowDimensions, items}) ({xd, yd, collidingWith} as r) = 
  let {x, y} = controls.direction 
      player = statusQuo.player 
      playingField = statusQuo.playingField 
      items = statusQuo.items in
  if (not (kittenFound player)) && (x /= 0 || y /= 0) then
    case (collision (updatePosition player (x, y) playingField) items) of
      --Just otherItem -> { statusQuo | player = { player | collidingWith <- otherItem.description } }
      Just otherItem ->
           let newPlayer = { player | collidingWith <- otherItem.description }
           in { statusQuo | player <- newPlayer }
      Nothing -> 
           let newPlayer = updatePosition ( {player | collidingWith <- ""} ) (x, y) playingField
           in { statusQuo | player <- newPlayer }
  else statusQuo

itemsToMake : (Int, Int) -> Int
itemsToMake (x, y) = max 10 (div (x * y) 20000)

--main : Signal Element
main
 =
  let boringState = { player = robot, items = [], playingField =  (10, 10) }
      makeItGo = Input <~ allDirectionalInputs ~ (constant [1,2,3,4])
  --TODO: make the randomized lists separately and pass them here via lift,
  --to avoid creating another level of signaling.
  in render <~ (foldp step boringState makeItGo)
