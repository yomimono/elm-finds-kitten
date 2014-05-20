import Window
import Keyboard
import List
import Char
import GameLogic (collision, itemsToMake, kittenFound)
import KittenConstants
import InputModel (Input, State, GamePiece, Colliding, allDirectionalInputs, makeNRandomInts)
import ItemRandomizer (generateItems, initialSeed)
import Render (render)
import TextField (toCartesianLimits, makeLimits)

robot = { char = "#", xd = 0, yd = 0, 
  collidingWith = "", 
  cd = gray }

updatePosition : GamePiece a -> (Int, Int) -> (Int, Int) -> GamePiece a
updatePosition r (x, y) windowDimensions = 
  let (xMin, yMin) = toCartesianLimits <| makeLimits windowDimensions
  in {r | xd <- (clamp ((-1) * xMin) (abs xMin) (r.xd + x))
        , yd <- (clamp ((-1) * yMin) (abs yMin) (r.yd + y))}

step : Input -> State -> State
step {controls, playingField, randomElements} statusQuo =
  let {x, y} = controls.direction 
      player = statusQuo.player 
      howManyItems = itemsToMake playingField
      items = generateItems playingField KittenConstants.rawItemList randomElements howManyItems
  in
  if (not (kittenFound player)) && (x /= 0 || y /= 0) then
    let obligatoryChanges = { statusQuo | playingField <- playingField, actionTaken <- True, items <- items } in
    case (collision (updatePosition player (x, y) playingField) items) of
      Just otherItem ->
           let newPlayer = { player | collidingWith <- otherItem.description }
           in { obligatoryChanges | player <- newPlayer }
      Nothing -> 
           let newPlayer = updatePosition ( {player | collidingWith <- ""} ) (x, y) playingField
           in { obligatoryChanges | player <- newPlayer }
  else { statusQuo | playingField <- playingField }

main : Signal Element
main =
  let boringState = {actionTaken = False, player = robot, items = [], playingField = (800, 600)}
      makeItGo = Input <~ allDirectionalInputs ~ Window.dimensions ~ combine (makeNRandomInts ((length KittenConstants.rawItemList) * 9 + 7) initialSeed)
  in render <~ (foldp step boringState makeItGo)
