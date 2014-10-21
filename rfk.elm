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

-- move the simulation forward one turn by returning a modified statusQuo
step : Input -> State -> State
step {controls, playingField, randomElements} statusQuo =
  let {x, y} = controls.direction 
      player = statusQuo.player 
      -- the number of non-kitten items is generated each turn based on window size.
      -- changing the window size results in a different number (and distribution) of items.
      howManyItems = itemsToMake playingField
      -- the list of random items is also regenerated each step; the only element of
      -- generation likely to change between turns is the window size.
      items = generateItems playingField KittenConstants.rawItemList randomElements howManyItems
      screenChange = (/=) playingField statusQuo.playingField
  in
  --only update state on movement or window size change
  if (not (kittenFound player)) && ((x /= 0 || y /= 0) || screenChange) then 
    let obligatoryChanges = 
            { statusQuo | playingField <- playingField, 
                          actionTaken <- True,
                          items <- items } in
    -- robot will be updated differently depending on whether robot is 
    -- touching something.  if robot is touching an item,
    -- update robot's collidingWith field to reflect the item's description,
    -- so the renderer can display the description (or the victory screen).
    case (collision (updatePosition player (x, y) playingField) items) of
      Just otherItem -> -- robot is touching an item!
           let newPlayer = { player | collidingWith <- otherItem.description }
           in { obligatoryChanges | player <- newPlayer }
      Nothing -> -- robot is not touching anything.
           let newPlayer = updatePosition ( {player | collidingWith <- ""} ) (x, y) playingField
           in { obligatoryChanges | player <- newPlayer }
  else { statusQuo | playingField <- playingField } -- if no movement, no change

main : Signal Element
main =
  let boringState = { -- this state will never be seen by the player
          actionTaken = False, 
          player = robot, 
          items = [], 
          playingField = (800, 600)
  } 
      -- 2000 items is OK, 3000 gives a too much recursion error
      -- this means the number of randomized 
      -- non-kitten items must be limited to (2000 - 6)/6,
      -- since we need 6 random numbers per non-kitten item
      -- 332 non-kitten items ought to be enough for anyone
      howManyRandom = (KittenConstants.maxItems * 9) + 7
      makeItGo = 
              Input <~ 
              allDirectionalInputs ~ 
              Window.dimensions ~ 
              combine (makeNRandomInts howManyRandom initialSeed)
  in render <~ (foldp step boringState makeItGo)
