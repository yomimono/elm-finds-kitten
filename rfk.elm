import Window
import Keyboard
import List

--positions really need to be in terms of fixed-width
--characters for this to work properly

--todos:
--actual collision detection (done)
--randomly placed objects
--endgame detection 
--endgame animation
--edge detection (done?)

type Item a = { a | char:String, description:String, xd:Int, yd:Int, isKitten:Bool }
type Colliding b = { b | collidingWith: String }
robot = { char = "@", xd = 0, yd = 0, 
  description = "Robot, sans kitten.", collidingWith = "", isKitten = False }
characters = "$%^&*()qwertyuiop[]{}asdfghjkl;:zxcvbnm,.<>"
kittenDescription = "You found kitten!  Good job, robot!"
items = [ { char = "#", description = kittenDescription,
            isKitten = True, xd = 2, yd = 2} ]

nextPoint : (Int, Int) -> (Int, Int) -> Element -> (Float, Float)
nextPoint (x, y) (w', h') roboElem =
  let (nextX, nextY) = (toFloat ((widthOf roboElem) * x), toFloat ((heightOf roboElem) * y))
      (w, h) = (toFloat w', toFloat h')
  in 
    if | nextX*2 > w -> (nextX - w, nextY)
       | nextY*2 > h -> (nextX, nextY - h)
       | otherwise -> (nextX, nextY)
    
  
fontify : String -> Text
fontify x = Text.color white ( monospace ( toText x) )

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
--moved away.23
getMessage : Colliding a -> Element
getMessage r = Text.text (fontify r.collidingWith)

kittenFound : Colliding a -> Bool
kittenFound r = r.collidingWith == kittenDescription

drawRobot : Element
drawRobot = Text.text (Text.color gray (monospace ( toText "[-]  \n(+)=C\n| |\n000")))

drawHeart : Element
drawHeart = Text.text (Text.color red (monospace (toText ".::. .::.\n:::::::::\n \':::::\'\n  \':::\'")))

drawKitten : Element
drawKitten = Text.text (Text.color orange (monospace 
  (toText (String.append " |\\_/|\n" " |0 0|__\n, =-*-=  \\\nc_c__(___)"))))

foundAnimation : (Int, Int) -> Colliding (Item {}) -> Element
foundAnimation (w,h) robot = 
    collage w h [
      filled black (rect (toFloat w) (toFloat h))
      , toForm drawRobot
      , toForm drawHeart
      , toForm drawKitten
    ]

render : (Int, Int) -> Colliding (Item {}) -> Element
render (w, h) robot =
  let roboElem = Text.text ( fontify robot.char )
  in case kittenFound robot of
    False -> collage w h [
      filled black (rect (toFloat w) (toFloat h))
      , move (nextPoint (robot.xd, robot.yd) (w, h) roboElem) (toForm roboElem)
      , move (nextPoint (robot.xd, robot.yd - 1) (w, h) roboElem) (toForm (getMessage robot)) 
      , move (nextPoint ((head items).xd, (head items).yd) (w, h) roboElem) (toForm (Text.text (fontify (head items).char)))
    ] --and add all of the items to this list as well
    True -> foundAnimation (w, h) robot

updatePosition : Item a -> (Int, Int) -> Item a
updatePosition r (x, y) = {r | xd <- r.xd + x, yd <- r.yd + y}

removeCollision : Colliding a -> Colliding a
removeCollision r = { r | collidingWith <- "" }

step : {x:Int, y:Int} -> Colliding(Item {}) -> Colliding(Item {})
step {x, y} ({xd, yd, collidingWith} as r) = 
  if x /= 0 || y /= 0 then
    case (collision (updatePosition r (x, y)) items) of
      Just otherItem -> { r | collidingWith <- otherItem.description }
      Nothing -> updatePosition (removeCollision r) (x, y)
  else r

input : Signal {x:Int, y:Int}
input = --let delta = lift (\t -> t/20) (fps 25)
        --in sampleOn delta (lift2 (,) delta Keyboard.arrows)
        Keyboard.arrows

main =
  lift2 render Window.dimensions (foldp step robot input)

