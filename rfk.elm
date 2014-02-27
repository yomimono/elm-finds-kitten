import Window
import Keyboard
import List

--positions really need to be in terms of fixed-width
--characters for this to work properly

--todos:
--actual collision detection
--randomly placed objects
--endgame detection 
--endgame animation
--edge detection (done?)

type Item a = { a | char:String, description:String, xd:Int, yd:Int }
type Colliding a = { a | collidingWith: Item {} }
robot = { char = "@", xd = 0, yd = 0, description = "Robot, sans kitten." }
characters = "$%^&*()qwertyuiop[]{}asdfghjkl;:zxcvbnm,.<>"
item = { char = "#", description = "An item.", xd = 2, yd = 2}
items = [ item ]

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

samePlace : Item a-> Item a-> Bool
samePlace robot item = robot.xd == item.xd && robot.yd == item.yd

--Determine the case where the robot is investigating an object.
--The robot shouldn't be able to collide with multiple things simultaneously,
--but if this does occur, we'll just take the first object.
collision : Item a -> [Item a] -> Maybe (Item a)
collision robot items = 
  let found = (List.filter (samePlace robot) items )
  in if List.isEmpty found then Nothing else Just (head found)

getDescription : Maybe (Item a) -> String
getDescription x = case x of 
  Just x -> x.description
  Nothing -> "NOT KITTEN"

render : (Int, Int) -> Item a-> Element
render (w, h) robot =
  let roboElem = Text.text ( fontify robot.char )
      message = Text.text ( fontify (getDescription (collision robot items)))
  in collage w h [
    filled black (rect (toFloat w) (toFloat h))
  , move (nextPoint (robot.xd, robot.yd) (w, h) roboElem) (toForm roboElem)
  , move (nextPoint (robot.xd, robot.yd - 1) (w, h) roboElem) (toForm message) 
  , move (nextPoint (item.xd, item.yd) (w, h) roboElem) (toForm (Text.text (fontify item.char)))
  ] --and add all of the items to this list as well


step : {x:Int, y:Int} -> Item a-> Item a
step {x, y} ({char, xd, yd} as r) = 
    --disallow movement over another item
    let next_robot = { char = r.char, xd = r.xd + x, yd = r.yd + y, description = r.description}
        investigating = collision next_robot items
    in if isJust investigating then { r | collidingWith = investigating } else next_robot

input : Signal {x:Int, y:Int}
input = --let delta = lift (\t -> t/20) (fps 25)
        --in sampleOn delta (lift2 (,) delta Keyboard.arrows)
        Keyboard.arrows

main =
  lift2 render Window.dimensions (foldp step robot input)

