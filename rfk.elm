import Window
import Keyboard

--positions really need to be in terms of fixed-width
--characters for this to work properly

--todos:
--actual collision detection
--randomly placed objects
--endgame detection 
--endgame animation
--edge detection (done?)

type Item = { char : String, description: String, xd : Int, yd : Int }
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

collision : Item -> [Item] -> Maybe Item
collision robot items = Just item --TODO: actual collision detection.

getDescription : Maybe Item -> String
getDescription x = case maybe of 
    Maybe p -> p.description
    Nothing -> ""

render : (Int, Int) -> Item -> Element
render (w, h) robot =
  let roboElem = Text.text ( fontify robot.char )
      message = Text.text ( fontify (.description (collision robot items)) )
  in collage w h [
    filled black (rect (toFloat w) (toFloat h))
  , move (nextPoint (robot.xd, robot.yd) (w, h) roboElem) (toForm roboElem)
  , move (nextPoint (robot.xd, robot.yd - 1) (w, h) roboElem) (toForm message) 
  , move (nextPoint (item.xd, item.yd) (w, h) roboElem) (toForm (Text.text (fontify item.char)))
  ] --and add all of the items to this list as well


step : {x:Int, y:Int} -> Item -> Item
step {x, y} ({char, xd, yd} as r) = 
    --disallow movement over another item
    let next_robot = { char = r.char, xd = r.xd + x, yd = r.yd + y, description = r.description}
    in if isJust (collision next_robot items) then robot else next_robot

input : Signal {x:Int, y:Int}
input = --let delta = lift (\t -> t/20) (fps 25)
        --in sampleOn delta (lift2 (,) delta Keyboard.arrows)
        Keyboard.arrows

main =
  lift2 render Window.dimensions (foldp step robot input)

