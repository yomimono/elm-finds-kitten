import Window
import Keyboard

--positions really need to be in terms of fixed-width
--characters for this to work properly

--todos:
--actual collision detection
--randomly placed objects
--endgame detection 
--endgame animation
--edge detection

type Item = { char : String, description: String, xd : Int, yd : Int }
robot = { char = "@", xd = 0, yd = 0, description = "Robot, sans kitten." }
item = { char = "#", description = "An item.", xd = 2, yd = 2}

nextPoint : (Int, Int) -> (Int, Int) -> Element -> (Float, Float)
nextPoint (x, y) (w', h') roboElem =
  let (nextX, nextY) = (toFloat ((widthOf roboElem) * x), toFloat ((heightOf roboElem) * y))
      (w, h) = (toFloat w', toFloat h')
  in 
    if | nextX*2 > w -> (nextX - w, nextY)
       | nextY*2 > h -> (nextX, nextY - h)
       | otherwise -> (nextX, nextY)
    
  
fontify : Text -> Text
fontify x = Text.color white ( monospace x )

collision : Item -> [Item] -> Item
collision robot items = item

render : (Int, Int) -> Item -> Element
render (w, h) robot =
  let roboElem = Text.text ( fontify ( toText robot.char ))
      message = Text.text ( fontify ( toText (.description (collision robot [item])) ) )
  in collage w h [
    filled black (rect (toFloat w) (toFloat h))
  , move (nextPoint (robot.xd, robot.yd) (w, h) roboElem) (toForm roboElem)
  , move (nextPoint (robot.xd, robot.yd - 1) (w, h) roboElem) (toForm message) 
  ] --and add all of the items to this list as well


step : {x:Int, y:Int} -> Item -> Item
step {x, y} ({char, xd, yd} as r) = { char = r.char, xd = r.xd + x, yd = r.yd + y, description = r.description}

input : Signal {x:Int, y:Int}
input = --let delta = lift (\t -> t/20) (fps 25)
        --in sampleOn delta (lift2 (,) delta Keyboard.arrows)
        Keyboard.arrows

main =
  lift2 render Window.dimensions (foldp step robot input)
