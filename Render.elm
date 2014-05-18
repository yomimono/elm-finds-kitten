module Render where

import Window
import List
import Char
import GameLogic (kittenFound)
import InputModel (Item, State)
import KittenConstants 
import TextField (toCartesianLimits, makeLimits)

fontify : Color -> String -> Text
fontify col x = Text.color col <| monospace <| toText x

nextPoint : (Int, Int) -> (Int, Int) -> Element -> (Float, Float)
nextPoint (x, y) (w', h') roboElem =
  let (nextX, nextY) = (toFloat ((widthOf roboElem) * x), 
                        toFloat ((heightOf roboElem) * y))
      (w, h) = (toFloat w', toFloat h')
  in 
    if | nextX*2 > w -> (nextX - w, nextY)
       | nextY*2 > h -> (nextX, nextY - h)
       | otherwise -> (nextX, nextY)

drawItemForm : (Int, Int) -> Item a -> Form
drawItemForm (w, h) item = 
    let element = Text.centered <| fontify item.cd item.char in
    move (nextPoint (item.xd, item.yd) (w, h) element) (toForm element)

getMessage : String -> Element
getMessage r = centered (bold (fontify white r))

drawRobot : Element
drawRobot = Text.centered (
     (fontify darkBlue "[-]   \n(") ++ (fontify darkRed "+") ++
     (fontify darkBlue ")") ++ 
     (fontify darkGreen "=C ") ++ 
     (fontify darkBlue "\n| | \n") ++ 
     (fontify gray "000 ")
   )

drawHeart : Element
drawHeart = Text.centered (
   (fontify red ".::. .::.\n") ++ 
   (fontify red ":::::::::\n") ++ 
   (fontify red " \':::::\'\n") ++ 
   (fontify red "  \':::\'")
  )

drawKitten : Element
drawKitten = Text.centered (
      (fontify orange " |\\_/|\n |") ++ 
      (fontify green "0 0") ++ 
      (fontify orange "|___\n ") ++ 
      (fontify white "=-") ++ (fontify lightRed "*") ++ 
      (fontify white "-=") ++ 
      (fontify orange "   \\\nc_c__(____)")
    )

foundAnimation : (Int, Int) -> Element
foundAnimation (w, h) =
   collage w h [
     filled black <| rect (toFloat w) (toFloat h)
     , toForm (flow down [
        (flow right [
          drawRobot,
          drawHeart,
          drawKitten
        ]),
        getMessage KittenConstants.kittenDescription])
     ]

--render : (Int, Int) -> (Colliding (Item {})) -> [Item a] -> Element
render : State -> Element
render {playingField, player, items} = 
  let (w, h) = playingField
      robot = player
      roboElem = Text.centered ( fontify white robot.char )
      (limitX, limitY) = toCartesianLimits <| makeLimits playingField
      upperLeft = (0, limitY + 3)
      message = move (nextPoint upperLeft playingField roboElem) (toForm (getMessage robot.collidingWith)) 
  in case kittenFound robot of
    False -> collage w h ( (++) ([
      filled black (rect (toFloat w) (toFloat h))
      , message
      , drawItemForm playingField robot
    ]) (map (drawItemForm playingField) items))
    True -> foundAnimation playingField
