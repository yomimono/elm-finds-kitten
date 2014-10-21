module Render where

import Window
import List
import Char
import Text
import GameLogic (kittenFound)
import InputModel (GamePiece, State)
import KittenConstants (programName, programVersion, repoLink, repoString, 
       rfkLink, rfkString, kittenDescription, instructions, refreshMe)
import TextField (toCartesianLimits, makeLimits)

fontify : Color -> String -> Text
fontify col x = 
        Text.color col ( monospace ( toText x ) )

-- given an (x,y) position and the window dimensions, 
-- along with an element to draw,
-- return the correct arguments to move to draw it in the right place
nextPoint : (Int, Int) -> (Int, Int) -> Element -> (Float, Float)
nextPoint (x, y) (w', h') char =
  let (nextX, nextY) = (toFloat ((widthOf char) * x), 
                        toFloat ((heightOf char) * y))
      (w, h) = (toFloat w', toFloat h')
  in 
    --refuse to draw anything off the edge of the screen
    if | nextX*2 > w -> (nextX - w, nextY) 
       | nextY*2 > h -> (nextX, nextY - h)
       | otherwise -> (nextX, nextY)

-- for any given gamepiece, draw it within the window bounds
drawItemForm : (Int, Int) -> GamePiece a -> Form
drawItemForm (w, h) item = 
    let element = centered <| fontify item.cd item.char in
    move (nextPoint (item.xd, item.yd) (w, h) element) (toForm element)

-- set up message text to read nicely
getMessage : String -> Element
getMessage r = centered (bold (fontify white r))

drawRobot : Element
drawRobot = leftAligned (
     (fontify darkBlue "[-] \n(") ++ 
     (fontify darkRed "+") ++
     (fontify darkBlue ")") ++ 
     (fontify darkGreen "=C \n") ++ 
     (fontify darkBlue "| | \n") ++ 
     (fontify gray "000 ")
   )

drawHeart : Element
drawHeart = centered (
   (fontify red ".::. .::.\n") ++ 
   (fontify red ":::::::::\n") ++ 
   (fontify red "\':::::\'\n") ++ 
   (fontify red "\':::\'")
  )

-- trust me, it's a kitten
drawKitten : Element
drawKitten = leftAligned(
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
        getMessage kittenDescription,
        centered (fontify grey refreshMe)])
     ]

-- intro screen is entirely static text & compliant with RFK RFC
showIntroScreen : Element
showIntroScreen = 
      flow down [
          leftAligned (fontify black (programName ++ " " ++ programVersion))
          , link repoLink (leftAligned <| fontify blue repoString)
          , link rfkLink (leftAligned <| fontify blue rfkString)
          , leftAligned (fontify black instructions)
      ]

render : State -> Element
render { actionTaken, playingField, player, items } = 
  if actionTaken == False then showIntroScreen else  
  let (w, h) = playingField
      robot = player
      roboElem = centered ( fontify white robot.char )
      (limitX, limitY) = toCartesianLimits <| makeLimits playingField
      upperLeft = (0, limitY + 2)
      message = move (nextPoint upperLeft playingField roboElem) (toForm (getMessage robot.collidingWith)) 
  in case kittenFound robot of
    False -> collage w h ( (++) ([
      filled black (rect (toFloat w) (toFloat h))
      , message
      , drawItemForm playingField robot
    ]) (map (drawItemForm playingField) items))
    True -> foundAnimation playingField
