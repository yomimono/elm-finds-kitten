module GameLogic where

import KittenConstants
import List
import InputModel (Colliding, Item, GamePiece)

kittenFound : Colliding a -> Bool
kittenFound r = r.collidingWith == KittenConstants.kittenDescription

itemsToMake : (Int, Int) -> Int
itemsToMake (x, y) = max 10 (div (x * y) 20000)

samePlace : GamePiece a -> GamePiece b -> Bool
samePlace robot item = robot.xd == item.xd && robot.yd == item.yd

collision : GamePiece b -> [GamePiece a] -> Maybe (GamePiece a)
collision robot items = 
  let found = (List.filter (samePlace robot) items )
  in if List.isEmpty found then Nothing else Just (head found)

