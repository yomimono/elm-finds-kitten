module GameLogic where

import KittenConstants
import InputModel (Colliding)

kittenFound : Colliding a -> Bool
kittenFound r = r.collidingWith == KittenConstants.kittenDescription

itemsToMake : (Int, Int) -> Int
itemsToMake (x, y) = max 10 (div (x * y) 20000)
