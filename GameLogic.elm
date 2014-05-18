module GameLogic where

import KittenConstants
import InputModel (Colliding)

kittenFound : Colliding a -> Bool
kittenFound r = r.collidingWith == KittenConstants.kittenDescription
