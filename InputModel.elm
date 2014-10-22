module InputModel where

import Char
import Keyboard
import Random

-- robot, kitten, and all non-kitten items are gamepieces
-- gamepieces have a character representation, an x/y position,
-- and a color.  robot's are constant; others are randomized.
type GamePiece gp = { gp | char:String, xd:Int, yd:Int, cd:Color }

-- kitten and non-kitten items have descriptions and kittenness
type Item a = { a | description:String, isKitten:Bool }

-- robot collides with things; collidingWith records the description
-- of the last item with which robot collided
type Colliding b = { b | collidingWith: String }

type State = {
   actionTaken: Bool, --has a valid key been pressed? used to determine
                      --whether the intro screen should be displayed
   playingField: (Int, Int), -- the size of the valid play field
   player: Colliding (GamePiece {}), -- robot's state
   --slowItems: [Item (GamePiece {})], --the state of all items, kitten and non-
   --fastItems: [Item (GamePiece {})], --the state of all items, kitten and non-
   items: [Item (GamePiece {})], --the state of all items, kitten and non-
   itemsMove: Bool
}

type Controls = {
   direction : { x: Int, y: Int }
}

type Input = {
   controls: Controls,
   playingField: (Int, Int),
   slowRandomElements: [Int], -- a list of random numbers which won't
                              -- likely change over the course of the game
   fastRandomElements: [Int], -- a list of random numbers which change 
                              -- every few seconds
   robotSleepy: Bool
}

-- for old-school roguelike players, allow movement with HJKL
viKeys : Signal { x:Int, y:Int }
viKeys =
  Keyboard.directions (Char.toCode 'K') (Char.toCode 'J') (Char.toCode 'H') (Char.toCode 'L')

-- merge all possible inputs for controlling robot
-- TODO: mobile, diagonal
allDirectionalInputs : Signal Controls
allDirectionalInputs = Controls <~ merges [ Keyboard.arrows, Keyboard.wasd, viKeys ]  

-- construct a big list of random numbers for later use in generating items
makeNRandomInts : Int -> (Signal a) -> [Signal Int]
makeNRandomInts howMany howToMake =
    if howMany <= 0 then []
    else 
       let thisRand = Random.range ((-1)*(2^10)) (2^10) howToMake in
       thisRand :: makeNRandomInts (howMany - 1) howToMake
