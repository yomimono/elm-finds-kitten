module InputModel where

import Char
import Keyboard
import Random

type GamePiece gp = { gp | char:String, xd:Int, yd:Int, cd:Color }

type Item a = { a | description:String, isKitten:Bool }

type Colliding b = { b | collidingWith: String }

type State = {
   actionTaken: Bool,
   playingField: (Int, Int),
   player: Colliding (GamePiece {}),
   items: [Item (GamePiece {})]
}

type Controls = {
   direction : { x: Int, y: Int }
}

type Input = {
   controls: Controls,
   playingField: (Int, Int),
   randomElements: [Int] --a long list of ints from which to generate random colors, positions, symbols, etc
}

viKeys : Signal { x:Int, y:Int }
viKeys =
  Keyboard.directions (Char.toCode 'K') (Char.toCode 'J') (Char.toCode 'H') (Char.toCode 'L')

allDirectionalInputs : Signal Controls
allDirectionalInputs = Controls <~ merges [ Keyboard.arrows, Keyboard.wasd, viKeys ]  --todo: mobile, diagonal

makeNRandomInts : Int -> (Signal a) -> [Signal Int]
makeNRandomInts howMany howToMake =
    if howMany <= 0 then []
    else 
       let thisRand = Random.range ((-1)*(2^16)) (2^16) howToMake in
       thisRand :: makeNRandomInts (howMany - 1) howToMake
