module InputModel where

import Char
import Keyboard

type Item a = { a | char:String, description:String, xd:Int, yd:Int, cd:Color, isKitten:Bool }

type Colliding b = { b | collidingWith: String }

type State = {
   player: Colliding (Item {}),
   items: [Item {}],
   playingField: (Int, Int)
}

type Controls = {
   direction : { x: Int, y: Int }
}

type Input = {
   controls: Controls,
   randomElements: [Int] --a long list of ints from which to generate random colors, positions, symbols, etc
}

viKeys : Signal { x:Int, y:Int }
viKeys =
  Keyboard.directions (Char.toCode 'K') (Char.toCode 'J') (Char.toCode 'H') (Char.toCode 'L')

allDirectionalInputs : Signal Controls
allDirectionalInputs = Controls <~ merges [ Keyboard.arrows, Keyboard.wasd, viKeys ]  --todo: mobile, diagonal
