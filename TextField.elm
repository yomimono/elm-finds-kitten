module TextField where

-- return the size of an arbitrary monospace character.
charDims : (Int, Int)
charDims =
   let sampleChar = centered <| monospace <| toText "@" 
   in (widthOf sampleChar, heightOf sampleChar)

-- given a width, height pair, e.g. from Window.Dimensions,
-- return the number of usable text columns and rows, based on
-- the size of a monospace character.
makeLimits : (Int, Int) -> (Int, Int)
makeLimits (w, h) = 
   let (charWidth, charHeight) = charDims
   in (w // charWidth, h // charHeight)

--map a pair of (w, h) values corresponding to the maxiumum number of 
--full columns and rows displayable on the screen,
--to the upper-right-hand value in a zero-centered cartesian plane
--(basically, this is a shim between the more generally-applicable makeLimits
--return value and the silly way I did this at first)
--also throw in a little rfk-specific logic - subtract a couple more rows for the status
toCartesianLimits : (Int, Int) -> (Int, Int)
toCartesianLimits (w, h) =
   (w // 2 - 1, h // 2 - 3)
