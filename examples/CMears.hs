module CMears where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Data.SBV hiding ((#))

-- https://github.com/diagrams/diagrams-lib/issues/8#issuecomment-16298660 ported to SBV

-- the layout specification
go :: Symbolic SBool
go = do
       [x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6] <- mkFreeVars 12 :: Symbolic [SDouble]
       solve [
          -- x1,x2,x3,x4 evenly spaced
          x2 - x1 .== x3 - x2
        , x3 - x2 .== x4 - x3

          -- spacing is 5
        , x2 - x1 .== 5
          -- x1,y1 at origin
        , x1 .== 0
        , y1 .== 0

          -- y1,y2,y3,y4 all equal
        , y1 .== y2
        , y1 .== y3
        , y1 .== y4

        -- x5,y5 on NNE-SSW line from x2,y2
        , y5 - y2 .== 2 * x5 - 2 * x2
        -- x5,y5 vertically aligned with x3,y3
        , x5 .== x3

        -- x6,y6 on NE-SW diagonal from x2,y2
        , y6 - y2 .== x6 - x2
        -- x6,y6 vertically aligned with x1,y1
        , x6 .== x1
        ]

aSolution :: IO [Double]
aSolution = do
  putStrLn "Solving..."
  result <- satWith z3 go
  putStrLn (show result)
  let Just model = extractModel result
  return model
  

-- this just draws labeled circles at the points determined by the
-- solver
draw :: [Double] -> Diagram SVG R2
draw [x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6] = 
   let pairs = [(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5),(x6,y6)]
      in position [ (p2 pair, f n) | (pair,n) <- zip pairs [1..] ]
  where f n = circle 1 <> text (show n)

-- :main --output test.svg
main :: IO ()
main = do
  sol <- aSolution
  defaultMain (draw sol # centerXY # pad 1.1 # bg white)
