module CMears where

import Diagrams.Prelude ((#),centerXY,pad,bg,white)
import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Constraints

-- based on https://github.com/diagrams/diagrams-lib/issues/8#issuecomment-16298660

-- the layout specification
go :: Symbolic SBool
go = do
       [p1,p2,p3,p4,p5,p6] <- mkFreeVars 6 :: Symbolic [P2]
       solve
        -- 1-4 evenly spaced on X axis, spacing is 50
        [ spacingX 50 [p1,p2,p3,p4]
        -- 1-4 same on Y axis
        , spacingY 0 [p1,p2,p3,p4]
        -- p5 on NNE-SSW line from p2
        , alignAxis (mkR2 2 (-1)) [p2,p5]
        -- p5 vertically aligned with p3
        , spacingX 0 [p5,p3]
        -- p6 on NE-SW diagonal from p2
        , alignAxis (mkR2 (-1) 1) [p6,p2]
        -- p6 vertically aligned with p1
        , spacingX 0 [p1,p6]
        -- p1 at origin
        , origin p1
        ]

-- this just draws labeled circles at the points determined by the solver
rtree :: Tree (RNode B R2 Annotation)
rtree = Node REmpty $ map (flip Node [] . RPrim . Prim) ([1..6] :: [Integer]) ++ [Node (RPrim (Prim (CPrim go))) []]

main :: IO ()
main = do
  sol <- renderRTree Constraint Options rtree
  defaultMain (sol # centerXY # pad 1.1 # bg white)
