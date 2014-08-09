module CMears where

import Diagrams.Prelude ((#),circle,text,(<>),centerXY,pad,bg,position,white,Diagram,fontSize,Measure(..))
import qualified Diagrams.Prelude as S
import qualified Diagrams.Backend.SVG as S
import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Constraints

-- https://github.com/diagrams/diagrams-lib/issues/8#issuecomment-16298660 ported to SBV

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
draw :: [Double] -> Diagram S.SVG S.R2
draw [x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6] = 
   let pairs = [(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5),(x6,y6)]
      in position [ (S.p2 pair, circle 10 <> (text (show n) # fontSize (Output 14))) | (pair,n) <- zip pairs [(1 :: Integer)..] ]

main :: IO ()
main = do
  sol <- runSolver go (return . draw)
  defaultMain (sol # centerXY # pad 1.1 # bg white)
