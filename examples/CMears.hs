module Main where

import           Diagrams.Core.Names
import           Diagrams.Core.Types
import           Diagrams.Core.Compile
import           Diagrams.TwoD.Types hiding (p2)
import Diagrams.Prelude ((#),centerXY,pad,bg,white)

import           Data.AffineSpace.Point(Point(..))
import qualified Data.Tree.DUAL            as D
import           Data.Monoid
import           Data.Monoid.MList(empty)
import           Data.SBV(bAnd)

import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Constraints

-- based on https://github.com/diagrams/diagrams-lib/issues/8#issuecomment-16298660

toQD :: Prim b v -> QDiagram b v m
toQD p = QD $ D.leaf empty (PrimLeaf p)

-- this just draws labeled circles at the points determined by the solver
dia :: Diagram B R2
dia = mconcat . map toQD $ map Prim (reverse circs) ++ [go]
  where
    -- the layout specification
    go = constraint $ bAnd
         -- 1-4 evenly spaced on X axis, spacing is 50
         [ spacingX 50 [p1,p2,p3,p4]
         -- 1-4 same on Y axis
         , spacingY 0 [p1,p2,p3,p4]
         -- p5 on NNE-SSW line from p2
         , alignAxis (V2 2 (-1)) [p2,p5]
         -- p5 vertically aligned with p3
         , spacingX 0 [p5,p3]
         -- p6 on NE-SW diagonal from p2
         , alignAxis (V2 (-1) 1) [p6,p2]
         -- p6 vertically aligned with p1
         , spacingX 0 [p1,p6]
         -- p1 at origin
         , origin p1
         ]
    [p1,p2,p3,p4,p5,p6] = ptvars
    nums = [1..6] :: [Integer]
    pts = map (("p"++) . show) nums
    ptvars :: [P2]
    ptvars = [ P $ V2 (evaluate $ pt .> XB) (evaluate $ pt .> YB) | pt <- pts]
    circs :: [Circle P2]
    circs = map (\(n,i,p) -> Circle (Just $ toName n) i p) (zip3 pts nums ptvars)

main :: IO ()
main = do
  sol <- renderDia Constraint Options dia
  defaultMain (sol # centerXY # pad 1.1 # bg white)
