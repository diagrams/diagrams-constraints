module CMears where

import           Diagrams.Core.Names
import           Diagrams.Core.Types
import           Diagrams.Core.Compile
import Diagrams.Prelude ((#),centerXY,pad,bg,white)

import           Data.AffineSpace.Point(Point(..))
import           Data.Tree

import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Constraints

-- based on https://github.com/diagrams/diagrams-lib/issues/8#issuecomment-16298660

-- this just draws labeled circles at the points determined by the solver
dtree :: DTree B R2 Annotation
dtree = Node DEmpty . map (flip Node [] . DPrim) $ map Prim circs ++ map Prim go
  where
    -- the layout specification
    go :: [CPrim]
    go = -- 1-4 evenly spaced on X axis, spacing is 50
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
    [p1,p2,p3,p4,p5,p6] = mkPts
    nums = [1..6] :: [Integer]
    pts = map (("p"++) . show) nums
    toCD = deref . toName
    ptvars :: [R2]
    ptvars = [ V2 (toCD (pt++"x")) (toCD (pt++"y")) | pt <- pts]
    mkPts :: [P2]
    mkPts = map P ptvars
    circs :: [Circle R2]
    circs = map (\(i,p) -> Circle i p) (zip nums ptvars)

main :: IO ()
main = do
  sol <- renderRTree Constraint Options (fromDTree dtree)
  defaultMain (sol # centerXY # pad 1.1 # bg white)
