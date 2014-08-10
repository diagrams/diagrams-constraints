module CMears where

import           Control.Monad.Reader

import           Diagrams.Core.Names
import           Diagrams.Core.Types
import           Diagrams.TwoD.Types hiding (p2)
import Diagrams.Prelude ((#),centerXY,pad,bg,white)

import           Data.AffineSpace.Point(Point(..))
import           Data.Map(Map)
import qualified Data.Map as M
import           Data.SBV(SDouble,solve)
import           Data.Tree

import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Constraints

-- based on https://github.com/diagrams/diagrams-lib/issues/8#issuecomment-16298660

-- the layout specification
go :: CPrim
go = CPrim (concat ptvars) $ do
       Just [p1,p2,p3,p4,p5,p6] <- asks mkPts
       lift $ solve
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
  where
    pts = map (("p"++) . show) ([1..6] :: [Integer])
    ptvars :: [[Name]]
    ptvars = [ map toName [pt++"x",pt++"y"] | pt <- pts]
    mkPts :: Map Name SDouble -> Maybe [P2]
    mkPts mp = mapM (fmap (\[x,y] -> P $ V2 x y) . mapM (flip M.lookup mp)) ptvars

-- this just draws labeled circles at the points determined by the solver
rtree :: Tree (RNode B R2 Annotation)
rtree = Node REmpty $ map (flip Node [] . RPrim . Prim) ([1..6] :: [Integer]) ++ [Node (RPrim (Prim go)) []]

main :: IO ()
main = do
  sol <- renderRTree Constraint Options rtree
  defaultMain (sol # centerXY # pad 1.1 # bg white)
