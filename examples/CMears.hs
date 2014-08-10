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
import qualified Data.Set as Set
import           Data.Tree

import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Constraints

-- based on https://github.com/diagrams/diagrams-lib/issues/8#issuecomment-16298660

-- this just draws labeled circles at the points determined by the solver
rtree :: Tree (RNode B R2 Annotation)
rtree = Node REmpty $ map (flip Node [] . RPrim . Prim) circs ++ [Node (RPrim (Prim go)) []]
  where
    -- the layout specification
    go :: CPrim
    go = CPrim (Set.fromList $ concat ptvars) $ do
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
    nums = [1..6] :: [Integer]
    pts = map (("p"++) . show) nums
    ptvars :: [[Name]]
    ptvars = [ map toName [pt++"x",pt++"y"] | pt <- pts]
    mkPts :: Map Name SDouble -> Maybe [P2]
    mkPts mp = mapM (fmap (\[x,y] -> P $ V2 x y) . mapM (flip M.lookup mp)) ptvars
    circs :: [Circle (V2 Name)]
    circs = map (\(i,[x,y]) -> Circle i (V2 x y)) (zip nums ptvars)

main :: IO ()
main = do
  sol <- renderRTree Constraint Options rtree
  defaultMain (sol # centerXY # pad 1.1 # bg white)
