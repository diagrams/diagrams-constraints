module Example where

{-
{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude
import qualified Data.Colour as C
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

spike :: Trail R2
spike = fromOffsets . map r2 $ [(1,3), (1,-3)]

burst = mconcat . take 13 . iterate (rotateBy (-1/13)) $ spike

colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen]

loopyStar = fc red
          . mconcat . map (cubicSpline True)
          . pathVertices
          . star (StarSkip 3)
          $ regPoly 7 1

path = fromVertices (map p2 [(0,0), (1,0.3), (2,0), (2.2,0.3)]) # lw 15
example = centerXY . vcat' with { sep = 0.1 }
          $ map (path #)
            [ lineCap LineCapButt   . lineJoin LineJoinMiter
            , lineCap LineCapRound  . lineJoin LineJoinRound
            , lineCap LineCapSquare . lineJoin LineJoinBevel
            , dashing [5,10,15,5] 0
            ]

s c     = square 1 # fc c
reds    = (s darkred ||| s red) === (s pink ||| s indianred)

-- We can use the [`colour`
-- library](http://hackage.haskell.org/package/colour) to generate
-- successively lighter shades of blue:
colors = iterate (C.blend 0.1 white) blue

-- An order-0 pentaflake is just a pentagon:
p = regPoly 5 1 # lw 0
pentaflake 0 = p
-- An [order-n pentaflake](http://mathworld.wolfram.com/Pentaflake.html) is an order-(n-1) pentaflake surrounded by five
-- more.  The `appends` function is useful here for positioning the five
-- pentaflakes around the central one.
pentaflake n = appends (p' # fc (colors !! (n-1)))
                       (zip vs (repeat (rotateBy (1/2) p')))
  where vs = take 5 . iterate (rotateBy (1/5))
                    . (if odd n then negateV else id) $ unitY
        p' = pentaflake (n-1)

pentaflake' n = pentaflake n # fc (colors !! n)

eff2 :: Diagram SVG R2
eff2 = text "F" # fc black <> square 1 # lw 0

eff :: Diagram SVG R2
eff = text "F" # fc black <> square 1 # lw 0 # lc white
rs  = map rotateBy [1/7, 2/7 .. 6/7]

eff1 :: Diagram SVG R2
eff1 = text "F" # fc black <> square 1 # lw 0
ts  = [ scale (1/2), id, scale 2,    scaleX 2,    scaleY 2
      ,     scale (-1), scaleX (-1), scaleY (-1)
      ]


eff2 :: Diagram SVG R2
eff2 = text "F" # fc black <> square 1 # lw 0 # lc white

eff :: Diagram SVG R2
eff = text "F" <> square 1 # lw 0

text' s t = text t # fontSize s <> strutY (s * 1.3)


example :: Diagram SVG R2
example = text "Hello world!" # fc black <> rect 8 2
example = pentaflake' 4
example = loopyStar # fillRule EvenOdd
      ||| strutX 1
      ||| loopyStar # fillRule Winding
example = hcat' with { sep = 1 } . take 4 . iterate (opacity 0.7) $ reds
example = eff2 # rotateBy (1/7)
example = hcat . map (eff #) $ rs
example = hcat . map (eff1 #) $ ts
example = (scaleX 2 `under` rotation (-1/8 :: CircleFrac)) eff2
example = eff
       <> reflectAbout (p2 (0.2,0.2)) (rotateBy (-1/10) unitX) eff
example = centerXY $
      text' 10 "Hello" # fc black # italic
  === text' 5 "there"  # fc black # bold # font "sans-serif"
  === text' 3 "world"  # fc green
example = square 3
        # fc green
        # lw 5.0 
        # clipBy (square 3.2 # rotateBy (1/10))
example = square 20 # lw 0.002
example = lw 1.0
        . mconcat
        . zipWith lc colors
        . map stroke . explodeTrail origin
        $ burst

main = defaultMain (pad 1.1 example)

-}