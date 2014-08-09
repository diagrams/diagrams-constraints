{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Constraints
-- Copyright   :  (c) 2014 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Laying out diagrams using constraints.
-----------------------------------------------------------------------------

module Diagrams.Constraints
  -- Re-export some of SBV
  ( SBool, Symbolic, SDouble, Boolean(..), (.==), satWith, z3, solve
  -- Re-export some of diagrams-lib & diagrams-core
  , r2, mkR2, render, RNode(..), Tree(..), Annotation, Prim(..),Backend(..)
  -- State stuff
  , def, execState, unR, CState(..)
  -- Constraint stuff
  , Constraint(..), Options(..), B, R2, P2, T2, CPrim(..), mkFreeVars,spacingX,spacingY,alignAxis,origin
  ) where

import           Control.Lens (view,Wrapped(..),Rewrapped,iso,_1,_2)
import           Control.Monad.State

import           Diagrams.Coordinates
import           Diagrams.Core.Types
import           Diagrams.Core (Transformation,Transformable(..),V,apply)
import           Diagrams.TwoD.Types
import qualified Diagrams.Backend.SVG as S
import qualified Diagrams.Prelude as S
import Diagrams.Prelude ((#),circle,text,(<>),fontSize)

import           Data.AffineSpace.Point(Point(..))
import           Data.Basis
import           Data.Data
import           Data.Default
import           Data.SBV(SBool,Symbolic,SDouble,mkSymWord,Quantifier(..),(.==),satWith,z3,solve,extractModel,Boolean(..))
import           Data.Tree
import           Data.VectorSpace hiding (project)


type instance V SDouble = SDouble

-- ScalarType from http://hackage.haskell.org/package/vector-space-0.8.7/docs/src/Data-AdditiveGroup.html#AdditiveGroup
instance AdditiveGroup SDouble where
  zeroV = 0
  (^+^) = (+)
  negateV = negate
-- ScalarType from http://hackage.haskell.org/package/vector-space-0.8.7/docs/src/Data-VectorSpace.html
instance VectorSpace SDouble where
  type Scalar SDouble = SDouble
  (*^) = (*)
instance InnerSpace SDouble where (<.>) = (*)
-- ScalarType from http://hackage.haskell.org/package/vector-space-0.8.7/docs/src/Data-Basis.html#HasBasis
instance HasBasis SDouble where
  type Basis SDouble = ()
  basisValue ()  = 1
  decompose s    = [((),s)]
  decompose' s   = const s

instance Transformable SDouble where
  transform = apply

data V2 a = V2 a a
  deriving (Eq, Typeable, Functor)

type ScalarR2 a = (Num a, Fractional a, ScalarR2Sym a, Show a)

instance (ScalarR2 a) => AdditiveGroup (V2 a) where
  zeroV = V2 0 0
  V2 x1 y1 ^+^ V2 x2 y2 = V2 (x1 + x2) (y1 + y2)
  negateV (V2 x y) = V2 (-x) (-y)

instance (ScalarR2 a) => Num (V2 a) where
  (+)                 = (^+^)
  V2 x1 y1 * V2 x2 y2 = V2 (x1 * x2) (y1 * y2)  -- this is sort of bogus
  (-)                 = (^-^)
  negate              = negateV
  abs (V2 x y)        = V2 (abs x) (abs y)
  signum (V2 x y)     = V2 (signum x) (signum y)
  fromInteger i       = V2 i' i'
    where i' = fromInteger i

instance (ScalarR2 a) => Fractional (V2 a) where
  V2 x1 y1 / V2 x2 y2 = V2 (x1/x2) (y1/y2)
  recip (V2 x y) = V2 (recip x) (recip y)
  fromRational r = V2 r' r'
    where r' = fromRational r

instance (ScalarR2 a) => Show (V2 a) where
  showsPrec p (V2 x y) = showParen (p >= 7) $
    showCoord x . showString " ^& " . showCoord y
   where
    showCoord = showParen True . shows

-- | Lens wrapped isomorphisms for V2.
instance (ScalarR2 a) => Wrapped (V2 a) where
    type Unwrapped (V2 a) = (a, a)
    _Wrapped' = iso unr2 r2
    {-# INLINE _Wrapped' #-}

instance (ScalarR2 a) => Rewrapped (V2 a) (V2 a)

type instance V (V2 a) = V2 a

instance (ScalarR2 a) => VectorSpace (V2 a) where
  type Scalar (V2 a) = a
  s *^ V2 x y = V2 (s*x) (s*y)

instance (ScalarR2 a) => HasBasis (V2 a) where
  type Basis (V2 a) = R2Basis
  basisValue XB          = V2 1 0
  basisValue YB          = V2 0 1

  decompose (V2 x y)             = [(XB, x), (YB, y)]

  decompose' (V2 x _) (XB)  = x
  decompose' (V2 _ y) (YB) = y

instance (ScalarR2 a) => InnerSpace (V2 a) where
  (V2 x1 y1) <.> (V2 x2 y2) = x1*x2 + y1*y2

instance (ScalarR2 a) => Coordinates (V2 a) where
  type FinalCoord (V2 a)    = a
  type PrevDim (V2 a)       = a
  type Decomposition (V2 a) = a :& a

  x ^& y           = V2 x y
  coords (V2 x y) = x :& y

instance (ScalarR2 a) => HasX (V2 a) where
    _x = r2Iso . _1

instance (ScalarR2 a) => HasY (V2 a) where
    _y = r2Iso . _2

instance (ScalarR2 a) => Transformable (V2 a) where
  transform = apply


class Sym a where
  {-# MINIMAL mkSym #-}
  mkSym :: Maybe Quantifier -> Maybe String -> Symbolic a
  symbolics :: [String] -> Symbolic [a]
  symbolics      = mapM symbolic
  symbolic  :: String -> Symbolic a
  symbolic       = free
  mkFreeVars n   = mapM (const free_)   [1 .. n]
  mkFreeVars :: Int -> Symbolic [a]
  mkForallVars n = mapM (const forall_) [1 .. n]
  mkForallVars :: Int -> Symbolic [a]
  mkExistVars n  = mapM (const exists_) [1 .. n]
  mkExistVars :: Int -> Symbolic [a]
  free_ :: Symbolic a
  free_    = mkSym Nothing      Nothing
  free :: String -> Symbolic a
  free     = mkSym Nothing    . Just
  forall_ :: Symbolic a
  forall_  = mkSym (Just ALL)   Nothing
  forall :: String -> Symbolic a
  forall   = mkSym (Just ALL) . Just
  exists_ :: Symbolic a
  exists_  = mkSym (Just EX)    Nothing
  exists  :: String -> Symbolic a
  exists   = mkSym (Just EX)  . Just

instance Sym SDouble where
  mkSym = mkSymWord

instance (Sym a) => Sym (V2 a) where
  mkSym q n = do
      x <- mkSym q (add n "x")
      y <- mkSym q (add n "y")
      return (V2 x y)
    where
      add = flip $ \x -> maybe Nothing $ \s -> Just (s ++ x)
  
instance (Sym a) => Sym (Point a) where
  mkSym q n = do
      v <- mkSym q n
      return (P v)

type R2 = V2 SDouble
type P2 = Point R2
type T2 = Transformation R2

origin :: P2 -> SBool
origin p = view _x p .== 0 &&& view _y p .== 0

spacingX :: SDouble -> [P2] -> SBool
spacingX sp xs = spacing sp $ map (view _x) xs

spacingY :: SDouble -> [P2] -> SBool
spacingY sp xs = spacing sp $ map (view _y) xs

alignAxis :: R2 -> [P2] -> SBool
alignAxis axis xs = spacing 0 $ map project xs -- TODO: allow stuff besides 0
  where
    project (P v) = (axis <.> v)

spacing :: SDouble -> [SDouble] -> SBool
spacing sp (x:y:[]) = y - x .== sp
spacing sp (x:y:xs) = y - x .== sp &&& spacing sp (y:xs)
spacing _ _ = error "spacing: not enough values"

-- | Constraint type token
data Constraint = Constraint

type B = Constraint

-- | Primitive constraint operation
data CPrim = CPrim (Symbolic SBool) deriving (Typeable)

type instance V CPrim = V2 SDouble
instance Transformable CPrim where
  transform _ = id

instance Renderable CPrim Constraint where
  render Constraint (CPrim x) = R . modify $ \(CS go r) ->
      CS (do
            y <- go
            x' <- x
            return $ x' &&& y
         )
         r

-- Todo: use a real type instead of Integer
type instance V Integer = V2 SDouble
instance Transformable Integer where
  transform _ = id

instance Renderable Integer Constraint where
  render Constraint n = R . modify $ \(CS go r) ->
      CS go
         (\(y:x:ps) -> do
            dia <- r ps
            return $ dia <> (S.moveTo (S.p2 (x,y)) $ circle 10 <> (text (show n) # fontSize (Output 14)))
         )

data CState = CS { comp :: Symbolic SBool, rFunc :: [Double] -> Result B R2 }

instance Default CState where
  def = CS (return true) (const (return S.mempty))

instance Backend B R2 where
  data Render  B R2 = R { unR :: State CState () }
  type Result  B R2 = IO (Diagram S.SVG S.R2)
  data Options B R2 = Options

  renderRTree Constraint Options rt = do
        let R st = toRender rt
            cstate = execState st def
        runSolver cstate

toRender :: Tree (RNode B R2 Annotation) -> Render B R2
toRender (Node (RPrim p) _) = render Constraint p
toRender (Node REmpty rs) = R $ mapM_ (unR . toRender) rs

runSolver :: CState -> Result B R2
runSolver (CS go r) = do
        putStrLn "Solving..."
        result <- satWith z3 go
        putStrLn (show result)
        let Just model = extractModel result -- TODO: actually match up points
        putStrLn (show model)
        r (reverse model)
