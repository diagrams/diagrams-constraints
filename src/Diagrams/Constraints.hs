{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
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

module Diagrams.Constraints where

import Prelude hiding (mapM, mapM_)

import           Control.Applicative
import           Control.Lens (view,Wrapped(..),Rewrapped,iso,_1,_2)
import           Control.Monad.Reader(ReaderT,asks,runReaderT)
import           Control.Monad.State(State,modify,execState)

import           Diagrams.Coordinates
import           Diagrams.Core.Names
import           Diagrams.Core.Types
import           Diagrams.Core (Transformation,Transformable(..),V,apply)
import           Diagrams.TwoD.Types
import qualified Diagrams.Backend.SVG as S
import qualified Diagrams.Prelude as S
import Diagrams.Prelude ((#),circle,text,fontSize)

import           Data.AffineSpace.Point(Point(..))
import           Data.Basis
import           Data.Data
import           Data.Default
import           Data.Foldable
import           Data.Map(Map)
import qualified Data.Map as M
import           Data.Maybe(fromJust)
import           Data.Monoid
import           Data.SBV hiding ((#),EqSymbolic(..))
import qualified Data.SBV as SBV
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Traversable
import           Data.Tree
import           Data.VectorSpace hiding (project)

deriving instance Foldable Point
deriving instance Traversable Point
deriving instance Typeable SBV

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
  deriving (Eq, Typeable, Functor, Foldable, Traversable)

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

-- | Constraint type token
data Constraint = Constraint

type B = Constraint

-- CPrim' is Applicative. I think it might have other instances, but I can't tell.
data CPrim' x = CPrim { variables :: (Set Name), cFunc :: ReaderT (Map Name SDouble) Symbolic x } deriving (Typeable, Functor)

instance Applicative CPrim' where
  pure = CPrim mempty . pure
  (CPrim v1 f) <*> (CPrim v2 x) = CPrim (v1 <> v2) (f <*> x)

instance Show (CPrim' x) where
  show (CPrim _ _) = "<CPrim>" -- TODO

-- | Primitive constraint operation
type CPrim = CPrim' SBool

type instance V CPrim = R2

instance Transformable CPrim where
  transform _ = id

instance Renderable CPrim Constraint where
  render Constraint x = R . modify $ \(CS go r) ->
      CS (x <> go) r

instance Boolean CPrim where
  true = pure true
  false = pure false
  bnot = fmap bnot
  (&&&) = liftA2 (&&&)
  (|||) = liftA2 (|||)
  (~&) = liftA2 (~&)
  (~|) = liftA2 (~|)
  (<+>) = liftA2 (<+>)
  (==>) = liftA2 (==>)
  (<=>) = liftA2 (<=>)
  fromBool = pure . fromBool
  
instance Monoid CPrim where
  mempty = true
  mappend = (&&&)

infix 4 .==, ./=

class EqSymbolic a where
  (.==) :: (EqSymbolic a) => a -> a -> CPrim
  (./=) :: (EqSymbolic a) => a -> a -> CPrim

type CDouble = CPrim' SDouble
type R2 = V2 CDouble
type P2 = Point R2
type T2 = Transformation R2

instance EqSymbolic CDouble where
  (.==) = liftA2 (SBV..==)
  (./=) = liftA2 (SBV../=)

instance Num CDouble where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional CDouble where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance Floating CDouble where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sqrt = fmap sqrt
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
  
type instance V CDouble = CDouble

instance AdditiveGroup CDouble where
  zeroV = pure zeroV
  (^+^) = liftA2 (^+^)
  negateV = fmap negateV

instance VectorSpace CDouble where
  type Scalar CDouble = CDouble
  (*^) = liftA2 (*^)

instance InnerSpace CDouble where (<.>) = liftA2 (<.>)
instance HasBasis CDouble where
  type Basis CDouble = Basis SDouble
  basisValue = pure . basisValue
  decompose s = [((),s)]
  decompose' s   = const s

instance Transformable CDouble where
  transform = apply

deref :: Name -> CDouble
deref n = CPrim (Set.singleton n) $ (fromJust <$> asks (M.lookup n))

origin :: P2 -> CPrim
origin p = view _x p .== 0 &&& view _y p .== 0

spacingX :: CDouble -> [P2] -> CPrim
spacingX sp xs = spacing sp $ map (view _x) xs

spacingY :: CDouble -> [P2] -> CPrim
spacingY sp xs = spacing sp $ map (view _y) xs

alignAxis :: R2 -> [P2] -> CPrim
alignAxis axis xs = spacing 0 $ map project xs -- TODO: allow stuff besides 0
  where
    project (P v) = (axis <.> v)

spacing :: CDouble -> [CDouble] -> CPrim
spacing sp (x:y:[]) = y - x .== sp
spacing sp (x:y:xs) = y - x .== sp &&& spacing sp (y:xs)
spacing _ _ = error "spacing: not enough values"

data Circle v = Circle Integer v deriving (Typeable)
type instance V (Circle v) = v
instance Transformable (Circle v) where
  transform _ = id

instance Renderable (Circle R2) Constraint where
  render Constraint (Circle n (V2 x y)) = R . modify $ \(CS go r) ->
      CS go (tweak r) -- TODO
    where
      tweak r mp = do
          dia <- r mp
          return $ dia <> (S.moveTo (S.p2 (x',y')) $ circle 10 <> (text (show n) # fontSize (Output 14)))
        where
          Just x' = M.lookup x mp
          Just y' = M.lookup y mp

data CState = CS { comp :: CPrim, rFunc :: Map Name Double -> Result B R2 }

instance Default CState where
  def = CS mempty (const (return S.mempty))

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
runSolver (CS (CPrim vars fun) r) = do
        putStrLn "Solving..."
        let varL = Set.elems vars
        result <- satWith z3 $ do
          varMap <- M.fromList <$> flip mapM varL (\n -> do
            var <- free (show n) :: Symbolic SDouble
            return (n,var))
          b <- runReaderT fun varMap
          return b
        putStrLn (show result)
        let strmodel = getModelDictionary result
            model = M.fromList (zip varL (map (fromCW . fromJust . flip M.lookup strmodel . show) varL))
        putStrLn (show model)
        r model
