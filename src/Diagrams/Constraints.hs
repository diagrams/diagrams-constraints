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
import           Control.Lens.Type
import           Control.Lens.Wrapped
import           Control.Lens(iso,view,from,mapping)
import           Control.Monad.Reader(Reader,reader,runReader,ReaderT(..),asks)
import           Control.Monad.State(State,modify,execState)
import           Data.Functor.Compose

import           Diagrams.Prelude hiding (view,(|||),project)
import           Diagrams.TwoD.Types.Generic
import           Diagrams.Core.Types
import           Diagrams.TwoD.Types
import qualified Diagrams.Backend.SVG as S

import           Data.AffineSpace.Point(Point(..))
import           Data.Basis
import           Data.Data
import           Data.Default
import           Data.Foldable
import           Data.Map(Map)
import qualified Data.Map as M
import           Data.Maybe(fromJust,fromMaybe)
import           Data.SBV hiding ((#),(.>),name)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Traversable
import           Data.Tree

----------------------------
-- SBV <-> diagrams-lib glue
deriving instance Foldable Point
deriving instance Traversable Point
instance IsName R2Basis
deriving instance Typeable SBV

type instance V SDouble = SDouble
type instance V Name = SDouble

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

instance (EqSymbolic a) => EqSymbolic (V2 a) where
  (V2 x1 y1) .== (V2 x2 y2) = x1 .== x2 &&& y1 .== y2

instance (EqSymbolic v) => EqSymbolic (Point v) where
  (P a) .== (P b) = a .== b

-- A large Applicative stack
type CFunc d s = ReaderT (Map Name d) (Compose Maybe s)

instance Show (CFunc d s x) where
  show _ = "<CFunc>" -- TODO

runCFunc :: CFunc d s x -> Map Name d -> Maybe (s x)
runCFunc f mp = getCompose $ runReaderT f mp

-- Lenses to make my life easier

readerLens :: Iso' (r -> a) (Reader r a)
readerLens = iso reader runReader

_comp :: Iso' (CFunc d s x) (Reader (Map Name d) (Maybe (s x)))
_comp = _Wrapped . readerLens . mapping _Wrapped

-- CPrim' is Applicative and Alternative
data CPrim'' d s x = CPrim { variables :: (Set Name), cFunc :: CFunc d s x } deriving (Typeable, Functor, Show)

type CPrim' = CPrim'' SDouble Symbolic

instance Applicative s => Applicative (CPrim'' d s) where
  pure = CPrim mempty . pure
  (CPrim v1 f) <*> (CPrim v2 x) = CPrim (v1 <> v2) (f <*> x)

-- | The Alternative instance is left-biased, due to the underlying Maybe.
instance Applicative s => Alternative (CPrim'' d s) where
  empty = CPrim mempty empty
  (CPrim v1 a) <|> (CPrim v2 b) = CPrim (v1 <> v2) (a <|> b)

class Evaluable d s n where
  type EvalResult d s n
  evaluate :: n -> CPrim'' d s (EvalResult d s n)

instance (Applicative s) => Evaluable d s Name where
  type EvalResult d s Name = d
  evaluate n = CPrim (Set.singleton n) (deref n)
    where
      deref :: (Applicative s) => Name -> CFunc d s d
      deref = view (from _comp) . fmap (fmap pure) . asks . M.lookup

instance Evaluable d s (CPrim'' d s a) where
  type EvalResult d s (CPrim'' d s a) = a
  evaluate = id

instance (Applicative s, Evaluable d s a) => Evaluable d s ([a]) where
  type EvalResult d s ([a]) = [EvalResult d s a]
  evaluate = traverse evaluate

instance (Applicative s, Evaluable d s a) => Evaluable d s (Point a) where
  type EvalResult d s (Point a) = Point (EvalResult d s a)
  evaluate = traverse evaluate

instance (Evaluable d IO a, EvalResult d IO a ~ Double) => Evaluable d IO (V2 a) where
  type EvalResult d IO (V2 a) = S.R2
  evaluate (V2 x y) = liftA2 mkR2 (evaluate x) (evaluate y)

instance (Evaluable d Symbolic a) => Evaluable d Symbolic (V2 a) where
  type EvalResult d Symbolic (V2 a) = V2 (EvalResult d Symbolic a)
  evaluate = traverse evaluate

-- Wrapped Boolean
type CBool = CPrim' SBool

-- We lift the operators with left/right identities to consider empty as the identity
lift2 :: (SBool -> SBool -> SBool) -> CBool -> (CBool -> CBool -> CBool)
lift2 f df a b = liftA2 f (a <|> df) (b <|> df)

instance Boolean CBool where
  true = pure true
  false = pure false
  bnot = fmap bnot
  (&&&) = lift2 (&&&) true
  (|||) = lift2 (|||) false
  (~&) = liftA2 (~&)
  (~|) = liftA2 (~|)
  (<+>) = lift2 (<+>) false
  x ==> y = liftA2 (==>) (x <|> true) y
  (<=>) = lift2 (<=>) true
  fromBool = pure . fromBool
  
-- Wrapped Double
type CDouble = CPrim' SDouble

-- | Bogus Eq instance
instance Eq CDouble where
  _ == _ = error "cannot compare CPrim' values"

-- | This instance supports only min/max
instance Ord CDouble where
  compare _ _ = error "cannot compare CPrim' values"
  min = liftA2 smin
  max = liftA2 smax

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

instance Real CDouble where
  toRational = error "cannot read from CDouble"

instance RealFrac CDouble where
  properFraction = error "cannot read from CDouble"

instance RealFloat CDouble where
  floatRadix = error "cannot read from CDouble"
  floatDigits = error "cannot read from CDouble"
  floatRange = error "cannot read from CDouble"
  decodeFloat = error "cannot read from CDouble"
  encodeFloat a b = pure $ encodeFloat a b
  exponent = error "cannot read from CDouble"
  significand = fmap significand
  scaleFloat i = fmap (scaleFloat i)
  isNaN = error "cannot read from CDouble"
  isInfinite = error "cannot read from CDouble"
  isDenormalized = error "cannot read from CDouble"
  isNegativeZero = error "cannot read from CDouble"
  isIEEE = error "cannot read from CDouble"
  atan2 = liftA2 atan2

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

type R2 = V2 CDouble
type P2 = Point R2
type T2 = Transformation R2

constraint :: CBool -> Prim B R2
constraint = Prim . ConstraintPrim

origin :: P2 -> CBool 
origin p = fmap (.== (P (V2 0 0))) (evaluate p)

spacingX :: CDouble -> [P2] -> CBool 
spacingX sp xs = spacing sp $ map (view _x) xs

spacingY :: CDouble -> [P2] -> CBool 
spacingY sp xs = spacing sp $ map (view _y) xs

alignAxis :: R2 -> [P2] -> CBool 
alignAxis axis xs = spacing 0 $ map project xs -- TODO: allow stuff besides 0
  where
    project (P v) = (axis <.> v)

spacing :: CDouble -> [CDouble] -> CBool 
spacing a b = liftA2 spacing' (evaluate a) (evaluate b)
  where
    spacing' sp (x:y:[]) = y - x .== sp
    spacing' sp (x:y:xs) = y - x .== sp &&& spacing' sp (y:xs)
    spacing' _ _ = error "spacing: not enough values"

-- | Constraint type token
data Constraint = Constraint deriving (Typeable, Eq, Ord, Show)

instance IsName Constraint -- for generated names

type B = Constraint

-- | Primitive constraint operation
newtype CPrimPrim = ConstraintPrim { getConstraint :: CPrim' SBool } deriving (Typeable,Show)

type instance V CPrimPrim = R2

instance Transformable CPrimPrim where
  transform _ = id

instance Renderable CPrimPrim Constraint where
  render Constraint (ConstraintPrim x) = R . modify $ \cs -> cs { comp = x &&& comp cs}

-- | Test rendering primitive
data Circle v = Circle (Maybe Name) Integer v deriving (Typeable)
type instance V (Circle v) = V v
instance (Transformable v) => Transformable (Circle v) where
  transform tr (Circle nm i v) = Circle nm i (transform tr v)

instance Renderable (Circle P2) Constraint where
  render Constraint (Circle mbname n vc) = R . modify $ f
   where
    f cs = cs
            { comp = comp cs &&& liftA2 (.==) (evaluate vc) (evaluate pt)
            , rFunc = liftA2 (<>) (rFunc cs) (drawCircle <$> evaluate pt)
            , nameSupply = maybe (nameSupply cs + 1) (const $ nameSupply cs) mbname }
     where
      name = fromMaybe (Constraint .> nameSupply cs) mbname
      xname = name .> XB
      yname = name .> YB
      pt = P (V2 xname yname)
      drawCircle xy = moveTo xy $ circle 10 <> (text (show n) # fontSize (Output 14))

data CState = CS { comp :: CBool, rFunc :: CPrim'' Double IO (Diagram S.SVG S.R2), nameSupply :: Integer }

instance Default CState where
  def = CS empty (pure mempty) 0

instance Backend B R2 where
  data Render  B R2 = R { unR :: State CState () }
  type Result  B R2 = IO (Diagram S.SVG S.R2)
  data Options B R2 = Options
     { finalAdjustments :: Diagram S.SVG S.R2 -> Diagram S.SVG S.R2
     , renderOpts :: Options S.SVG S.R2
     }

  renderRTree Constraint opts rt = do
        let R st = toRender rt
            cstate = execState st def
        dia <- runSolver cstate
        return $ finalAdjustments opts dia

instance Default (Options B R2) where
  def = Options id undefined

toRender :: Tree (RNode B R2 Annotation) -> Render B R2
toRender (Node (RPrim p) _) = render Constraint p
toRender (Node REmpty rs) = R $ mapM_ (unR . toRender) rs
toRender (Node (RStyle _) rs) = R $ mapM_ (unR . toRender) rs
toRender (Node (RAnnot _) rs) = R $ mapM_ (unR . toRender) rs

runSolver :: CState -> Result B R2
runSolver (CS (CPrim vars fun) r _) = do
        let varL = Set.elems vars
        result <- satWith z3 $ do
          varMap <- M.fromList <$> flip mapM varL (\n -> do
            var <- free (show n) :: Symbolic SDouble
            return (n,var))
          b <- fromMaybe (pure true) $ runCFunc fun varMap
          return b
        let strmodel = getModelDictionary result
            model = M.fromList (zip varL (map (fromCW . fromJust . flip M.lookup strmodel . show) varL)) :: Map Name Double
        putStrLn (show model)
        fromMaybe (pure mempty) $ runCFunc (cFunc r) model
