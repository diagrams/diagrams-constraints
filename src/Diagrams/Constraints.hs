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

import           Diagrams.Prelude hiding (view,(|||),project, R2, P2)
import           Diagrams.Core.Types
import           Diagrams.TwoD.Types hiding (R2, P2)
import qualified Diagrams.TwoD.Types as S
import qualified Diagrams.Backend.SVG as S

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
deriving instance Typeable SBV

-- | These instances should be automatic...
instance (EqSymbolic a) => EqSymbolic (V2 a) where
  (V2 x1 y1) .== (V2 x2 y2) = x1 .== x2 &&& y1 .== y2

-- | These instances should be automatic...
instance (EqSymbolic (v a)) => EqSymbolic (Point v a) where
  (P a) .== (P b) = a .== b

-- | An Applicative stack.  It takes a map from names to d's and returns Nothing if any of the required names are missing. See 'runCFunc' for the unrolled type.
type CFunc d s = ReaderT (Map Name d) (Compose Maybe s)

-- | TODO
instance Show (CFunc d s x) where
  show _ = "<CFunc>" 

-- | Run a 'CFunc'.
runCFunc :: CFunc d s x -> Map Name d -> Maybe (s x)
runCFunc f mp = getCompose $ runReaderT f mp

-- | A lens for Reader. No idea where it properly belongs.
readerLens :: Iso' (r -> a) (Reader r a)
readerLens = iso reader runReader

-- | A lens to work directly with the Maybe part of the computation.
_comp :: Iso' (CFunc d s x) (Reader (Map Name d) (Maybe (s x)))
_comp = _Wrapped . readerLens . mapping _Wrapped

-- | CPrim'' keeps track of the names that the 'CFunc' needs. Ideally this should be abstract and only expose the evaluate function of 'Evaluable d s Name' and its instances. It also needs a better name.
data CPrim'' d s x = CPrim { variables :: (Set Name), cFunc :: CFunc d s x } deriving (Typeable, Functor, Show)

-- | CPrim'' is Applicative and Alternative, but not a Monad.
instance Applicative s => Applicative (CPrim'' d s) where
  pure = CPrim mempty . pure
  (CPrim v1 f) <*> (CPrim v2 x) = CPrim (v1 <> v2) (f <*> x)

-- | The Alternative instance is left-biased, due to the underlying Maybe.
instance Applicative s => Alternative (CPrim'' d s) where
  empty = CPrim mempty empty
  (CPrim v1 a) <|> (CPrim v2 b) = CPrim (v1 <> v2) (a <|> b)

-- | An 'Evaluable' n can be evaluated in the 'CPrim''' monad to give an 'EvalResult d s n'
class Evaluable d s n where
  type EvalResult d s n
  evaluate :: n -> CPrim'' d s (EvalResult d s n)

-- | Evaluating Names dereferences them from the name map.
instance (Applicative s) => Evaluable d s Name where
  type EvalResult d s Name = d
  evaluate n = CPrim (Set.singleton n) (deref n)
    where
      deref :: (Applicative s) => Name -> CFunc d s d
      deref = view (from _comp) . fmap (fmap pure) . asks . M.lookup

-- | Evaluating computations returns the computation unchanged.
instance Evaluable d s (CPrim'' d s a) where
  type EvalResult d s (CPrim'' d s a) = a
  evaluate = id

-- | Other instance for a Traversable type
instance (Applicative s, Evaluable d s a) => Evaluable d s ([a]) where
  type EvalResult d s ([a]) = [EvalResult d s a]
  evaluate = traverse evaluate

-- | Other instance for a Traversable type
instance (Applicative s, Traversable f, Evaluable d s a) => Evaluable d s (Point f a) where
  type EvalResult d s (Point f a) = Point f (EvalResult d s a)
  evaluate = traverse evaluate

-- | Other instance for a Traversable type
instance (Applicative s, Evaluable d s a) => Evaluable d s (V2 a) where
  type EvalResult d s (V2 a) = V2 (EvalResult d s a)
  evaluate = traverse evaluate

-- | CPrim'' specialized to be over the sbv package's monad and use only 'SDouble'. Ideally we would be able to store other types, such as 'SInteger', but I haven't had yet integrated the vault package (which will replace Map when integrated).
type CPrim' = CPrim'' SDouble Symbolic

-- | A wrapped computation that returns a boolean value.
type CBool = CPrim' SBool

-- | Lift the operator to consider empty / Nothing as the identity df
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
  
-- | A wrapped computation that returns a Double value. Instance of many numeric types.
type CDouble = CPrim' SDouble

-- | A bogus Eq instance
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

-- | bogus
instance Real CDouble where
  toRational = error "cannot read from CDouble"

-- | bogus
instance RealFrac CDouble where
  properFraction = error "cannot read from CDouble"

-- | bogus except for atan2 and a few others
instance RealFloat CDouble where
  floatRadix = error "cannot read from CDouble"
  floatDigits = error "cannot read from CDouble"
  floatRange = error "cannot read from CDouble"
  decodeFloat = error "cannot read from CDouble"
  encodeFloat a b = error "x" -- pure $ encodeFloat a b
  exponent = error "cannot read from CDouble"
--  significand = fmap significand
--  scaleFloat i = fmap (scaleFloat i)
  isNaN = error "cannot read from CDouble"
  isInfinite = error "cannot read from CDouble"
  isDenormalized = error "cannot read from CDouble"
  isNegativeZero = error "cannot read from CDouble"
  isIEEE = error "cannot read from CDouble"
--  atan2 = liftA2 atan2

-- | Pairs of computations of doubles.
type R2 = V2 CDouble
-- | Synonym for brevity
type P2 = Point V2 CDouble
-- | Another synonym for brevity (not currently used IIRC)
type T2 = Transformation V2 CDouble

-- | True if the given point is at the origin
origin :: P2 -> CBool 
origin p = fmap (.== (P (V2 0 0))) (evaluate p)

-- | True if the given points are spaced horizontally at the given spacing
spacingX :: CDouble -> [P2] -> CBool 
spacingX sp xs = spacing sp $ map (view _x) xs

-- | True if the given points are spaced vertically at the given spacing
spacingY :: CDouble -> [P2] -> CBool 
spacingY sp xs = spacing sp $ map (view _y) xs

-- | True if the given points are all on the given slope/axis.
alignAxis :: R2 -> [P2] -> CBool 
alignAxis axis xs = spacing 0 $ map project xs -- TODO: allow stuff besides 0
  where
    project (P v) = (axis `dot` v)

-- | True if the given values are spaced with the given spacing
spacing :: CDouble -> [CDouble] -> CBool 
spacing a b = liftA2 spacing' (evaluate a) (evaluate b)
  where
    spacing' sp (x:y:[]) = y - x .== sp
    spacing' sp (x:y:xs) = y - x .== sp &&& spacing' sp (y:xs)
    spacing' _ _ = error "spacing: not enough values"

-- | Constraint type token for the backend
data Constraint = Constraint deriving (Typeable, Eq, Ord, Show)

-- | Constraint is used as a token for generated names
instance IsName Constraint 

-- | Synonym to allow switching backends easily
type B = Constraint

type instance V B = V2
type instance N B = CDouble

-- | Primitive constraint operation. Ensures that the given CBool is true.
newtype ConstraintPrim = ConstraintPrim { getConstraint :: CBool } deriving (Typeable,Show)

type instance V ConstraintPrim = V2 -- todo: ConstraintPrim should take some more type parameters
type instance N ConstraintPrim = CDouble -- todo: ConstraintPrim should take some more type parameters

-- | Constraint primitives do not transform.
instance Transformable ConstraintPrim where
  transform _ = id

-- | Constraint primitives are passed only to the solver.
instance Renderable ConstraintPrim Constraint where
  render Constraint (ConstraintPrim x) = R . modify $ \cs -> cs { comp = x &&& comp cs}

-- | Wrap a boolean computation as a primitive that constraints the boolean to be true
constraint :: CBool -> Prim B V2 CDouble
constraint = Prim . ConstraintPrim

-- | Test rendering primitive. Draws a fixed-size circle containing the given Integer at the given point, while naming the point using the given name (if given).
data Circle v = Circle (Maybe Name) Integer v deriving (Typeable)
type instance V (Circle v) = V v
type instance N (Circle v) = N v
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
      xname = name .> "xb"
      yname = name .> "yb"
      pt = P (V2 xname yname)
      drawCircle xy = moveTo xy $ circle 10 <> (text (show n) # fontSizeO 14)

-- | The computation state during solving/rendering. This is pure state with no instances besides 'Default' due to the 'nameSupply'.
-- | 'comp' is built up to be the constraint computation passed to the solver, while 'rFunc' takes the solver's computed values and produces a diagram.
data CState = CS { comp :: CBool, rFunc :: CPrim'' Double IO (Diagram S.SVG), nameSupply :: Integer }

instance Default CState where
  def = CS empty (pure mempty) 0

instance Backend B V2 CDouble where
  data Render  B V2 CDouble = R { unR :: State CState () }
  type Result  B V2 CDouble = IO (Diagram S.SVG) -- todo: allow other backends besides SVG and use their renderDia rather than just returning a diagram
  data Options B V2 CDouble = Options
     { -- | Adjustments to make to the final diagram before passing it off to the real rendering backend.
       finalAdjustments :: Diagram S.SVG -> Diagram S.SVG
       -- | Options for the real rendering backend (currently unimplemented)
     , renderOpts :: Options S.SVG V2 Double
     }

  renderRTree Constraint opts rt = do
        let R st = toRender rt
            cstate = execState st def
        dia <- runSolver cstate
        return $ finalAdjustments opts dia

instance Default (Options B V2 CDouble) where
  def = Options id undefined

-- | Convert an RTree into the state monad used for rendering
toRender :: Tree (RNode B V2 CDouble Annotation) -> Render B V2 CDouble
toRender (Node (RPrim p) _) = render Constraint p
toRender (Node REmpty rs) = R $ mapM_ (unR . toRender) rs
toRender (Node (RStyle _) rs) = R $ mapM_ (unR . toRender) rs
toRender (Node (RAnnot _) rs) = R $ mapM_ (unR . toRender) rs

-- | Solve the constraints
runSolver :: CState -> Result B V2 CDouble
runSolver (CS (CPrim vars fun) r _) = do
        let varL = Set.elems vars
        result <- satWith z3 $ do
          varMap <- M.fromList <$> flip mapM varL (\n -> do
            var <- free (show n) :: Symbolic SDouble
            return (n,var))
          b <- fromMaybe (pure true) $ runCFunc fun varMap
          return b
        putStrLn (show result)
        let strmodel = getModelDictionary result
            model = M.fromList (zip varL (map (fromCW . fromJust . flip M.lookup strmodel . show) varL)) :: Map Name Double
        putStrLn (show model)
        fromMaybe (pure mempty) $ runCFunc (cFunc r) model
