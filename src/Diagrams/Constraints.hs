{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
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

import           Data.List                   (intercalate, intersperse)
import           Data.Typeable
import           Control.Lens                hiding (transform)
import           Control.Monad.State
import           Diagrams.Core.Transform     (matrixHomRep)
import           Diagrams.Prelude            hiding (Attribute, Render, (<>))
import           Diagrams.TwoD.Path          (getFillRule)
import           Diagrams.TwoD.Text

-- | @Constraint@ primops
data Constraint = Constraint
    deriving (Show, Typeable)

type B = Constraint

{-

instance Backend B R2 where
  data Render  B R2 = R SvgRenderM
  type Result  SVG R2 = S.Svg
  data Options SVG R2 = SVGOptions
                        { _size :: SizeSpec2D Double  -- ^ The requested size.
                        , _svgDefinitions :: Maybe S.Svg
                          -- ^ Custom definitions that will be added to the @defs@
                          --   section of the output.
                        }

  renderRTree _ opts rt = evalState svgOutput initialSvgRenderState
    where
      svgOutput = do
        let R r = toRender rt
            (w,h) = sizePair (opts^.size)
        svg <- r
        return $ R.svgHeader w h (opts^.svgDefinitions) $ svg

  adjustDia c opts d = adjustDia2D size c opts (d # reflectY)
-}

data ConstraintState = ConstraintState { test :: Bool }

makeLenses ''ConstraintState

initialConstraintState :: ConstraintState
initialConstraintState = ConstraintState True

-- | Monad to keep track of state.
type ConstraintM = State ConstraintState B
