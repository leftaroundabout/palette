-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Colour.Palette.Continuous
-- Copyright   :  (c) 2015 Justus Sagemüller
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  sagemueller@geo.uni-koeln.de
--
-- Continuous palettes, e.g. for colour schemes of 2D-plots.
--
-----------------------------------------------------------------------------

module Data.Colour.Palette.Continuous
       (-- * Synonym for Colour Double

         Kolor
       , DiscretePalette
       , ContinuousPalette

         -- * Interpolating discrete palettes to continuous ones
       , PaletteInterpolation

       ) where

import           Data.Colour
import           Data.Colour.SRGB         (RGB(..), toSRGB, sRGB)
import           Data.Colour.RGBSpace.HSV
import           Data.Colour.Names
import           Data.Colour.Palette.Types

type DiscretePalette = [Kolor]
type ContinuousPalette = Double -> Kolor

data PaletteInterpolation
   = PaletteIndexLinear   -- ^ Linear interpolation between the two nearest indices in the palette.

interpolatePalette :: PaletteInterpolation -> DiscretePalette -> ContinuousPalette
interpolatePalette PaletteIndexLinear cols = undefined

