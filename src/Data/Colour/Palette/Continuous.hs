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
       (-- * Type synonyms

         Kolor
       , DiscretePalette
       , ContinuousPalette

         -- * Interpolating discrete palettes to continuous ones
       , PaletteInterpolation(..)
       , interpolatePalette

       ) where

import           Data.Colour
import           Data.Colour.SRGB         (RGB(..), toSRGB, sRGB)
import           Data.Colour.RGBSpace.HSV
import           Data.Colour.Names
import           Data.Colour.Palette.Types
import qualified Data.Array as Arr

type DiscretePalette = [Kolor]
type ContinuousPalette = Double -> Kolor

data PaletteInterpolation
   = PaletteIndexLinear   -- ^ Linear interpolation between the two nearest indices
                          --   in the palette. The simplest and fastest interpolation,
                          --   but generally only looks good if the discrete palette
                          --   is already pretty smooth resolved.

interpolatePalette :: PaletteInterpolation -> DiscretePalette -> ContinuousPalette
interpolatePalette PaletteIndexLinear cols = contin
 where cv = Arr.listArray (0,m) cols
       m = length cols - 1
       contin x
        | x <= 0               = cv Arr.! 0
        | x >= fromIntegral m  = cv Arr.! m
        | x' <- floor x        = blend (x - fromIntegral x') (cv Arr.! succ x') (cv Arr.! x')

