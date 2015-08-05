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
       (
         smoothPalette
         -- * Type synonyms

       , Kolor
       , DiscretePalette
       , ContinuousPalette

         -- * Interpolating discrete palettes to continuous ones
         -- ** Configuration for interpolation
       , PaletteExtension, interpolationRange, interpolationSmoothness
       , InterpolationDomain(..), InterpolationKind(..)
         -- ** General interpolation function
       , interpolatePalette

       ) where

import           Data.Colour
import           Data.Colour.SRGB         (RGB(..), toSRGB, sRGB)
import           Data.Colour.RGBSpace.HSV
import           Data.Colour.Names
import           Data.Colour.Palette.Types
import qualified Data.Array as Arr
import           Data.Array ((!))
import           Data.Monoid
import           Data.Default


-- | Shortcut for @'interpolatePalette' 'def'@: cubic interpolation of a
--   finite discrete palette to a colour-valued function on the real numbers.
smoothPalette :: DiscretePalette -> ContinuousPalette
smoothPalette = interpolatePalette def

type DiscretePalette = [Kolor]
type ContinuousPalette = Double -> Kolor

-- | Settings for how to extend a discrete palette to a continuous one.
data PaletteExtension = PaletteExtension InterpolationDomain InterpolationKind

instance Default PaletteExtension where
  def = PaletteExtension def def

-- | @'Lens'' 'PaletteExtension' 'InterpolationDomain'@
interpolationRange :: Functor f => (InterpolationDomain -> f InterpolationDomain)
                                    -> PaletteExtension -> f PaletteExtension
interpolationRange f (PaletteExtension a b) = fmap (\a' -> PaletteExtension a' b) (f a)

-- | @'Lens'' 'PaletteExtension' 'InterpolationKind'@
interpolationSmoothness :: Functor f => (InterpolationKind -> f InterpolationKind)
                                    -> PaletteExtension -> f PaletteExtension
interpolationSmoothness f (PaletteExtension a b) = fmap (\b' -> PaletteExtension a b') (f b)


        
data InterpolationDomain
   = IndexDomain           -- ^ Interpolate between the list elements with the
                           --   two nearest indices. For a list of length /n/,
                           --   the continuation is a function on the domain
                           --   [0, /n/-1] and outside this interval it simply
                           --   gives the outermost colours in the list.
   | NormalisedDomain      -- ^ Cover the entire palette in the interval [0, 1].
   | SymNormalisedDomain   -- ^ Cover the entire palette in the interval [-1, 1].
   | CompleteRealsDomain   -- ^ Spread the palette out to the whole of &#x211d;.
                           --   The outermost colours can then only be reached
                           --   as a limit /x/ &#x2192; &#xb1;&#x221e;.
                           --   This is the default.
   | IndexCyclic           -- ^ Close the palette to a periodic function: after
                           --   the last colour, wrap back to the first one.
                           --   The palette indices still correspond to their
                           --   original colours.
   | NormalisedCyclic      -- ^ Close the palette to a function with
                           --   period 2 &#x22c5; &#x3c0;.


instance Default InterpolationDomain where
  def = CompleteRealsDomain

data InterpolationKind
   = LinearInterpolate   -- ^ Affine interpolation between the two nearest indices
                         --   in the palette. The simplest and fastest interpolation,
                         --   but if the discrete palette is not already pretty smooth
                         --   resolved, the &#x201c;kink&#x201d; color may look a
                         --   bit like a thin stripe in the spectrum.
   | CubicInterpolate    -- ^ Catmull-Rom spline. Should be best for most applications.
                         --   Note that this can possibly leave the colour gamut, but
                         --   apparently not in a harmful way.
                         --   This is the default setting.
   | StepTruncate        -- ^ No interpolation, just use the colour closest to the given index.

instance Default InterpolationKind where
  def = CubicInterpolate


interpolatePalette :: PaletteExtension
                   -> DiscretePalette  -- ^ Must be finite.
                   -> ContinuousPalette
interpolatePalette _ [] = const mempty
interpolatePalette (PaletteExtension IndexDomain StepTruncate) cols = contin
 where cv = Arr.listArray (0,m) cols
       m = length cols - 1
       contin = (cv Arr.!) . min m . max 0 . round
interpolatePalette (PaletteExtension IndexDomain LinearInterpolate) cols = contin
 where cv = Arr.listArray (0,m) cols
       m = length cols - 1
       contin x
        | x <= 0               = cv ! 0
        | x >= fromIntegral m  = cv ! m
        | x' <- floor x        = blend (x - fromIntegral x') (cv ! succ x') (cv ! x')
interpolatePalette (PaletteExtension IndexDomain CubicInterpolate) cols = contin
 where cv = Arr.listArray (-1,m+1) $ [head cols] ++ cols ++ [last cols]
       m = length cols - 1
       contin x
        | x <= 0               = cv ! 0
        | x >= fromIntegral m  = cv ! m
        | x' <- floor x        = let t = x - fromIntegral x'
                                 in affineCombo [ ( t * ((2-t)*t - 1)/2,   cv ! (x'-1) )
                                                , ( (t^2*(3*t - 5) + 2)/2, cv ! x'     )
                                                , ( t * ((4-3*t)*t + 1)/2, cv ! (x'+1) ) ]
                                                                         ( cv ! (x'+2) )
       sdiff (RGB r g b) (RGB r' g' b') = RGB (r'-r) (g'-g) (b'-b)
interpolatePalette (PaletteExtension IndexCyclic intp) cols@(c:_)
   = interpolatePalette (PaletteExtension IndexDomain intp) (last cols:cols++[c]) . (+1) . (`mod'`m)
 where m = fromIntegral $ length cols
interpolatePalette (PaletteExtension NormalisedCyclic intp) cols
   = interpolatePalette (PaletteExtension IndexCyclic intp) cols . (m/2/pi *)
 where m = fromIntegral $ length cols
interpolatePalette (PaletteExtension NormalisedDomain intp) cols
   = interpolatePalette (PaletteExtension IndexDomain intp) cols . ((m-1)*)
 where m = fromIntegral $ length cols
interpolatePalette (PaletteExtension SymNormalisedDomain intp) cols
   = interpolatePalette (PaletteExtension IndexDomain intp) cols . ((m-1)/2*) . (+1)
 where m = fromIntegral $ length cols
interpolatePalette (PaletteExtension CompleteRealsDomain intp) cols
   = interpolatePalette (PaletteExtension SymNormalisedDomain intp) cols . spread
 where spread x = x / (1 + abs x)


mod' :: RealFrac a => a -> a -> a
n`mod'`d | n<0        = d - (-n)`mod'`d
         | otherwise  = d * r
 where (_,r) = properFraction $ n/d


