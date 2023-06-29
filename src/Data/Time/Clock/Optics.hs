{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Time.Clock.Optics
( TimeDiff (..),
) where

import Optics

import qualified Data.Time.Clock as Clock
import qualified Data.Time.LocalTime as TimeLT

import Data.Time.LocalTime.Optics (LocalTime(..))

fromAddTime :: Num n => (n -> s -> t) -> n -> Iso s t t s
fromAddTime f x = iso (f x) (f (negate x))
{-# INLINE fromAddTime #-}

class (LocalTime td) => TimeDiff td where
  {-# MINIMAL #-}
  addTime :: Clock.NominalDiffTime -> Iso' td td
  addTime = fromAddTime (over localtime . TimeLT.addLocalTime)
  {-# INLINE addTime #-}
  subTime :: Clock.NominalDiffTime -> Iso' td td
  subTime = re . addTime

instance TimeDiff TimeLT.LocalTime where
  addTime = fromAddTime TimeLT.addLocalTime

instance TimeDiff TimeLT.ZonedTime

instance TimeDiff Clock.UTCTime where
  addTime = fromAddTime Clock.addUTCTime
