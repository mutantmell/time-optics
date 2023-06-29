{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Data.Time.Calendar.Optics (
    AsDay (..),
    CalendarDiff (..),
    Date (..),
) where

import Optics

import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format.ISO8601 as TimeIso
import qualified Data.Time.LocalTime as TimeLT

import qualified Data.ByteString as SByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as SText
import qualified Data.Text.Lazy as LText

import qualified Data.Text.Lazy.Optics as LTextOptics
import Data.Text.Optics (unpacked)
import qualified Data.Text.Strict.Optics as STextOptics

-- data Gregorian = Gregorian { year :: !Int, monthOfYear :: !Int, dayOfMonth :: !Int }
--  deriving (Eq, Show, Generic, Ord)

first' :: (a, b, c) -> a
first' (yr, _, _) = yr
second' :: (a, b, c) -> b
second' (_, mn, _) = mn
third' :: (a, b, c) -> c
third' (_, _, dy) = dy

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f (a, b, c) = f a b c
fromGregorian' :: (Calendar.Year, Calendar.MonthOfYear, Calendar.DayOfMonth) -> Calendar.Day
fromGregorian' = uncurry3 Calendar.fromGregorian

dayOfMonth' :: Lens' Calendar.Day Calendar.DayOfMonth
dayOfMonth' = lens (third' . Calendar.toGregorian) update
  where
    update dy dom = dy & Calendar.toGregorian & setDay & fromGregorian'
      where
        setDay (yr, moy, _) = (yr, moy, dom)

monthOfYear' :: Lens' Calendar.Day Calendar.MonthOfYear
monthOfYear' = lens (second' . Calendar.toGregorian) update
  where
    update dy moy = dy & Calendar.toGregorian & setMonth & fromGregorian'
      where
        setMonth (yr, _, dom) = (yr, moy, dom)

year' :: Lens' Calendar.Day Calendar.Year
year' = lens (first' . Calendar.toGregorian) update
  where
    update dy yr = dy & Calendar.toGregorian & setYear & fromGregorian'
      where
        setYear (_, moy, dom) = (yr, moy, dom)

class Date dt where
    {-# MINIMAL day #-}
    day :: Lens' dt Calendar.Day
    dayOfMonth :: Lens' dt Calendar.DayOfMonth
    dayOfMonth = day % dayOfMonth'
    {-# INLINE dayOfMonth #-}
    monthOfYear :: Lens' dt Calendar.MonthOfYear
    monthOfYear = day % monthOfYear'
    {-# INLINE monthOfYear #-}
    year :: Lens' dt Calendar.Year
    year = day % year'
    {-# INLINE year #-}

instance Date Calendar.Day where
    day = castOptic simple
    {-# INLINE day #-}

instance Date Clock.UTCTime where
    day = lens Clock.utctDay (\utct dy -> utct{Clock.utctDay = dy})
    {-# INLINE day #-}

instance Date TimeLT.LocalTime where
    day = lens TimeLT.localDay (\ld dy -> ld{TimeLT.localDay = dy})
    {-# INLINE day #-}

instance Date TimeLT.ZonedTime where
    day =
        lens
            (TimeLT.localDay . TimeLT.zonedTimeToLocalTime)
            ( \zt dy ->
                zt
                    { TimeLT.zonedTimeToLocalTime = zt.zonedTimeToLocalTime{TimeLT.localDay = dy}
                    }
            )
    {-# INLINE day #-}

fromAddTime :: Num n => (n -> s -> t) -> n -> Iso s t t s
fromAddTime f x = iso (f x) (f (negate x))
{-# INLINE fromAddTime #-}

calendarDiffIso :: Lens' a Calendar.Day -> Calendar.CalendarDiffDays -> Iso' a a
calendarDiffIso f = \cd -> iso (addDiff cd) (subDiff cd)
  where
    addDiff = over f . Calendar.addGregorianDurationRollOver
    subDiff = addDiff . Calendar.scaleCalendarDiffDays (-1)
{-# INLINE calendarDiffIso #-}

class (Date cd) => CalendarDiff cd where
  {-# MINIMAL #-}
  addDays :: Integer -> Iso' cd cd
  addDays = fromAddTime (over day . Calendar.addDays)
  {-# INLINE addDays #-}
  subDays :: Integer -> Iso' cd cd
  subDays = re . addDays
  {-# INLINE subDays #-}
  addCalendarDiff :: Calendar.CalendarDiffDays -> Iso' cd cd
  addCalendarDiff = calendarDiffIso day
  {-# INLINE addCalendarDiff #-}
  subCalendarDiff :: Calendar.CalendarDiffDays -> Iso' cd cd
  subCalendarDiff = re . addCalendarDiff
  {-# INLINE subCalendarDiff #-}

instance CalendarDiff Calendar.Day where
  addDays = fromAddTime Calendar.addDays
  {-# INLINE addDays #-}
  addCalendarDiff = calendarDiffIso (castOptic simple)
  {-# INLINE addCalendarDiff #-}

instance CalendarDiff Clock.UTCTime
instance CalendarDiff TimeLT.LocalTime
instance CalendarDiff TimeLT.ZonedTime

class AsDay t where
    {-# MINIMAL _Day #-}
    _Day :: Prism' t Calendar.Day

instance AsDay Calendar.Day where
    _Day = castOptic simple
    {-# INLINE _Day #-}

dayToIntegral :: (Integral a, Integral b, Integral c) => Calendar.Day -> (a, b, c)
dayToIntegral = fmt . Calendar.toGregorian
  where
    fmt (y, m, d) = (fromIntegral y, fromIntegral m, fromIntegral d)

dayFromIntegral :: (Integral a, Integral b, Integral c) => (a, b, c) -> Maybe Calendar.Day
dayFromIntegral (y, m, d) = Calendar.fromGregorianValid (fromIntegral y) (fromIntegral m) (fromIntegral d)

instance (Integral a, Integral b, Integral c) => AsDay (a, b, c) where
    _Day = prism' dayToIntegral dayFromIntegral
    {-# INLINE _Day #-}

instance AsDay String where
    _Day = prism' TimeIso.iso8601Show TimeIso.iso8601ParseM
    {-# INLINE _Day #-}

instance AsDay SText.Text where
    _Day = unpacked % _Day
    {-# INLINE _Day #-}

instance AsDay LText.Text where
    _Day = unpacked % _Day
    {-# INLINE _Day #-}

instance AsDay SByteString.ByteString where
    _Day = STextOptics.utf8 % _Day
    {-# INLINE _Day #-}

instance AsDay LByteString.ByteString where
    _Day = LTextOptics.utf8 % _Day
    {-# INLINE _Day #-}
