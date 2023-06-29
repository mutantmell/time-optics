{-# LANGUAGE FlexibleInstances #-}

module Data.Time.Format.Optics (
    TimeFormat (..),
    calendarFormat,
    yearMonthFormat,
    yearFormat,
    centuryFormat,
    ordinalDateFormat,
    weekDateFormat,
    yearWeekFormat,
    timeOfDayFormat,
    hourMinuteFormat,
    hourFormat,
    timeOffsetFormat,
    timeOfDayAndOffsetFormat,
    localTimeFormat,
    zonedTimeFormat,
    utcTimeFormat,
    rfc822DateFormat,
) where

import Optics

import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeF
import qualified Data.Time.Format.ISO8601 as TimeIso
import qualified Data.Time.LocalTime as TimeLT

import qualified Data.ByteString as SByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as SText
import qualified Data.Text.Lazy as LText

import qualified Data.Text.Lazy.Optics as LTextOptics
import Data.Text.Optics (unpacked)
import qualified Data.Text.Strict.Optics as STextOptics

import Data.Time.LocalTime.Optics (ZonedTime)

class TimeFormat f where
    {-# MINIMAL iso8601, isoformat, timeformat' #-}
    iso8601 :: (TimeIso.ISO8601 t) => Prism' f t
    isoformat :: TimeIso.Format t -> Prism' f t
    timeformat' ::
        (TimeF.FormatTime t, TimeF.ParseTime t) =>
        TimeF.TimeLocale ->
        String ->
        Prism' f t
    timeformat ::
        (TimeF.FormatTime t, TimeF.ParseTime t) =>
        String ->
        Prism' f t
    timeformat = timeformat' TimeF.defaultTimeLocale
    {-# INLINE timeformat #-}

instance TimeFormat String where
    iso8601 = prism' TimeIso.iso8601Show TimeIso.iso8601ParseM
    {-# INLINE iso8601 #-}
    isoformat fmt = prism' (TimeIso.formatShow fmt) (TimeIso.formatParseM fmt)
    {-# INLINE isoformat #-}
    timeformat' loc fmt = prism' (TimeF.formatTime loc fmt) (TimeF.parseTimeM False loc fmt)
    {-# INLINE timeformat' #-}

instance TimeFormat SText.Text where
    iso8601 = unpacked % iso8601
    {-# INLINE iso8601 #-}
    isoformat fmt = unpacked % isoformat fmt
    {-# INLINE isoformat #-}
    timeformat' loc fmt = unpacked % timeformat' loc fmt
    {-# INLINE timeformat' #-}

instance TimeFormat LText.Text where
    iso8601 = unpacked % iso8601
    {-# INLINE iso8601 #-}
    isoformat fmt = unpacked % isoformat fmt
    {-# INLINE isoformat #-}
    timeformat' loc fmt = unpacked % timeformat' loc fmt
    {-# INLINE timeformat' #-}

instance TimeFormat SByteString.ByteString where
    iso8601 = STextOptics.utf8 % iso8601
    {-# INLINE iso8601 #-}
    isoformat fmt = STextOptics.utf8 % isoformat fmt
    {-# INLINE isoformat #-}
    timeformat' loc fmt = STextOptics.utf8 % timeformat' loc fmt
    {-# INLINE timeformat' #-}

instance TimeFormat LByteString.ByteString where
    iso8601 = LTextOptics.utf8 % iso8601
    {-# INLINE iso8601 #-}
    isoformat fmt = LTextOptics.utf8 % isoformat fmt
    {-# INLINE isoformat #-}
    timeformat' loc fmt = LTextOptics.utf8 % timeformat' loc fmt
    {-# INLINE timeformat' #-}

calendarFormat :: (TimeFormat f) => Prism' f Calendar.Day
calendarFormat = isoformat (TimeIso.calendarFormat TimeIso.ExtendedFormat)

yearMonthFormat :: (TimeFormat f) => Prism' f (Integer, Int)
yearMonthFormat = isoformat TimeIso.yearMonthFormat

yearFormat :: (TimeFormat f) => Prism' f Integer
yearFormat = isoformat TimeIso.yearFormat

centuryFormat :: (TimeFormat f) => Prism' f Integer
centuryFormat = isoformat (TimeIso.centuryFormat)

ordinalDateFormat :: (TimeFormat f) => Prism' f Calendar.Day
ordinalDateFormat = isoformat (TimeIso.ordinalDateFormat TimeIso.ExtendedFormat)

weekDateFormat :: (TimeFormat f) => Prism' f Calendar.Day
weekDateFormat = isoformat (TimeIso.weekDateFormat TimeIso.ExtendedFormat)

yearWeekFormat :: (TimeFormat f) => Prism' f (Integer, Int)
yearWeekFormat = isoformat (TimeIso.yearWeekFormat TimeIso.ExtendedFormat)

timeOfDayFormat :: (TimeFormat f) => Prism' f TimeLT.TimeOfDay
timeOfDayFormat = isoformat (TimeIso.timeOfDayFormat TimeIso.ExtendedFormat)

hourMinuteFormat :: (TimeFormat f) => Prism' f TimeLT.TimeOfDay
hourMinuteFormat = isoformat (TimeIso.hourMinuteFormat TimeIso.ExtendedFormat)

hourFormat :: (TimeFormat f) => Prism' f TimeLT.TimeOfDay
hourFormat = isoformat TimeIso.hourFormat

timeOffsetFormat :: (TimeFormat f) => Prism' f TimeLT.TimeZone
timeOffsetFormat = isoformat (TimeIso.timeOffsetFormat TimeIso.ExtendedFormat)

timeOfDayAndOffsetFormat :: (TimeFormat f) => Prism' f (TimeLT.TimeOfDay, TimeLT.TimeZone)
timeOfDayAndOffsetFormat = isoformat (TimeIso.timeOfDayAndOffsetFormat TimeIso.ExtendedFormat)

localTimeFormat :: (TimeFormat f) => Prism' f TimeLT.LocalTime
localTimeFormat = isoformat (TimeIso.localTimeFormat TimeIso.iso8601Format TimeIso.iso8601Format)

zonedTimeFormat :: (TimeFormat f) => Prism' f TimeLT.ZonedTime
zonedTimeFormat = isoformat (TimeIso.zonedTimeFormat TimeIso.iso8601Format TimeIso.iso8601Format TimeIso.ExtendedFormat)

utcTimeFormat :: (TimeFormat f) => Prism' f Clock.UTCTime
utcTimeFormat = isoformat (TimeIso.utcTimeFormat TimeIso.iso8601Format TimeIso.iso8601Format)

rfc822DateFormat :: (TimeFormat f, TimeF.ParseTime zt, TimeF.FormatTime zt, ZonedTime zt) => Prism' f zt
rfc822DateFormat = timeformat TimeF.rfc822DateFormat
