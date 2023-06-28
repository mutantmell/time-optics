{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Time.LocalTime.Optics (
    AsTimeOfDay (..),
    AsLocalTime (..),
    AsZonedTime (..),
    AsUtcTime (..),
    TimeOfDay (..),
    TimeZone (..),
    LocalTime (..),
    ZonedTime (..),
    localTimeUtc,
    utcZonedTime,
) where

import Data.Time.Calendar.Optics (Date (..))
import Optics

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

import Data.Fixed (Pico)
import Data.Ord (clamp)

picoPart :: Pico -> Pico
picoPart p = p - (fromIntegral @Int $ round p)

whole' :: Lens' Pico Int
whole' = lens round (\pc sc -> fromIntegral sc + picoPart pc)

hours' :: Lens' TimeLT.TimeOfDay Int
hours' = lens TimeLT.todHour (\tod hr -> tod{TimeLT.todHour = hr & clamp (0, 23)})
minutes' :: Lens' TimeLT.TimeOfDay Int
minutes' = lens TimeLT.todMin (\tod mn -> tod{TimeLT.todMin = mn & clamp (0, 59)})
seconds' :: Lens' TimeLT.TimeOfDay Pico
seconds' = lens TimeLT.todSec (\tod po -> tod{TimeLT.todSec = po & clamp (0, 60.999999999999)})

frac :: Int -> Lens' Pico Int
frac n = lens toFrac setFrac
  where
    toFrac pico = truncate (pico * fromIntegral n) `rem` n
    clampFrac = clamp (0, fromIntegral $ n - 1)
    setFrac pico v = whole + part
      where
        whole = fromIntegral @Integer (round pico)
        part = (fromIntegral (clampFrac v) * 1 / fromIntegral n)

millis' :: Lens' Pico Int
millis' = frac 1_000
micros' :: Lens' Pico Int
micros' = frac 1_000_000
nanos' :: Lens' Pico Int
nanos' = frac 1_000_000_000
picos' :: Lens' Pico Int
picos' = frac 1_000_000_000_000

dtlt :: Iso' Clock.DiffTime TimeLT.TimeOfDay
dtlt = iso TimeLT.pastMidnight TimeLT.sinceMidnight

class TimeOfDay tod where
    {-# MINIMAL timeOfDay | diffTime #-}
    timeOfDay :: Lens' tod TimeLT.TimeOfDay
    timeOfDay = diffTime % dtlt
    diffTime :: Lens' tod Clock.DiffTime
    diffTime = timeOfDay % re dtlt
    hours :: Lens' tod Int
    hours = timeOfDay % hours'
    minutes :: Lens' tod Int
    minutes = timeOfDay % minutes'
    seconds :: Lens' tod Pico
    seconds = timeOfDay % seconds'
    wholeSeconds :: Lens' tod Int
    wholeSeconds = seconds % whole'
    milliseconds :: Lens' tod Int
    milliseconds = seconds % millis'
    microseconds :: Lens' tod Int
    microseconds = seconds % micros'
    nanoseconds :: Lens' tod Int
    nanoseconds = seconds % nanos'
    picoseconds :: Lens' tod Int
    picoseconds = seconds % picos'

instance TimeOfDay TimeLT.TimeOfDay where
    timeOfDay = castOptic simple
    diffTime = castOptic (re dtlt)
    hours = hours'
    minutes = minutes'
    seconds = seconds'
    wholeSeconds = seconds % whole'
    milliseconds = seconds % millis'
    microseconds = seconds % micros'
    nanoseconds = seconds % nanos'
    picoseconds = seconds % picos'

instance TimeOfDay Clock.DiffTime where
    timeOfDay = castOptic dtlt
    diffTime = castOptic simple

utctDiffTime :: Lens' Clock.UTCTime Clock.DiffTime
utctDiffTime = lens Clock.utctDayTime (\utct dt -> utct{Clock.utctDayTime = dt})

instance TimeOfDay Clock.UTCTime where
    timeOfDay = utctDiffTime % timeOfDay
    diffTime = castOptic utctDiffTime

instance TimeOfDay TimeLT.LocalTime where
    timeOfDay = lens TimeLT.localTimeOfDay (\lt tod -> lt{TimeLT.localTimeOfDay = tod})

instance TimeOfDay TimeLT.ZonedTime where
    timeOfDay = localtime % timeOfDay

class TimeZone tz where
    {-# MINIMAL timezone #-}
    type Converted tz
    timezone :: Lens tz (Converted tz) TimeLT.TimeZone TimeLT.TimeZone

instance TimeZone TimeLT.TimeZone where
    type Converted TimeLT.TimeZone = TimeLT.TimeZone
    timezone = castOptic simple

instance TimeZone Clock.UTCTime where
    type Converted Clock.UTCTime = TimeLT.ZonedTime
    timezone = lens (const TimeLT.utc) (flip TimeLT.utcToZonedTime)

instance TimeZone TimeLT.ZonedTime where
    type Converted TimeLT.ZonedTime = TimeLT.ZonedTime
    timezone = lens TimeLT.zonedTimeZone (\zt tz -> zt{TimeLT.zonedTimeZone = tz})

class (TimeOfDay lt, Date lt) => LocalTime lt where
    {-# MINIMAL #-}
    localtime :: Lens' lt TimeLT.LocalTime
    localtime = lens getter setter
      where
        getter lt = TimeLT.LocalTime (lt ^. day) (lt ^. timeOfDay)
        setter v lt =
            v
                & day .~ (lt ^. day)
                & timeOfDay .~ (lt ^. timeOfDay)

instance LocalTime Clock.UTCTime
instance LocalTime TimeLT.LocalTime where
    localtime = castOptic simple
instance LocalTime TimeLT.ZonedTime where
    localtime = lens TimeLT.zonedTimeToLocalTime (\zt lt -> zt{TimeLT.zonedTimeToLocalTime = lt})

class (LocalTime zt, TimeZone zt) => ZonedTime zt where
    {-# MINIMAL #-}
    zonedtime :: Lens zt (Converted zt) TimeLT.ZonedTime TimeLT.ZonedTime
    zonedtime = lens getter setter
      where
        getter lt = TimeLT.ZonedTime (lt ^. localtime) (lt ^. getting timezone)
        setter v lt =
            v
                & localtime .~ (lt ^. localtime)
                & timezone .~ (lt ^. timezone)

instance ZonedTime Clock.UTCTime
instance ZonedTime TimeLT.ZonedTime where
    zonedtime = castOptic simple

localTimeUtc :: Iso' TimeLT.LocalTime Clock.UTCTime
localTimeUtc = iso toUtc fromUtc
  where
    toUtc lt = Clock.UTCTime (lt & TimeLT.localDay) (lt & TimeLT.localTimeOfDay & TimeLT.sinceMidnight)
    fromUtc utct = TimeLT.LocalTime (utct & Clock.utctDay) (utct & Clock.utctDayTime & TimeLT.pastMidnight)

utcZonedTime :: Iso' TimeLT.ZonedTime (TimeLT.TimeZone, Clock.UTCTime)
utcZonedTime = iso toUtc fromUtc
  where
    toUtc zt = (zt ^. timezone, zt & TimeLT.zonedTimeToUTC)
    fromUtc (tz, utct) = TimeLT.utcToZonedTime tz utct

class AsTimeOfDay t where
    {-# MINIMAL _TimeOfDay #-}
    _TimeOfDay :: Prism' t TimeLT.TimeOfDay

instance AsTimeOfDay TimeLT.TimeOfDay where
    _TimeOfDay = castOptic simple

instance AsTimeOfDay String where
    _TimeOfDay = prism' TimeIso.iso8601Show TimeIso.iso8601ParseM

instance AsTimeOfDay SText.Text where
    _TimeOfDay = unpacked % _TimeOfDay

instance AsTimeOfDay LText.Text where
    _TimeOfDay = unpacked % _TimeOfDay

instance AsTimeOfDay Clock.DiffTime where
    _TimeOfDay = castOptic dtlt

class AsLocalTime t where
    {-# MINIMAL _LocalTime #-}
    _LocalTime :: Prism' t TimeLT.LocalTime

instance AsLocalTime TimeLT.LocalTime where
    _LocalTime = castOptic simple

instance AsLocalTime Clock.UTCTime where
    _LocalTime = castOptic (re localTimeUtc)

instance AsLocalTime String where
    _LocalTime = prism' TimeIso.iso8601Show TimeIso.iso8601ParseM

instance AsLocalTime SText.Text where
    _LocalTime = unpacked % _LocalTime

instance AsLocalTime LText.Text where
    _LocalTime = unpacked % _LocalTime

instance AsLocalTime SByteString.ByteString where
    _LocalTime = STextOptics.utf8 % _LocalTime

instance AsLocalTime LByteString.ByteString where
    _LocalTime = LTextOptics.utf8 % _LocalTime

class (AsLocalTime t) => AsUtcTime t where
    {-# MINIMAL _UtcTime #-}
    _UtcTime :: Prism' t Clock.UTCTime

instance AsUtcTime Clock.UTCTime where
    _UtcTime = castOptic simple

instance AsUtcTime TimeLT.LocalTime where
    _UtcTime = castOptic localTimeUtc

instance AsUtcTime String where
    _UtcTime = prism' TimeIso.iso8601Show TimeIso.iso8601ParseM

instance AsUtcTime SText.Text where
    _UtcTime = unpacked % _UtcTime

instance AsUtcTime LText.Text where
    _UtcTime = unpacked % _UtcTime

instance AsUtcTime SByteString.ByteString where
    _UtcTime = STextOptics.utf8 % _UtcTime

instance AsUtcTime LByteString.ByteString where
    _UtcTime = LTextOptics.utf8 % _UtcTime

class AsZonedTime t where
    {-# MINIMAL _ZonedTime #-}
    _ZonedTime :: Prism' t TimeLT.ZonedTime

instance AsZonedTime TimeLT.ZonedTime where
    _ZonedTime = castOptic simple

instance AsZonedTime String where
    _ZonedTime = prism' TimeIso.iso8601Show TimeIso.iso8601ParseM

instance AsZonedTime SText.Text where
    _ZonedTime = unpacked % _ZonedTime

instance AsZonedTime LText.Text where
    _ZonedTime = unpacked % _ZonedTime

instance AsZonedTime SByteString.ByteString where
    _ZonedTime = STextOptics.utf8 % _ZonedTime

instance AsZonedTime LByteString.ByteString where
    _ZonedTime = LTextOptics.utf8 % _ZonedTime
