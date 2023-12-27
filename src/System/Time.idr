module System.Time

import Control.Monad.Trans

import public Data.Fin
import public Data.Nat
import Data.String

import System       -- for `sleep` for `IO` implementations
import System.Clock -- for `clockTime` for `IO` implementations

%default total

----------------------------------------------------------
--- Common stuff for absolute time and finite duration ---
----------------------------------------------------------

public export %inline
UntypedTime : Type
UntypedTime = Nat

export
interface TimeValue a where
  (.millis)  : UntypedTime -> a
  (.seconds) : UntypedTime -> a
  (.minutes) : UntypedTime -> a
  (.hours)   : UntypedTime -> a
  (.days)    : UntypedTime -> a

  -- back conversions may lose data
  (.asMillis)  : a -> UntypedTime
  (.asSeconds) : a -> UntypedTime
  (.asMinutes) : a -> UntypedTime
  (.asHours)   : a -> UntypedTime
  (.asDays)    : a -> UntypedTime

  -- appropriate components when we represent this time value as `DD HH:MM:SS.ms`
  (.millisComponent)  : a -> Fin 1000
  (.secondsComponent) : a -> Fin 60
  (.minutesComponent) : a -> Fin 60
  (.hoursComponent)   : a -> Fin 24
  (.daysComponent)    : a -> Nat

  x.seconds = (1000 * x).millis
  x.minutes = (60 * x).seconds
  x.hours   = (60 * x).minutes
  x.days    = (24 * x).hours

  x.asSeconds = divNatNZ x.asMillis  1000 SIsNonZero
  x.asMinutes = divNatNZ x.asSeconds 60   SIsNonZero
  x.asHours   = divNatNZ x.asMinutes 60   SIsNonZero
  x.asDays    = divNatNZ x.asHours   24   SIsNonZero

  x.millisComponent  = restrict _ $ cast x.asMillis
  x.secondsComponent = restrict _ $ cast x.asSeconds
  x.minutesComponent = restrict _ $ cast x.asMinutes
  x.hoursComponent   = restrict _ $ cast x.asHours
  x.daysComponent    = x.asDays

---------------------
--- Absolute time ---
---------------------

-- Well, absolute time is anyway a finite duration from some point in past...
namespace AbsTime

  export
  record Time where
    constructor MkTime
    millis : UntypedTime

  export
  TimeValue Time where
    (.millis)   = MkTime
    (.asMillis) = millis

  export
  Eq Time where
    (==) = (==) `on` (.asMillis)

  export
  Ord Time where
    compare = compare `on` (.asMillis)

----------------------------
--- Finite time duration ---
----------------------------

namespace FiniteDuration

  export
  record FinDuration where
    constructor MkFinDuration
    millis : UntypedTime

  export
  TimeValue FinDuration where
    (.millis) = MkFinDuration
    (.asMillis) = millis

  export %defaulthint
  DefaultTimeValue : TimeValue FinDuration
  DefaultTimeValue = %search

  export
  Eq FinDuration where
    (==) = (==) `on` (.asMillis)

  export
  Ord FinDuration where
    compare = compare `on` (.asMillis)

  export
  Semigroup FinDuration where
    (<+>) = MkFinDuration .: (+) `on` (.asMillis)

  export
  Monoid FinDuration where
    neutral = MkFinDuration 0

  export
  (*) : Nat -> FinDuration -> FinDuration
  n * d = (n * d.asMillis).millis

  -- May lose data! `z * (d / z)` may be less than `d`
  export
  (/) : FinDuration -> (z : Nat) -> (0 _ : NonZero z) => FinDuration
  d / z = (divNatNZ d.asMillis z %search).millis

  -- Gives a zero length duration if longer duration is subtracted from a smaller one
  export
  (-) : FinDuration -> FinDuration -> FinDuration
  x - y = (x.asMillis `minus` y.asMillis).millis

------------------------------------------------------------
--- Operations between absolute time and finite duration ---
------------------------------------------------------------

-- This is an absolute difference. `y + (x - y)` may be more than `x` if `x < y`
export
(-) : Time -> Time -> FinDuration
x - y = ((x `max` y).asMillis `minus` (x `min` y).asMillis).millis

export
(+) : Time -> FinDuration -> Time
t + d = (t.asMillis + d.asMillis).millis

----------------------
--- Interpolations ---
----------------------

timeComponent : Cast v Nat => v -> (pre, descSg, descPl : String) -> List String
timeComponent d pre descSg descPl = let d : Nat := cast d in if d == 0 then [] else
  ["\{pre}\{show d}\{if d == 1 then descSg else descPl}"]

joinOr : (ifEmpty : String) -> List (List String) -> List String
joinOr ifEmpty [] = [ ifEmpty ]
joinOr _       xs = join xs

ifNotNull : (List a -> List a) -> List a -> List a
ifNotNull _ [] = []
ifNotNull f xs = f xs

stripFinZeros : String -> String
stripFinZeros = pack . reverse . dropWhile (== '0') . reverse . unpack

namespace FiniteDuration

  export
  [Wrds] Interpolation FinDuration where
    interpolate tv = joinBy " " $ joinOr "0 sec"
      [ timeComponent tv.daysComponent    "" " day"  " days"
      , timeComponent tv.hoursComponent   "" " hr"   " hr"
      , timeComponent tv.minutesComponent "" " min"  " min"
      , timeComponent tv.secondsComponent "" " sec"  " sec"
      , timeComponent tv.millisComponent  "" " ms"   " ms"
      ]

  export
  [Words] Interpolation FinDuration where
    interpolate tv = joinBy ", " $ joinOr "0 seconds"
      [ timeComponent tv.daysComponent    "" " day"         " days"
      , timeComponent tv.hoursComponent   "" " hour"        " hours"
      , timeComponent tv.minutesComponent "" " minute"      " minutes"
      , timeComponent tv.secondsComponent "" " second"      " seconds"
      , timeComponent tv.millisComponent  "" " millisecond" " milliseconds"
      ]

  export
  [Semicoloned] Interpolation FinDuration where
    interpolate tv = concat $ do
      let s  = tv.secondsComponent
          ms = tv.millisComponent
      timeComponent tv.daysComponent "" "d" "d" ++
        [ padLeft 2 '0' $ show tv.hoursComponent
        , ":"
        , padLeft 2 '0' $ show tv.minutesComponent
        ] ++ if s == 0 && ms == 0 then [] else
          ":" :: padLeft 2 '0' (show s) :: (("." ++) . padLeft 3 '0' <$> timeComponent ms "" "" "")

  export
  [ISO8601] Interpolation FinDuration where
    interpolate d = do
      let date = [ timeComponent d.daysComponent "" "D" "D" ]
      let time = ifNotNull (["T"]::)
                   [ timeComponent d.hoursComponent   "" "H" "H"
                   , timeComponent d.minutesComponent "" "M" "M"
                   , let s  = d.secondsComponent
                         ms = d.millisComponent
                     in if s == 0 && ms == 0 then [] else
                          show s :: (("." ++) . stripFinZeros . padLeft 3 '0'<$> timeComponent ms "" "" "") ++ ["S"]
                   ]
      strCons 'P' . concat . joinOr "T0S" $ date ++ time

  export %defaulthint %inline
  DefaultFinDurationInterpolation : Interpolation FinDuration
  DefaultFinDurationInterpolation = Wrds

------------------
--- Interfaces ---
------------------

public export
interface Timed m where
  currentTime : m Time

public export
interface Timed m => Monad m => CanSleep m where
  sleepTill : Time -> m Unit
  sleepTill t = sleepFor $ t - !currentTime

  sleepFor : FinDuration -> m Unit
  sleepFor d = sleepTill $ !currentTime + d

----------------------------------------
--- Implementations for `MonadTrans` ---
----------------------------------------

namespace Timed

  export
  [Trans] Timed m => MonadTrans t => Monad m => Timed (t m) where
    currentTime = lift currentTime

namespace CanSleep

  export
  [Trans] CanSleep m => MonadTrans t => Monad m => Monad (t m) => CanSleep (t m) using Timed.Trans where
    sleepFor  = lift . sleepFor
    sleepTill = lift . sleepTill

--------------------------------
--- Implementations for `IO` ---
--------------------------------

namespace Timed

  export
  [HasIO] HasIO io => Timed io where
    currentTime = liftIO $ (.millis) . fromInteger . millisOfClock <$> clockTime UTC where
      millisOfClock : Clock _ -> Integer
      millisOfClock (MkClock secs nanos) = secs * 1000 + nanos `div` 1000000

namespace CanSleep

  export
  [HasIO] HasIO io => CanSleep io using Timed.HasIO where
    sleepFor d = do
      sleep $ cast d.asSeconds
      let (msComp ** _) = toIntWithPrf d.millisComponent
      usleep msComp
      where
        %inline
        toIntWithPrf : Fin 1000 -> (x : Int ** So (x >= 0))
        toIntWithPrf k = (cast $ finToNat k ** believe_me {- we are converting from `Nat` -} Oh)

export
Timed IO where
  currentTime = currentTime @{HasIO}

export
CanSleep IO where
  sleepFor = sleepFor @{HasIO}
