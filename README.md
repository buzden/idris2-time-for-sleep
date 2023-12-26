<!-- idris
module README

import Control.Monad.State.Interface

import System.Time

%default total
-->

# Time (for sleep)

Some data type for time and finite time duration,
plus an interface for getting the current time and sleeping for some time.

## Idea behind

The main reason why this library happened is to provide a way of overloadable timed sleeping
with potential for non-blocking sleeping for monads that support this.

## Time data

Time cat be represented with two data types: `Time` and `FinDuration`.

One type is for an absolute time, the other is for a distance between two points in time.

There are common operations of creation and querying to these data types and they are put into an interface `TimeValue`.
For external conversions (including creation with integer literals) an `UntypedTime` type is used.

For example:

```idris
timeToWait : FinDuration
timeToWait = 100.millis

hms : TimeValue v => v -> String
hms v = "\{show v.hoursComponent}:\{show v.minutesComponent}:\{show v.secondsComponent}.\{show v.millisComponent}"

conv : UntypedTime
conv = 6.days.asSeconds
```

<!-- idris
main_some_hms : IO Unit
main_some_hms = putStrLn $ hms $ 6.hours <+> 5.seconds

main_conv : IO Unit
main_conv = putStrLn $ show conv
-->

Durations can be added using the semigroup's operation `<+>` and they can be multiplied and divided by a natural number.
Underlying time value is discrete, so beware of rounding when dividing.

```idris
timeToWait' : FinDuration
timeToWait' = timeToWait <+> 12.seconds

twiceAsLong : FinDuration
twiceAsLong = 2 * timeToWait'

aThird : FinDuration
aThird = timeToWait / 3
```

<!-- idris
main_mult_correct : IO Unit
main_mult_correct = putStrLn $ show $ twiceAsLong.asMillis == 2 * timeToWait'.asMillis
-->

Also, duration can be got as a difference between two time points (an absolute value),
and time can be shifted using the duration.

```idris
shiftByTwoSec : Time -> Time
shiftByTwoSec t = t + 2.seconds

passedTime : (start, current : Time) -> FinDuration
passedTime start current = current - start
```

### Interpolation

`Interpolation` interface is implemented for `FinDuration` data type.
So, you can use it for printing meaningful text, say

```idris
errorMessage : FinDuration -> String
errorMessage duration = "Error occurred after \{duration}"
```

<!-- idris
main_errorMessageCorrect : IO Unit
main_errorMessageCorrect = do
  putStrLn $ errorMessage $ 3.minutes <+> 5.seconds
-->

There is a default interpolation which prints period in words, but tries to be short.
Also, some non-default ones also exist, e.g.

```idris
log : FinDuration -> (msg : String) -> IO ()
log d msg = do
  let _ = ISO8601
  putStrLn "\{d}: \{msg}"
```

Then call like

<!-- idris
main_printLog : IO Unit
main_printLog = do {
-->
```idris
log (3.minutes <+> 5.seconds) "start"
```
<!-- idris
 }
-->

would print something like

```console
PT3M5S: start
```

Currently, several interpolation forms exist:

- the default, `Wrds`: `3 min 5 sec`
- `Words`: `3 minutes, 5 seconds`
- `Semicoloned`: `00:03:05`
- `ISO8601` (for periods): `PT3M5S`

## Interfaces

For getting the current time in a context `m`, there is an interface `Timed m` which provides a function `currentTime`.

```idris
sinceStored : MonadState Time m => Timed m => m FinDuration
sinceStored = pure $ !currentTime - !get
```

There is also an interface allowing to sleep for the given duration or to sleep till the desired moment of time:

```idris
printMetered : CanSleep m => HasIO m => FinDuration -> String -> m ()
printMetered d str = do
  putStrLn str
  sleepFor d
  putStrLn str
  sleepFor d
  putStrLn str
```

<!-- idris
main_print_metered : IO Unit
main_print_metered = printMetered 100.millis "lalala"
-->
