module InterpolationsCommon

import public System.Time

%default total

interestingValues : List FinDuration
interestingValues =
  [ 5.seconds
  , 5.seconds <+> 3.millis
  , 3.millis
  , 6.days
  , 7.days <+> 3.millis
  , 1.minutes
  , 1.minutes <+> 45.millis
  , 1.minutes <+> 450.millis
  , 1.minutes <+> 999.millis
  , 1.minutes <+> 100.millis
  , 1000.millis
  , 5.hours <+> 17.minutes
  , 5.hours <+> 10.minutes
  , 5.hours <+> 7.minutes
  , 10.hours <+> 40.minutes
  , 15.hours <+> 7.minutes
  , 15.hours <+> 17.minutes
  , 25.hours <+> 17.minutes
  , 25.hours <+> 17.seconds
  , 25.hours <+> 7.seconds
  , 25.hours <+> 17.millis
  , 25.hours <+> 170.millis
  , 25.hours <+> 5.seconds <+> 17.millis
  , 25.hours <+> 5.seconds <+> 17.millis <+> 1.days
  ]

export
runPrints : Interpolation FinDuration => IO ()
runPrints = for_ interestingValues $ \d => putStrLn "\{d}"
