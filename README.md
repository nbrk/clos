# clos

![LOS](https://www.army-technology.com/wp-content/uploads/sites/3/2017/09/mbtlaw_8.jpg)

Combined Line-of-Sight (LOS) with non-deterministic and deterministic models for
abstract discrete grids.

See the code; four main functions are:

``` haskell
los :: Emplaced Sensor -> Emplaced Emitter -> [Emplaced Obstructor] -> LOS

losProbabilistic :: RandomGen g => Emplaced Sensor -> Emplaced Emitter -> [Emplaced Obstructor] -> g -> LOS

clos :: Emplaced [Sensor] -> Emplaced [Emitter] -> [Emplaced Obstructor] -> CLOS

closProbabilistic :: RandomGen g => Emplaced [Sensor] -> Emplaced [Emitter] -> [Emplaced Obstructor] -> g -> CLOS

```

"Combined" routines (i.e. `clos` and `closProbabilistic`) and `CLOS` models
complex objects with multiple sensors over various energies (visual-sight,
thermal-sight, etc.). When the signal-seeker from a `Sensor` over an `Energy` channel passes through some `Obstructor`s into the `Emitter`, the `SignalStrength` will be greater than zero (each `Obstructor` upon activation reduces signal strength of the seeker). This gives us basis for comparison of `LOS` structures and selection of the best sensor-emitter pair among all functioning combinations (`CLOS` contains **sorted positive sightings**).
