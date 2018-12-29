module Types where

import           Linear

-- | Type of the information transmission channel
data Energy
  = AllEnergy
  | VisualEnergy
  | ThermalEnergy
  | SoundEnergy
  | OtherEnergy String
  deriving (Eq, Show)


-- | Energy beam strength [0 .. 1]
type SignalStrength = Double


-- | Energy beam reduction value [0 .. 1]
type SignalReduction = Double


-- | Euclidean distance on the discrete grid
type Distance = Double


-- | Probability [0 .. 1]
type Probability = Double


-- | Random value [0 .. 1]
type RandomValue = Double


-- | Active sensor of energy
data Sensor
  = Sensor
  { _sensorName        :: String
  , _sensorEnergy      :: Energy
  , _sensorAttenuation :: Distance -> SignalStrength
  }

instance Show Sensor where
  show s = "Sensor { "
           ++ "_sensorName = " ++ _sensorName s ++ ", "
           ++ "_sensorEnergy = "++(show . _sensorEnergy) s
           ++ " }"


-- | Passive emitter (point) of energy
data Emitter
  = Emitter
  { _emitterName        :: String
  , _emitterEnergy      :: Energy
  , _emitterConcealment :: Distance -> SignalReduction
  }

instance Show Emitter where
  show s = "Emitter { "
           ++ "_emitterName = " ++ _emitterName s ++ ", "
           ++ "_emitterEnergy = "++(show . _emitterEnergy) s
           ++ " }"


-- | Passive probabilistic blocking of the beam
data Obstructor
  = Obstructor
  { _obstructorName        :: String
  , _obstructorEnergy      :: Energy
  , _obstructorProbability :: Probability
  , _obstructorReduction   :: SignalReduction
  } deriving (Show)


-- | Things that are put on the grid and have positions
type Emplaced a = (V2 Int, a)

-- N.B.:
-- instance Comonad ((,) e)
-- instance Semigroup m => ComonadApply ((,) m)


-- | Information about given signal pass-through calc
data LOS =
  LOS
  { _losDistance           :: Distance
  , _losPath               :: [V2 Int]
  , _losBeamEnergy         :: Energy
  , _losCompatibleEnergy   :: Bool
  , _losUnobstructedSignal :: SignalStrength
  , _losSignal             :: SignalStrength
  , _losObstructors        :: [(Emplaced Obstructor, RandomValue)]
  } deriving (Show)


data CLOS =
  CLOS
  { _closDistance      :: Distance
  , _closPath          :: [V2 Int]
--  , _closIndividualSignals :: [(Sensor, Emitter, SignalStrength)]
  , _closAllBeams      :: [(Sensor, Emitter, LOS)]
  , _closPositiveBeams :: [(Sensor, Emitter, LOS)]
  } deriving (Show)

-- data GridActor = GridActor
--   { _gridActorName        :: String
--   , _gridActorSensors     :: [Sensor]
--   , _gridActorEmitters    :: [Emitter]
--   , _gridActorObstructors :: [Obstructor]
--   } deriving (Show)



-- data LOS
--   = LOS
--   { _losObstructors :: [(Obstructor, V2 Int)]
--   , _losDistance    :: Double
--   , _losPath        :: [V2 Int]
--   } deriving (Show)


