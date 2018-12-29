{-# LANGUAGE ScopedTypeVariables #-}
module CLOS where

import           Bresenham
import           Types

import           Control.Comonad
import           Control.Monad
import           Control.Monad.State
import           Data.List           (reverse, sortBy)
import           Data.Maybe
import           Linear
import           System.Random


at :: a -> V2 Int -> Emplaced a
at a pos = (pos, a)


positionOf :: Emplaced a -> V2 Int
positionOf = fst


closProbabilistic :: RandomGen g => Emplaced [Sensor]
                  -> Emplaced [Emitter] -> [Emplaced Obstructor]
                  -> g -> CLOS
closProbabilistic a b c d =
  clos' a b c (Just d)


clos :: Emplaced [Sensor] -> Emplaced [Emitter]
     -> [Emplaced Obstructor] -> CLOS
clos a b c =
  clos' a b c (Nothing :: Maybe StdGen)


clos' :: RandomGen g => Emplaced [Sensor] -> Emplaced [Emitter]
     -> [Emplaced Obstructor] -> Maybe g -> CLOS
clos' ses' ems' obs g = let
  ses = extract ses'
  ems = extract ems'
  sepos = positionOf ses'
  empos = positionOf ems'
  sepairs = [(se, em) | se <- ses, em <- ems]
  path = bresenham sepos empos
  allbs = map
        (\(se, em) ->
           let l = los'
                   (se `at` sepos)
                   (em `at` empos)
                   obs
                   path
                   g
           in
             (se, em, l)
        )
        sepairs
  pstvbs = filter
        (\(s, e, l) -> _losCompatibleEnergy l
                       && _losSignal l > 0
        )
        allbs
  in
    CLOS
    { _closDistance = distance
                      (fmap fromIntegral sepos)
                      (fmap fromIntegral empos)
    , _closPath = path
    , _closAllBeams = allbs
    , _closPositiveBeams = reverse $ sortBy
                           (\(_, _, l1) (_, _, l2) ->
                              losCompare l1 l2
                           )
                           pstvbs
    }



los' :: RandomGen g => Emplaced Sensor
     -> Emplaced Emitter -> [Emplaced Obstructor]
     -> [V2 Int] -> Maybe g -> LOS
los' se em allobs path mbg = let
  sepos = positionOf se
  empos = positionOf em
  d = distance
      (fmap fromIntegral sepos)
      (fmap fromIntegral empos)
  projf f = (f . extract)
  attf = projf _sensorAttenuation se
  concf = projf _emitterConcealment em
  usig = (attf d) - (concf d)
  obs = filter
        (\o -> compatibleEnergy
               (projf _sensorEnergy se)
               (projf _obstructorEnergy o))
        (alongPath path allobs)
  rvals = case mbg of
            Just g  -> randomRs (0, 1) g :: [Double]
            Nothing -> repeat 1
  robs = zip obs rvals
  redsum = foldl
         (\acc (o, rval) ->
            let prob = projf _obstructorProbability o
                red = projf _obstructorReduction o
            in
              if rval >= prob then acc + red else acc
         )
         0
         robs
  osig = usig - redsum
  in
    LOS
    { _losDistance = d
    , _losPath = path
    , _losBeamEnergy = projf _sensorEnergy se
    , _losCompatibleEnergy =
        compatibleEnergy
        (projf _sensorEnergy se)
        (projf _emitterEnergy em)
    , _losUnobstructedSignal = usig
    , _losSignal = osig
    , _losObstructors = robs
    }


losProbabilistic :: RandomGen g => Emplaced Sensor
                 -> Emplaced Emitter -> [Emplaced Obstructor]
                 -> g -> LOS
losProbabilistic a b c d =
  los' a b c path (Just d)
  where
    path = bresenham
           (positionOf a)
           (positionOf b)



los :: Emplaced Sensor -> Emplaced Emitter
    -> [Emplaced Obstructor] -> LOS
los a b c =
  los' a b c path (Nothing :: Maybe StdGen)
  where
    path = bresenham
           (positionOf a)
           (positionOf b)



-- | Filter all `Emplaced a` that lie along the given path
alongPath :: [V2 Int] -> [Emplaced a]
                -> [Emplaced a]
alongPath ps obs = obs'
  where obs' =
          filter
          (\o -> elem (positionOf o) ps)
          obs


-- | Compatibility (i.e. possibility of sense) of two energies
compatibleEnergy :: Energy -> Energy -> Bool
compatibleEnergy AllEnergy _ = True
compatibleEnergy _ AllEnergy = True
compatibleEnergy x y         = x == y


-- | Selects best line of sight (even if the energies don't match)
losCompare :: LOS -> LOS -> Ordering
--losCompare l1 l2 | not $ compatibleEnergy (_losBeamEnergy l1) (_losBeamEnergy l2)
--  = Nothing
losCompare l1 l2 =
  let ((l1pts, l2pts) :: (Int, Int)) =
        flip execState (0, 0) $
        do
          let sc1, sc2 :: Int -> State (Int, Int) ()
              sc1 i = modify (\(p, x) -> (p + i, x))
              sc2 i = modify (\(x, p) -> (x, p + i))
          when (_losDistance l1 < _losDistance l2) (sc1 1)
          when (_losDistance l2 < _losDistance l1) (sc2 1)
          when (length (_losPath l1) < length (_losPath l2)) (sc1 1)
          when (length (_losPath l2) < length (_losPath l1)) (sc2 1)
          when (_losSignal l1 > _losSignal l2) (sc1 100)
          when (_losSignal l2 > _losSignal l1) (sc2 100)
  in
    compare l1pts l2pts
--    if l1pts >= l2pts then l1 else l2


