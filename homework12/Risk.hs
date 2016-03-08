{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show


casualty :: DieValue -> DieValue -> (Army, Army)
casualty att def
  | att > def = (0,1)
  | otherwise = (1,0)

casualties :: [DieValue] -> [DieValue] -> (Army, Army)
casualties attacks defenses = (attackers_dead, defenders_dead)
  where attackers_dead = sum $ map fst results
        defenders_dead = sum $ map snd results
        results = zipWith casualty attacks defenses

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
            attacks  <- liftM (reverse . sort) $ replicateM n_attacks die
            defenses <- liftM (reverse . sort) $ replicateM n_defenses die
            return $ Battlefield (attackers bf - fst (casualties attacks defenses)) (defenders bf - snd (casualties attacks defenses))
  where n_attacks  = min 3 $ max 0 (attackers bf - 1)
        n_defenses = min 2 (defenders bf)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf
    | defenders bf == 0 = return bf
    | attackers bf  < 2 = return bf
    | otherwise         = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = liftM ((/1000) . victories) $ replicateM 1000 $ invade bf
  where attackersWon x = defenders x == 0
        victories      = fromIntegral . length . filter attackersWon
