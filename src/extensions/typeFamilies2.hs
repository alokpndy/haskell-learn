-- {-# LANGUAGE ConstrainedClassMethods #-} -- Allow a class method's type to place additional constraints on a class type variable.
-- {-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module TypeFamilies2 where


type Position = (Int, Int)

class Player p where
    playerPosition :: p -> Position
    playerMoveTo :: Position -> p -> p

-- class GameState g where
--     getPlayer :: (Player p) => g -> p
--     getMonsterPositions :: g -> [Position]

data PlayerData = PlayerData { pos :: Position }
instance Player PlayerData where
    playerPosition = pos
    playerMoveTo x p = p { pos = x }

{--
data GameStateData = GameStateData PlayerData [Position]
instance GameState GameStateData where
  getPlayer           (GameStateData p _) = p ----Error at p -> Couldn't match expected type ‘p’ with actual type ‘PlayerData’
  getMonsterPositions (GameStateData _ mPoses) = mPoses
--}

-- Using Type Families
class (Player (PlayerType g)) => GameState g where
    type PlayerType g :: *
    getPlayer ::  g -> PlayerType g
    getMonsterPositions :: g  -> [Position]

data GameStateData = GameStateData PlayerData [Position]
instance GameState GameStateData where
     type PlayerType GameStateData = PlayerData
     getPlayer           (GameStateData p _) = p
     getMonsterPositions (GameStateData _ mPoses) = mPoses


checkForCollisions :: GameState s => s -> [Position] -> Bool
checkForCollisions s ps =
                        let
                             p    = getPlayer s
                             pPos = playerPosition p
                        in
                            pPos `elem` ps
