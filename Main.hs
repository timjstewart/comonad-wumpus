module Main where

import Control.Comonad

data Tile = Clean | Dirty | Pit
  deriving Show

data World a = World [a] a [a]
  deriving Show

moveLeft :: World a -> World a
moveLeft w@(World [] _ _) = w
moveLeft (World as b cs) = World (init as) (last as) (b:cs)

moveRight :: World a -> World a
moveRight w@(World _ _ []) = w
moveRight (World as b cs) = World (as++[b]) (head cs) (tail cs)

world0 = World [Dirty, Clean] Dirty [Pit, Dirty, Clean]

instance Functor World where
  fmap f (World as b cs) = World (fmap f as) (f b) (fmap f cs)

instance Comonad World where
    extract (World _ b _) = b
    duplicate w = World [] w []
    extend f (World as b cs) = World (fmap (f . world) as) ((f . world) b) (fmap (f . world) cs) 
                               where world x = World [] x []

cleanTile :: Tile -> Tile
cleanTile Dirty = Clean
cleanTile t = t

main = do
  let w1 = world0
  let w2 = (moveLeft . moveLeft) w1
  let w3 = (moveRight . moveRight) w2
  print w1
  print w2
  print w3
  print $ fmap cleanTile w3
  print $ extract w3
  print $ extend (\(World as b cs) -> cleanTile b) w3
