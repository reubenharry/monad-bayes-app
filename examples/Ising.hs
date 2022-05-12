{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Ising where

import Graphics.Gloss.Interface.IO.Simulate (simulateIO, Display (FullScreen))
import Graphics.Gloss (white, Picture (Text, Polygon, Color, Circle, Pictures), simulate, Rectangle (Rectangle), black, red, translate)
import Control.Monad.Bayes.Class (MonadSample(bernoulli, uniformD), factor, MonadInfer)
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Bayes.Traced (mh)
import Control.Monad.Bayes.Weighted
import Data.Functor.Compose (Compose(..))
import qualified Data.Vector as V
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep, ifoldMapRep)
import Data.Functor.Identity (Identity(..))
import Control.Arrow ((***), Arrow (first))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Control.Comonad (Comonad(..))
import Graphics.Gloss.Data.Color (Color)
import Control.Monad (filterM)
import Debug.Trace (trace)
import Data.Monoid (Sum(Sum, getSum))
import Control.Monad.Bayes.Inference.SMC (smcMultinomial)
import Control.Monad.Bayes.Population (runPopulation)

-- The Ising model is a famous model in statistical mechanics. 
-- See the wonderful https://jaan.io/how-does-physics-connect-machine-learning/ 
-- We can express it as a probabilistic program
-- and simulate it by running a Markov Chain forward.

-- The code is heavily inspired by the excellent 
-- https://chrispenner.ca/posts/conways-game-of-life
-- which uses a representable comonad to perform efficient local updates to a grid.
-- The approach would extent easily to similar models with different topologies.


-- comonadic scoring: in each markov blanket: there might be something deep about markov blankets
-- here




------
-- Define a board as a length 20 array of length 20 arrays
-- Define a grid as a board and a pointer to a square
------

-- | Grid a ~ ([[a]], (Int, Int))
type Grid a = Store (Compose VBounded VBounded) a

-- | VBounded a ~ [a]
newtype VBounded a = VBounded (V.Vector a)
  deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
  distribute = distributeRep

-- | size of the Ising grid
gridSize :: Int
gridSize = 20

-- | you can view a grid as a function from indices to square values
-- or as an array of square values.
-- These two structures are isomorphic, as witnessed by: 
-- index . tabulate === id === tabulate . index
instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i = v V.! (i `mod` gridSize)
  tabulate desc = VBounded $ V.generate gridSize desc

-- | make a grid
mkGrid :: [(Int, Int)] -> Grid Bool
mkGrid xs = store (`elem` xs) (0, 0)

-----
-- use the comonadic nature of the grid to apply rules at each square
-----

type Rule = Grid Bool -> Int

-- | comonadically calculate the energy of the grid
interactionEnergy :: Rule
interactionEnergy g = numNeighboursSame
  where
    numNeighboursSame = length (filter (==color) neighbours)
    color = extract g
    neighbours = experiment (\s -> addCoords s <$> neighbourCoords) g
    addCoords (x, y) (x', y') = (x + x', y + y')
    neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]


-- | The prior is uniform over all configurations of the grid
-- We score states based on their energy
model :: MonadInfer m => m (Grid Bool)
model = do
    let allCoords = [(x,y) | x <- [0..19], y <- [0..19]]
    coords <- filterM (const $ bernoulli 0.5) allCoords
    let grid = mkGrid coords
        StoreT (Identity energy) _ = extend interactionEnergy grid
        score  = getSum $ foldMap Sum energy
    factor $ exp $ fromIntegral score
    return grid



---
-- render a grid into ascii
---

display :: Grid Bool -> String
display (StoreT (Identity (Compose grid)) _) = 
  foldMap (\c -> 
    foldMap (\x -> if x then "1" else "0") c <> "\n") grid

samples = do
  s <- sampleIO $ prior $ mh 10 model
  mapM_ (\x -> putStrLn ("\n\n" <> display x)) $ reverse s


---
-- render a grid into a Gloss picture
---

render :: Grid Color -> Picture
render (StoreT (Identity (Compose grid)) _) =
  ifoldMapRep (\l ->
    ifoldMapRep
      (\h c ->
        translate
          (size * fromIntegral l)
          (size * fromIntegral h)
          (Color c square))) grid

size :: Float
size = 10

square :: Picture
square =  Polygon [(0,0), (0,size), (size,size), (size,0)]



main :: IO ()
main = do
    -- i <- foo
    -- print i
    grid <- fmap cycle $ sampleIO $ prior $ mh 10 model

    simulate FullScreen white 20 grid (render . undefined . head) (\_ _ -> tail)
    -- simulateIO FullScreen white 10 (bernoulli 0.5) (\x -> do b <- sampleIO x; return $ Text $ show b) (\_ _ -> return)
