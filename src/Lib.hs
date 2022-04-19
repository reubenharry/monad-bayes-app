{-# LANGUAGE OverloadedStrings #-}

module Lib where
import Control.Monad.Bayes.Inference.SMC (smcMultinomial)
import Control.Monad.Bayes.Population (runPopulation)
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Bayes.Class (normalPdf)


-- import System.Random.MWC (createSystemRandom)
-- import System.Random.MWC (createSystemRandom)
import qualified Graphics.Vega.VegaLite as VL hiding (Point)
import IHaskell.Display.Hvega (vlShow)
import Data.Aeson (ToJSON(toJSON), Value)
import Data.Aeson (encode)
-- import qualified Data.ByteString.Lazy.Char8 as BL
-- import Data.Text (Text, pack)

import Control.Monad (liftM2, replicateM, forM, forM_, (<=<))
import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Inference.SMC as SMC
import Control.Monad.Bayes.Inference.RMSMC as RMSMC
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Traced.Static (Traced)
import Control.Monad.Bayes.Inference.SMC

-- import Numeric.Log
import Control.Monad.Bayes.Class

import Data.List (partition)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Population
import Graphics.Vega.VegaLite hiding (Point, density)
import IHaskell.Display.Hvega (vlShow)
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe)
import Pipes (Producer, (>->))
import qualified Pipes as P
import Pipes.Prelude (unfoldr)
import qualified Pipes.Prelude as P
import Debug.Trace (traceM)
import Control.Applicative (Applicative(liftA2))
import qualified Control.Monad.Bayes.Population as PP


-- functional reactive application: guess if you're a bot or not in real time:
    -- GOOD: click on panel to make samples, and have a mixture model run

-- Hamilton: evolve a system forward under a distribution

-- Monte carlo sample to access the ensemble of a lattice or similar

-- parsing

-- lenses:
    -- a lens to set latent variable:

        -- Lens (m Bool) (m Bool)
a :: Producer Bool m ()
a = undefined 

type Action = Char
type State = Char
type Observation = Char

agent :: MonadInfer m => Producer Action m ()
agent = unfoldr step 'b'  

step :: MonadInfer m => State -> m (Either () (Action, State))
step 'a' = return $ Left ()
step oldState = do
    o <- observation oldState
    -- action <- uniformD "abcdefghijklmnopqrstuvwxyz"
 
    inferredState <- inferState o
    -- traceM $ show inferredState

    action <- makeAction inferredState
    -- condition (action == inferredState)
    newState <- progress action oldState
    return $ Right ( action, newState )

progress :: MonadSample m => Action -> State -> m State
progress a s 
    | a==s = return 'a'
    | otherwise = return a

observation :: MonadSample m => State -> m Observation
observation s = do
    f <- bernoulli 0.7
    if f then uniformD "abc" else return s 

makeAction :: Monad m => State -> m Action
makeAction = return 

inferState :: (MonadSample m, MonadCond m) => Observation -> m State
inferState obs = do
    pr <- uniformD "abc"
    o <- observation pr
    condition (o == obs)
    return pr

dup :: b -> (b, b)
dup x = (x,x)

-- f :: (Weighted SamplerIO) Char
-- f = (sampleIO . runWeighted . runPopulation . (spawn 10 >>)) $ P.runEffect (P.hoist resampleSystematic agent >-> P.scan undefined undefined undefined >-> P.drain)

f = (sampleIO . runWeighted . runPopulation . rmsmc 20 20 20 ) $ ( P.fold  (\x y -> x <> [y]) [] id (agent))

-- f' = (enumerate) $ P.runEffect (agent >-> P.drain)


-- g = P.runEffect

type Light = Bool



latentTransition :: MonadSample f => (Double, Double) -> f (Double, Double)
latentTransition (x,y) = liftA2 (,) (normal x 1) (normal y 1)


walk :: MonadSample m => Producer (Double,Double) m r
walk = P.unfoldr step2 (0,0)

resultOfWalk :: MonadSample m => m [(Double, Double)]
resultOfWalk =  P.fold  (\x y -> x <> [y]) [] id (walk >-> P.take 10)

toList :: MonadInfer m => P.Producer a m () -> m [a]
toList prod = P.fold  (\x y -> x <> [y]) [] id (prod >-> P.take 10)

observedWalk :: MonadInfer m => m [(Double, Double, Double, Double)]
observedWalk = toList (walk >-> P.mapM (\(x,y) -> do
    x' <- (normal x 0.1) 
    y' <- (normal y 0.1)
    return (x,y,x',y')))

conditioning :: (MonadSample m, MonadCond m) => P.Producer ((Double,Double), (Double,Double)) m ()
conditioning = P.zip walk (P.each [(0,0)]) >-> P.chain (\((x,y), (x',y')) -> factor (normalPdf x 0.1 x'))

-- smcrmRes = sampleIO $ runPopulation $ rmsmcLocal 10 10 10 (toList conditioning)


step2 s = Right <$> do
    new <- (latentTransition s)
    return (new, new)

newtype Point = Point {toPair :: [(Double,Double)]}

instance Num Point where
    Point p1 + Point p2 = Point (zipWith (\(x,y)  (x',y') -> (x+x',y+y') ) p1 p2)
    fromInteger (_) = undefined


-- aa :: Int -> m [(a, Log Double)]
-- aa :: Sequential (Population (SamplerIO )) a
--     -> m
--         [(a,  Log Double)]
-- aa = sampleIO . runPopulation . sir ((PP.hoist put) . resampleMultinomial) 10 10


-- pr x = do
--     x' <- x
--     tell x'
--     return x'