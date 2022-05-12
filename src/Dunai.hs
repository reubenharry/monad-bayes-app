{-# LANGUAGE TupleSections #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dunai where


import Data.MonadicStreamFunction
import Control.Monad.Bayes.Class (MonadSample(random, normal, bernoulli, uniform, uniformD, categorical), MonadInfer, condition, factor, normalPdf)
import Control.Monad.Bayes.Sampler (SamplerIO, sampleIO, sampleIOfixed, sampleSTfixed)
import Control.Monad.Trans.MSF
import Control.Monad.Cont (MonadTrans(lift), MonadIO (liftIO))
import Control.Monad.Identity
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Traced (mh)
-- import FRP.BearRiver (time, SF, Time, ClockInfo)
import qualified Control.Monad.Reader as R
import Graphics.Gloss (Display(..), white, Picture (Circle, Translate, Pictures))
import Graphics.Gloss.Interface.FRP.Yampa
import FRP.Yampa (time, integral, noise, SF, VectorSpace, derivative, Event (NoEvent, Event))
import GHC.Float (double2Float, float2Double)
import System.Random (mkStdGen)
import qualified FRP.Yampa as Y
import Numeric.Log
import GHC.IO (unsafePerformIO)
import Linear (V2(..), Metric (distance), Additive (..), normalize)
import qualified Linear as L
import Graphics.Gloss.Interface.IO.Game (Event(..), Key (Char))
import Control.Monad.Bayes.Population (runPopulation, resampleMultinomial, spawn)
import Control.Lens ((^.))
import Data.List (sortOn)
import Data.Ord (Down(..))
import Control.Monad.Bayes.Enumerator (enumerate, Enumerator, logExplicit)
import Data.Vector (fromList)
import Debug.Trace (traceM)
import qualified Debug.Trace as Debug
import qualified Numeric.Log as Log
-- import qualified FRP.Yampa as Y
-- import FRP.BearRiver

-- random wandering bot: random walk:
    -- discrete
    -- continuous
-- speaking bot?
-- bot that has behavior, but updates behavior given input:
    -- every step, it makes a guess where to go (ideally in a concurrent fashion based on new info)

gl = playYampa FullScreen white 30 sf

-- ran :: SF () Double
ran :: SF a Double
ran = noise (mkStdGen 4)

initialPrior :: MonadSample m => m Double
initialPrior = do
    x <- normal 10 1
    return (x*10)

bae :: SF a (Double -> Double)
bae = Y.loopPre initialPrior
 (arr \(d, knowledge) ->
    let newD = unsafePerformIO $ sampleIO $ stateToAction <$> knowledge
    in (newD, knowledge))
    -- where
        -- newKnowledge = do
        --     x <- knowledge
        --     -- factor $ Exp $ log d
        --     return x

position :: SF a Double
position = arr $ const 0

stateToAction d a = d - a

-- stateToAction :: (Double, Double) -> (Double, Double) -> (Double, Double)
-- stateToAction (rx, ry) (x,y) =
--     let norm = ((x-rx)**2+(y-ry)**2)
--     in (((x-rx))/10, ((y-ry))/10)

dist :: MonadInfer m => m (V2 Double)
dist = do
    x <- uniformD [-300, 300] -- uniform (-2000) 2000
    let y = 200
    return (V2 x y)

ex :: SF (Y.Event InputEvent) (V2 Double)
ex = Y.loopPre (0, dist) $ arr \(ev, (pos@(V2 x' y'), d)) ->
    let newD = case ev of
            NoEvent -> d
            -- Event (EventMotion (x,y)) -> 
            Event (EventKey (Char i) _ _ _) -> do
                k@(V2 a b) <- d
                -- traceM (show a)
                factor (case (a, i) of 
                    (300, 'r') -> 1
                    (-300, 'l') -> 1
                    (-300, 'r') -> 0.9
                    (300, 'l') -> 0.9)
                    -- (300, _) -> 0.1)
                -- factor (Exp $ log $ recip $ abs (float2Double x - x'))
                return k

            _ -> d
    in  (pos,

            (pos + normalize ( (expectation id newD) - pos ), Debug.trace (show $ enumerate $ newD) newD))
                --  (fmap (fst . head. sortOn (Down . snd))) $ runPopulation $  newD) - pos ), newD))

-- expectation :: (a -> Double) -> Enumerator a -> Double
expectation :: (Num (f Double), Functor f) =>
    (t -> f Double) -> Enumerator t -> f Double
expectation f = Prelude.sum . map (\(x, w) -> f x L.^* (exp . ln) w) . normalizeWeights . logExplicit

normaliz :: [Log Double] -> [Log Double]
normaliz xs = map (/ z) xs
  where
    z = Log.sum xs

-- | Divide all weights by their sum.
normalizeWeights :: [(a, Log Double)] -> [(a, Log Double)]
normalizeWeights ls = zip xs ps
  where
    (xs, ws) = unzip ls
    ps = normaliz ws

fromEnumerate :: MonadSample m => [(a, Double)] -> m a
fromEnumerate m = let (as,bs) = unzip m in do
    traceM (show bs)
    i <- categorical $ fromList $ ln . Exp <$> bs
    return (as !! i)

instance VectorSpace (V2 Double) Double where
  zeroVector = 0
  x *^ y = x L.*^ y
  x ^+^ y  = x L.^+^ y
  dot = L.dot

sf = ex >>> toPic

-- sf = (bae <*> position) >>> integral >>> 

toPic :: SF (V2 Double) Picture
toPic = arr \(V2 x y) -> Pictures [
    Translate (double2Float x) (double2Float y) $ Circle 10.0,
    Translate (-300) 200 $ Circle 5.0,
    Translate (300) 200 $ Circle 5.0]

-- toPic :: SF Double Picture
-- toPic = arr (\x -> Translate (double2Float x) (double2Float 0) $ Circle 10.0)

-- sf = ran  >>>
--     arr (\t ->  100 * sin (double2Float (t - 0.5)))
--     >>> integral
--     >>> arr (\t -> Translate t 1 $ Circle 5.0)


-- t :: MonadSample m => SF m () Double
-- t = accumulateWith (const $ id) 0.5 

-- -- f :: MonadSample m => MSF m () Double
-- -- f = t >>> arrM (\x -> do 
-- --         fl <- bernoulli 0.5
-- --         if fl then normal x 0.001 else return x)

-- h :: 
--   ((IO))
--   ()
-- h = sampleIO $ flip runReaderT 0 $ reactimate $ t >>> time >>> arrM (lift . liftIO . print)

-- -- g = sampleIO $ reactimate $ arrM (const $ liftIO (void getLine)) >>> f >>> arrM (liftIO . print)

-- dup x = (x,x)

msf :: MonadInfer m => MSF (MaybeT m) Double Double
msf =
    arrM (lift . model)
    >>> exitWhen (>0.99)

-- m :: IO ()
m = reactimateMaybe
    (constM (lift readLn) >>> morphS (mapMaybeT (fmap head . sampleIOfixed . prior . mh 1000)) msf >>> arrM (lift . print))
    -- (replicate 1000 ())

model :: MonadInfer m => Double -> m Double
model y = do
    x <- normal 0 1
    -- condition (x > y)
    factor $ normalPdf x 0.1 y
    return x

type Observation = Bool
type Action = Bool
type Input = ()

agent :: MSF SamplerIO Observation (Input, Action)
system :: MSF SamplerIO (Input, Action) Observation
agent = arrM (\x -> liftIO getLine >> fmap ((),) (if x then bernoulli 0.9 else bernoulli 0.1) <* liftIO (print x))
system = arrM (\(_, x) -> return x)


whole :: MSF SamplerIO () ()
whole = feedback True (system >>> agent)

run = sampleIO $ reactimate whole

simpler :: IO ()
simpler = reactimate $ feedback (0, ()) (second $ first a) where
    a = arrM (\x -> getLine >> print (x+1) >> return (x+1))

-- (undefined >>> arrM (\x -> return $ x+1) >>> undefined)