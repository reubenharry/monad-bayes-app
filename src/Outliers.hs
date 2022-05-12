module Outliers where
import Control.Monad.Bayes.Class 
import Control.Monad.Bayes.Sampler
import System.Random.MWC (initialize, fromSeed)

-- regressionWithOutliersData :: (MonadSample m, Traversable t) => t Double -> m (t (Double, Bool))
-- regressionWithOutliersData xs = do
--     slope <- normal 0 2
--     intercept <- normal 0 2
--     noise <- gamma 1 1
--     prob_outlier <- uniform 0 0.5 

--     let forward x = do
--         is_outlier <- bernoulli prob_outlier
--         let (mu, std) = if is_outlier
--             then (0, 2)
--             else (x*slope + intercept, noise)
--         y <- normal mu std
--         return (y, is_outlier)

--     mapM forward xs

-- t = do
--     seed <- undefined 
--     state <- initialize $ fromSeed seed 
--     sampleIOwith (bernoulli 0.4) undefined

-- countOutliersWithWeight :: [((a, [Bool]), Double)] -> [(Double, Double)]
-- countOutliersWithWeight = foldr 
--     (\((_,lb),w) li -> 
--         [ if b then (num1+w, num2) else (num1,num2+w) | ((b),(num1, num2)) <- zip lb li]) 
--     (Prelude.repeat (0,0))