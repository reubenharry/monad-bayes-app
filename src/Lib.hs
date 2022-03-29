module Lib where
import Control.Monad.Bayes.Inference.SMC (smcMultinomial)
import Control.Monad.Bayes.Population (runPopulation)
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Bayes.Class (normalPdf)


-- functional reactive application: guess if you're a bot or not in real time:
    -- GOOD: click on panel to make samples, and have a mixture model run

-- Hamilton: evolve a system forward under a distribution

-- Monte carlo sample to access the ensemble of a lattice or similar

-- parsing

-- lenses:
    -- a lens to set latent variable:

        -- Lens (m Bool) (m Bool)

