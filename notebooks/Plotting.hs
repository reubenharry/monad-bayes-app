{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


module Plotting where

import Graphics.Vega.VegaLite hiding (length, filter)
import IHaskell.Display.Hvega
import qualified Data.Text as T


barplot (xs, ys) =
    let enc = encoding
                    . position X [ PName "X", PmType Nominal]
                    . position Y [ PName "Y", PmType Quantitative ]
        -- . color cluster
        -- . shape cluster

    -- scaleOpts = [ SType ScLog, SDomain (DNumbers [3.5, 32]), SNice (IsNice False) ]
    -- cluster = [ MName "Year"]

        dat = (dataFromColumns [ ] 
                    . dataColumn "X" (Strings (T.pack . show <$> xs))
                    . dataColumn "Y" (Numbers ys)) []
    --   . dataColumn "Year" (Strings [ "2010", "2014", "2015" ])) []

    in toVegaLite [ 
                dat,
                mark Bar []
                , enc []
                , width 200
                , height 200
                ]

scatterplot (xs, ys) =
    let enc = encoding
                    . position X [ PName "X", PmType Quantitative]
                    . position Y [ PName "Y", PmType Quantitative ]
        -- . color cluster
        -- . shape cluster

    -- scaleOpts = [ SType ScLog, SDomain (DNumbers [3.5, 32]), SNice (IsNice False) ]
    -- cluster = [ MName "Year"]

        dat = (dataFromColumns [ ] 
                    . dataColumn "X" (Numbers xs)
                    . dataColumn "Y" (Numbers ys)) []
    --   . dataColumn "Year" (Strings [ "2010", "2014", "2015" ])) []

    in toVegaLite [ 
                dat,
                mark Circle []
                , enc []
                , width 200
                , height 200
                ]

class Plottable a where
    plotVega :: a -> VegaLiteLab

instance Show a => Plottable [(a, Double)] where
    plotVega ls = vlShow $ barplot $ unzip ls


