{- -*- mode: haskell; coding: utf-8-unix -*-  -}

module Common
  ( dbeta
  , qbeta
  , pbeta
  , line'
  , dropDownTicks )
where

import qualified Numeric.SpecFunctions as N

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

pbeta :: Double -> Double -> Double -> Double
pbeta a b = N.incompleteBeta a b

qbeta :: Double -> Double -> Double -> Double
qbeta a b = N.invIncompleteBeta a b

dbeta :: Double -> Double -> Double -> Double
dbeta a b x = exp $ c + (a - 1) * lp + (b - 1) * lq
  where c = - N.logBeta a b
        lp = log x
        lq = log (1 - x)

line' :: Double -> String -> [[(x,y)]] -> EC l (PlotLines x y)
line' w title values = liftEC $ do
    color <- takeColor
    plot_lines_title .= title
    plot_lines_values .= values
    plot_lines_style . line_width .= w
    plot_lines_style . line_color .= color

dropDownTicks :: AxisData x -> AxisData x
dropDownTicks ad = ad { _axis_ticks = map (\(x,y)->(x,-y)) (_axis_ticks ad) }

-- EOF
