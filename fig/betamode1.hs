{- -*- mode: haskell; coding: utf-8-unix -*-  -}

module Main where

import Common

import Numeric (showFFloat)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

curve :: Int -> Double -> [(Double,Double)]
curve n j = map f [0..n]
  where h = 1 / fromIntegral n
        g = dbeta (j+0.5) (10.5-j)
        f k = (x, g x)
          where x = h * fromIntegral k

axisGridAt :: [x] -> AxisData x -> AxisData x
axisGridAt gvs ad = ad { _axis_grid = gvs }

setLayout :: (Show x, RealFloat x) => EC (Layout x y) ()
setLayout = do
    layout_title .= []
    layout_legend .= Nothing
    -- hide y axis
    layout_left_axis_visibility . axis_show_line  .= False
    layout_left_axis_visibility . axis_show_ticks .= False
    layout_y_axis . laxis_override .= axisLabelsOverride [] . axisGridHide
    -- x axis grid and ticks
    let lpar = def { _la_nLabels = 6, _la_nTicks = 11 }
    layout_x_axis . laxis_generate .= dropDownTicks . scaledAxis lpar (0,1)
    layout_x_axis . laxis_override .= axisGridAt [1/6]
    layout_x_axis . laxis_style . axis_line_style .= solidLine 0.5 (opaque black)
    layout_x_axis . laxis_style . axis_grid_style .= dashedLine 0.5 [2,2] (opaque black)
    -- x axis labels
    layout_x_axis . laxis_style . axis_label_gap .= 7
    layout_x_axis . laxis_style . axis_label_style .= fsXLabel
    -- x axis title
    layout_x_axis . laxis_title .= "x"
    layout_x_axis . laxis_title_style .= fsTitle

fsXLabel :: FontStyle
fsXLabel = def { _font_size = 10,
                 _font_name = "TeX Gyre Pagella" }

fsTitle :: FontStyle
fsTitle = def { _font_size = 10,
                _font_name  = "TeX Gyre Pagella",
                _font_slant = FontSlantItalic }

outputFile :: (FileOptions, String)
-- outputFile = (FileOptions (800,500) PNG, "betamode1_fig.png")
outputFile = (FileOptions (400,250) PDF, "betamode1_fig.pdf")

main :: IO ()
main = toFile (fst outputFile) (snd outputFile) $ do
    setLayout
    setColors $ repeat (opaque black)
    let n = 1024
    plot (line' 0.5 "" [curve n 2])
