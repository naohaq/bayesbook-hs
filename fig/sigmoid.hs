{- -*- mode: haskell; coding: utf-8-unix -*-  -}

module Main where

import Common

import Numeric (showFFloat)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

sigmoid :: (RealFloat a) => a -> a
sigmoid z = 1 / (1 + exp (-z))

dsigmoid :: (RealFloat a) => a -> a
dsigmoid z = 1 / w - 1 / (w^2)
  where w = 1 + exp (-z)

curve :: (RealFloat a) => Int -> [(a,a)]
curve n = map f [0..n]
  where sx = -10
        ex = 10
        h = (ex - sx) / fromIntegral n
        f k = (x, dsigmoid x)
          where x = h * fromIntegral k + sx

setLayout :: EC (Layout Double Double) ()
setLayout = do
    layout_title .= []
    layout_legend .= Nothing
    -- y axis
    layout_y_axis . laxis_override .= dropDownTicks
    layout_y_axis . laxis_style . axis_line_style .= solidLine 0.5 (opaque black)
    layout_y_axis . laxis_style . axis_grid_style .= dashedLine 0.5 [3,3] (opaque lightgrey)
    layout_y_axis . laxis_style . axis_label_style .= fsLabel
    layout_y_axis . laxis_title .= "y"
    layout_y_axis . laxis_title_style .= fsTitle
    -- x axis
    layout_x_axis . laxis_override .= dropDownTicks
    layout_x_axis . laxis_style . axis_line_style .= solidLine 0.5 (opaque black)
    layout_x_axis . laxis_style . axis_grid_style .= dashedLine 0.5 [3,3] (opaque lightgrey)
    layout_x_axis . laxis_style . axis_label_gap .= 7
    layout_x_axis . laxis_style . axis_label_style .= fsLabel
    layout_x_axis . laxis_title .= "x"
    layout_x_axis . laxis_title_style .= fsTitle

fsLabel :: FontStyle
fsLabel = def { _font_size = 10,
                _font_name = "TeX Gyre Pagella" }

fsTitle :: FontStyle
fsTitle = def { _font_size = 10,
                _font_name  = "TeX Gyre Pagella",
                _font_slant = FontSlantItalic }

outputFile :: (FileOptions, String)
-- outputFile = (FileOptions (1200,600) PNG, "logitfn_fig.png")
outputFile = (FileOptions (500,300) PDF, "sigmoid_fig.pdf")

main :: IO ()
main = toFile (fst outputFile) (snd outputFile) $ do
    setLayout
    setColors $ repeat (opaque black)
    let n = 8192
    plot (line' 0.5 "" [curve n])
