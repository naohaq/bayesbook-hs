{- -*- mode: haskell; coding: utf-8-unix -*-  -}

module Main where

import Common

import Numeric (showFFloat)
import Numeric.Tools.Integration

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

pb :: (RealFloat a) => a -> a
pb x = log (x / (1-x))

qb :: (RealFloat a) => a -> a
qb z = 1 / (1 + exp (-z))

curve :: Int -> Double -> [(Double,Double)]
curve n j = map f [0..n]
  where sx = 0.0001
        ex = 0.99
        h = (ex - sx) / fromIntegral n
        g = dbeta (j+1) (11-j) . qb
        res = quadSimpson defQuad (pb 0.0001,pb 0.99) g
        Just a = quadRes res
        f k = (pb x, g (pb x) / a)
          where x = h * fromIntegral k + sx

xlabels :: (RealFloat a) => [(a,String)]
xlabels = map (\(x,y)->(pb x,y)) ls
  where ls = [(0.0001,"0.0001"),(0.001,"0.001"),(0.01,"0.01"),(0.1,"0.1"),(0.5,"0.5"),(0.9,"0.9"),(0.99,"0.99")]

logitAxis :: (RealFloat a) => AxisFn a
logitAxis _ = ad
  where ad = makeAxis' realToFrac realToFrac (map (const [])) (lvs,tvs,gvs)
        lvs = map fst xlabels
        tvs = lvs
        gvs = take 9 $ tail lvs

setLayout :: (RealFloat x) => EC (Layout x y) ()
setLayout = do
    layout_title .= []
    layout_legend .= Nothing
    -- hide y axis
    layout_left_axis_visibility . axis_show_line  .= False
    layout_left_axis_visibility . axis_show_ticks .= False
    layout_y_axis . laxis_override .= axisLabelsOverride [] . axisGridHide
    -- x axis grid and ticks
    layout_x_axis . laxis_generate .= dropDownTicks . logitAxis
    layout_x_axis . laxis_override .= axisLabelsOverride xlabels
    layout_x_axis . laxis_style . axis_line_style .= solidLine 0.5 (opaque black)
    layout_x_axis . laxis_style . axis_grid_style .= dashedLine 0.5 [3,3] (opaque lightgrey)
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
-- outputFile = (FileOptions (1175,600) PNG, "binomlik3_fig.png")
outputFile = (FileOptions (590,300) PDF, "binomlik3_fig.pdf")
-- outputFile = (FileOptions (400,250) PS, "binomlik3_fig.ps")

main :: IO ()
main = toFile (fst outputFile) (snd outputFile) $ do
    setLayout
    setColors $ repeat (opaque black)
    let n = 8192
    mapM_ (\k -> plot (line' 0.5 "" [curve n k])) [1..5]

-- EOF
