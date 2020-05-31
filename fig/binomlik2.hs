{- -*- mode: haskell; coding: utf-8-unix -*-  -}

module Main where

import Common

import Numeric (showFFloat)
import Numeric.Tools.Integration

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

pb :: (RealFloat a) => a -> a
pb x = 2 / pi * asin (sqrt x)

qb :: (RealFloat a) => a -> a
qb z = sin (pi*z/2) ^ 2

curve :: Int -> Double -> [(Double,Double)]
curve n j = map f [0..n]
  where h = 1 / fromIntegral n
        g = dbeta (j+1) (11-j) . qb
        res = quadSimpson defQuad (0,1) g
        Just a = quadRes res
        f k = (x, g x / a)
          where x = h * fromIntegral k

xlabels :: (RealFloat a) => [(a,String)]
xlabels = [(pb 0, "0")] ++ (map (f . (/ 10) . fromIntegral) [1..9]) ++ [(pb 1, "1")]
  where f x = (pb x, showFFloat (Just 1) x "")

jeffreysAxis :: (RealFloat a) => AxisFn a
jeffreysAxis _ = ad
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
    layout_x_axis . laxis_generate .= dropDownTicks . jeffreysAxis
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
-- outputFile = (FileOptions (800,500) PNG, "binomlik2_fig.png")
outputFile = (FileOptions (400,250) PDF, "binomlik2_fig.pdf")
-- outputFile = (FileOptions (400,250) PS, "binomlik2_fig.ps")

main :: IO ()
main = toFile (fst outputFile) (snd outputFile) $ do
    setLayout
    setColors $ repeat (opaque black)
    let n = 1024
    mapM_ (\k -> plot (line' 0.5 "" [curve n k])) [1..5]

-- EOF
