{- -*- mode: haskell; coding: utf-8-unix -*-  -}

module Main where

import Common

import Numeric (showFFloat)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

logit :: (RealFloat a) => a -> a
logit x = log (x / (1 - x))

sigmoid :: (RealFloat a) => a -> a
sigmoid z = 1 / (1 + exp (-z))

curve :: (RealFloat a) => Int -> [(a,a)]
curve n = map f [0..n]
  where sx = logit 0.0001
        ex = logit 0.99
        h = (ex - sx) / fromIntegral n
        f k = (x, sigmoid x)
          where x = h * fromIntegral k + sx

ylabels :: (RealFloat a) => [(a,String)]
ylabels = [(0,""),(0.0001,""),(0.001,""),(0.01,"0.01"),(0.1,"0.1"),(0.5,"0.5"),(0.9,"0.9"),(0.99,"0.99"),(1,"")]

yAxis :: (RealFloat a) => AxisFn a
yAxis _ = makeAxis' realToFrac realToFrac (map (const [])) (lvs,tvs,gvs)
  where lvs = map fst ylabels
        tvs = lvs
        gvs = lvs

xlabels :: (RealFloat a) => [(a,String)]
xlabels = map (\x->(logit x,"")) ls0 ++ map (\x->(x, showFFloat (Just 1) x "")) ls1
  where ls0 = [0.0001, 0.001, 0.01, 0.1, 0.5, 0.9, 0.99]
        ls1 = [-8,-6,-4,-2,0,2,4]

logitAxis :: (RealFloat a) => AxisFn a
logitAxis _ = makeAxis' realToFrac realToFrac (map (const [])) (lvs,tvs,gvs)
  where lvs = map fst xlabels
        tvs = lvs
        gvs = lvs

setLayout :: EC (Layout Double Double) ()
setLayout = do
    layout_title .= []
    layout_legend .= Nothing
    -- y axis
    layout_y_axis . laxis_generate .= dropDownTicks . yAxis
    layout_y_axis . laxis_override .= axisLabelsOverride (filter ((>0).length.snd) ylabels)
    layout_y_axis . laxis_style . axis_line_style .= solidLine 0.5 (opaque black)
    layout_y_axis . laxis_style . axis_grid_style .= dashedLine 0.5 [3,3] (opaque lightgrey)
    layout_y_axis . laxis_style . axis_label_style .= fsLabel
    layout_y_axis . laxis_title .= "y"
    layout_y_axis . laxis_title_style .= fsTitle
    -- x axis
    layout_x_axis . laxis_generate .= dropDownTicks . logitAxis
    layout_x_axis . laxis_override .= axisLabelsOverride xlabels
    layout_x_axis . laxis_style . axis_line_style .= solidLine 0.5 (opaque black)
    layout_x_axis . laxis_style . axis_grid_style .= dashedLine 0.5 [3,3] (opaque lightgrey)
    layout_x_axis . laxis_style . axis_label_gap .= 7
    layout_x_axis . laxis_style . axis_label_style .= fsLabel
    layout_x_axis . laxis_title .= "logit(y)"
    layout_x_axis . laxis_title_style .= fsTitle

fsLabel :: FontStyle
fsLabel = def { _font_size = 10,
                _font_name = "TeX Gyre Pagella" }

fsTitle :: FontStyle
fsTitle = def { _font_size = 10,
                _font_name  = "TeX Gyre Pagella" }

outputFile :: (FileOptions, String)
-- outputFile = (FileOptions (1200,600) PNG, "logitfn_fig.png")
outputFile = (FileOptions (600,300) PDF, "logitfn_fig.pdf")

main :: IO ()
main = toFile (fst outputFile) (snd outputFile) $ do
    setLayout
    setColors $ repeat (opaque black)
    let n = 1024
    plot (line' 0.5 "" [curve n])

-- EOF
