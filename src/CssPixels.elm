module CssPixels exposing (CssPixels, cssPixel, cssPixels, inCssPixels, point)

import Point2d exposing (Point2d)
import Quantity exposing (Quantity)


type CssPixels
    = CssPixels Never


cssPixel : Quantity number CssPixels
cssPixel =
    Quantity.unsafe 1


cssPixels : number -> Quantity number CssPixels
cssPixels =
    Quantity.unsafe


inCssPixels : Quantity number CssPixels -> number
inCssPixels =
    Quantity.unwrap


point : Float -> Float -> Point2d CssPixels coordinate
point x y =
    Point2d.unsafe { x = x, y = y }
