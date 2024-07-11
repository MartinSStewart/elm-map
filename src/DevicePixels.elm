module DevicePixels exposing (DevicePixels, devicePixel, devicePixels, inDevicePixels, point)

import Point2d exposing (Point2d)
import Quantity exposing (Quantity)


type DevicePixels
    = DevicePixels Never


devicePixel : Quantity number DevicePixels
devicePixel =
    Quantity.unsafe 1


devicePixels : number -> Quantity number DevicePixels
devicePixels =
    Quantity.unsafe


inDevicePixels : Quantity number DevicePixels -> number
inDevicePixels =
    Quantity.unwrap


point : Float -> Float -> Point2d DevicePixels coordinate
point x y =
    Point2d.unsafe { x = x, y = y }
