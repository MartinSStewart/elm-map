module ZoomLevel exposing (ZoomLevel(..), fromLogZoom, toLogZoom, fromLinearZoom, toLinearZoom, maxZoom, minZoom)

{-|

@docs ZoomLevel, fromLogZoom, toLogZoom, fromLinearZoom, toLinearZoom, maxZoom, minZoom

-}


{-| The scaling factor used for the map. Larger values means you're more zoomed in.
-}
type ZoomLevel
    = ZoomLevel Float


{-| Get the zoom amount on a logarithmic scale. Each time we increase by one unit, the map gets twice as large. This is what mapbox uses for requesting vector tiles from their server. Also interpolating between two zoom values in logarithmic units is what's used when smoothly zooming in or out.
-}
toLogZoom : ZoomLevel -> Float
toLogZoom (ZoomLevel zoomLevel) =
    zoomLevel


{-| Get the zoom amount on a linear scale.
-}
toLinearZoom : ZoomLevel -> Float
toLinearZoom (ZoomLevel zoomLevel) =
    2 ^ zoomLevel


{-| Convert an int to a linear zoom value.
-}
fromLinearZoom : Float -> ZoomLevel
fromLinearZoom zoom =
    logBase 2 zoom |> clamp minZoom_ maxZoom_ |> ZoomLevel


{-| Convert an int to a logarithmic zoom value.
-}
fromLogZoom : Float -> ZoomLevel
fromLogZoom zoom =
    clamp minZoom_ maxZoom_ zoom |> ZoomLevel


{-| The closest you can zoom in on the map.
-}
maxZoom : ZoomLevel
maxZoom =
    fromLogZoom maxZoom_


maxZoom_ =
    22


{-| The farthest you can zoom out on the map.
-}
minZoom : ZoomLevel
minZoom =
    fromLogZoom minZoom_


minZoom_ =
    2
