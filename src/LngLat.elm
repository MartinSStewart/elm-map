module LngLat exposing (LngLat, minimum, maximum, distance)

{-|

@docs LngLat, toString, minimum, maximum, distance

-}

import Angle exposing (Angle)
import Length exposing (Length)
import Quantity
import Vector3d


{-| A LngLat represents a geographic position with longitude and latitude.
-}
type alias LngLat =
    { lng : Angle
    , lat : Angle
    }


{-| Return a new `LngLat` with the smallest longitude and latitude from each.
-}
minimum : LngLat -> LngLat -> LngLat
minimum a b =
    { lng = Quantity.min a.lng b.lng, lat = Quantity.min a.lat b.lat }


{-| Return a new `LngLat` with the largest longitude and latitude from each.
-}
maximum : LngLat -> LngLat -> LngLat
maximum a b =
    { lng = Quantity.max a.lng b.lng, lat = Quantity.max a.lat b.lat }


{-| Find the distance between two points.
-}
distance : LngLat -> LngLat -> Length
distance pointA pointB =
    let
        lat1 =
            pointA.lat

        lon1 =
            pointA.lng

        lat2 =
            pointB.lat

        lon2 =
            pointB.lng

        r =
            6371.009

        dLat =
            Quantity.difference lat2 lat1

        dLon =
            Quantity.difference lon2 lon1

        a =
            (Angle.sin (dLat |> Quantity.divideBy 2) ^ 2)
                + (Angle.cos lat1
                    * Angle.cos lat2
                    * (Angle.sin (dLon |> Quantity.divideBy 2) ^ 2)
                  )

        c =
            2 * atan2 (sqrt a) (sqrt (1 - a))

        d =
            r * c
    in
    Length.kilometers d
