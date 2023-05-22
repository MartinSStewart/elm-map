module LngLat exposing (LngLat, minimum, maximum, distance)

{-|

@docs LngLat, toString, minimum, maximum, distance

-}

import Length exposing (Length)


{-| A LngLat represents a geographic position with longitude and latitude.
-}
type alias LngLat =
    { lng : Float
    , lat : Float
    }


{-| Return a new `LngLat` with the smallest longitude and latitude from each.
-}
minimum : LngLat -> LngLat -> LngLat
minimum a b =
    { lng = min a.lng b.lng, lat = min a.lat b.lat }


{-| Return a new `LngLat` with the largest longitude and latitude from each.
-}
maximum : LngLat -> LngLat -> LngLat
maximum a b =
    { lng = max a.lng b.lng, lat = max a.lat b.lat }


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
            6371

        dLat =
            deg2rad (lat2 - lat1)

        dLon =
            deg2rad (lon2 - lon1)

        a =
            (sin (dLat / 2) * sin (dLat / 2))
                + (cos (deg2rad lat1)
                    * cos (deg2rad lat2)
                    * sin (dLon / 2)
                    * sin (dLon / 2)
                  )

        c =
            2 * atan2 (sqrt a) (sqrt (1 - a))

        d =
            r * c
    in
    Length.kilometers d


deg2rad deg =
    deg * (pi / 180)
