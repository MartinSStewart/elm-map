module GridPointDict exposing (GridPointDict, empty, fromList, get, insert, member, toList)

import Dict exposing (Dict)


type alias GridPoint =
    { gridX : Int
    , gridY : Int
    , zoom : Int
    }


type GridPointDict a
    = GridPointDict (Dict ( Int, Int, Int ) a)


empty : GridPointDict a
empty =
    GridPointDict Dict.empty


get : GridPoint -> GridPointDict a -> Maybe a
get key (GridPointDict dict) =
    Dict.get ( key.gridX, key.gridY, key.zoom ) dict


member : GridPoint -> GridPointDict a -> Bool
member key (GridPointDict dict) =
    Dict.member ( key.gridX, key.gridY, key.zoom ) dict


insert : GridPoint -> a -> GridPointDict a -> GridPointDict a
insert key value (GridPointDict dict) =
    GridPointDict (Dict.insert ( key.gridX, key.gridY, key.zoom ) value dict)


toList : GridPointDict a -> List ( GridPoint, a )
toList (GridPointDict dict) =
    Dict.toList dict
        |> List.map
            (\( ( gridX, gridY, zoom ), value ) ->
                ( { gridX = gridX
                  , gridY = gridY
                  , zoom = zoom
                  }
                , value
                )
            )


fromList : List ( GridPoint, a ) -> GridPointDict a
fromList list =
    list
        |> List.map (\( point, value ) -> ( ( point.gridX, point.gridY, point.zoom ), value ))
        |> Dict.fromList
        |> GridPointDict
