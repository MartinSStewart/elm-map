module Main exposing (main)

import Browser
import Html exposing (Html)
import Http
import MapViewer
import Pixels
import ZoomLevel


type alias Model =
    { map : MapViewer.Model, mapData : MapViewer.MapData }


type Msg
    = MapMsg MapViewer.Msg


mapboxApiKey =
    MapViewer.mapboxAccessToken "pk.eyJ1IjoiYXQxMjMxMjMxMjMiLCJhIjoiY2t0dWh2NW9rMWE0czMycWVleWJsMWNxcyJ9.rhtVgETrZT4_LboePjCCpA"


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd msg )
init flags =
    ( { map =
            MapViewer.init
                { lng = 0, lat = 40 }
                (ZoomLevel.fromLogZoom 8)
                1
                ( Pixels.pixels 800, Pixels.pixels 600 )
      , mapData =
            MapViewer.initMapData
                "https://raw.githubusercontent.com/MartinSStewart/elm-map/master/public/dinProMediumEncoded.json"
                MapViewer.defaultStyle
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MapMsg mapMsg ->
            let
                { newModel, newMapData, outMsg, cmd } =
                    MapViewer.update mapboxApiKey model.mapData mapMsg model.map
            in
            ( { model | map = newModel, mapData = newMapData }, Cmd.map MapMsg cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    MapViewer.subscriptions model.mapData model.map |> Sub.map MapMsg


view : Model -> Html Msg
view model =
    MapViewer.view [] model.mapData model.map |> Html.map MapMsg
