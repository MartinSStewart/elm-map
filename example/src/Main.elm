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
                { lng = -0.3, lat = 51.5 }
                (ZoomLevel.fromLogZoom 12)
                1
                ( Pixels.pixels 800, Pixels.pixels 600 )
      , mapData =
            MapViewer.initMapData
                "https://raw.githubusercontent.com/MartinSStewart/elm-map/master/public/dinProMediumEncoded.json"
                mapStyle
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


rgb : Float -> Float -> Float -> Color
rgb red green blue =
    { red = red, green = green, blue = blue }


rgb255 : Float -> Float -> Float -> Color
rgb255 red green blue =
    { red = red / 255, green = green / 255, blue = blue / 255 }


type alias Color =
    { red : Float, green : Float, blue : Float }


mapStyle : MapViewer.Style
mapStyle =
    { water = rgb 0.65 0.88 0.97
    , ground = rgb 0.87 0.85 0.83
    , buildings = rgb255 200 198 184
    , nature = rgb255 160 200 130
    , background = rgb 0.87 0.85 0.83
    , primaryRoad = rgb 0.9 0.9 0.9
    , primaryRoadOutline = rgb 0.5 0.5 0.5
    , primaryRoadLink = rgb 0.9 0.9 0.9
    , primaryRoadLinkOutline = rgb 0.5 0.5 0.5
    , secondaryRoad = rgb 0.91 0.91 0.91
    , secondaryRoadOutline = rgb 0.7 0.7 0.7
    , secondaryRoadLink = rgb 0.91 0.91 0.91
    , secondaryRoadLinkOutline = rgb 0.7 0.7 0.7
    , tertiaryRoad = rgb 0.9 0.9 0.9
    , tertiaryRoadOutline = rgb 0.5 0.5 0.5
    , tertiaryRoadLink = rgb 0.9 0.9 0.9
    , tertiaryRoadLinkOutline = rgb 0.5 0.5 0.5
    , motorway = rgb 0.5 0.63 1.0
    , motorwayOutline = rgb 1 1 1
    , motorwayLink = rgb 1 0.63 0.35
    , motorwayLinkOutline = rgb 1 1 1
    , trunkRoad = rgb 0.95 0.82 0.38
    , trunkRoadOutline = rgb 1 1 1
    , trunkRoadLink = rgb 0.95 0.82 0.38
    , trunkRoadLinkOutline = rgb 1 1 1
    , railroad = rgb 0.7 0.7 0.6
    , railroadOutline = rgb 0.7 0.7 0.6
    , tramline = rgb 0.7 0.7 0.6
    , tramlineOutline = rgb 0.7 0.7 0.6
    , subway = rgb 0.7 0.7 0.6
    , subwayOutline = rgb 0.7 0.7 0.6
    , narrowGaugeRailroad = rgb 0.7 0.7 0.6
    , narrowGaugeRailroadOutline = rgb 0.7 0.7 0.6
    , trail = rgb 0.73 0.73 0.65
    , trailOutline = rgb 0.73 0.73 0.65
    , footway = rgb 0.91 0.91 0.91
    , footwayOutline = rgb 0.7 0.7 0.7
    , residentialRoad = rgb 0.91 0.91 0.91
    , residentialRoadOutline = rgb 0.7 0.7 0.7
    , road = rgb 0.91 0.91 0.91
    , roadOutline = rgb 0.7 0.7 0.7
    , pedestrianPath = rgb 0.91 0.91 0.91
    , pedestrianPathOutline = rgb 0.7 0.7 0.7
    , unclassifiedRoad = rgb 0.91 0.91 0.91
    , unclassifiedRoadOutline = rgb 0.7 0.7 0.7
    , platform = rgb 0.91 0.91 0.91
    , platformOutline = rgb 0.7 0.7 0.7
    , livingStreet = rgb 0.91 0.91 0.91
    , livingStreetOutline = rgb255 100 240 100
    , serviceRoad = rgb 0.91 0.91 0.91
    , serviceRoadOutline = rgb 0.7 0.7 0.7
    , placeLabel = rgb255 50 50 50
    , placeLabelOutline = rgb 1 1 1
    , roadLabel = rgb255 50 50 50
    , roadLabelOutline = rgb 1 1 1
    }
