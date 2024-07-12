module Basic exposing (main)

import Angle
import Browser
import CssPixels
import Html exposing (Html)
import Html.Attributes
import Http
import MapViewer
import Pixels
import ZoomLevel


type alias Model =
    { map : MapViewer.Model, mapData : MapViewer.MapData }


type Msg
    = MapMsg MapViewer.Msg


{-| Documentation on how to generate your own Mapbox API: <https://docs.mapbox.com/help/troubleshooting/how-to-use-mapbox-securely/#access-tokens>
-}
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


{-| DPI is hardcoded in this example but you'll want to get this with JS `window.devicePixelRatio` and then pass it in with ports. If you don't do this then the map will be blurry.
-}
devicePixelRatio =
    2


init : () -> ( Model, Cmd msg )
init _ =
    ( { map =
            MapViewer.init
                { lng = Angle.degrees 0, lat = Angle.degrees 40 }
                (ZoomLevel.fromLogZoom 8)
                devicePixelRatio
                ( CssPixels.cssPixels 800, CssPixels.cssPixels 600 )
      , mapData =
            MapViewer.initMapData
                { fntPath = "https://raw.githubusercontent.com/MartinSStewart/elm-map/master/public/dinProMedium.fnt"
                , imagePath = "https://raw.githubusercontent.com/MartinSStewart/elm-map/master/public/dinProMedium.png"
                }
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
    Html.div
        []
        [ MapViewer.view MapMsg model.mapData model.map
        , Html.div
            [ Html.Attributes.style "font-family" "sans-serif"
            , Html.Attributes.style "transform" "translateY(-24px)"
            , Html.Attributes.style "display" "inline-block"
            ]
            [ MapViewer.attribution ]
        ]
