module Basic exposing (main)

import Angle
import Browser
import Camera3d exposing (Camera3d)
import CssPixels
import Html exposing (Html)
import Http
import MapViewer exposing (MapCoordinates)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector4 exposing (Vec4)
import Pixels
import Point2d
import Quantity exposing (Unitless)
import WebGL exposing (Shader)
import WebGL.Matrices
import WebGL.Texture exposing (Texture)
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


canvasSize =
    ( CssPixels.cssPixels 800, CssPixels.cssPixels 600 )


init : () -> ( Model, Cmd msg )
init _ =
    ( { map =
            MapViewer.init
                { lng = Angle.degrees 0, lat = Angle.degrees 40 }
                (ZoomLevel.fromLogZoom 8)
                devicePixelRatio
                canvasSize
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
    let
        mapCamera : Camera3d Unitless MapCoordinates
        mapCamera =
            MapViewer.camera model.map

        center =
            Point2d.toUnitless model.mapTokensCenter

        aspectRatio : Float
        aspectRatio =
            Quantity.ratio (Tuple.first canvasSize) (Tuple.second canvasSize)

        zoom =
            MapViewer.viewZoom model.map |> ZoomLevel.toLinearZoom

        cameraMatrix : Mat4
        cameraMatrix =
            WebGL.Matrices.viewProjectionMatrix
                mapCamera
                { nearClipDepth = Quantity.float (0.1 / zoom)
                , farClipDepth = Quantity.float (1000 / zoom)
                , aspectRatio = aspectRatio
                }
                |> Math.Matrix4.translate3 center.x center.y 0
    in
    MapViewer.viewWith
        { inputsEnabled = False
        , overrideCamera = Nothing
        , attributes = []
        , fillColor = MapViewer.defaultStyle.ground
        }
        (drawMarkers cameraMatrix aspectRatio texture model)
        MapMsg
        model.mapData
        model.map


drawMarkers : Mat4 -> Float -> Texture -> Model -> List WebGL.Entity
drawMarkers cameraMatrix aspectRatio texture model =
    [ WebGL.entityWith
        []
        mapVertexShader
        mapFragmentShader
        model.mapMarkersMesh
        { onlyShowOne = -1
        , view = cameraMatrix
        , aspect = aspectRatio
        , texture = texture
        , zoom = MapViewer.viewZoom model.map |> ZoomLevel.toLinearZoom
        }
    ]


mapVertexShader :
    Shader
        MapVertex
        { u
            | view : Mat4
            , aspect : Float
            , zoom : Float
            , onlyShowOne : Float
        }
        { vColor : Vec4, vTexPosition : Vec2 }
mapVertexShader =
    [glsl|
attribute float id;
attribute vec2 position;
attribute vec2 texPosition;
attribute vec2 offset;
attribute vec3 color;
attribute float maxZoom;
attribute float minZoom;
uniform mat4 view;
uniform float aspect;
uniform float zoom;
uniform float onlyShowOne;
varying vec2 vTexPosition;
varying vec4 vColor;

void main () {
    //into clip space
    vec4 currentProjected = view * vec4(position, 0.0, 1.0);

    gl_Position =
        (zoom <= maxZoom && zoom >= minZoom) || onlyShowOne == id
            ? currentProjected + vec4(
                vec2(offset.x / aspect,offset.y) * currentProjected.w,
                0.0,
                0.0)
            : vec4(999.0, 999.0, 999.0, 999.0);
    vTexPosition = texPosition;
    vColor =
        (onlyShowOne == -1.0 || onlyShowOne == id)
            ? vec4(color, 1.0)
            : vec4(color * 0.08, 0.08);
}
|]


mapFragmentShader : Shader {} { u | texture : Texture } { vColor : Vec4, vTexPosition : Vec2 }
mapFragmentShader =
    [glsl|
precision mediump float;
uniform sampler2D texture;
varying vec2 vTexPosition;
varying vec4 vColor;

void main () {
    gl_FragColor = texture2D(texture, vTexPosition) * vec4(vColor);
}
|]
