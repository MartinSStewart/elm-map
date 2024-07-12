module WithMarkers exposing (main)

import Angle
import Browser
import Camera3d exposing (Camera3d)
import CssPixels
import Geometry.Interop.LinearAlgebra.Point2d as Point2d
import Html exposing (Html)
import LngLat exposing (LngLat)
import MapViewer exposing (MapCoordinates)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Point2d exposing (Point2d)
import Quantity exposing (Unitless)
import Task
import WebGL exposing (Shader)
import WebGL.Matrices
import WebGL.Texture exposing (Texture)
import ZoomLevel


type alias Model =
    { map : MapViewer.Model
    , mapData : MapViewer.MapData
    , mapMarkers : MapMarkers
    }


type MapMarkers
    = TextureLoading
    | TextureLoaded MapMarkersData
    | TextureFailedToLoad WebGL.Texture.Error


type alias MapMarkersData =
    { texture : Texture
    , mesh : WebGL.Mesh MapMarkerVertex
    , center : Point2d Unitless MapCoordinates
    }


type Msg
    = MapMsg MapViewer.Msg
    | GotMapMarkerTexture (Result WebGL.Texture.Error Texture)


{-| Documentation on how to generate your own Mapbox API token: <https://docs.mapbox.com/help/troubleshooting/how-to-use-mapbox-securely/#access-tokens>
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


{-| DPI is hardcoded in this example but you'll want to get this with JS `window.devicePixelRatio` and then pass it in with ports. If you don't do this then the map will either be blurry or potentially laggy.
-}
devicePixelRatio =
    2


canvasSize =
    ( CssPixels.cssPixels 800, CssPixels.cssPixels 600 )


init : () -> ( Model, Cmd Msg )
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
      , mapMarkers = TextureLoading
      }
    , WebGL.Texture.loadWith
        { magnify = WebGL.Texture.linear
        , minify = WebGL.Texture.linear
        , horizontalWrap = WebGL.Texture.clampToEdge
        , verticalWrap = WebGL.Texture.clampToEdge
        , flipY = False
        }
        "https://raw.githubusercontent.com/MartinSStewart/elm-map/master/public/mapMarker.png"
        |> Task.attempt GotMapMarkerTexture
    )


points : List LngLat
points =
    [ { lng = Angle.degrees 0, lat = Angle.degrees 40 }
    , { lng = Angle.degrees 0.1, lat = Angle.degrees 40 }
    , { lng = Angle.degrees 0.2, lat = Angle.degrees 40.1 }
    ]


lngLatToQuad : LngLat -> List MapMarkerVertex
lngLatToQuad lngLat =
    let
        position =
            MapViewer.lngLatToWorld lngLat |> Point2d.toVec2

        color =
            Vec3.vec3 1 0 0
    in
    [ { position = position
      , offset = Vec2.vec2 0 1
      , texPosition = Vec2.vec2 0 1
      , color = color
      }
    , { position = position
      , offset = Vec2.vec2 1 1
      , texPosition = Vec2.vec2 1 1
      , color = color
      }
    , { position = position
      , offset = Vec2.vec2 1 0
      , texPosition = Vec2.vec2 1 0
      , color = color
      }
    , { position = position
      , offset = Vec2.vec2 0 0
      , texPosition = Vec2.vec2 0 0
      , color = color
      }
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MapMsg mapMsg ->
            let
                { newModel, newMapData, outMsg, cmd } =
                    MapViewer.update mapboxApiKey model.mapData mapMsg model.map
            in
            ( { model | map = newModel, mapData = newMapData }, Cmd.map MapMsg cmd )

        GotMapMarkerTexture result ->
            ( { model
                | mapMarkers =
                    case result of
                        Ok texture ->
                            TextureLoaded
                                { center =
                                    Point2d.centroidN (List.map MapViewer.lngLatToWorld points)
                                        |> Maybe.withDefault Point2d.origin
                                , texture = texture
                                , mesh = List.concatMap lngLatToQuad points |> quadsToMesh
                                }

                        Err error ->
                            TextureFailedToLoad error
              }
            , Cmd.none
            )


quadsToMesh : List a -> WebGL.Mesh a
quadsToMesh vertices =
    WebGL.indexedTriangles
        vertices
        (getQuadIndicesHelper vertices 0 [])


getQuadIndicesHelper : List a -> Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
getQuadIndicesHelper list indexOffset newList =
    case list of
        _ :: _ :: _ :: _ :: rest ->
            getQuadIndicesHelper
                rest
                (indexOffset + 1)
                (( 4 * indexOffset + 3, 4 * indexOffset + 1, 4 * indexOffset )
                    :: ( 4 * indexOffset + 2, 4 * indexOffset + 1, 4 * indexOffset + 3 )
                    :: newList
                )

        _ ->
            newList


subscriptions : Model -> Sub Msg
subscriptions model =
    MapViewer.subscriptions model.mapData model.map |> Sub.map MapMsg


view : Model -> Html Msg
view model =
    Html.div
        []
        [ case model.mapMarkers of
            TextureLoaded _ ->
                Html.text ""

            TextureLoading ->
                Html.div [] [ Html.text "Loading map marker texture..." ]

            TextureFailedToLoad _ ->
                Html.div [] [ Html.text "There was an error while loading the map maker texture" ]
        , MapViewer.viewWith
            { inputsEnabled = False
            , overrideCamera = Nothing
            , attributes = []
            , fillColor = MapViewer.defaultStyle.ground
            }
            (case model.mapMarkers of
                TextureLoaded { mesh, center, texture } ->
                    let
                        mapCamera : Camera3d Unitless MapCoordinates
                        mapCamera =
                            MapViewer.camera model.map

                        { x, y } =
                            Point2d.toUnitless center

                        aspectRatio : Float
                        aspectRatio =
                            Quantity.ratio (Tuple.first canvasSize) (Tuple.second canvasSize)

                        zoom : Float
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
                                |> Math.Matrix4.translate3 x y 0
                    in
                    [ WebGL.entityWith
                        []
                        mapMarkerVertexShader
                        mapMarkerFragmentShader
                        mesh
                        { view = cameraMatrix
                        , aspect = aspectRatio
                        , texture = texture
                        , zoom = MapViewer.viewZoom model.map |> ZoomLevel.toLinearZoom
                        }
                    ]

                TextureLoading ->
                    []

                TextureFailedToLoad _ ->
                    []
            )
            MapMsg
            model.mapData
            model.map
        ]


type alias MapMarkerVertex =
    { position : Vec2
    , offset : Vec2
    , texPosition : Vec2
    , color : Vec3
    }


mapMarkerVertexShader :
    Shader
        MapMarkerVertex
        { u | view : Mat4, aspect : Float, zoom : Float }
        { vColor : Vec4, vTexPosition : Vec2 }
mapMarkerVertexShader =
    [glsl|
attribute vec2 position;
attribute vec2 texPosition;
attribute vec2 offset;
attribute vec3 color;
uniform mat4 view;
uniform float aspect;
uniform float zoom;
varying vec2 vTexPosition;
varying vec4 vColor;

void main () {
    //into clip space
    vec4 currentProjected = view * vec4(position, 0.0, 1.0);

    gl_Position =
        currentProjected + vec4( vec2(offset.x / aspect,offset.y) * currentProjected.w, 0.0, 0.0);
    vTexPosition = texPosition;
    vColor = vec4(color, 1.0);
}
|]


mapMarkerFragmentShader : Shader {} { u | texture : Texture } { vColor : Vec4, vTexPosition : Vec2 }
mapMarkerFragmentShader =
    [glsl|
precision mediump float;
uniform sampler2D texture;
varying vec2 vTexPosition;
varying vec4 vColor;

void main () {
    gl_FragColor = texture2D(texture, vTexPosition) * vec4(vColor);
}
|]
