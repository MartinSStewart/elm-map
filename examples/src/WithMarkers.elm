module WithMarkers exposing (main)

import Angle
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import CssPixels exposing (CssPixels)
import Direction2d
import Geometry.Interop.LinearAlgebra.Point2d as Point2d
import Html exposing (Html)
import Html.Attributes
import List.Extra
import LngLat exposing (LngLat)
import MapViewer exposing (CanvasCoordinates, MapCoordinates)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Unitless)
import Task
import Vector2d
import WebGL exposing (Shader)
import WebGL.Matrices
import WebGL.Settings.Blend as Blend
import WebGL.Texture exposing (Texture)
import ZoomLevel


type alias Model =
    { map : MapViewer.Model
    , mapData : MapViewer.MapData
    , mapMarkers : MapMarkers
    , selectedMapMarker : Maybe Int
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
      , selectedMapMarker = Just 1
      }
    , WebGL.Texture.loadWith
        { magnify = WebGL.Texture.linear
        , minify = WebGL.Texture.linear
        , horizontalWrap = WebGL.Texture.clampToEdge
        , verticalWrap = WebGL.Texture.clampToEdge
        , flipY = True
        }
        "https://raw.githubusercontent.com/MartinSStewart/elm-map/master/public/mapMarker.png"
        |> Task.attempt GotMapMarkerTexture
    )


points : List LngLat
points =
    [ { lng = Angle.degrees 0, lat = Angle.degrees 40 }
    , { lng = Angle.degrees 0.1, lat = Angle.degrees 40.1 }
    , { lng = Angle.degrees 0.2, lat = Angle.degrees 40.2 }
    ]


lngLatToQuad :
    Quantity Float CssPixels
    -> Float
    -> Point2d Unitless MapCoordinates
    -> Int
    -> LngLat
    -> List MapMarkerVertex
lngLatToQuad height aspectRatio center id lngLat =
    let
        position =
            MapViewer.lngLatToWorld lngLat
                |> Point2d.translateBy (Vector2d.from center Point2d.origin)
                |> Point2d.toVec2

        color =
            Vec3.vec3 1 0 0

        size2 =
            Quantity.ratio height (Tuple.second canvasSize) * 2
    in
    [ { id = toFloat id
      , position = position
      , offset = Vec2.vec2 (-size2 * aspectRatio / 2) size2
      , texPosition = Vec2.vec2 0 1
      , color = color
      }
    , { id = toFloat id
      , position = position
      , offset = Vec2.vec2 (size2 * aspectRatio / 2) size2
      , texPosition = Vec2.vec2 1 1
      , color = color
      }
    , { id = toFloat id
      , position = position
      , offset = Vec2.vec2 (size2 * aspectRatio / 2) 0
      , texPosition = Vec2.vec2 1 0
      , color = color
      }
    , { id = toFloat id
      , position = position
      , offset = Vec2.vec2 (-size2 * aspectRatio / 2) 0
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

                maybeSelected =
                    case outMsg of
                        Just (MapViewer.PointerPressed _) ->
                            model.selectedMapMarker

                        Just (MapViewer.PointerReleased { canvasPosition, dragDistance }) ->
                            if dragDistance |> Quantity.lessThan (CssPixels.cssPixels 16) then
                                List.Extra.findIndex (insideMapMarker newModel canvasPosition) points

                            else
                                model.selectedMapMarker

                        Nothing ->
                            model.selectedMapMarker
            in
            ( { model | map = newModel, mapData = newMapData, selectedMapMarker = maybeSelected }
            , Cmd.map MapMsg cmd
            )

        GotMapMarkerTexture result ->
            ( { model
                | mapMarkers =
                    case result of
                        Ok texture ->
                            let
                                ( textureWidth, textureHeight ) =
                                    WebGL.Texture.size texture

                                aspectRatio =
                                    toFloat textureWidth / toFloat textureHeight

                                center =
                                    Point2d.centroidN (List.map MapViewer.lngLatToWorld points)
                                        |> Maybe.withDefault Point2d.origin
                            in
                            TextureLoaded
                                { center = center
                                , texture = texture
                                , mesh =
                                    List.indexedMap (lngLatToQuad (CssPixels.cssPixels 60) aspectRatio center) points
                                        |> List.concat
                                        |> quadsToMesh
                                }

                        Err error ->
                            TextureFailedToLoad error
              }
            , Cmd.none
            )


{-| Detect if a point is inside the map marker shape. It's not pixel perfect but close enough.
-}
insideMapMarker : MapViewer.Model -> Point2d CssPixels CanvasCoordinates -> LngLat -> Bool
insideMapMarker mapModel point mapMarkerPos =
    let
        markerCanvasPos : Point2d CssPixels CanvasCoordinates
        markerCanvasPos =
            MapViewer.lngLatToWorld mapMarkerPos |> MapViewer.worldToCanvas mapModel
    in
    case Direction2d.from point markerCanvasPos of
        Just direction ->
            let
                distance : Quantity Float CssPixels
                distance =
                    Point2d.distanceFrom markerCanvasPos point

                distance2 : Quantity Float CssPixels
                distance2 =
                    Point2d.distanceFrom
                        (Point2d.translateBy
                            (Vector2d.fromTuple CssPixels.cssPixels ( 0, -42 ))
                            markerCanvasPos
                        )
                        point

                angleDiff =
                    Direction2d.angleFrom Direction2d.y direction
                        |> Quantity.abs

                isInsidePointyBit =
                    (angleDiff |> Quantity.lessThan (Angle.degrees 24))
                        && (distance |> Quantity.lessThan (CssPixels.cssPixels 45))

                isInsideCircleBit =
                    distance2 |> Quantity.lessThan (CssPixels.cssPixels 15)
            in
            isInsidePointyBit || isInsideCircleBit

        Nothing ->
            True


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
            { inputsEnabled = True
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
                        [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]
                        mapMarkerVertexShader
                        mapMarkerFragmentShader
                        mesh
                        { selectedId =
                            case model.selectedMapMarker of
                                Just index ->
                                    toFloat index

                                Nothing ->
                                    -1
                        , view = cameraMatrix
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
    { id : Float
    , position : Vec2
    , offset : Vec2
    , texPosition : Vec2
    , color : Vec3
    }


mapMarkerVertexShader :
    Shader
        MapMarkerVertex
        { u | selectedId : Float, view : Mat4, aspect : Float, zoom : Float }
        { vColor : Vec4, vTexPosition : Vec2 }
mapMarkerVertexShader =
    [glsl|
attribute float id;
attribute vec2 position;
attribute vec2 texPosition;
attribute vec2 offset;
attribute vec3 color;
uniform float selectedId;
uniform mat4 view;
uniform float aspect;
uniform float zoom;
varying vec2 vTexPosition;
varying vec4 vColor;

void main () {
    //into clip space
    vec4 currentProjected = view * vec4(position, 0.0, 1.0);
    gl_Position = currentProjected + vec4( vec2(offset.x / aspect,offset.y) * currentProjected.w, 0.0, 0.0);
    vTexPosition = texPosition;
    vColor =
        id == selectedId
            ? vec4(1.0, 1.0, 1.0, 1.0)
            : vec4(color, 1.0);
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
