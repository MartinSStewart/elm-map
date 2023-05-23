module MapViewer exposing
    ( init, initMapData, update, view, subscriptions, resizeCanvas, Model, MapData, Msg, OutMsg(..)
    , MapboxAccessToken, mapboxAccessToken, mapboxAccessTokenToString
    , defaultStyle, Style, Color
    , animateZoom, animateZoomAt, animateViewBounds, withPositionAndZoom, viewPosition, viewZoom, viewportHeight, camera, lngLatToWorld, canvasToWorld, canvasSize, DevicePixels, CanvasCoordinates, WorldCoordinates
    , attribution, loadTile
    , GridPoint, PointerEvent, Value(..), getTags, withViewBounds
    )

{-|


# Wiring

In order for the map viewer to work correctly, make sure all of these functions are called in the appropriate places within your app.

@docs init, initMapData, update, view, subscriptions, resizeCanvas, Model, MapData, Msg, OutMsg


# Authentication

@docs MapboxAccessToken, mapboxAccessToken, mapboxAccessTokenToString


# Styling

There is a limited amount of styling you can do to the map viewer. Likely you'll want something more in which case create an issue explaining what you need. If it's something simple then it might get added to this package, but in more complicated cases it might be better to clone this package and modify it to suit your exact needs.

@docs defaultStyle, rgb, Style, Color


# Panning and zooming

@docs animateZoom, animateZoomAt, animateViewBounds, withPositionAndZoom, viewPosition, viewZoom, viewportHeight, camera, lngLatToWorld, canvasToWorld, canvasSize, DevicePixels, CanvasCoordinates, WorldCoordinates


# Misc

@docs attribution, loadTile

-}

import Angle
import Array exposing (Array)
import AssocList as Dict exposing (Dict)
import Axis2d
import Axis3d
import Bitwise
import BoundingBox2d exposing (BoundingBox2d)
import Browser.Events
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode
import Camera3d exposing (Camera3d)
import Circle2d exposing (Circle2d)
import Dict as RegularDict
import Direction2d exposing (Direction2d)
import Direction3d
import Duration
import Font exposing (Font, Glyph)
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Pointer
import Html.Events.Extra.Touch exposing (Touch)
import Html.Events.Extra.Wheel
import Http
import Int64 exposing (Int64)
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import LngLat exposing (LngLat)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d
import Polyline2d
import Process
import ProtobufDecode exposing (Decoder)
import Quantity exposing (Quantity(..), Unitless)
import Random
import Random.List
import Rectangle2d exposing (Rectangle2d)
import Serialize
import Task
import Time
import TriangularMesh exposing (TriangularMesh)
import Url.Builder
import Vector2d exposing (Vector2d)
import Viewpoint3d
import WebGL exposing (Shader)
import WebGL.Matrices
import WebGL.Settings
import WebGL.Settings.Blend
import WebGL.Settings.StencilTest
import ZoomLevel exposing (ZoomLevel)


{-| -}
type DevicePixels
    = DevicePixel Never


{-| -}
type CanvasCoordinates
    = CanvasCoordinates Never


{-| API key for mapbox. We need this in order to load map data. You can create one like this:

    import MapViewer

    key =
        MapViewer.mapboxAccessToken myAccessToken

-}
type MapboxAccessToken
    = MapboxAccessToken String


{-| API key for mapbox. We need this in order to load map data. You can follow this link for instructions on how to create one <https://docs.mapbox.com/help/getting-started/access-tokens/>
-}
mapboxAccessToken : String -> MapboxAccessToken
mapboxAccessToken =
    MapboxAccessToken


{-| Convert an access token back into a string.
-}
mapboxAccessTokenToString : MapboxAccessToken -> String
mapboxAccessTokenToString (MapboxAccessToken a) =
    a


{-| -}
type Model
    = Model
        { viewPosition : Point2d Unitless WorldCoordinates
        , viewZoom : ZoomLevel
        , zoomAnimation : Maybe ZoomAnimation
        , pointerIsDown : Maybe { dragDistance : Quantity Float Pixels }
        , touches : RegularDict.Dict Int (Point2d Pixels CanvasCoordinates)
        , canvasSize : ( Quantity Int Pixels, Quantity Int Pixels )
        , devicePixelRatio : Float
        , lastAnimationFrame : Maybe Time.Posix
        , tileLoadDebounceCounter : Int
        , debouncePending : Bool
        , tileLoadPending : Bool
        }


type MapData
    = MapData
        { style : InternalStyle
        , data : Dict GridPoint TileState
        , font : FontState
        }


type FontState
    = FontNotLoading String
    | FontLoading
    | FontLoaded Font
    | FontLoadFailed Http.Error


type ZoomAnimation
    = ZoomInOrOut { zoomAt : Point2d Unitless WorldCoordinates, zoomLevel : ZoomLevel }
    | PanAndZoom
        { zoomAtStart : Point2d Unitless WorldCoordinates
        , zoomAtEnd : Point2d Unitless WorldCoordinates
        , startTime : Maybe Time.Posix
        , zoomLevelStart : ZoomLevel
        , zoomLevelEnd : ZoomLevel
        }


type TileState
    = TileLoading
    | TileLoadedRaw Bytes
    | TileLoaded Tile
    | TileLoadedWithText TileWithText
    | TileError


{-| -}
type Msg
    = GotData GridPoint (Result Http.Error Bytes)
    | MouseWheelMoved Html.Events.Extra.Wheel.Event
    | ZoomAtAnimation ZoomAnimation Time.Posix
    | NextRenderStep
    | DebounceFinished Int
    | PointerMoved PointerEvent
    | PointerDown PointerEvent
    | PointerUp PointerEvent
    | PointerLeave PointerEvent
    | TouchStart
    | TouchMoved
    | GotFont (Result Http.Error Font)


type alias PointerEvent =
    { pointerId : Int, position : Point2d Pixels CanvasCoordinates }


type alias VisibleTile =
    { zoom : Int, relativeZoom : Float, matrix : Mat4, result : TileState }


tileSize : Float -> Vector2d Unitless WorldCoordinates
tileSize zoom =
    Vector2d.unitless (1 / 2 ^ zoom) (1 / 2 ^ zoom)


{-| Mapbox and OpenStreetMap require that you give credit for use of their vector tile data. Here's a quick way you can add that. You can read more about it here <https://docs.mapbox.com/help/getting-started/attribution/#text-attribution>
-}
attribution : Html msg
attribution =
    Html.div
        [ Html.Attributes.style "font-size" "10px"
        , Html.Attributes.style "background-color" "rgba(255, 255, 255, 0.2)"
        , Html.Attributes.style "padding" "3px"
        ]
        [ Html.a
            [ Html.Attributes.href "https://www.mapbox.com/about/maps/"
            , Html.Attributes.style "text-decoration" "none"
            , Html.Attributes.style "color" "rgb(120, 120, 120)"
            ]
            [ Html.text "© Mapbox" ]
        , Html.span [ Html.Attributes.style "padding" "3px" ] []
        , Html.a
            [ Html.Attributes.href "http://www.openstreetmap.org/about/"
            , Html.Attributes.style "text-decoration" "none"
            , Html.Attributes.style "color" "rgb(120, 120, 120)"
            ]
            [ Html.text "© OpenStreetMap" ]
        , Html.span [ Html.Attributes.style "padding" "3px" ] []
        , Html.a
            [ Html.Attributes.href "https://www.mapbox.com/map-feedback"
            , Html.Attributes.style "text-decoration" "none"
            , Html.Attributes.style "color" "rgb(120, 120, 120)"
            ]
            [ Html.text "Improve this map" ]
        ]


styleToInternal : Style -> InternalStyle
styleToInternal style =
    { water = colorToVec4 style.water
    , ground = colorToVec4 style.ground
    , buildings = colorToVec4 style.buildings
    , nature = colorToVec4 style.nature
    , background = colorToVec4 style.background
    , primaryRoad = colorToVec3 style.primaryRoad
    , primaryRoadOutline = colorToVec3 style.primaryRoadOutline
    , primaryRoadLink = colorToVec3 style.primaryRoadLink
    , primaryRoadLinkOutline = colorToVec3 style.primaryRoadLinkOutline
    , secondaryRoad = colorToVec3 style.secondaryRoad
    , secondaryRoadOutline = colorToVec3 style.secondaryRoadOutline
    , secondaryRoadLink = colorToVec3 style.secondaryRoadLink
    , secondaryRoadLinkOutline = colorToVec3 style.secondaryRoadLinkOutline
    , tertiaryRoad = colorToVec3 style.tertiaryRoad
    , tertiaryRoadOutline = colorToVec3 style.tertiaryRoadOutline
    , tertiaryRoadLink = colorToVec3 style.tertiaryRoadLink
    , tertiaryRoadLinkOutline = colorToVec3 style.tertiaryRoadLinkOutline
    , motorway = colorToVec3 style.motorway
    , motorwayOutline = colorToVec3 style.motorwayOutline
    , motorwayLink = colorToVec3 style.motorwayLink
    , motorwayLinkOutline = colorToVec3 style.motorwayLinkOutline
    , trunkRoad = colorToVec3 style.trunkRoad
    , trunkRoadOutline = colorToVec3 style.trunkRoadOutline
    , trunkRoadLink = colorToVec3 style.trunkRoadLink
    , trunkRoadLinkOutline = colorToVec3 style.trunkRoadLinkOutline
    , railroad = colorToVec3 style.railroad
    , railroadOutline = colorToVec3 style.railroadOutline
    , tramline = colorToVec3 style.tramline
    , tramlineOutline = colorToVec3 style.tramlineOutline
    , subway = colorToVec3 style.subway
    , subwayOutline = colorToVec3 style.subwayOutline
    , narrowGaugeRailroad = colorToVec3 style.narrowGaugeRailroad
    , narrowGaugeRailroadOutline = colorToVec3 style.narrowGaugeRailroadOutline
    , trail = colorToVec3 style.trail
    , trailOutline = colorToVec3 style.trailOutline
    , footway = colorToVec3 style.footway
    , footwayOutline = colorToVec3 style.footwayOutline
    , residentialRoad = colorToVec3 style.residentialRoad
    , residentialRoadOutline = colorToVec3 style.residentialRoadOutline
    , road = colorToVec3 style.road
    , roadOutline = colorToVec3 style.roadOutline
    , pedestrianPath = colorToVec3 style.pedestrianPath
    , pedestrianPathOutline = colorToVec3 style.pedestrianPathOutline
    , unclassifiedRoad = colorToVec3 style.unclassifiedRoad
    , unclassifiedRoadOutline = colorToVec3 style.unclassifiedRoadOutline
    , platform = colorToVec3 style.platform
    , platformOutline = colorToVec3 style.platformOutline
    , livingStreet = colorToVec3 style.livingStreet
    , livingStreetOutline = colorToVec3 style.livingStreetOutline
    , serviceRoad = colorToVec3 style.serviceRoad
    , serviceRoadOutline = colorToVec3 style.serviceRoadOutline
    , placeLabel = colorToVec3 style.placeLabel
    , placeLabelOutline = colorToVec3 style.placeLabelOutline
    , roadLabel = colorToVec3 style.roadLabel
    , roadLabelOutline = colorToVec3 style.roadLabelOutline
    }


type alias Color =
    { red : Float, green : Float, blue : Float }


rgb : Float -> Float -> Float -> Color
rgb red green blue =
    { red = red, green = green, blue = blue }


colorToVec4 : Color -> Vec4
colorToVec4 { red, green, blue } =
    Vec4.vec4 red green blue 1


colorToVec3 : Color -> Vec3
colorToVec3 { red, green, blue } =
    Vec3.vec3 red green blue


defaultStyle : Style
defaultStyle =
    { water = rgb 0.65 0.88 0.97
    , ground = rgb 0.87 0.85 0.83
    , buildings = rgb 0.84 0.82 0.8
    , nature = rgb 0.74 0.86 0.68
    , background = rgb 0.87 0.85 0.83
    , primaryRoad = rgb 0.9 0.9 0.9
    , primaryRoadOutline = rgb 0.84 0.85 0.89
    , primaryRoadLink = rgb 0.9 0.9 0.9
    , primaryRoadLinkOutline = rgb 0.84 0.85 0.89
    , secondaryRoad = rgb 0.91 0.91 0.91
    , secondaryRoadOutline = rgb 0.91 0.91 0.91
    , secondaryRoadLink = rgb 0.91 0.91 0.91
    , secondaryRoadLinkOutline = rgb 0.91 0.91 0.91
    , tertiaryRoad = rgb 0.9 0.9 0.9
    , tertiaryRoadOutline = rgb 0.84 0.85 0.89
    , tertiaryRoadLink = rgb 0.9 0.9 0.9
    , tertiaryRoadLinkOutline = rgb 0.84 0.85 0.89
    , motorway = rgb 1 0.63 0.35
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
    , footwayOutline = rgb 0.91 0.91 0.91
    , residentialRoad = rgb 0.91 0.91 0.91
    , residentialRoadOutline = rgb 0.91 0.91 0.91
    , road = rgb 0.91 0.91 0.91
    , roadOutline = rgb 0.91 0.91 0.91
    , pedestrianPath = rgb 0.91 0.91 0.91
    , pedestrianPathOutline = rgb 0.91 0.91 0.91
    , unclassifiedRoad = rgb 0.91 0.91 0.91
    , unclassifiedRoadOutline = rgb 0.91 0.91 0.91
    , platform = rgb 0.91 0.91 0.91
    , platformOutline = rgb 0.91 0.91 0.91
    , livingStreet = rgb 0.91 0.91 0.91
    , livingStreetOutline = rgb 0.91 0.91 0.91
    , serviceRoad = rgb 0.91 0.91 0.91
    , serviceRoadOutline = rgb 0.91 0.91 0.91
    , placeLabel = rgb 0 0 0
    , placeLabelOutline = rgb 1 1 1
    , roadLabel = rgb 0 0 0
    , roadLabelOutline = rgb 1 1 1
    }


type alias Style =
    { water : Color
    , ground : Color
    , buildings : Color
    , nature : Color
    , background : Color
    , primaryRoad : Color
    , primaryRoadOutline : Color
    , primaryRoadLink : Color
    , primaryRoadLinkOutline : Color
    , secondaryRoad : Color
    , secondaryRoadOutline : Color
    , secondaryRoadLink : Color
    , secondaryRoadLinkOutline : Color
    , tertiaryRoad : Color
    , tertiaryRoadOutline : Color
    , tertiaryRoadLink : Color
    , tertiaryRoadLinkOutline : Color
    , motorway : Color
    , motorwayOutline : Color
    , motorwayLink : Color
    , motorwayLinkOutline : Color
    , trunkRoad : Color
    , trunkRoadOutline : Color
    , trunkRoadLink : Color
    , trunkRoadLinkOutline : Color
    , railroad : Color
    , railroadOutline : Color
    , tramline : Color
    , tramlineOutline : Color
    , subway : Color
    , subwayOutline : Color
    , narrowGaugeRailroad : Color
    , narrowGaugeRailroadOutline : Color
    , trail : Color
    , trailOutline : Color
    , footway : Color
    , footwayOutline : Color
    , residentialRoad : Color
    , residentialRoadOutline : Color
    , road : Color
    , roadOutline : Color
    , pedestrianPath : Color
    , pedestrianPathOutline : Color
    , unclassifiedRoad : Color
    , unclassifiedRoadOutline : Color
    , platform : Color
    , platformOutline : Color
    , livingStreet : Color
    , livingStreetOutline : Color
    , serviceRoad : Color
    , serviceRoadOutline : Color
    , placeLabel : Color
    , placeLabelOutline : Color
    , roadLabel : Color
    , roadLabelOutline : Color
    }


init :
    LngLat
    -> ZoomLevel
    -> Float
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Model
init position startingZoom devicePixelRatio canvasSize_ =
    Model
        { viewPosition = lngLatToWorld position
        , viewZoom = startingZoom
        , zoomAnimation = Nothing
        , pointerIsDown = Nothing
        , touches = RegularDict.empty
        , canvasSize = canvasSize_
        , devicePixelRatio = devicePixelRatio
        , lastAnimationFrame = Nothing
        , tileLoadDebounceCounter = 0
        , debouncePending = False
        , tileLoadPending = True
        }


initMapData : String -> Style -> MapData
initMapData fontPath style =
    MapData
        { style = styleToInternal style
        , data = Dict.empty
        , font = FontNotLoading fontPath
        }


{-| Smoothly zoom in by a certain amount.

    -- Zoom in so the map appears twice times as large
    Map.zoomIn 1 map

    -- Zoom in so the map appears four times as large
    Map.zoomIn 2 map

    -- Zoom out so the map appears half as large
    Map.zoomIn -1 map

-}
animateZoom : Float -> Model -> Model
animateZoom zoomAmount (Model model) =
    { model
        | zoomAnimation =
            ZoomInOrOut
                { zoomAt = model.viewPosition
                , zoomLevel = ZoomLevel.toLogZoom model.viewZoom + zoomAmount |> ZoomLevel.fromLogZoom
                }
                |> Just
        , debouncePending = True
    }
        |> Model


{-| Smoothly zoom in by a certain amount while keeping a specific point on the map fixed in place. You can imagine this as zooming in at the point under your cursor.
-}
animateZoomAt : Point2d Unitless WorldCoordinates -> Float -> Model -> Model
animateZoomAt position zoomAmount (Model model) =
    { model
        | zoomAnimation =
            ZoomInOrOut
                { zoomAt = position
                , zoomLevel = ZoomLevel.toLogZoom model.viewZoom + zoomAmount |> ZoomLevel.fromLogZoom
                }
                |> Just
        , debouncePending = True
    }
        |> Model


{-| Set the view position and zoom level without any animation. This will end any ongoing view animation.
-}
withPositionAndZoom : Point2d Unitless WorldCoordinates -> ZoomLevel -> Model -> Model
withPositionAndZoom position zoomLevel (Model model) =
    { model
        | viewPosition = position
        , viewZoom = zoomLevel
        , zoomAnimation = Nothing
        , debouncePending = True
    }
        |> Model


withViewBounds :
    { left : Int, right : Int, top : Int, bottom : Int }
    -> LngLat
    -> LngLat
    -> Model
    -> Model
withViewBounds padding point0 point1 (Model model) =
    let
        viewBoundsHelper_ =
            viewBoundsHelper padding point0 point1 (Model model)
    in
    { model
        | zoomAnimation = Nothing
        , viewZoom = viewBoundsHelper_.viewZoom
        , viewPosition = viewBoundsHelper_.viewPosition
        , debouncePending = True
    }
        |> Model


{-| Smoothly adjust the zoom and view position so that the map fits within the given bounds.
-}
animateViewBounds : { left : Int, right : Int, top : Int, bottom : Int } -> LngLat -> LngLat -> Model -> Model
animateViewBounds padding point0 point1 (Model model) =
    let
        viewBoundsHelper_ =
            viewBoundsHelper padding point0 point1 (Model model)
    in
    { model
        | zoomAnimation =
            PanAndZoom
                { zoomAtStart = model.viewPosition
                , zoomAtEnd = viewBoundsHelper_.viewPosition
                , startTime = Nothing
                , zoomLevelStart = model.viewZoom
                , zoomLevelEnd = viewBoundsHelper_.viewZoom
                }
                |> Just
        , debouncePending = True
    }
        |> Model


viewBoundsHelper :
    { left : Int, right : Int, top : Int, bottom : Int }
    -> LngLat
    -> LngLat
    -> Model
    -> { viewPosition : Point2d Unitless WorldCoordinates, viewZoom : ZoomLevel }
viewBoundsHelper { left, right, top, bottom } point0 point1 (Model model) =
    let
        pointA : Point2d Unitless WorldCoordinates
        pointA =
            lngLatToWorld point0

        pointB : Point2d Unitless WorldCoordinates
        pointB =
            lngLatToWorld point1

        center : Point2d Unitless WorldCoordinates
        center =
            Point2d.centroid pointA [ pointB ]

        size : { x : Float, y : Float }
        size =
            Vector2d.from pointA pointB |> Vector2d.toUnitless

        ( canvasWidth, canvasHeight ) =
            model.canvasSize

        aspectRatio : Float
        aspectRatio =
            Quantity.ratio
                (canvasWidth |> Quantity.minus paddingWidth |> Quantity.toFloatQuantity)
                (canvasHeight |> Quantity.minus paddingHeight |> Quantity.toFloatQuantity)

        sizeAspectRatio =
            size.x / size.y |> abs

        paddingWidth =
            left + right |> Pixels.pixels

        paddingHeight =
            top + bottom |> Pixels.pixels

        zoomLevelEnd =
            if sizeAspectRatio < aspectRatio then
                getViewportZoom
                    model.devicePixelRatio
                    (canvasHeight |> Quantity.minus paddingHeight)
                    (abs size.y)

            else
                getViewportZoom
                    model.devicePixelRatio
                    (canvasWidth |> Quantity.minus paddingWidth)
                    (abs size.x)

        centerOffset : Vector2d Unitless WorldCoordinates
        centerOffset =
            Vector2d.from
                (screenToWorld_ zoomLevelEnd center (Model model) Point2d.origin)
                (screenToWorld_
                    zoomLevelEnd
                    center
                    (Model model)
                    (Point2d.pixels
                        (toFloat (right - left) / 2)
                        (toFloat (bottom - top) / 2)
                    )
                )
    in
    { viewPosition = Point2d.translateBy centerOffset center
    , viewZoom = zoomLevelEnd
    }


{-| Subscriptions the map listens for. You need to make sure this is wired up with the rest of your app in order for the map to work.
-}
subscriptions : MapData -> Model -> Sub Msg
subscriptions (MapData mapData) (Model model) =
    let
        unrenderedTilesExist : Bool
        unrenderedTilesExist =
            Dict.toList mapData.data
                |> List.any
                    (\( _, tile ) ->
                        case tile of
                            TileLoaded _ ->
                                True

                            TileLoadedRaw _ ->
                                True

                            TileLoading ->
                                False

                            TileLoadedWithText _ ->
                                False

                            TileError ->
                                False
                    )

        fontNotLoading =
            case mapData.font of
                FontNotLoading _ ->
                    True

                _ ->
                    False
    in
    Sub.batch
        [ case model.zoomAnimation of
            Just zoomingAt ->
                Browser.Events.onAnimationFrame (ZoomAtAnimation zoomingAt)

            _ ->
                Sub.none
        , if unrenderedTilesExist || model.tileLoadPending || model.debouncePending || fontNotLoading then
            Browser.Events.onAnimationFrame (\_ -> NextRenderStep)

          else
            Sub.none
        ]


tileZoom : Model -> Int
tileZoom (Model model) =
    floor (ZoomLevel.toLogZoom model.viewZoom)


targetZoomAndPosition : Model -> { zoomLevel : ZoomLevel, viewPosition : Point2d Unitless WorldCoordinates }
targetZoomAndPosition (Model model) =
    case model.zoomAnimation of
        Just (ZoomInOrOut zoomInOrOut) ->
            { zoomLevel = zoomInOrOut.zoomLevel, viewPosition = zoomInOrOut.zoomAt }

        Just (PanAndZoom panAndZoom) ->
            { zoomLevel = panAndZoom.zoomLevelEnd, viewPosition = panAndZoom.zoomAtEnd }

        Nothing ->
            { zoomLevel = model.viewZoom, viewPosition = model.viewPosition }


pointToGridMinCorner : Int -> Point2d Unitless WorldCoordinates -> GridPoint
pointToGridMinCorner zoom point =
    let
        { x, y } =
            Point2d.toUnitless point
    in
    { gridX = x * 2 ^ toFloat zoom |> floor
    , gridY = y * 2 ^ toFloat zoom |> floor
    , zoom = zoom
    }


{-| Convert a longitude latitude coordinate into a world coordinate.
-}
lngLatToWorld : LngLat -> Point2d Unitless WorldCoordinates
lngLatToWorld lngLat =
    Point2d.unitless
        ((lngLat.lng + 180) / 360)
        (0.5 * (1 - (logBase e (tan (lngLat.lat * pi / 180) + 1 / cos (lngLat.lat * pi / 180)) / pi)))


tileCount : Int -> Int
tileCount zoom =
    2 ^ max 0 zoom


visibleRegion_ : Model -> BoundingBox2d Unitless WorldCoordinates
visibleRegion_ (Model model) =
    let
        topLeft =
            canvasToWorld (Model model) Point2d.origin

        ( canvasWidth, canvasHeight ) =
            model.canvasSize

        bottomRight =
            canvasToWorld
                (Model model)
                (Point2d.xy
                    (Quantity.toFloatQuantity canvasWidth)
                    (Quantity.toFloatQuantity canvasHeight)
                )
    in
    BoundingBox2d.from topLeft bottomRight


gridInsideRegion : BoundingBox2d Unitless WorldCoordinates -> GridPoint -> Bool
gridInsideRegion region gridPoint =
    BoundingBox2d.intersects
        (BoundingBox2d.from (gridMinCornerToPoint gridPoint) (gridMaxCornerToPoint gridPoint))
        region


loadMissingWithDebounce : Model -> ( Model, Cmd Msg )
loadMissingWithDebounce (Model model) =
    ( Model { model | tileLoadDebounceCounter = model.tileLoadDebounceCounter + 1 }
    , Process.sleep 300
        |> Task.perform (\() -> DebounceFinished (model.tileLoadDebounceCounter + 1))
    )


loadMissingTiles : MapboxAccessToken -> MapData -> Model -> ( MapData, Cmd Msg )
loadMissingTiles accessToken (MapData mapData) (Model model) =
    let
        zoomAndPosition =
            targetZoomAndPosition (Model model)

        zoom =
            ZoomLevel.toLogZoom zoomAndPosition.zoomLevel |> floor

        tileCount_ =
            tileCount zoom - 1

        topLeft : GridPoint
        topLeft =
            screenToWorld_ zoomAndPosition.zoomLevel zoomAndPosition.viewPosition (Model model) Point2d.origin
                |> pointToGridMinCorner zoom

        ( canvasWidth, canvasHeight ) =
            model.canvasSize

        bottomRight =
            screenToWorld_
                zoomAndPosition.zoomLevel
                zoomAndPosition.viewPosition
                (Model model)
                (Point2d.xy
                    (Quantity.toFloatQuantity canvasWidth)
                    (Quantity.toFloatQuantity canvasHeight)
                )
                |> pointToGridMinCorner zoom

        minGridPoint =
            { gridX = min topLeft.gridX bottomRight.gridX |> clamp 0 tileCount_
            , gridY = min topLeft.gridY bottomRight.gridY |> clamp 0 tileCount_
            }

        maxGridPoint =
            { gridX = max topLeft.gridX bottomRight.gridX |> clamp 0 tileCount_
            , gridY = max topLeft.gridY bottomRight.gridY |> clamp 0 tileCount_
            }

        midpointX =
            toFloat (maxGridPoint.gridX - minGridPoint.gridX) / 2 + toFloat minGridPoint.gridX

        midpointY =
            toFloat (maxGridPoint.gridY - minGridPoint.gridY) / 2 + toFloat minGridPoint.gridY

        xRange =
            List.range minGridPoint.gridX maxGridPoint.gridX

        yRange =
            List.range minGridPoint.gridY maxGridPoint.gridY

        list =
            Dict.toList mapData.data

        ( priorityTiles, newTiles ) =
            List.partition
                (\( gridPoint, _ ) ->
                    (gridPoint.zoom == 6)
                        || ((gridPoint.zoom == topLeft.zoom)
                                && (gridPoint.gridX >= minGridPoint.gridX)
                                && (gridPoint.gridX <= maxGridPoint.gridX)
                                && (gridPoint.gridY >= minGridPoint.gridY)
                                && (gridPoint.gridY <= maxGridPoint.gridY)
                           )
                )
                list

        model2 =
            { mapData
                | data =
                    Random.step
                        (Random.List.choices 100 newTiles)
                        (Random.initialSeed topLeft.gridX)
                        |> Tuple.first
                        |> Tuple.first
                        |> (++) priorityTiles
                        |> Dict.fromList
            }
    in
    xRange
        |> List.concatMap
            (\x ->
                List.filterMap
                    (\y ->
                        let
                            gridPoint =
                                { gridX = x, gridY = y, zoom = zoom }
                        in
                        case Dict.get gridPoint model2.data of
                            Just _ ->
                                Nothing

                            Nothing ->
                                Just gridPoint
                    )
                    yRange
            )
        |> List.sortBy (\{ gridX, gridY } -> abs (toFloat gridX - midpointX) + abs (toFloat gridY - midpointY) |> negate)
        |> List.foldl
            (\gridPoint ( model3, cmd ) ->
                ( { model3 | data = Dict.insert gridPoint TileLoading model3.data }
                , Cmd.batch [ cmd, loadTile accessToken (GotData gridPoint) gridPoint ]
                )
            )
            ( model2, Cmd.none )
        |> Tuple.mapFirst MapData


{-| Load a mapbox vector tile which you can then add to the map with `Map.update (Map.GotData loadTileResult) map`. You probably won't need to use this as the map automatically load tiles as needed.
-}
loadTile : MapboxAccessToken -> (Result Http.Error Bytes -> msg) -> GridPoint -> Cmd msg
loadTile accessToken onLoad position =
    Http.get
        { url =
            Url.Builder.crossOrigin
                "https://api.mapbox.com"
                [ "v4"
                , "mapbox.mapbox-streets-v8,mapbox.mapbox-terrain-v2"
                , String.fromInt position.zoom
                , String.fromInt position.gridX
                , String.fromInt position.gridY ++ ".vector.pbf"
                ]
                [ Url.Builder.string "sku" "101PpDmy6cuZN"
                , Url.Builder.string "access_token" (mapboxAccessTokenToString accessToken)
                ]
        , expect =
            Http.expectBytesResponse onLoad
                (\response ->
                    case response of
                        Http.BadUrl_ url ->
                            Err (Http.BadUrl url)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata _ ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.GoodStatus_ _ body ->
                            Ok body
                )
        }


camera_ : Point2d Unitless WorldCoordinates -> Quantity Float Unitless -> Camera3d Unitless WorldCoordinates
camera_ point viewportHeight_ =
    let
        { x, y } =
            Point2d.toUnitless point
    in
    Camera3d.orthographic
        { viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.fromUnitless { x = x, y = y, z = 0 }
                , eyePoint = Point3d.fromUnitless { x = x, y = y, z = -1 }
                , upDirection = Direction3d.negativeY
                }
        , viewportHeight = viewportHeight_
        }


{-| Get the camera that defines what part of the map is being viewed.
-}
camera : Model -> Camera3d Unitless WorldCoordinates
camera (Model model) =
    camera_ model.viewPosition (viewportHeight model.devicePixelRatio model.canvasSize model.viewZoom)


{-| The height of the viewport in world units
-}
viewportHeight : Float -> ( Quantity Int Pixels, Quantity Int Pixels ) -> ZoomLevel -> Quantity Float Unitless
viewportHeight devicePixelRatio_ ( _, canvasHeight ) zoom =
    let
        tileSize_ =
            1 / ZoomLevel.toLinearZoom zoom

        tilePixelSize =
            512 / devicePixelRatio_

        canvasHeight_ =
            Pixels.inPixels canvasHeight |> toFloat
    in
    tileSize_ * canvasHeight_ / tilePixelSize |> Quantity.float


{-| Get how zoomed in the camera is.
-}
getViewportZoom : Float -> Quantity Int Pixels -> Float -> ZoomLevel
getViewportZoom devicePixelRatio_ canvasLength viewportHeight_ =
    let
        tilePixelSize =
            512 / devicePixelRatio_

        canvasHeight =
            Pixels.inPixels canvasLength |> toFloat
    in
    canvasHeight / (tilePixelSize * viewportHeight_) |> ZoomLevel.fromLinearZoom


screenToWorld_ : ZoomLevel -> Point2d Unitless WorldCoordinates -> Model -> Point2d Pixels CanvasCoordinates -> Point2d Unitless WorldCoordinates
screenToWorld_ zoom viewPosition_ (Model model) screenPosition =
    let
        camera2 : Camera3d Unitless WorldCoordinates
        camera2 =
            camera_ viewPosition_ (viewportHeight model.devicePixelRatio model.canvasSize zoom)

        ( canvasWidth, canvasHeight ) =
            model.canvasSize

        screenRectangle : Rectangle2d Pixels CanvasCoordinates
        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero (Quantity.toFloatQuantity canvasHeight))
                (Point2d.xy (Quantity.toFloatQuantity canvasWidth) Quantity.zero)
    in
    Camera3d.ray camera2 screenRectangle screenPosition
        |> Axis3d.originPoint
        |> (\p -> Point3d.toUnitless p |> (\a -> Point2d.unitless a.x a.y))


{-| Convert a canvas position to a world position.
-}
canvasToWorld : Model -> Point2d Pixels CanvasCoordinates -> Point2d Unitless WorldCoordinates
canvasToWorld (Model model) screenPosition =
    let
        ( canvasWidth, canvasHeight ) =
            model.canvasSize

        screenRectangle : Rectangle2d Pixels CanvasCoordinates
        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero (Quantity.toFloatQuantity canvasHeight))
                (Point2d.xy (Quantity.toFloatQuantity canvasWidth) Quantity.zero)
    in
    Camera3d.ray (camera (Model model)) screenRectangle screenPosition
        |> Axis3d.originPoint
        |> (\p -> Point3d.toUnitless p |> (\a -> Point2d.unitless a.x a.y))


type OutMsg
    = PointerPressed
        { canvasPosition : Point2d Pixels CanvasCoordinates
        , worldPosition : Point2d Unitless WorldCoordinates
        }
    | PointerReleased
        { canvasPosition : Point2d Pixels CanvasCoordinates
        , worldPosition : Point2d Unitless WorldCoordinates
        , dragDistance : Quantity Float Pixels
        }


{-| Position of the camera center point.
-}
viewPosition : Model -> Point2d Unitless WorldCoordinates
viewPosition (Model model) =
    model.viewPosition


{-| Zoom level of the camera.
-}
viewZoom : Model -> ZoomLevel
viewZoom (Model model) =
    model.viewZoom


scaleLinearly : Float -> ZoomLevel -> ZoomLevel
scaleLinearly scalar zoomLevel =
    ZoomLevel.toLinearZoom zoomLevel * scalar |> ZoomLevel.fromLinearZoom


{-| The map update function. Make sure to wire this up with the rest of your app so any `Map.msg` are handled correctly.
-}
update :
    MapboxAccessToken
    -> MapData
    -> Msg
    -> Model
    -> { newModel : Model, newMapData : MapData, outMsg : Maybe OutMsg, cmd : Cmd Msg }
update accessToken (MapData mapData) msg (Model model) =
    case msg of
        GotData position result ->
            { newModel = Model model
            , newMapData =
                { mapData
                    | data =
                        Dict.insert
                            position
                            (case result of
                                Ok bytes ->
                                    TileLoadedRaw bytes

                                Err _ ->
                                    TileError
                            )
                            mapData.data
                }
                    |> MapData
            , outMsg = Nothing
            , cmd = Cmd.none
            }

        GotFont result ->
            case result of
                Ok font ->
                    { newModel = Model model
                    , newMapData = MapData { mapData | font = FontLoaded font }
                    , outMsg = Nothing
                    , cmd = Cmd.none
                    }

                Err error ->
                    { newModel = Model model
                    , newMapData = MapData { mapData | font = FontLoadFailed error }
                    , outMsg = Nothing
                    , cmd = Cmd.none
                    }

        PointerDown event ->
            { newModel =
                { model
                    | pointerIsDown = Just { dragDistance = Quantity.zero }
                    , touches =
                        RegularDict.insert event.pointerId event.position model.touches
                }
                    |> Model
            , newMapData = MapData mapData
            , outMsg =
                PointerPressed
                    { canvasPosition = event.position
                    , worldPosition = canvasToWorld (Model model) event.position
                    }
                    |> Just
            , cmd = Cmd.none
            }

        PointerUp event ->
            let
                model2 =
                    { model | touches = RegularDict.remove event.pointerId model.touches }
            in
            if RegularDict.isEmpty model2.touches then
                { newModel = Model { model2 | pointerIsDown = Nothing }
                , newMapData = MapData mapData
                , outMsg =
                    case model2.pointerIsDown of
                        Just mouseDown ->
                            PointerReleased
                                { canvasPosition = event.position
                                , worldPosition = canvasToWorld (Model model) event.position
                                , dragDistance = mouseDown.dragDistance
                                }
                                |> Just

                        Nothing ->
                            Nothing
                , cmd = Cmd.none
                }

            else
                { newModel = Model model2
                , newMapData = MapData mapData
                , outMsg = Nothing
                , cmd = Cmd.none
                }

        PointerLeave event ->
            let
                model2 =
                    { model | touches = RegularDict.remove event.pointerId model.touches }
            in
            { newModel =
                if RegularDict.isEmpty model2.touches then
                    Model { model2 | pointerIsDown = Nothing }

                else
                    Model model2
            , newMapData = MapData mapData
            , outMsg = Nothing
            , cmd = Cmd.none
            }

        MouseWheelMoved event ->
            if abs event.deltaY < 20 then
                let
                    mousePosition : Point2d Pixels CanvasCoordinates
                    mousePosition =
                        clientPosToScreen event.mouseEvent

                    zoomDelta =
                        2 ^ -(event.deltaY * 0.015)

                    ( model2, cmd ) =
                        zoomAt
                            (canvasToWorld (Model model) mousePosition)
                            zoomDelta
                            (Model model)
                            |> (\{ position, zoom } ->
                                    { model
                                        | viewPosition = position
                                        , viewZoom = zoom
                                    }
                               )
                            |> Model
                            |> loadMissingWithDebounce
                in
                { newModel = model2, newMapData = MapData mapData, outMsg = Nothing, cmd = cmd }

            else
                let
                    zoomDelta : Float
                    zoomDelta =
                        if event.deltaY > 0 then
                            0.5

                        else
                            2

                    mousePosition : Point2d Pixels CanvasCoordinates
                    mousePosition =
                        clientPosToScreen event.mouseEvent

                    ( model2, cmd ) =
                        { model
                            | zoomAnimation =
                                ZoomInOrOut
                                    { zoomLevel = scaleLinearly zoomDelta model.viewZoom
                                    , zoomAt = canvasToWorld (Model model) mousePosition
                                    }
                                    |> Just
                        }
                            |> Model
                            |> loadMissingWithDebounce
                in
                { newModel = model2, newMapData = MapData mapData, outMsg = Nothing, cmd = cmd }

        ZoomAtAnimation zoomingAt time ->
            { newModel =
                case zoomingAt of
                    ZoomInOrOut zoomInOrOut ->
                        let
                            zoom =
                                ZoomLevel.toLinearZoom model.viewZoom

                            targetZoom =
                                ZoomLevel.toLinearZoom zoomInOrOut.zoomLevel

                            zoomSpeed =
                                1.1
                        in
                        if zoom * zoomSpeed > targetZoom && zoom / zoomSpeed < targetZoom then
                            let
                                newPositionAndZoom : { position : Point2d Unitless WorldCoordinates, zoom : ZoomLevel }
                                newPositionAndZoom =
                                    zoomAt zoomInOrOut.zoomAt (targetZoom / zoom) (Model model)
                            in
                            { model
                                | lastAnimationFrame = Just time
                                , viewZoom = zoomInOrOut.zoomLevel
                                , viewPosition = newPositionAndZoom.position
                                , zoomAnimation = Nothing
                            }
                                |> Model

                        else
                            let
                                newPositionAndZoom : { position : Point2d Unitless WorldCoordinates, zoom : ZoomLevel }
                                newPositionAndZoom =
                                    zoomAt
                                        zoomInOrOut.zoomAt
                                        (if zoom < targetZoom then
                                            zoomSpeed

                                         else
                                            1 / zoomSpeed
                                        )
                                        (Model model)
                            in
                            { model
                                | lastAnimationFrame = Just time
                                , viewZoom = newPositionAndZoom.zoom
                                , viewPosition = newPositionAndZoom.position
                            }
                                |> Model

                    PanAndZoom panAndZoom ->
                        case panAndZoom.startTime of
                            Just startTime ->
                                let
                                    zoomDuration =
                                        ZoomLevel.toLogZoom panAndZoom.zoomLevelStart
                                            - ZoomLevel.toLogZoom panAndZoom.zoomLevelEnd
                                            |> abs
                                            |> logBase 2
                                            |> (*) 0.6
                                            |> max 0.5
                                            |> Duration.seconds

                                    panDuration =
                                        Point2d.distanceFrom panAndZoom.zoomAtStart panAndZoom.zoomAtEnd
                                            |> Quantity.toFloat
                                            |> (*) 100
                                            |> sqrt
                                            |> max 0.5
                                            |> Duration.seconds

                                    duration =
                                        Quantity.max zoomDuration panDuration

                                    t =
                                        Quantity.ratio (Duration.from startTime time) duration
                                            |> clamp 0 1

                                    newZoomLevel =
                                        ((ZoomLevel.toLogZoom panAndZoom.zoomLevelEnd - startZoom) * t + startZoom)
                                            |> ZoomLevel.fromLogZoom

                                    startZoom =
                                        ZoomLevel.toLogZoom panAndZoom.zoomLevelStart

                                    endZoom =
                                        ZoomLevel.toLogZoom panAndZoom.zoomLevelEnd

                                    t2 =
                                        1 - (2 ^ ((startZoom - endZoom) * t)) * (1 - t)
                                in
                                { model
                                    | zoomAnimation =
                                        if t >= 1 then
                                            Nothing

                                        else
                                            PanAndZoom panAndZoom |> Just
                                    , viewPosition =
                                        Point2d.interpolateFrom panAndZoom.zoomAtStart panAndZoom.zoomAtEnd t2
                                    , viewZoom = newZoomLevel
                                }
                                    |> Model

                            Nothing ->
                                { model
                                    | zoomAnimation = PanAndZoom { panAndZoom | startTime = Just time } |> Just
                                }
                                    |> Model
            , newMapData = MapData mapData
            , outMsg = Nothing
            , cmd = Cmd.none
            }

        NextRenderStep ->
            let
                model2 =
                    { model | debouncePending = False, tileLoadPending = False }

                ( Model model3, cmd ) =
                    if model.debouncePending then
                        loadMissingWithDebounce (Model model2)

                    else
                        ( Model model2, Cmd.none )

                ( MapData mapData2, cmd2 ) =
                    if model.tileLoadPending then
                        loadMissingTiles accessToken (MapData mapData) (Model model3)

                    else
                        ( MapData mapData, Cmd.none )

                mapData3 =
                    { mapData2
                        | font =
                            case mapData2.font of
                                FontNotLoading _ ->
                                    FontLoading

                                _ ->
                                    mapData2.font
                    }

                fontCmd : Cmd Msg
                fontCmd =
                    case mapData2.font of
                        FontNotLoading fontPath ->
                            Http.get
                                { url = fontPath
                                , expect =
                                    Http.expectJson
                                        GotFont
                                        (Serialize.getJsonDecoder identity Font.codec)
                                }

                        _ ->
                            Cmd.none

                cmds =
                    Cmd.batch [ cmd, cmd2, fontCmd ]
            in
            case
                Dict.toList mapData3.data
                    |> List.filterMap
                        (\( coord, tile ) ->
                            case tile of
                                TileLoadedRaw a ->
                                    Just ( coord, a )

                                _ ->
                                    Nothing
                        )
            of
                ( coord, bytes ) :: _ ->
                    let
                        a =
                            case Decode.decode (tileDecoder mapData3.style coord (Bytes.width bytes)) bytes of
                                Just value ->
                                    TileLoaded value

                                Nothing ->
                                    TileError
                    in
                    { newModel = Model model3
                    , newMapData = MapData { mapData3 | data = Dict.insert coord a mapData3.data }
                    , outMsg = Nothing
                    , cmd = cmds
                    }

                [] ->
                    case mapData.font of
                        FontLoaded font ->
                            { newModel = Model model3
                            , newMapData = handleNextTileToTileText model3.devicePixelRatio font (MapData mapData3)
                            , outMsg = Nothing
                            , cmd = cmds
                            }

                        _ ->
                            { newModel = Model model3
                            , newMapData = MapData mapData3
                            , outMsg = Nothing
                            , cmd = cmds
                            }

        TouchStart ->
            { newModel = Model model, newMapData = MapData mapData, outMsg = Nothing, cmd = Cmd.none }

        PointerMoved event ->
            let
                model2 =
                    { model
                        | touches =
                            RegularDict.update
                                event.pointerId
                                (Maybe.map (\_ -> event.position))
                                model.touches
                    }

                ( model3, cmd ) =
                    case ( RegularDict.values model2.touches, RegularDict.values model.touches ) of
                        ( [ single ], [ oldSingle ] ) ->
                            let
                                newPosition : Point2d Unitless WorldCoordinates
                                newPosition =
                                    let
                                        v =
                                            Vector2d.from
                                                (canvasToWorld (Model model2) single)
                                                (canvasToWorld (Model model2) oldSingle)
                                    in
                                    Point2d.translateBy v model2.viewPosition
                                        |> (\p ->
                                                Point2d.toUnitless p
                                                    |> (\{ x, y } ->
                                                            { x = clamp 0 0.99999 x, y = clamp 0 0.99999 y }
                                                                |> Point2d.fromUnitless
                                                       )
                                           )
                            in
                            { model2
                                | viewPosition = newPosition
                                , pointerIsDown =
                                    case model.pointerIsDown of
                                        Just pointerIsDown ->
                                            { pointerIsDown
                                                | dragDistance =
                                                    Quantity.plus
                                                        (Point2d.distanceFrom oldSingle single)
                                                        pointerIsDown.dragDistance
                                            }
                                                |> Just

                                        _ ->
                                            Nothing
                            }
                                |> Model
                                |> loadMissingWithDebounce

                        ( [ first, second ], [ oldFirst, oldSecond ] ) ->
                            let
                                newDistance : Quantity Float Pixels
                                newDistance =
                                    Point2d.distanceFrom first second |> Quantity.max Pixels.pixel

                                oldDistance : Quantity Float Pixels
                                oldDistance =
                                    Point2d.distanceFrom oldFirst oldSecond |> Quantity.max Pixels.pixel

                                newCenter : Point2d Unitless WorldCoordinates
                                newCenter =
                                    Point2d.centroid first [ second ]
                                        |> canvasToWorld (Model model)

                                oldCenter : Point2d Pixels CanvasCoordinates
                                oldCenter =
                                    Point2d.centroid oldFirst [ oldSecond ]

                                scaleChange : Float
                                scaleChange =
                                    Quantity.ratio newDistance oldDistance

                                newPosition : Point2d Unitless WorldCoordinates
                                newPosition =
                                    let
                                        v =
                                            Vector2d.from
                                                newCenter
                                                (canvasToWorld (Model model2) oldCenter)
                                    in
                                    Point2d.translateBy v model2.viewPosition
                                        |> clampViewPosition
                            in
                            zoomAt newCenter scaleChange (Model { model2 | viewPosition = newPosition })
                                |> (\{ position, zoom } ->
                                        { model2
                                            | viewPosition = position
                                            , viewZoom = zoom
                                        }
                                   )
                                |> Model
                                |> loadMissingWithDebounce

                        _ ->
                            ( Model model2, Cmd.none )
            in
            { newModel = model3, newMapData = MapData mapData, outMsg = Nothing, cmd = cmd }

        DebounceFinished counter ->
            if counter == model.tileLoadDebounceCounter then
                let
                    ( mapData2, cmd ) =
                        loadMissingTiles accessToken (MapData mapData) (Model model)
                in
                { newModel = Model model, newMapData = mapData2, outMsg = Nothing, cmd = cmd }

            else
                { newModel = Model model, newMapData = MapData mapData, outMsg = Nothing, cmd = Cmd.none }

        TouchMoved ->
            { newModel = Model model, newMapData = MapData mapData, outMsg = Nothing, cmd = Cmd.none }


handleNextTileToTileText : Float -> Font -> MapData -> MapData
handleNextTileToTileText devicePixelRatio font (MapData mapData) =
    case
        Dict.toList mapData.data
            |> List.filterMap
                (\( coord, tile ) ->
                    case tile of
                        TileLoaded a ->
                            Just ( coord, a )

                        _ ->
                            Nothing
                )
    of
        ( coord, tile ) :: _ ->
            let
                neighbors :
                    { roadLabelChars : List (Point2d Unitless WorldCoordinates)
                    , placeLabelChars : List (Circle2d Unitless WorldCoordinates)
                    }
                neighbors =
                    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ), ( 0, 1 ), ( 0, -1 ) ]
                        |> List.filterMap
                            (\( x, y ) ->
                                case
                                    Dict.get
                                        { gridX = coord.gridX + x, gridY = coord.gridY + y, zoom = coord.zoom }
                                        mapData.data
                                of
                                    Just (TileLoadedWithText neighborTile) ->
                                        Just neighborTile

                                    _ ->
                                        Nothing
                            )
                        |> List.foldl
                            (\{ roadLabelChars, placeLabelChars } state ->
                                { roadLabelChars = roadLabelChars ++ state.roadLabelChars
                                , placeLabelChars = placeLabelChars ++ state.placeLabelChars
                                }
                            )
                            { roadLabelChars = [], placeLabelChars = [] }
            in
            { mapData
                | data =
                    Dict.insert
                        coord
                        (tileToTileWithText mapData.style devicePixelRatio font neighbors coord tile
                            |> TileLoadedWithText
                        )
                        mapData.data
            }
                |> MapData

        [] ->
            MapData mapData


clientPosToScreen : { a | offsetPos : ( Float, Float ) } -> Point2d Pixels CanvasCoordinates
clientPosToScreen { offsetPos } =
    Point2d.fromTuple Pixels.pixels offsetPos


clampViewPosition : Point2d Unitless WorldCoordinates -> Point2d Unitless WorldCoordinates
clampViewPosition p =
    Point2d.toUnitless p
        |> (\{ x, y } ->
                { x = clamp 0 0.99999 x, y = clamp 0 0.99999 y }
                    |> Point2d.fromUnitless
           )


zoomAt : Point2d Unitless WorldCoordinates -> Float -> Model -> { position : Point2d Unitless WorldCoordinates, zoom : ZoomLevel }
zoomAt zoomPoint zoomDelta (Model model) =
    let
        ( canvasWidth, canvasHeight ) =
            model.canvasSize

        canvasCenter =
            Vector2d.xy
                (Quantity.toFloatQuantity canvasWidth)
                (Quantity.toFloatQuantity canvasHeight)
                |> Vector2d.scaleBy 0.5
                |> (\v -> Point2d.translateBy v Point2d.origin)
                |> canvasToWorld (Model model)
    in
    { position =
        Point2d.translateBy
            (Vector2d.from zoomPoint canvasCenter |> Vector2d.scaleBy (1 / zoomDelta))
            zoomPoint
            |> clampViewPosition
    , zoom = scaleLinearly zoomDelta model.viewZoom
    }


{-| If the canvas size or device pixel ratio changes, make sure to call this.
-}
resizeCanvas :
    Float
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Model
    -> Model
resizeCanvas devicePixelRatio_ canvasSize_ (Model model) =
    Model { model | canvasSize = canvasSize_, devicePixelRatio = devicePixelRatio_, debouncePending = True }


{-| Returns the size of the canvas in css pixels and in device pixels. If your device pixel ratio is 1 then these values are the same. If the device pixel ratio is 2 then the device pixel canvas size will be twice as large as the css pixel canvas size.

Important to note: the css pixel canvas size might not exactly match the canvas size you specified in `init` or `resizeCanvas`. This is because the canvas size needs to adjusted so that devicePixelRatio \* cssPixelCanvasSize gives integer values for devicePixelCanvasSize. If we didn't do this, the rendered canvas would be slightly blurry.

-}
canvasSize :
    Model
    ->
        { canvasSize : ( Quantity Int Pixels, Quantity Int Pixels )
        , devicePixelCanvasSize : ( Quantity Int DevicePixels, Quantity Int DevicePixels )
        }
canvasSize (Model model) =
    let
        ( canvasWidth, canvasHeight ) =
            model.canvasSize

        findValue : Quantity Int Pixels -> ( Int, Int )
        findValue value =
            List.range 0 9
                |> List.map ((+) (Pixels.inPixels value))
                |> List.find
                    (\v ->
                        let
                            a =
                                toFloat v * model.devicePixelRatio
                        in
                        a == toFloat (round a) && modBy 2 (round a) == 0
                    )
                |> Maybe.map (\v -> ( v, toFloat v * model.devicePixelRatio |> round ))
                |> Maybe.withDefault ( Pixels.inPixels value, toFloat (Pixels.inPixels value) * model.devicePixelRatio |> round )

        ( w, actualW ) =
            findValue canvasWidth

        ( h, actualH ) =
            findValue canvasHeight
    in
    { canvasSize = ( Pixels.pixels w, Pixels.pixels h )
    , devicePixelCanvasSize = ( Quantity actualW, Quantity actualH )
    }


{-| Draw the map! You can add additional layers on top of the map as well though for now this isn't easy to do unless you are well versed in how to use `elm-explorations/webgl`. The plan is to add helper functions in a future version of this package that make it easier.
-}
view : List WebGL.Entity -> MapData -> Model -> Html Msg
view extraLayers (MapData mapData) (Model model) =
    let
        ( cssWindowWidth, cssWindowHeight ) =
            perfectSize.canvasSize

        perfectSize =
            canvasSize (Model model)

        ( canvasWidth, canvasHeight ) =
            model.canvasSize

        aspectRatio =
            Quantity.ratio
                (Quantity.toFloatQuantity canvasWidth)
                (Quantity.toFloatQuantity canvasHeight)

        viewMatrix : Mat4
        viewMatrix =
            WebGL.Matrices.viewProjectionMatrix
                (camera (Model model))
                { nearClipDepth = Quantity.float 0.1
                , farClipDepth = Quantity.float 10
                , aspectRatio = aspectRatio
                }

        zoom : Int
        zoom =
            tileZoom (Model model)

        normalZoom : Float
        normalZoom =
            ZoomLevel.toLinearZoom model.viewZoom

        visibleBounds : BoundingBox2d Unitless WorldCoordinates
        visibleBounds =
            visibleRegion_ (Model model)

        drawRoad : Bool -> { a | roadLayer : WebGL.Mesh RoadVertex } -> Float -> Mat4 -> WebGL.Entity
        drawRoad isOutline tile relativeZoom matrix =
            WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.back ]
                roadVertexShader
                roadFragmentShader
                tile.roadLayer
                { color = Vec4.vec4 0.7 0.7 0.7 1
                , matrix = matrix
                , isOutline =
                    if isOutline then
                        1

                    else
                        0
                , offsetScale =
                    if zoom > 14 then
                        (normalZoom / (2 ^ 14)) / relativeZoom

                    else
                        1 / relativeZoom
                }

        visibleTiles : List (List VisibleTile)
        visibleTiles =
            Dict.toList mapData.data
                |> List.filterMap
                    (\( position, result ) ->
                        let
                            gridX2 =
                                position.gridX * 2

                            gridY2 =
                                position.gridY * 2

                            isVisible =
                                List.any
                                    (\childPosition ->
                                        gridInsideRegion visibleBounds childPosition && not (Dict.member childPosition mapData.data)
                                    )
                                    [ { zoom = position.zoom + 1, gridX = gridX2, gridY = gridY2 + 1 }
                                    , { zoom = position.zoom + 1, gridX = gridX2 + 1, gridY = gridY2 + 1 }
                                    , { zoom = position.zoom + 1, gridX = gridX2 + 1, gridY = gridY2 }
                                    , { zoom = position.zoom + 1, gridX = gridX2, gridY = gridY2 }
                                    ]
                        in
                        if
                            (position.zoom == zoom && gridInsideRegion visibleBounds position)
                                || (position.zoom < zoom && isVisible)
                        then
                            let
                                modelMatrix =
                                    Math.Matrix4.makeTranslate3 point.x point.y 0
                                        |> Math.Matrix4.scale3 x y 0

                                matrix : Mat4
                                matrix =
                                    Math.Matrix4.mul viewMatrix modelMatrix

                                point =
                                    gridMinCornerToPoint position |> Point2d.toUnitless

                                { x, y } =
                                    tileSize (toFloat position.zoom) |> Vector2d.toUnitless
                            in
                            Just
                                { relativeZoom = normalZoom / (2 ^ toFloat position.zoom)
                                , matrix = matrix
                                , result = result
                                , zoom = position.zoom
                                }

                        else
                            Nothing
                    )
                |> List.gatherEqualsBy .zoom
                |> List.sortBy (Tuple.first >> .zoom)
                |> List.map (\( head, rest ) -> head :: rest)

        contour : List { a | matrix : Mat4, result : TileState } -> List WebGL.Entity
        contour tiles =
            List.concatMap
                (\{ matrix, result } ->
                    case result of
                        TileLoaded tile ->
                            drawLayer False matrix mapData.style.water tile.waterLayer

                        TileLoadedWithText tile ->
                            drawLayer False matrix mapData.style.water tile.waterLayer

                        TileError ->
                            []

                        TileLoading ->
                            []

                        TileLoadedRaw _ ->
                            []
                )
                tiles

        nature : List { a | matrix : Mat4, result : TileState } -> List WebGL.Entity
        nature tiles =
            List.concatMap
                (\{ matrix, result } ->
                    case result of
                        TileLoaded tile ->
                            drawLayer False matrix mapData.style.nature tile.natureLayer

                        TileLoadedWithText tile ->
                            drawLayer False matrix mapData.style.nature tile.natureLayer

                        TileError ->
                            []

                        TileLoading ->
                            []

                        TileLoadedRaw _ ->
                            []
                )
                tiles

        buildings : List { a | matrix : Mat4, result : TileState } -> List WebGL.Entity
        buildings tiles =
            List.concatMap
                (\{ matrix, result } ->
                    case result of
                        TileLoaded tile ->
                            drawLayer False matrix mapData.style.buildings tile.buildingLayer

                        TileLoadedWithText tile ->
                            drawLayer False matrix mapData.style.buildings tile.buildingLayer

                        TileError ->
                            []

                        TileLoading ->
                            []

                        TileLoadedRaw _ ->
                            []
                )
                tiles

        background : List { a | matrix : Mat4, result : TileState } -> List WebGL.Entity
        background tiles =
            List.concatMap
                (\{ matrix, result } ->
                    case result of
                        TileLoaded _ ->
                            [ WebGL.entityWith
                                []
                                vertexShader
                                fragmentShader
                                square
                                { color = mapData.style.ground
                                , matrix = matrix
                                }
                            ]

                        TileLoadedWithText _ ->
                            [ WebGL.entityWith
                                []
                                vertexShader
                                fragmentShader
                                square
                                { color = mapData.style.ground
                                , matrix = matrix
                                }
                            ]

                        TileError ->
                            []

                        TileLoading ->
                            []

                        TileLoadedRaw _ ->
                            []
                )
                tiles

        roads : Bool -> List VisibleTile -> List WebGL.Entity
        roads isOutline tiles =
            List.filterMap
                (\{ relativeZoom, matrix, result } ->
                    case result of
                        TileLoaded tile ->
                            drawRoad isOutline tile relativeZoom matrix |> Just

                        TileLoadedWithText tile ->
                            drawRoad isOutline tile relativeZoom matrix |> Just

                        TileError ->
                            Nothing

                        TileLoading ->
                            Nothing

                        TileLoadedRaw _ ->
                            Nothing
                )
                tiles

        roadLabels : List VisibleTile -> List WebGL.Entity
        roadLabels tiles =
            List.filterMap
                (\{ matrix, result, relativeZoom } ->
                    case result of
                        TileLoaded _ ->
                            Nothing

                        TileLoadedWithText tile ->
                            WebGL.entityWith
                                [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                labelVertexShader
                                roadFragmentShader
                                tile.roadLabelLayer
                                { color = Vec4.vec4 0.7 0.7 0.7 1
                                , matrix = matrix
                                , offsetScale = model.devicePixelRatio / relativeZoom
                                }
                                |> Just

                        TileError ->
                            Nothing

                        TileLoading ->
                            Nothing

                        TileLoadedRaw _ ->
                            Nothing
                )
                tiles

        placeLabels : List VisibleTile -> List WebGL.Entity
        placeLabels tiles =
            List.filterMap
                (\{ matrix, result, relativeZoom } ->
                    case result of
                        TileLoaded _ ->
                            Nothing

                        TileLoadedWithText tile ->
                            WebGL.entityWith
                                [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                labelVertexShader
                                roadFragmentShader
                                tile.placeLabelLayer
                                { color = Vec4.vec4 0.7 0.7 0.7 1
                                , matrix = matrix
                                , offsetScale = model.devicePixelRatio / relativeZoom
                                }
                                |> Just

                        TileError ->
                            Nothing

                        TileLoading ->
                            Nothing

                        TileLoadedRaw _ ->
                            Nothing
                )
                tiles
    in
    WebGL.toHtmlWith
        [ WebGL.stencil 0
        , WebGL.clearColor
            (Vec4.getX mapData.style.background)
            (Vec4.getY mapData.style.background)
            (Vec4.getZ mapData.style.background)
            (Vec4.getW mapData.style.background)
        , WebGL.antialias
        ]
        ([ Html.Attributes.width (Quantity.unwrap (Tuple.first perfectSize.devicePixelCanvasSize))
         , Html.Attributes.height (Quantity.unwrap (Tuple.second perfectSize.devicePixelCanvasSize))
         , Html.Attributes.style "width" (String.fromInt (Pixels.inPixels cssWindowWidth) ++ "px")
         , Html.Attributes.style "height" (String.fromInt (Pixels.inPixels cssWindowHeight) ++ "px")
         , Html.Events.Extra.Wheel.onWheel MouseWheelMoved
         , Html.Events.Extra.Touch.onStart (\_ -> TouchStart)
         , Html.Events.Extra.Touch.onMove (\_ -> TouchMoved)
         , Html.Events.Extra.Pointer.onDown (eventToPointerEvent PointerDown)
         , Html.Events.Extra.Pointer.onUp (eventToPointerEvent PointerUp)
         , Html.Events.Extra.Pointer.onLeave (eventToPointerEvent PointerLeave)
         ]
            ++ (case model.pointerIsDown of
                    Just _ ->
                        [ Html.Events.Extra.Pointer.onMove (eventToPointerEvent PointerMoved) ]

                    Nothing ->
                        []
               )
        )
        (List.concatMap
            (\tiles ->
                background tiles
                    ++ nature tiles
                    ++ contour tiles
                    ++ buildings tiles
                    ++ roads True tiles
                    ++ roads False tiles
                    ++ roadLabels tiles
                    ++ placeLabels tiles
            )
            visibleTiles
            ++ extraLayers
        )


eventToPointerEvent : (PointerEvent -> msg) -> Html.Events.Extra.Pointer.Event -> msg
eventToPointerEvent msg event =
    msg { pointerId = event.pointerId, position = clientPosToScreen event.pointer }


drawLayer : Bool -> Mat4 -> Vec4 -> WebGL.Mesh Vertex -> List WebGL.Entity
drawLayer invertFill matrix color mesh =
    [ WebGL.entityWith
        [ WebGL.Settings.StencilTest.test
            { ref = 1
            , mask = 0xFF
            , test = WebGL.Settings.StencilTest.always
            , fail = WebGL.Settings.StencilTest.invert
            , zfail = WebGL.Settings.StencilTest.invert
            , zpass = WebGL.Settings.StencilTest.invert
            , writeMask = 0xFF
            }
        , WebGL.Settings.colorMask False False False False
        ]
        vertexShader
        fragmentShader
        mesh
        { color = Vec4.vec4 0 0 0 1
        , matrix = matrix
        }
    , WebGL.entityWith
        [ WebGL.Settings.StencilTest.test
            { ref = 0
            , mask = 0xFF
            , test =
                if invertFill then
                    WebGL.Settings.StencilTest.greaterOrEqual

                else
                    WebGL.Settings.StencilTest.less
            , fail = WebGL.Settings.StencilTest.replace
            , zfail = WebGL.Settings.StencilTest.replace
            , zpass = WebGL.Settings.StencilTest.replace
            , writeMask = 0xFF
            }
        , WebGL.Settings.Blend.add WebGL.Settings.Blend.srcAlpha WebGL.Settings.Blend.oneMinusSrcAlpha
        ]
        vertexShader
        fragmentShader
        viewportSquare
        { color = color
        , matrix = Math.Matrix4.identity
        }
    ]


viewportSquare : WebGL.Mesh Vertex
viewportSquare =
    WebGL.triangleFan
        [ { x = -1, y = -1 }
        , { x = 1, y = -1 }
        , { x = 1, y = 1 }
        , { x = -1, y = 1 }
        ]


square : WebGL.Mesh Vertex
square =
    WebGL.triangleFan
        [ { x = 0, y = 0 }
        , { x = 1, y = 0 }
        , { x = 1, y = 1 }
        , { x = 0, y = 1 }
        ]


vertexShader : Shader Vertex { a | matrix : Mat4 } {}
vertexShader =
    [glsl|

attribute float x;
attribute float y;
uniform mat4 matrix;

void main () {
  gl_Position = matrix * vec4(vec2(x, y), 0.0, 1.0);
}

|]


fragmentShader : Shader {} { a | color : Vec4 } {}
fragmentShader =
    [glsl|
precision mediump float;
uniform vec4 color;

void main () {
    gl_FragColor = color;
}
    |]


labelVertexShader : Shader LabelVertex { a | matrix : Mat4, offsetScale : Float } { color2 : Vec3 }
labelVertexShader =
    [glsl|

attribute float positionX;
attribute float positionY;
attribute float offsetX;
attribute float offsetY;
attribute vec3 color;
uniform mat4 matrix;
uniform float offsetScale;
varying vec3 color2;

void main () {
  gl_Position = matrix * vec4(vec2(positionX, positionY) + vec2(offsetX, offsetY) * offsetScale, 0.0, 1.0);
  color2 = color;
}

|]


roadVertexShader : Shader RoadVertex { a | matrix : Mat4, isOutline : Float, offsetScale : Float } { color2 : Vec3 }
roadVertexShader =
    [glsl|
attribute float positionX;
attribute float positionY;
attribute float offsetX;
attribute float offsetY;
attribute vec3 color;
attribute vec3 outlineColor;
uniform mat4 matrix;
uniform float isOutline;
uniform float offsetScale;
varying vec3 color2;

void main () {
  gl_Position = matrix * vec4(vec2(positionX, positionY) + (isOutline == 1.0 ? 1.0 : 0.7) * vec2(offsetX, offsetY) * offsetScale, 0.0, 1.0);
  color2 = isOutline == 1.0 ? outlineColor : color;
}

|]


roadFragmentShader : Shader {} a { color2 : Vec3 }
roadFragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 color2;

        void main () {
            gl_FragColor = vec4(color2, 1.0);
        }
    |]



------ Vector tile decoding ------


{-| -}
type WorldCoordinates
    = WorldCoordinates Never


type alias InternalStyle =
    { water : Vec4
    , ground : Vec4
    , buildings : Vec4
    , nature : Vec4
    , background : Vec4
    , primaryRoad : Vec3
    , primaryRoadOutline : Vec3
    , primaryRoadLink : Vec3
    , primaryRoadLinkOutline : Vec3
    , secondaryRoad : Vec3
    , secondaryRoadOutline : Vec3
    , secondaryRoadLink : Vec3
    , secondaryRoadLinkOutline : Vec3
    , tertiaryRoad : Vec3
    , tertiaryRoadOutline : Vec3
    , tertiaryRoadLink : Vec3
    , tertiaryRoadLinkOutline : Vec3
    , motorway : Vec3
    , motorwayOutline : Vec3
    , motorwayLink : Vec3
    , motorwayLinkOutline : Vec3
    , trunkRoad : Vec3
    , trunkRoadOutline : Vec3
    , trunkRoadLink : Vec3
    , trunkRoadLinkOutline : Vec3
    , railroad : Vec3
    , railroadOutline : Vec3
    , tramline : Vec3
    , tramlineOutline : Vec3
    , subway : Vec3
    , subwayOutline : Vec3
    , narrowGaugeRailroad : Vec3
    , narrowGaugeRailroadOutline : Vec3
    , trail : Vec3
    , trailOutline : Vec3
    , footway : Vec3
    , footwayOutline : Vec3
    , residentialRoad : Vec3
    , residentialRoadOutline : Vec3
    , road : Vec3
    , roadOutline : Vec3
    , pedestrianPath : Vec3
    , pedestrianPathOutline : Vec3
    , unclassifiedRoad : Vec3
    , unclassifiedRoadOutline : Vec3
    , platform : Vec3
    , platformOutline : Vec3
    , livingStreet : Vec3
    , livingStreetOutline : Vec3
    , serviceRoad : Vec3
    , serviceRoadOutline : Vec3
    , placeLabel : Vec3
    , placeLabelOutline : Vec3
    , roadLabel : Vec3
    , roadLabelOutline : Vec3
    }


type alias GridPoint =
    { gridX : Int, gridY : Int, zoom : Int }


type alias Tile =
    { waterLayer : WebGL.Mesh Vertex
    , natureLayer : WebGL.Mesh Vertex
    , buildingLayer : WebGL.Mesh Vertex
    , roadLayer : WebGL.Mesh RoadVertex
    , roads : List Road
    , placeLabels : List PlaceLabel
    }


type TextAnchor
    = Center
    | Left
    | Right
    | Top
    | Bottom
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight


type alias PlaceLabel =
    { text : String
    , position : Point2d Unitless Unitless
    , class : PlaceClass
    , textAnchor : TextAnchor
    , symbolRank : Int
    , isCapital : Bool
    }


type PlaceClass
    = Country
    | State
    | Settlement
    | SettlementSubdivision


type alias TileWithText =
    { waterLayer : WebGL.Mesh Vertex
    , natureLayer : WebGL.Mesh Vertex
    , buildingLayer : WebGL.Mesh Vertex
    , roadLayer : WebGL.Mesh RoadVertex
    , roadLabelLayer : WebGL.Mesh TextVertex
    , placeLabelLayer : WebGL.Mesh TextVertex
    , roadLabelChars : List (Point2d Unitless WorldCoordinates)
    , placeLabelChars : List (Circle2d Unitless WorldCoordinates)
    }


type GeomType
    = Unknown
    | Point
    | LineString
    | Polygon


type Value
    = StringValue String
    | FloatValue
    | DoubleValue
    | Int64Value
    | Uint64Value Int64
    | Sint64Value
    | BoolValue


type alias FeatureTemp =
    { id : Int64
    , tags : List Int
    , type_ : GeomType
    , mesh : Bytes --WebGL.Mesh Vertex
    }


type Layer
    = WaterLayer (WebGL.Mesh Vertex)
    | NatureLayer (WebGL.Mesh Vertex)
    | BuildingLayer (WebGL.Mesh Vertex)
    | RoadLayer (WebGL.Mesh RoadVertex) (List Road)
    | PlaceLabelLayer (List PlaceLabel)


type alias LayerTemp =
    { version : Int
    , name : String
    , features : List FeatureTemp
    , keys : Array String
    , values : Array Value
    , extent : Int
    }


type alias Vertex =
    { x : Float, y : Float }


type alias RoadVertex =
    { positionX : Float, positionY : Float, offsetX : Float, offsetY : Float, color : Vec3, outlineColor : Vec3 }


type alias LabelVertex =
    { positionX : Float, positionY : Float, offsetX : Float, offsetY : Float, color : Vec3 }


type alias LandcoverBuilder =
    { index : Int, indices : List ( Int, Int, Int ), vertices : List Vertex }


type alias RoadBuilder =
    { index : Int
    , vertices : List RoadVertex
    , roads : List Road
    , indices : List ( Int, Int, Int )
    }


type alias RoadLabelBuilder =
    { index : Int
    , vertices : List TextVertex
    , indices : List ( Int, Int, Int )
    , newGlyphs : List (Point2d Unitless WorldCoordinates)
    }


type alias PlaceLabelBuilder =
    { index : Int
    , vertices : List TextVertex
    , indices : List ( Int, Int, Int )
    , newGlyphs : List (Circle2d Unitless WorldCoordinates)
    }


type alias TextVertex =
    { positionX : Float, positionY : Float, offsetX : Float, offsetY : Float, color : Vec3 }


type alias Road =
    { path : Nonempty (Point2d Unitless Unitless)
    , roadName : String
    , alwaysDrawLabel : Bool
    }


tileToTileWithText :
    InternalStyle
    -> Float
    -> Font
    -> { roadLabelChars : List (Point2d Unitless WorldCoordinates), placeLabelChars : List (Circle2d Unitless WorldCoordinates) }
    -> GridPoint
    -> Tile
    -> TileWithText
tileToTileWithText style devicePixelRatio font existingGlyphs tilePosition tile =
    let
        placeLabels : PlaceLabelBuilder
        placeLabels =
            List.foldl
                (drawPlaceLabels style devicePixelRatio font tilePosition)
                { index = 0
                , indices = []
                , vertices = []
                , newGlyphs = []
                }
                tile.placeLabels

        updatedExistingGlyphs =
            { roadLabelChars = existingGlyphs.roadLabelChars
            , placeLabelChars = placeLabels.newGlyphs ++ existingGlyphs.placeLabelChars
            }

        roadLabels : RoadLabelBuilder
        roadLabels =
            List.foldl
                (\road builder ->
                    drawRoadLabels style devicePixelRatio font updatedExistingGlyphs tilePosition builder road
                )
                { index = 0
                , indices = []
                , vertices = []
                , newGlyphs = []
                }
                tile.roads
    in
    { waterLayer = tile.waterLayer
    , natureLayer = tile.natureLayer
    , buildingLayer = tile.buildingLayer
    , roadLayer = tile.roadLayer
    , roadLabelLayer = WebGL.indexedTriangles (List.reverse roadLabels.vertices) roadLabels.indices
    , placeLabelLayer = WebGL.indexedTriangles (List.reverse placeLabels.vertices) placeLabels.indices
    , roadLabelChars = roadLabels.newGlyphs
    , placeLabelChars = placeLabels.newGlyphs
    }


tileDecoder : InternalStyle -> GridPoint -> Int -> Decode.Decoder Tile
tileDecoder style tilePosition width =
    Decode.loop
        { width = width
        , model =
            { waterLayer = WebGL.triangles []
            , natureLayer = WebGL.triangles []
            , buildingLayer = WebGL.triangles []
            , roadLayer = WebGL.triangles []
            , roads = []
            , placeLabels = []
            }
        }
        (\state ->
            if state.width == 0 then
                Decode.succeed (Decode.Done state.model)

            else if state.width < 0 then
                Decode.fail

            else
                ProtobufDecode.varIntDecoder
                    |> Decode.andThen
                        (\( usedBytes, _ ) ->
                            ProtobufDecode.varIntDecoder
                                |> Decode.map (\( n, wireType ) -> ( usedBytes + n, wireType ))
                        )
                    |> Decode.andThen
                        (\( usedBytes, width_ ) ->
                            Decode.map
                                (\( n, maybeLayer ) ->
                                    let
                                        model =
                                            state.model
                                    in
                                    Decode.Loop
                                        { width = state.width - usedBytes - n
                                        , model =
                                            case maybeLayer of
                                                Just (WaterLayer mesh) ->
                                                    { model | waterLayer = mesh }

                                                Just (NatureLayer mesh) ->
                                                    { model | natureLayer = mesh }

                                                Just (RoadLayer mesh roads) ->
                                                    { model | roadLayer = mesh, roads = roads }

                                                Just (PlaceLabelLayer labels) ->
                                                    { model | placeLabels = labels }

                                                Just (BuildingLayer mesh) ->
                                                    { model | buildingLayer = mesh }

                                                Nothing ->
                                                    model
                                        }
                                )
                                (layerDecoder_ style tilePosition width_)
                        )
        )


geomTypeDecoder : Decode.Decoder ( Int, GeomType )
geomTypeDecoder =
    ProtobufDecode.int32
        |> Decode.map
            (\( usedBytes, value ) ->
                ( usedBytes
                , case value of
                    1 ->
                        Point

                    2 ->
                        LineString

                    3 ->
                        Polygon

                    _ ->
                        Unknown
                )
            )


valueDecoder : Decoder Value
valueDecoder =
    ProtobufDecode.message (StringValue "")
        [ ProtobufDecode.optional 1 ProtobufDecode.string (\v _ -> StringValue v)
        , ProtobufDecode.optional 2 ProtobufDecode.float (\_ _ -> FloatValue)
        , ProtobufDecode.optional 3 ProtobufDecode.double (\_ _ -> DoubleValue)
        , ProtobufDecode.optional 4 ProtobufDecode.int64 (\_ _ -> Int64Value)
        , ProtobufDecode.optional 5 ProtobufDecode.uint64 (\v _ -> Uint64Value v)
        , ProtobufDecode.optional 6 ProtobufDecode.sint64 (\_ _ -> Sint64Value)
        , ProtobufDecode.optional 7 ProtobufDecode.bool (\_ _ -> BoolValue)
        ]


decodeWaterGeometryV2 : { a | type_ : GeomType, mesh : Bytes } -> Maybe (WebGL.Mesh Vertex)
decodeWaterGeometryV2 feature =
    Decode.decode
        (geometryDecoderV2 (Bytes.width feature.mesh))
        feature.mesh


decodeLandcoverGeometry : LayerTemp -> FeatureTemp -> LandcoverBuilder -> LandcoverBuilder
decodeLandcoverGeometry layer feature builder =
    case getTagsV2 layer feature "class" of
        Just (StringValue "grass") ->
            Decode.map
                (\geometry ->
                    let
                        geometry_ : List (List Vertex)
                        geometry_ =
                            List.map (List.map Point2d.unwrap) geometry
                    in
                    List.foldl
                        (\vertices builder2 ->
                            let
                                length =
                                    List.length vertices
                            in
                            { index = builder2.index + length
                            , indices = indexVertices builder2.index length ++ builder2.indices
                            , vertices = vertices ++ builder2.vertices
                            }
                        )
                        builder
                        geometry_
                )
                (geometryDecoder (Bytes.width feature.mesh))
                |> (\decoder ->
                        case Decode.decode decoder feature.mesh of
                            Just newBuilder ->
                                newBuilder

                            Nothing ->
                                builder
                   )

        _ ->
            builder


decodeBuildingGeometry : FeatureTemp -> LandcoverBuilder -> LandcoverBuilder
decodeBuildingGeometry feature builder =
    Decode.map
        (\geometry ->
            let
                geometry_ : List (List Vertex)
                geometry_ =
                    List.map (List.map Point2d.unwrap) geometry
            in
            List.foldl
                (\vertices builder2 ->
                    let
                        length =
                            List.length vertices
                    in
                    { index = builder2.index + length
                    , indices = indexVertices builder2.index length ++ builder2.indices
                    , vertices = vertices ++ builder2.vertices
                    }
                )
                builder
                geometry_
        )
        (geometryDecoder (Bytes.width feature.mesh))
        |> (\decoder ->
                case Decode.decode decoder feature.mesh of
                    Just newBuilder ->
                        newBuilder

                    Nothing ->
                        builder
           )


getTags : { a | keys : Array String, values : Array Value } -> { b | tags : List Int } -> RegularDict.Dict String Value
getTags layer feature =
    List.groupsOf 2 feature.tags
        |> List.map
            (\list ->
                case list of
                    [ keyIndex, valueIndex ] ->
                        case
                            ( Array.get keyIndex layer.keys
                            , Array.get valueIndex layer.values
                            )
                        of
                            ( Just key, Just value ) ->
                                ( key, value )

                            _ ->
                                ( "bad key", StringValue "bad value" )

                    _ ->
                        ( "bad key", StringValue "bad value" )
            )
        |> RegularDict.fromList


arrayFindIndex : (a -> Bool) -> Array a -> Maybe Int
arrayFindIndex isEqual array =
    arrayFindIndexHelper isEqual array 0


arrayFindIndexHelper : (a -> Bool) -> Array a -> Int -> Maybe Int
arrayFindIndexHelper isEqual array index =
    case Array.get index array of
        Just value ->
            if isEqual value then
                Just index

            else
                arrayFindIndexHelper isEqual array (index + 1)

        Nothing ->
            Nothing


getTagsV2 : { a | keys : Array String, values : Array Value } -> { b | tags : List Int } -> String -> Maybe Value
getTagsV2 layer feature key =
    case arrayFindIndex ((==) key) layer.keys of
        Just index ->
            case getValueFromTag index feature.tags of
                Just value ->
                    Array.get value layer.values

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


getValueFromTag : Int -> List Int -> Maybe Int
getValueFromTag tagIndex tags =
    case tags of
        key :: value :: rest ->
            if key == tagIndex then
                Just value

            else
                getValueFromTag tagIndex rest

        _ ->
            Nothing


bytesToList : Bytes -> List Int
bytesToList bytes =
    Decode.decode
        (Decode.loop
            ( Bytes.width bytes, [] )
            (\( count, list ) ->
                if count == 0 then
                    Decode.Done (List.reverse list) |> Decode.succeed

                else
                    Decode.unsignedInt8
                        |> Decode.map (\value -> Decode.Loop ( count - 1, value :: list ))
            )
        )
        bytes
        |> Maybe.withDefault []


decodeRoadGeometryV2 :
    InternalStyle
    -> RoadBuilder
    -> { a | keys : Array String, values : Array Value }
    -> FeatureTemp
    -> RoadBuilder
decodeRoadGeometryV2 style roadBuilder layer feature =
    let
        maybeName : Maybe String
        maybeName =
            case getTagsV2 layer feature "name" of
                Just (StringValue value) ->
                    Just value

                _ ->
                    Nothing

        maybeRoadData : Maybe { color : Vec3, outlineColor : Vec3, width : Float, alwaysDrawLabel : Bool }
        maybeRoadData =
            case getTagsV2 layer feature "type" of
                Just (StringValue value) ->
                    case value of
                        "primary" ->
                            { color = style.primaryRoad
                            , outlineColor = style.primaryRoadOutline
                            , width = 0.005
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "primary_link" ->
                            { color = style.primaryRoadLink
                            , outlineColor = style.primaryRoadLinkOutline
                            , width = 0.0025
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "secondary" ->
                            { color = style.secondaryRoad
                            , outlineColor = style.secondaryRoadOutline
                            , width = 0.004
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "secondary_link" ->
                            { color = style.secondaryRoadLink
                            , outlineColor = style.secondaryRoadLinkOutline
                            , width = 0.002
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "tertiary" ->
                            { color = style.tertiaryRoad
                            , outlineColor = style.tertiaryRoadOutline
                            , width = 0.003
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "tertiary_link" ->
                            { color = style.tertiaryRoadLink
                            , outlineColor = style.tertiaryRoadLinkOutline
                            , width = 0.0015
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "residential" ->
                            { color = style.residentialRoad
                            , outlineColor = style.residentialRoadOutline
                            , width = 0.002
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "motorway" ->
                            { color = style.motorway
                            , outlineColor = style.motorwayOutline
                            , width = 0.005
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "motorway_link" ->
                            { color = style.motorwayLink
                            , outlineColor = style.motorwayLinkOutline
                            , width = 0.0025
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "road" ->
                            { color = style.road
                            , outlineColor = style.roadOutline
                            , width = 0.001
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "trunk" ->
                            { color = style.trunkRoad
                            , outlineColor = style.trunkRoadOutline
                            , width = 0.005
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "trunk_link" ->
                            { color = style.trunkRoadLink
                            , outlineColor = style.trunkRoadLinkOutline
                            , width = 0.0025
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "pedestrian" ->
                            { color = style.pedestrianPath
                            , outlineColor = style.pedestrianPathOutline
                            , width = 0.002
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "unclassified" ->
                            { color = style.unclassifiedRoad
                            , outlineColor = style.unclassifiedRoadOutline
                            , width = 0.002
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "platform" ->
                            { color = style.platform
                            , outlineColor = style.platformOutline
                            , width = 0.002
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "rail" ->
                            { color = style.railroad
                            , outlineColor = style.railroadOutline
                            , width = 0.0005
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "tram" ->
                            { color = style.tramline
                            , outlineColor = style.tramlineOutline
                            , width = 0.0004
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "subway" ->
                            { color = style.subway
                            , outlineColor = style.subwayOutline
                            , width = 0.0004
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "narrow_gauge" ->
                            { color = style.narrowGaugeRailroad
                            , outlineColor = style.narrowGaugeRailroadOutline
                            , width = 0.0004
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "trail" ->
                            { color = style.trail
                            , outlineColor = style.trailOutline
                            , width = 0.0004
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "footway" ->
                            { color = style.footway
                            , outlineColor = style.footwayOutline
                            , width = 0.0004
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "living_street" ->
                            { color = style.livingStreet
                            , outlineColor = style.livingStreetOutline
                            , width = 0.001
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "service" ->
                            { color = style.serviceRoad
                            , outlineColor = style.serviceRoadOutline
                            , width = 0.001
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        --"track" ->
                        --    Nothing
                        --
                        --"level_crossing" ->
                        --    Nothing
                        --
                        --"ferry" ->
                        --    Nothing
                        --
                        --"ferry_auto" ->
                        --    Nothing
                        --
                        --"footway" ->
                        --    Nothing
                        --
                        --"steps" ->
                        --    Nothing
                        --
                        --"sidewalk" ->
                        --    Nothing
                        --
                        --"crossing" ->
                        --    Nothing
                        --
                        --"cycleway" ->
                        --    Nothing
                        --
                        --"traffic_signals" ->
                        --    Nothing
                        --
                        --"turning_circle" ->
                        --    Nothing
                        --
                        --"mini_roundabout" ->
                        --    Nothing
                        --
                        --"turning_loop" ->
                        --    Nothing
                        _ ->
                            Nothing

                --name ->
                --    let
                --        _ =
                --            Debug.log "" name
                --    in
                --    { color = Vec3.vec3 1 0 0
                --    , outlineColor = style.serviceRoadOutline
                --    , width = 0.001
                --    , alwaysDrawLabel = False
                --    }
                --        |> Just
                _ ->
                    Nothing
    in
    case maybeRoadData of
        Just roadData ->
            Decode.map
                (decodeRoadGeometryHelper roadBuilder roadData maybeName)
                (geometryDecoder (Bytes.width feature.mesh))
                |> (\decoder -> Decode.decode decoder feature.mesh)
                |> Maybe.withDefault roadBuilder

        Nothing ->
            roadBuilder


decodeRoadGeometryV1 :
    InternalStyle
    -> RoadBuilder
    -> { a | keys : Array String, values : Array Value }
    -> FeatureTemp
    -> RoadBuilder
decodeRoadGeometryV1 style roadBuilder layer feature =
    let
        --_ =
        --    Debug.log ""
        --        ( { keys = layer.keys, values = layer.values }
        --        , { tags = feature.tags
        --          , mesh =
        --                Decode.decode
        --                    (Decode.loop
        --                        ( Bytes.width feature.mesh, [] )
        --                        (\( count, list ) ->
        --                            if count == 0 then
        --                                Decode.Done (List.reverse list) |> Decode.succeed
        --
        --                            else
        --                                Decode.unsignedInt8
        --                                    |> Decode.map (\value -> Decode.Loop ( count - 1, value :: list ))
        --                        )
        --                    )
        --                    feature.mesh
        --          , type_ = feature.type_
        --          }
        --        )
        tags =
            getTags layer feature

        maybeName : Maybe String
        maybeName =
            case RegularDict.get "name" tags of
                Just (StringValue value) ->
                    Just value

                _ ->
                    Nothing

        maybeRoadData : Maybe { color : Vec3, outlineColor : Vec3, width : Float, alwaysDrawLabel : Bool }
        maybeRoadData =
            case RegularDict.get "type" tags of
                Just (StringValue value) ->
                    case value of
                        "primary" ->
                            { color = style.primaryRoad
                            , outlineColor = style.primaryRoadOutline
                            , width = 0.005
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "primary_link" ->
                            { color = style.primaryRoadLink
                            , outlineColor = style.primaryRoadLinkOutline
                            , width = 0.0025
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "secondary" ->
                            { color = style.secondaryRoad
                            , outlineColor = style.secondaryRoadOutline
                            , width = 0.004
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "secondary_link" ->
                            { color = style.secondaryRoadLink
                            , outlineColor = style.secondaryRoadLinkOutline
                            , width = 0.002
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "tertiary" ->
                            { color = style.tertiaryRoad
                            , outlineColor = style.tertiaryRoadOutline
                            , width = 0.003
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "tertiary_link" ->
                            { color = style.tertiaryRoadLink
                            , outlineColor = style.tertiaryRoadLinkOutline
                            , width = 0.0015
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "residential" ->
                            { color = style.residentialRoad
                            , outlineColor = style.residentialRoadOutline
                            , width = 0.002
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "motorway" ->
                            { color = style.motorway
                            , outlineColor = style.motorwayOutline
                            , width = 0.005
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "motorway_link" ->
                            { color = style.motorwayLink
                            , outlineColor = style.motorwayLinkOutline
                            , width = 0.0025
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "road" ->
                            { color = style.road
                            , outlineColor = style.roadOutline
                            , width = 0.001
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "trunk" ->
                            { color = style.trunkRoad
                            , outlineColor = style.trunkRoadOutline
                            , width = 0.005
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "trunk_link" ->
                            { color = style.trunkRoadLink
                            , outlineColor = style.trunkRoadLinkOutline
                            , width = 0.0025
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "pedestrian" ->
                            { color = style.pedestrianPath
                            , outlineColor = style.pedestrianPathOutline
                            , width = 0.002
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "unclassified" ->
                            { color = style.unclassifiedRoad
                            , outlineColor = style.unclassifiedRoadOutline
                            , width = 0.002
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "platform" ->
                            { color = style.platform
                            , outlineColor = style.platformOutline
                            , width = 0.002
                            , alwaysDrawLabel = True
                            }
                                |> Just

                        "rail" ->
                            { color = style.railroad
                            , outlineColor = style.railroadOutline
                            , width = 0.0005
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "tram" ->
                            { color = style.tramline
                            , outlineColor = style.tramlineOutline
                            , width = 0.0004
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "subway" ->
                            { color = style.subway
                            , outlineColor = style.subwayOutline
                            , width = 0.0004
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "narrow_gauge" ->
                            { color = style.narrowGaugeRailroad
                            , outlineColor = style.narrowGaugeRailroadOutline
                            , width = 0.0004
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "trail" ->
                            { color = style.trail
                            , outlineColor = style.trailOutline
                            , width = 0.0004
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "footway" ->
                            { color = style.footway
                            , outlineColor = style.footwayOutline
                            , width = 0.0004
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "living_street" ->
                            { color = style.livingStreet
                            , outlineColor = style.livingStreetOutline
                            , width = 0.001
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        "service" ->
                            { color = style.serviceRoad
                            , outlineColor = style.serviceRoadOutline
                            , width = 0.001
                            , alwaysDrawLabel = False
                            }
                                |> Just

                        --"track" ->
                        --    Nothing
                        --
                        --"level_crossing" ->
                        --    Nothing
                        --
                        --"ferry" ->
                        --    Nothing
                        --
                        --"ferry_auto" ->
                        --    Nothing
                        --
                        --"footway" ->
                        --    Nothing
                        --
                        --"steps" ->
                        --    Nothing
                        --
                        --"sidewalk" ->
                        --    Nothing
                        --
                        --"crossing" ->
                        --    Nothing
                        --
                        --"cycleway" ->
                        --    Nothing
                        --
                        --"traffic_signals" ->
                        --    Nothing
                        --
                        --"turning_circle" ->
                        --    Nothing
                        --
                        --"mini_roundabout" ->
                        --    Nothing
                        --
                        --"turning_loop" ->
                        --    Nothing
                        _ ->
                            Nothing

                --name ->
                --    let
                --        _ =
                --            Debug.log "" name
                --    in
                --    { color = Vec3.vec3 1 0 0
                --    , outlineColor = style.serviceRoadOutline
                --    , width = 0.001
                --    , alwaysDrawLabel = False
                --    }
                --        |> Just
                _ ->
                    Nothing
    in
    case maybeRoadData of
        Just roadData ->
            Decode.map
                (decodeRoadGeometryHelper roadBuilder roadData maybeName)
                (geometryDecoder (Bytes.width feature.mesh))
                |> (\decoder -> Decode.decode decoder feature.mesh)
                |> Maybe.withDefault roadBuilder

        Nothing ->
            roadBuilder


gridMinCornerToPoint : GridPoint -> Point2d Unitless WorldCoordinates
gridMinCornerToPoint gridPoint =
    Point2d.unitless
        (toFloat gridPoint.gridX / 2 ^ toFloat gridPoint.zoom)
        (toFloat gridPoint.gridY / 2 ^ toFloat gridPoint.zoom)


gridMaxCornerToPoint : GridPoint -> Point2d Unitless WorldCoordinates
gridMaxCornerToPoint gridPoint =
    Point2d.unitless
        (toFloat (gridPoint.gridX + 1) / 2 ^ toFloat gridPoint.zoom)
        (toFloat (gridPoint.gridY + 1) / 2 ^ toFloat gridPoint.zoom)


tileCoordToWorld : GridPoint -> Point2d Unitless Unitless -> Point2d Unitless WorldCoordinates
tileCoordToWorld tilePosition point =
    let
        v =
            Vector2d.from Point2d.origin point
                |> Vector2d.scaleBy (1 / 2 ^ toFloat tilePosition.zoom)
                |> Vector2d.unwrap
                |> Vector2d.unsafe
    in
    gridMinCornerToPoint tilePosition |> Point2d.translateBy v


roadGlyphScale : number
roadGlyphScale =
    35000


placeLabelGlyphScale : PlaceClass -> number
placeLabelGlyphScale class =
    case class of
        Country ->
            20000

        State ->
            25000

        Settlement ->
            30000

        SettlementSubdivision ->
            35000


roadGlyphRadius : Float -> Font -> Int -> Quantity Float Unitless
roadGlyphRadius devicePixelRatio font zoom =
    (toFloat (font.bboxTop - font.bboxBottom) / (roadGlyphScale * 2))
        |> (*) (devicePixelRatio / 2 ^ toFloat zoom)
        |> Quantity.float


placeLabelGlyphRadius : Float -> Font -> Int -> PlaceClass -> Quantity Float Unitless
placeLabelGlyphRadius devicePixelRatio font zoom class =
    (toFloat (font.bboxTop - font.bboxBottom) / (placeLabelGlyphScale class * 2))
        |> (*) (devicePixelRatio / 2 ^ toFloat zoom)
        |> Quantity.float


roadLabelMeshV2 :
    InternalStyle
    -> Float
    -> Font
    -> GridPoint
    -> List (Point2d Unitless WorldCoordinates)
    -> { roadLabelChars : List (Point2d Unitless WorldCoordinates), placeLabelChars : List (Circle2d Unitless WorldCoordinates) }
    -> Nonempty (Point2d Unitless Unitless)
    -> String
    -> Maybe { mesh : TriangularMesh LabelVertex, newGlyphs : List (Point2d Unitless WorldCoordinates) }
roadLabelMeshV2 style devicePixelRatio font tilePosition newGlyphs existingChars path text =
    let
        glyphs : List Glyph
        glyphs =
            String.toList text
                |> List.map
                    (\char ->
                        case RegularDict.get char font.glyphs of
                            Just glyph_ ->
                                glyph_

                            Nothing ->
                                font.missingGlyph
                    )

        width : Quantity Float Unitless
        width =
            List.map (\glyph -> glyph.horizontalAdvance) glyphs
                |> List.sum
                |> toFloat
                |> (*) (1 / roadGlyphScale)
                |> Quantity.float

        pathLength_ : Quantity Float Unitless
        pathLength_ =
            pathLength path
    in
    if pathLength_ |> Quantity.lessThan width then
        Nothing

    else
        let
            centerPosition =
                moveAlongPathV2
                    (Quantity.plus (Quantity.multiplyBy 0.5 width) startAt)
                    path
                    |> .position
                    |> Point2d.unwrap

            startAt : Quantity Float Unitless
            startAt =
                pathLength_ |> Quantity.minus width |> Quantity.multiplyBy 0.5

            start =
                moveAlongPathV2 startAt path

            path_ =
                if
                    Direction2d.toVector start.direction
                        |> Vector2d.xComponent
                        |> Quantity.greaterThanZero
                then
                    path

                else
                    List.Nonempty.reverse path
        in
        List.foldl
            (\glyph state ->
                if state.isTooCurvy then
                    state

                else
                    let
                        { position, direction } =
                            moveAlongPathV2 (toFloat state.offset / roadGlyphScale |> Quantity.float |> Quantity.plus startAt) path_
                    in
                    if Direction2d.angleFrom start.direction direction |> Quantity.abs |> Quantity.lessThan (Angle.degrees 45) then
                        { list =
                            TriangularMesh.mapVertices
                                (\glyphPoint ->
                                    let
                                        glyphPosition =
                                            Point2d.toUnitless glyphPoint.position

                                        position2 =
                                            Point2d.unwrap position

                                        x =
                                            (glyphPosition.x + toFloat font.bboxRight / -2) / roadGlyphScale

                                        y =
                                            (glyphPosition.y + toFloat font.capHeight / -2) / -roadGlyphScale

                                        angle =
                                            Direction2d.toAngle direction |> Angle.inRadians

                                        cos2 =
                                            cos angle

                                        sin2 =
                                            sin angle
                                    in
                                    { positionX = centerPosition.x
                                    , positionY = centerPosition.y
                                    , offsetX = (cos2 * x - sin2 * y) + position2.x - centerPosition.x
                                    , offsetY = (sin2 * x + cos2 * y) + position2.y - centerPosition.y
                                    , color =
                                        if glyphPoint.isOutline then
                                            style.roadLabelOutline

                                        else
                                            style.roadLabel
                                    }
                                )
                                glyph.path
                                :: state.list
                        , offset = state.offset + glyph.horizontalAdvance
                        , newGlyphs = tileCoordToWorld tilePosition position :: state.newGlyphs
                        , isTooCurvy = state.isTooCurvy
                        }

                    else
                        { list = [], offset = 0, newGlyphs = [], isTooCurvy = True }
            )
            { list = [], offset = 0, newGlyphs = [], isTooCurvy = False }
            glyphs
            |> (\state ->
                    if
                        not state.isTooCurvy
                            && checkCollisions
                                (roadLabelCharCollision devicePixelRatio font tilePosition.zoom)
                                state.newGlyphs
                                existingChars.roadLabelChars
                            && checkCollisions
                                (\a b -> circlesIntersect (Circle2d.withRadius (roadGlyphRadius devicePixelRatio font tilePosition.zoom) a) b)
                                state.newGlyphs
                                existingChars.placeLabelChars
                            && checkCollisions (roadLabelCharCollision devicePixelRatio font tilePosition.zoom) state.newGlyphs newGlyphs
                    then
                        Just
                            { mesh = TriangularMesh.combine state.list
                            , newGlyphs = state.newGlyphs
                            }

                    else
                        Nothing
               )


checkCollisions : (a -> b -> Bool) -> List a -> List b -> Bool
checkCollisions intersectionFunc listA listB =
    --List.all (\a -> List.all (intersectionFunc a >> not) listB) listA
    not <| List.any (\a -> List.any (intersectionFunc a) listB) listA


placeLabelMesh :
    InternalStyle
    -> Float
    -> Font
    -> GridPoint
    -> PlaceLabel
    -> { mesh : TriangularMesh LabelVertex, newGlyphs : List (Circle2d Unitless WorldCoordinates) }
placeLabelMesh style devicePixelRatio font tilePosition label =
    let
        glyphs : List Glyph
        glyphs =
            String.toList label.text
                |> List.map
                    (\char ->
                        case RegularDict.get char font.glyphs of
                            Just glyph_ ->
                                glyph_

                            Nothing ->
                                font.missingGlyph
                    )

        scale : number
        scale =
            placeLabelGlyphScale label.class

        marginY =
            -800 / scale

        halfFontHeight =
            toFloat font.capHeight / (2 * scale)

        { offsetX, offsetY } =
            case label.textAnchor of
                Right ->
                    { offsetX = -1 / scale, offsetY = halfFontHeight }

                Center ->
                    { offsetX = -1 / (2 * scale), offsetY = halfFontHeight }

                Left ->
                    { offsetX = 0, offsetY = halfFontHeight }

                Top ->
                    { offsetX = -1 / (2 * scale), offsetY = -marginY - halfFontHeight }

                Bottom ->
                    { offsetX = -1 / (2 * scale), offsetY = marginY + halfFontHeight }

                TopRight ->
                    { offsetX = -1 / scale, offsetY = -marginY - halfFontHeight }

                TopLeft ->
                    { offsetX = 0, offsetY = -marginY - halfFontHeight }

                BottomRight ->
                    { offsetX = -1 / scale, offsetY = marginY + halfFontHeight }

                BottomLeft ->
                    { offsetX = 0, offsetY = marginY + halfFontHeight }

        startPosition : Point2d Unitless Unitless
        startPosition =
            List.map (\glyph -> glyph.horizontalAdvance) glyphs
                |> List.sum
                |> toFloat
                |> (*) offsetX
                |> (\x -> Point2d.translateBy (Vector2d.unitless x offsetY) Point2d.origin)

        labelPoint =
            Point2d.unwrap label.position
    in
    List.foldl
        (\glyph state ->
            let
                position =
                    Point2d.translateBy
                        (Vector2d.unitless (toFloat state.offset / scale) 0)
                        startPosition
            in
            { list =
                TriangularMesh.mapVertices
                    (\glyphPoint ->
                        let
                            { x, y } =
                                glyphPoint.position
                                    |> Point2d.scaleAbout Point2d.origin (1 / scale)
                                    |> Point2d.mirrorAcross Axis2d.x
                                    |> Point2d.translateBy (Vector2d.from Point2d.origin position)
                                    |> Point2d.unwrap
                        in
                        { positionX = labelPoint.x
                        , positionY = labelPoint.y
                        , offsetX = x
                        , offsetY = y
                        , color =
                            if glyphPoint.isOutline then
                                style.placeLabelOutline

                            else
                                style.placeLabel
                        }
                    )
                    glyph.path
                    :: state.list
            , offset = state.offset + glyph.horizontalAdvance
            , newGlyphs =
                Circle2d.withRadius
                    (placeLabelGlyphRadius devicePixelRatio font tilePosition.zoom label.class)
                    (tileCoordToWorld
                        tilePosition
                        (Point2d.translateBy
                            (Vector2d.from Point2d.origin label.position)
                            position
                        )
                    )
                    :: state.newGlyphs
            }
        )
        { list =
            if label.isCapital then
                [ ring 10 (240 / scale) (160 / scale) label.position (Vec3.vec3 0 0 0)
                , circle 10 (100 / scale) label.position (Vec3.vec3 0 0 0)
                , circle 10 (280 / scale) label.position (Vec3.vec3 1 1 1)
                ]

            else
                case label.class of
                    Country ->
                        []

                    State ->
                        []

                    Settlement ->
                        [ circle 10 (160 / scale) label.position (Vec3.vec3 0 0 0)
                        , circle 10 (200 / scale) label.position (Vec3.vec3 1 1 1)
                        ]

                    SettlementSubdivision ->
                        [ circle 10 (160 / scale) label.position (Vec3.vec3 0 0 0)
                        , circle 10 (200 / scale) label.position (Vec3.vec3 1 1 1)
                        ]
        , offset = 0
        , newGlyphs = []
        }
        glyphs
        |> (\state ->
                { mesh = List.reverse state.list |> TriangularMesh.combine, newGlyphs = state.newGlyphs }
           )


ring : Int -> Float -> Float -> Point2d a b -> Vec3 -> TriangularMesh LabelVertex
ring detail outerRadius innerRadius position color =
    let
        { x, y } =
            Point2d.unwrap position

        outerVertices : List LabelVertex
        outerVertices =
            List.range 0 (detail - 1)
                |> List.map
                    (\index ->
                        let
                            t =
                                toFloat index * (2 * pi / toFloat detail)
                        in
                        { positionX = x
                        , positionY = y
                        , offsetX = cos t * outerRadius
                        , offsetY = sin t * outerRadius
                        , color = color
                        }
                    )

        innerVertices : List LabelVertex
        innerVertices =
            List.range 0 (detail - 1)
                |> List.map
                    (\index ->
                        let
                            t =
                                toFloat index * (2 * pi / toFloat detail)
                        in
                        { positionX = x
                        , positionY = y
                        , offsetX = cos t * innerRadius
                        , offsetY = sin t * innerRadius
                        , color = color
                        }
                    )

        indices : List ( Int, Int, Int )
        indices =
            List.range 0 (detail - 1)
                |> List.concatMap
                    (\index ->
                        [ ( index
                          , detail + modBy detail (index + 1)
                          , detail + index
                          )
                        , ( index
                          , modBy detail (index + 1)
                          , detail + modBy detail (index + 1)
                          )
                        ]
                    )
    in
    TriangularMesh.indexed (innerVertices ++ outerVertices |> Array.fromList) indices


circle : Int -> Float -> Point2d a b -> Vec3 -> TriangularMesh LabelVertex
circle detail radius position color =
    let
        { x, y } =
            Point2d.unwrap position

        vertices : Array LabelVertex
        vertices =
            List.range 0 (detail - 1)
                |> List.map
                    (\index ->
                        let
                            t =
                                toFloat index * (2 * pi / toFloat detail)
                        in
                        { positionX = x
                        , positionY = y
                        , offsetX = cos t * radius
                        , offsetY = sin t * radius
                        , color = color
                        }
                    )
                |> Array.fromList

        indices : List ( Int, Int, Int )
        indices =
            List.range 0 (detail - 2)
                |> List.map (\index -> ( 0, index + 2, index + 1 ))
    in
    TriangularMesh.indexed vertices indices


roadLabelCharCollision : Float -> Font -> Int -> Point2d Unitless WorldCoordinates -> Point2d Unitless WorldCoordinates -> Bool
roadLabelCharCollision devicePixelRatio font zoom a b =
    Point2d.distanceFrom a b |> Quantity.lessThan (roadGlyphRadius devicePixelRatio font zoom |> Quantity.multiplyBy 2)


circlesIntersect : Circle2d units coordinates -> Circle2d units coordinates -> Bool
circlesIntersect a b =
    Point2d.distanceFrom (Circle2d.centerPoint a) (Circle2d.centerPoint b)
        |> Quantity.lessThanOrEqualTo (Quantity.plus (Circle2d.radius a) (Circle2d.radius b))


moveAlongPath :
    Quantity Float Unitless
    -> Nonempty (Point2d Unitless Unitless)
    -> { position : Point2d Unitless Unitless, direction : Direction2d Unitless }
moveAlongPath distanceAlong path =
    let
        current =
            List.Nonempty.head path
    in
    case List.Nonempty.tail path of
        next :: rest ->
            let
                distance : Quantity Float Unitless
                distance =
                    Point2d.distanceFrom current next
            in
            if distance |> Quantity.lessThan distanceAlong then
                moveAlongPath (distanceAlong |> Quantity.minus distance) (Nonempty next rest)

            else
                { position = Point2d.interpolateFrom current next (Quantity.ratio distanceAlong distance)
                , direction = Direction2d.from current next |> Maybe.withDefault Direction2d.x
                }

        [] ->
            { position = current, direction = Direction2d.x }


moveAlongPathV2 :
    Quantity Float Unitless
    -> Nonempty (Point2d Unitless Unitless)
    -> { position : Point2d Unitless Unitless, direction : Direction2d Unitless }
moveAlongPathV2 distanceAlong path =
    let
        current =
            List.Nonempty.head path
    in
    case List.Nonempty.tail path of
        next :: rest ->
            let
                distance : Quantity Float Unitless
                distance =
                    Point2d.distanceFrom current next
            in
            if distance |> Quantity.lessThan distanceAlong then
                moveAlongPath (distanceAlong |> Quantity.minus distance) (Nonempty next rest)

            else
                { position = Point2d.interpolateFrom current next (Quantity.ratio distanceAlong distance)
                , direction = Direction2d.from current next |> Maybe.withDefault Direction2d.x
                }

        [] ->
            { position = current, direction = Direction2d.x }


pathLength : Nonempty (Point2d Unitless Unitless) -> Quantity Float Unitless
pathLength path =
    List.Nonempty.toList path
        |> Polyline2d.fromVertices
        |> Polyline2d.length


drawRoadLabels :
    InternalStyle
    -> Float
    -> Font
    -> { roadLabelChars : List (Point2d Unitless WorldCoordinates), placeLabelChars : List (Circle2d Unitless WorldCoordinates) }
    -> GridPoint
    -> RoadLabelBuilder
    -> Road
    -> RoadLabelBuilder
drawRoadLabels style devicePixelRatio font existingChars tilePosition textBuilder road =
    if tilePosition.zoom < 14 && not road.alwaysDrawLabel then
        textBuilder

    else
        case roadLabelMeshV2 style devicePixelRatio font tilePosition textBuilder.newGlyphs existingChars road.path road.roadName of
            Just { mesh, newGlyphs } ->
                let
                    vertices : Array LabelVertex
                    vertices =
                        TriangularMesh.vertices mesh

                    indices : List ( Int, Int, Int )
                    indices =
                        TriangularMesh.faceIndices mesh

                    maxIndex : Int
                    maxIndex =
                        List.map (\( a, b, c ) -> max a b |> max c) indices
                            |> List.maximum
                            |> Maybe.withDefault 0
                in
                { index = textBuilder.index + maxIndex + 1
                , vertices = List.reverse (Array.toList vertices) ++ textBuilder.vertices
                , indices =
                    List.map
                        (\( a, b, c ) ->
                            ( a + textBuilder.index
                            , b + textBuilder.index
                            , c + textBuilder.index
                            )
                        )
                        indices
                        ++ textBuilder.indices
                , newGlyphs = newGlyphs ++ textBuilder.newGlyphs
                }

            Nothing ->
                textBuilder


drawPlaceLabels : InternalStyle -> Float -> Font -> GridPoint -> PlaceLabel -> PlaceLabelBuilder -> PlaceLabelBuilder
drawPlaceLabels style devicePixelRatio font tilePosition label placeLabelBuilder =
    let
        { mesh, newGlyphs } =
            placeLabelMesh style devicePixelRatio font tilePosition label

        vertices : Array LabelVertex
        vertices =
            TriangularMesh.vertices mesh

        indices : List ( Int, Int, Int )
        indices =
            TriangularMesh.faceIndices mesh

        maxIndex : Int
        maxIndex =
            List.map (\( a, b, c ) -> max a b |> max c) indices
                |> List.maximum
                |> Maybe.withDefault 0
    in
    { index = placeLabelBuilder.index + maxIndex + 1
    , vertices = List.reverse (Array.toList vertices) ++ placeLabelBuilder.vertices
    , indices =
        List.map
            (\( a, b, c ) ->
                ( a + placeLabelBuilder.index
                , b + placeLabelBuilder.index
                , c + placeLabelBuilder.index
                )
            )
            indices
            ++ placeLabelBuilder.indices
    , newGlyphs = newGlyphs ++ placeLabelBuilder.newGlyphs
    }


decodeRoadGeometryHelper :
    RoadBuilder
    -> { width : Float, color : Vec3, outlineColor : Vec3, alwaysDrawLabel : Bool }
    -> Maybe String
    -> List (List (Point2d Unitless Unitless))
    -> RoadBuilder
decodeRoadGeometryHelper roadBuilder { width, color, outlineColor, alwaysDrawLabel } maybeName geometry =
    List.foldl
        (\lineString roadBuilder2 ->
            case lineString of
                first :: rest ->
                    let
                        result =
                            List.foldl
                                (\position state ->
                                    let
                                        { x, y } =
                                            Point2d.unwrap position

                                        previousPoint =
                                            Point2d.unwrap state.previous

                                        normal =
                                            Vector2d.from position state.previous
                                                |> Vector2d.normalize
                                                |> Vector2d.scaleBy width

                                        perpendicular =
                                            Vector2d.perpendicularTo normal

                                        perpendicularPoint =
                                            Vector2d.unwrap perpendicular

                                        negativePerpendicular =
                                            Vector2d.reverse perpendicular

                                        negativePerpendicularPoint =
                                            Vector2d.unwrap negativePerpendicular

                                        index =
                                            state.roadBuilder.index
                                    in
                                    { isFirst = False
                                    , previous = position
                                    , path = List.Nonempty.cons position state.path
                                    , roadBuilder =
                                        { vertices =
                                            [ { positionX = x
                                              , positionY = y
                                              , offsetX = negativePerpendicularPoint.x
                                              , offsetY = negativePerpendicularPoint.y
                                              , color = color
                                              , outlineColor = outlineColor
                                              }
                                            , { positionX = x
                                              , positionY = y
                                              , offsetX = perpendicularPoint.x
                                              , offsetY = perpendicularPoint.y
                                              , color = color
                                              , outlineColor = outlineColor
                                              }
                                            , { positionX = previousPoint.x
                                              , positionY = previousPoint.y
                                              , offsetX = perpendicularPoint.x
                                              , offsetY = perpendicularPoint.y
                                              , color = color
                                              , outlineColor = outlineColor
                                              }
                                            , { positionX = previousPoint.x
                                              , positionY = previousPoint.y
                                              , offsetX = negativePerpendicularPoint.x
                                              , offsetY = negativePerpendicularPoint.y
                                              , color = color
                                              , outlineColor = outlineColor
                                              }
                                            ]
                                                ++ state.roadBuilder.vertices
                                        , roads = state.roadBuilder.roads
                                        , indices =
                                            (if state.isFirst then
                                                [ ( index, index + 2, index + 1 )
                                                , ( index, index + 3, index + 2 )
                                                ]

                                             else
                                                [ ( index, index - 2, index - 1 )
                                                , ( index + 1, index - 2, index - 1 )
                                                , ( index, index + 2, index + 1 )
                                                , ( index, index + 3, index + 2 )
                                                ]
                                            )
                                                ++ state.roadBuilder.indices
                                        , index = index + 4
                                        }
                                    }
                                )
                                { isFirst = True
                                , previous = first
                                , path = Nonempty first []
                                , roadBuilder = roadBuilder2
                                }
                                rest
                    in
                    { index = result.roadBuilder.index
                    , indices = result.roadBuilder.indices
                    , vertices = result.roadBuilder.vertices
                    , roads =
                        (case maybeName of
                            Just name ->
                                if String.length name > 1 then
                                    [ { roadName = name
                                      , path = result.path
                                      , alwaysDrawLabel = alwaysDrawLabel
                                      }
                                    ]

                                else
                                    []

                            Nothing ->
                                []
                        )
                            ++ result.roadBuilder.roads
                    }

                [] ->
                    roadBuilder
        )
        roadBuilder
        geometry


layerTempToLayer : InternalStyle -> GridPoint -> LayerTemp -> Maybe Layer
layerTempToLayer style tilePosition layer =
    case layer.name of
        "building" ->
            List.foldl
                (\feature state ->
                    decodeBuildingGeometry feature state
                )
                { index = 0, indices = [], vertices = [] }
                layer.features
                |> (\a -> WebGL.indexedTriangles (List.reverse a.vertices) a.indices)
                |> BuildingLayer
                |> Just

        "water" ->
            case layer.features of
                first :: _ ->
                    case decodeWaterGeometryV2 first of
                        Just mesh ->
                            WaterLayer mesh |> Just

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        "landcover" ->
            List.foldl
                (\feature state ->
                    decodeLandcoverGeometry layer feature state
                )
                { index = 0, indices = [], vertices = [] }
                layer.features
                |> (\a -> WebGL.indexedTriangles (List.reverse a.vertices) a.indices)
                |> NatureLayer
                |> Just

        "road" ->
            if tilePosition.zoom < 7 then
                Nothing

            else
                let
                    roadBuilder : RoadBuilder
                    roadBuilder =
                        List.foldl
                            (\feature state -> decodeRoadGeometryV2 style state layer feature)
                            { index = 0, vertices = [], roads = [], indices = [] }
                            layer.features
                in
                RoadLayer
                    (WebGL.indexedTriangles (List.reverse roadBuilder.vertices) roadBuilder.indices)
                    roadBuilder.roads
                    |> Just

        "place_label" ->
            List.foldl
                (\feature state ->
                    let
                        tags : RegularDict.Dict String Value
                        tags =
                            getTags layer feature

                        maybeClass : Maybe PlaceClass
                        maybeClass =
                            case RegularDict.get "class" tags of
                                Just (StringValue text) ->
                                    case text of
                                        "country" ->
                                            Just Country

                                        "state" ->
                                            Just State

                                        "settlement" ->
                                            Just Settlement

                                        "settlement_subdivision" ->
                                            Just SettlementSubdivision

                                        _ ->
                                            Nothing

                                _ ->
                                    Nothing
                    in
                    case ( RegularDict.get "name" tags, maybeClass, RegularDict.get "symbolrank" tags ) of
                        ( Just (StringValue label), Just class, Just (Uint64Value symbolRank) ) ->
                            case Decode.decode (pointDecoder (Bytes.width feature.mesh)) feature.mesh of
                                Just point ->
                                    let
                                        { x, y } =
                                            Point2d.toUnitless point

                                        symbolRank_ : Int
                                        symbolRank_ =
                                            Int64.toInt symbolRank
                                    in
                                    if x < 0 || y < 0 || x > 1 || y > 1 || symbolRank_ - 3 > tilePosition.zoom then
                                        state

                                    else
                                        { text = label
                                        , position = point
                                        , class = class
                                        , isCapital =
                                            RegularDict.get "capital" tags
                                                == Just (Uint64Value (Int64.fromInt 2))
                                        , textAnchor =
                                            case RegularDict.get "text_anchor" tags of
                                                Just (StringValue anchorText) ->
                                                    toTextAnchor anchorText

                                                _ ->
                                                    Center
                                        , symbolRank = symbolRank_
                                        }
                                            :: state

                                Nothing ->
                                    state

                        _ ->
                            state
                )
                []
                layer.features
                |> PlaceLabelLayer
                |> Just

        _ ->
            Nothing


toTextAnchor : String -> TextAnchor
toTextAnchor anchorText =
    case anchorText of
        "bottom" ->
            Bottom

        "bottom-left" ->
            BottomLeft

        "bottom-right" ->
            BottomRight

        "left" ->
            Left

        "right" ->
            Right

        "top" ->
            Top

        "top-left" ->
            TopLeft

        "top-right" ->
            TopRight

        _ ->
            Center


layerDecoder_ : InternalStyle -> GridPoint -> Int -> Decode.Decoder ( Int, Maybe Layer )
layerDecoder_ style tilePosition width =
    Decode.loop
        { width = width
        , model = LayerTemp 1 "" [] Array.empty Array.empty 4096
        }
        (\state ->
            if state.width == 0 then
                ( width, layerTempToLayer style tilePosition state.model ) |> Decode.Done |> Decode.succeed

            else if state.width < 0 then
                Decode.fail

            else
                ProtobufDecode.varIntDecoder
                    |> Decode.andThen
                        (\( usedBytes, value ) ->
                            if Bitwise.and 0x07 value == 2 then
                                ProtobufDecode.varIntDecoder
                                    |> Decode.map (\( n, wireType ) -> ( usedBytes + n, ( Bitwise.shiftRightZfBy 3 value, wireType ) ))

                            else
                                Decode.succeed ( usedBytes, ( Bitwise.shiftRightZfBy 3 value, -1 ) )
                        )
                    |> Decode.andThen
                        (\( usedBytes, ( fieldNumber, wireType ) ) ->
                            case fieldNumber of
                                4 ->
                                    Decode.map
                                        (\( n, value ) ->
                                            Decode.Loop
                                                { width = state.width - usedBytes - n
                                                , model =
                                                    { version = state.model.version
                                                    , name = state.model.name
                                                    , features = state.model.features
                                                    , keys = state.model.keys
                                                    , values = Array.push value state.model.values
                                                    , extent = state.model.extent
                                                    }
                                                }
                                        )
                                        (valueDecoder wireType)

                                3 ->
                                    Decode.map
                                        (\( n, key ) ->
                                            Decode.Loop
                                                { width = state.width - usedBytes - n
                                                , model =
                                                    { version = state.model.version
                                                    , name = state.model.name
                                                    , features = state.model.features
                                                    , keys = Array.push key state.model.keys
                                                    , values = state.model.values
                                                    , extent = state.model.extent
                                                    }
                                                }
                                        )
                                        (ProtobufDecode.string wireType)

                                2 ->
                                    Decode.map
                                        (\( n, feature ) ->
                                            Decode.Loop
                                                { width = state.width - usedBytes - n
                                                , model =
                                                    { version = state.model.version
                                                    , name = state.model.name
                                                    , features = feature :: state.model.features
                                                    , keys = state.model.keys
                                                    , values = state.model.values
                                                    , extent = state.model.extent
                                                    }
                                                }
                                        )
                                        (featureDecoder_ wireType)

                                15 ->
                                    Decode.map
                                        (\( n, version ) ->
                                            Decode.Loop
                                                { width = state.width - usedBytes - n
                                                , model =
                                                    { version = version
                                                    , name = state.model.name
                                                    , features = state.model.features
                                                    , keys = state.model.keys
                                                    , values = state.model.values
                                                    , extent = state.model.extent
                                                    }
                                                }
                                        )
                                        ProtobufDecode.uint32

                                1 ->
                                    Decode.map
                                        (\( n, name ) ->
                                            let
                                                newWidth =
                                                    state.width - usedBytes - n
                                            in
                                            Decode.Loop
                                                { width = newWidth
                                                , model =
                                                    { version = state.model.version
                                                    , name = name
                                                    , features = state.model.features
                                                    , keys = state.model.keys
                                                    , values = state.model.values
                                                    , extent = state.model.extent
                                                    }
                                                }
                                        )
                                        (ProtobufDecode.string wireType)

                                5 ->
                                    Decode.map
                                        (\( n, extent_ ) ->
                                            Decode.Loop
                                                { width = state.width - usedBytes - n
                                                , model =
                                                    { version = state.model.version
                                                    , name = state.model.name
                                                    , features = state.model.features
                                                    , keys = state.model.keys
                                                    , values = state.model.values
                                                    , extent = extent_
                                                    }
                                                }
                                        )
                                        ProtobufDecode.uint32

                                _ ->
                                    Decode.fail
                        )
        )


emptyBytes =
    Bytes.Encode.sequence [] |> Bytes.Encode.encode


featureDecoder_ : Int -> Decode.Decoder ( Int, FeatureTemp )
featureDecoder_ width =
    Decode.loop
        { width = width
        , model = FeatureTemp (Int64.fromInt 0) [] Unknown emptyBytes
        }
        (\state ->
            if state.width == 0 then
                Decode.succeed
                    (Decode.Done ( width, state.model ))

            else if state.width < 0 then
                Decode.fail

            else
                ProtobufDecode.varIntDecoder
                    |> Decode.andThen
                        (\( usedBytes, value ) ->
                            if Bitwise.and 0x07 value == 2 then
                                ProtobufDecode.varIntDecoder
                                    |> Decode.map (\( n, wireType ) -> ( usedBytes + n, ( Bitwise.shiftRightZfBy 3 value, wireType ) ))

                            else
                                Decode.succeed ( usedBytes, ( Bitwise.shiftRightZfBy 3 value, -1 ) )
                        )
                    |> Decode.andThen
                        (\( usedBytes, ( fieldNumber, wireType ) ) ->
                            case fieldNumber of
                                1 ->
                                    Decode.map
                                        (\( n, id ) ->
                                            Decode.Loop
                                                { width = state.width - usedBytes - n
                                                , model =
                                                    { id = id
                                                    , tags = state.model.tags
                                                    , type_ = state.model.type_
                                                    , mesh = state.model.mesh
                                                    }
                                                }
                                        )
                                        (ProtobufDecode.uint64 wireType)

                                2 ->
                                    Decode.loop
                                        ( wireType, state.model.tags )
                                        (ProtobufDecode.stepPackedField wireType ProtobufDecode.uint32)
                                        |> Decode.map
                                            (\( n, tags ) ->
                                                Decode.Loop
                                                    { width = state.width - usedBytes - n
                                                    , model =
                                                        { id = state.model.id
                                                        , tags = tags
                                                        , type_ = state.model.type_
                                                        , mesh = state.model.mesh
                                                        }
                                                    }
                                            )

                                3 ->
                                    Decode.map
                                        (\( n, geomType ) ->
                                            Decode.Loop
                                                { width = state.width - usedBytes - n
                                                , model =
                                                    { id = state.model.id
                                                    , tags = state.model.tags
                                                    , type_ = geomType
                                                    , mesh = state.model.mesh
                                                    }
                                                }
                                        )
                                        geomTypeDecoder

                                4 ->
                                    Decode.bytes wireType
                                        |> Decode.map
                                            (\geometry ->
                                                Decode.Loop
                                                    { width = state.width - usedBytes - wireType
                                                    , model =
                                                        { id = state.model.id
                                                        , tags = state.model.tags
                                                        , type_ = state.model.type_
                                                        , mesh = geometry
                                                        }
                                                    }
                                            )

                                _ ->
                                    Decode.fail
                        )
        )


indexVertices : Int -> Int -> List ( Int, Int, Int )
indexVertices offset length =
    List.range 0 (length - 3)
        |> List.map (\index -> ( offset, offset + index + 1, offset + index + 2 ))


pointDecoder : Int -> Decode.Decoder (Point2d Unitless Unitless)
pointDecoder fullWidth =
    Decode.loop
        ( fullWidth
        , { penPosition = { x = 0, y = 0 }
          , polygons = Nothing
          }
        )
        (\( fullWidth_, model ) ->
            let
                bytesRemaining =
                    fullWidth_
            in
            if bytesRemaining == 0 then
                case model.polygons of
                    Just point ->
                        Decode.Done point |> Decode.succeed

                    Nothing ->
                        Decode.fail

            else if bytesRemaining < 0 then
                Decode.fail

            else
                Decode.andThen
                    (\( w, value ) ->
                        case Bitwise.and 0x07 value of
                            -- MoveTo
                            1 ->
                                decodePolygonParameters model.penPosition value
                                    |> Decode.map
                                        (\{ bytesUsed, penPosition, polygon } ->
                                            Decode.Loop
                                                ( bytesRemaining - w - bytesUsed
                                                , { penPosition = penPosition
                                                  , polygons = List.head polygon
                                                  }
                                                )
                                        )

                            _ ->
                                Decode.fail
                    )
                    ProtobufDecode.uint32
        )


geometryDecoder : Int -> Decode.Decoder (List (List (Point2d Unitless Unitless)))
geometryDecoder fullWidth =
    Decode.loop
        ( fullWidth
        , { penPosition = { x = 0, y = 0 }
          , polygons = []
          }
        )
        (\( fullWidth_, model ) ->
            let
                bytesRemaining =
                    fullWidth_
            in
            if bytesRemaining == 0 then
                Decode.Done model.polygons |> Decode.succeed

            else if bytesRemaining < 0 then
                Decode.fail

            else
                Decode.andThen
                    (\( w, value ) ->
                        case Bitwise.and 0x07 value of
                            -- MoveTo
                            1 ->
                                decodePolygonParameters model.penPosition value
                                    |> Decode.map
                                        (\{ bytesUsed, penPosition } ->
                                            Decode.Loop
                                                ( bytesRemaining - w - bytesUsed
                                                , { penPosition = penPosition
                                                  , polygons = model.polygons
                                                  }
                                                )
                                        )

                            -- LineTo
                            2 ->
                                decodePolygonParameters model.penPosition value
                                    |> Decode.map
                                        (\{ bytesUsed, penPosition, polygon } ->
                                            Decode.Loop
                                                ( bytesRemaining - w - bytesUsed
                                                , { penPosition = penPosition
                                                  , polygons = polygon :: model.polygons
                                                  }
                                                )
                                        )

                            -- ClosePath
                            7 ->
                                Decode.Loop ( bytesRemaining - w, model ) |> Decode.succeed

                            _ ->
                                Decode.fail
                    )
                    ProtobufDecode.uint32
        )


geometryDecoderV2 : Int -> Decode.Decoder (WebGL.Mesh Vertex)
geometryDecoderV2 fullWidth =
    Decode.loop
        ( fullWidth
        , { penPosition = { x = 0, y = 0 }
          , vertices = []
          }
        )
        (\( fullWidth_, model ) ->
            let
                bytesRemaining =
                    fullWidth_
            in
            if bytesRemaining == 0 then
                WebGL.triangles model.vertices
                    |> Decode.Done
                    |> Decode.succeed

            else if bytesRemaining < 0 then
                Decode.fail

            else
                Decode.andThen
                    (\( w, value ) ->
                        case Bitwise.and 0x07 value of
                            -- MoveTo
                            1 ->
                                decodeMovePenV2 model.penPosition value
                                    |> Decode.map
                                        (\{ bytesUsed, penPosition } ->
                                            Decode.Loop
                                                ( bytesRemaining - w - bytesUsed
                                                , { penPosition = penPosition
                                                  , vertices = model.vertices
                                                  }
                                                )
                                        )

                            2 ->
                                decodePolygonParametersV2 model.vertices model.penPosition value
                                    |> Decode.map
                                        (\{ bytesUsed, penPosition, vertices } ->
                                            Decode.Loop
                                                ( bytesRemaining - w - bytesUsed
                                                , { penPosition = penPosition
                                                  , vertices = vertices
                                                  }
                                                )
                                        )

                            -- ClosePath
                            7 ->
                                Decode.Loop ( bytesRemaining - w, model ) |> Decode.succeed

                            _ ->
                                Decode.fail
                    )
                    ProtobufDecode.uint32
        )


extend =
    4096


decodePolygonParameters :
    { x : Int, y : Int }
    -> Int
    -> Decode.Decoder { bytesUsed : Int, penPosition : { x : Int, y : Int }, polygon : List (Point2d Unitless Unitless) }
decodePolygonParameters penPositionStart value =
    Decode.loop
        { totalBytesUsed = 0
        , parametersLeft = Bitwise.shiftRightZfBy 3 value
        , polygon =
            [ Point2d.unitless
                (toFloat penPositionStart.x / extend)
                (toFloat penPositionStart.y / extend)
            ]
        , penPosition = penPositionStart
        }
        (\{ penPosition, totalBytesUsed, parametersLeft, polygon } ->
            if parametersLeft > 0 then
                ProtobufDecode.uint32
                    |> Decode.andThen
                        (\( usedBytes1, value1 ) ->
                            ProtobufDecode.uint32
                                |> Decode.map
                                    (\( usedBytes2, value2 ) ->
                                        let
                                            newPenX =
                                                penPosition.x + ProtobufDecode.zigzagDecoder value1

                                            newPenY =
                                                penPosition.y + ProtobufDecode.zigzagDecoder value2
                                        in
                                        Decode.Loop
                                            { totalBytesUsed = usedBytes1 + usedBytes2 + totalBytesUsed
                                            , parametersLeft = parametersLeft - 1
                                            , polygon =
                                                Point2d.unitless
                                                    (toFloat newPenX / extend)
                                                    (toFloat newPenY / extend)
                                                    :: polygon
                                            , penPosition = { x = newPenX, y = newPenY }
                                            }
                                    )
                        )

            else if parametersLeft == 0 then
                Decode.Done
                    { bytesUsed = totalBytesUsed
                    , polygon = polygon
                    , penPosition = penPosition
                    }
                    |> Decode.succeed

            else
                Decode.fail
        )


decodeMovePenV2 : { x : Int, y : Int } -> Int -> Decode.Decoder { bytesUsed : Int, penPosition : { x : Int, y : Int } }
decodeMovePenV2 penPositionStart value =
    Decode.loop
        { totalBytesUsed = 0
        , parametersLeft = Bitwise.shiftRightZfBy 3 value
        , penPosition = penPositionStart
        }
        (\{ penPosition, totalBytesUsed, parametersLeft } ->
            if parametersLeft > 0 then
                ProtobufDecode.uint32
                    |> Decode.andThen
                        (\( usedBytes1, value1 ) ->
                            ProtobufDecode.uint32
                                |> Decode.map
                                    (\( usedBytes2, value2 ) ->
                                        Decode.Loop
                                            { totalBytesUsed = usedBytes1 + usedBytes2 + totalBytesUsed
                                            , parametersLeft = parametersLeft - 1
                                            , penPosition =
                                                { x = penPosition.x + ProtobufDecode.zigzagDecoder value1
                                                , y = penPosition.y + ProtobufDecode.zigzagDecoder value2
                                                }
                                            }
                                    )
                        )

            else if parametersLeft == 0 then
                Decode.Done
                    { bytesUsed = totalBytesUsed
                    , penPosition = penPosition
                    }
                    |> Decode.succeed

            else
                Decode.fail
        )


decodePolygonParametersV2 :
    List ( Vertex, Vertex, Vertex )
    -> { x : Int, y : Int }
    -> Int
    ->
        Decode.Decoder
            { bytesUsed : Int
            , penPosition : { x : Int, y : Int }
            , vertices : List ( Vertex, Vertex, Vertex )
            }
decodePolygonParametersV2 currentVertices penPositionStart value =
    let
        vec : Vertex
        vec =
            { x = toFloat penPositionStart.x / extend
            , y = toFloat penPositionStart.y / extend
            }
    in
    Decode.loop
        { totalBytesUsed = 0
        , parametersLeft = Bitwise.shiftRightZfBy 3 value
        , vertices = currentVertices
        , penPosition = vec
        , previousPenPosition = vec
        }
        (\{ penPosition, previousPenPosition, totalBytesUsed, parametersLeft, vertices } ->
            if parametersLeft > 0 then
                ProtobufDecode.uint32
                    |> Decode.andThen
                        (\( usedBytes1, value1 ) ->
                            ProtobufDecode.uint32
                                |> Decode.map
                                    (\( usedBytes2, value2 ) ->
                                        let
                                            newPen : Vertex
                                            newPen =
                                                { x = penPosition.x + toFloat (ProtobufDecode.zigzagDecoder value1) / extend
                                                , y = penPosition.y + toFloat (ProtobufDecode.zigzagDecoder value2) / extend
                                                }

                                            newVec : Vertex
                                            newVec =
                                                { x = newPen.x
                                                , y = newPen.y
                                                }
                                        in
                                        Decode.Loop
                                            { totalBytesUsed = usedBytes1 + usedBytes2 + totalBytesUsed
                                            , parametersLeft = parametersLeft - 1
                                            , vertices = ( newVec, vec, previousPenPosition ) :: vertices
                                            , penPosition = newPen
                                            , previousPenPosition = newVec
                                            }
                                    )
                        )

            else if parametersLeft == 0 then
                Decode.Done
                    { bytesUsed = totalBytesUsed
                    , vertices = vertices
                    , penPosition = { x = round (penPosition.x * extend), y = round (penPosition.y * extend) }
                    }
                    |> Decode.succeed

            else
                Decode.fail
        )
