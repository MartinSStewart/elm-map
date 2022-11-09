module Font exposing (Font, Glyph, fontCodec, main)

{-| This module converts an svg font into something that we can render to a webgl canvas.

Steps to create a new svg font:

1.  Load the font into FontForge (<https://fontforge.org/en-US/downloads/>)
2.  File -> Generate Fonts...
3.  Select SVG font from the dropdown
4.  Press Generate

-}

import AssocList as Dict exposing (Dict)
import Browser
import CubicSpline2d
import Dict as RegularDict
import File.Download
import Hex
import Html
import Http
import Json.Encode
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Parser exposing ((|.), (|=), Parser)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Unitless)
import Serialize exposing (Codec)
import SvgParser exposing (SvgNode(..))
import Toop exposing (T5(..))
import TriangularMesh exposing (TriangularMesh)
import Vector2d exposing (Vector2d)


type alias GlyphPoint =
    { position : Point2d Unitless Unitless, isOutline : Bool }


{-| Copied from <https://github.com/Spaxe/svg-pathd/blob/f7930cedc8f37ce6be76ce4e7829f37ebc59bb6b/src/Svg/PathD.elm#L81>
-}
type Segment
    = M ( Int, Int )
    | L ( Int, Int )
    | H Int
    | V Int
    | Z
    | C ( Int, Int ) ( Int, Int ) ( Int, Int )
    | S ( Int, Int ) ( Int, Int )
    | Q ( Int, Int ) ( Int, Int )
    | T ( Int, Int )
    | A ( Int, Int ) Int Bool Bool ( Int, Int )
    | Md ( Int, Int )
    | Ld ( Int, Int )
    | Hd Int
    | Vd Int
    | Zd
    | Cd ( Int, Int ) ( Int, Int ) ( Int, Int )
    | Sd ( Int, Int ) ( Int, Int )
    | Qd ( Int, Int ) ( Int, Int )
    | Td ( Int, Int )
    | Ad ( Int, Int ) Int Bool Bool ( Int, Int )


type alias FontFace =
    { ascent : Int
    , descent : Int
    , xHeight : Int
    , capHeight : Int
    , bboxLeft : Int
    , bboxRight : Int
    , bboxTop : Int
    , bboxBottom : Int
    }


type alias Font =
    { ascent : Int
    , descent : Int
    , xHeight : Int
    , capHeight : Int
    , bboxLeft : Int
    , bboxRight : Int
    , bboxTop : Int
    , bboxBottom : Int
    , missingGlyph : Glyph
    , glyphs : RegularDict.Dict Char Glyph
    }


type alias Glyph =
    { horizontalAdvance : Int, path : TriangularMesh GlyphPoint }


main : Program {} {} (Result Http.Error String)
main =
    Browser.element
        { init = \_ -> ( {}, Http.get { url = "/dinProMedium.txt", expect = Http.expectString identity } )
        , update =
            \result _ ->
                case result of
                    Ok ok ->
                        let
                            content =
                                case svgToFont ok of
                                    Ok a ->
                                        Serialize.encodeToJson fontCodec a
                                            |> Json.Encode.encode 0

                                    Err _ ->
                                        ""
                        in
                        ( {}, File.Download.string "dinProMediumEncoded.json" "application/json" content )

                    Err _ ->
                        ( {}, Cmd.none )
        , view = \_ -> Html.text ""
        , subscriptions = \_ -> Sub.none
        }


fontCodec : Codec e Font
fontCodec =
    Serialize.record Font
        |> Serialize.field .ascent Serialize.int
        |> Serialize.field .descent Serialize.int
        |> Serialize.field .xHeight Serialize.int
        |> Serialize.field .capHeight Serialize.int
        |> Serialize.field .bboxLeft Serialize.int
        |> Serialize.field .bboxRight Serialize.int
        |> Serialize.field .bboxTop Serialize.int
        |> Serialize.field .bboxBottom Serialize.int
        |> Serialize.field .missingGlyph glyphCodec
        |> Serialize.field
            .glyphs
            (Serialize.dict charCodec glyphCodec)
        |> Serialize.finishRecord


charCodec : Codec e Char
charCodec =
    Serialize.map
        (\text -> String.toList text |> List.head |> Maybe.withDefault ' ')
        String.fromChar
        Serialize.string


svgToFont : String -> Result () Font
svgToFont svgText =
    case String.lines svgText |> List.drop 2 |> String.join "\n" |> SvgParser.parseToNode of
        Ok ok ->
            case ok of
                SvgElement node ->
                    case node.children of
                        [ _, SvgElement defs ] ->
                            case defs.children of
                                [ SvgElement font_ ] ->
                                    case font_.children of
                                        (SvgElement fontFace) :: (SvgElement missingGlyph) :: glyphs ->
                                            let
                                                parsedGlyphs : RegularDict.Dict Char Glyph
                                                parsedGlyphs =
                                                    List.filterMap
                                                        (\node_ ->
                                                            (case node_ of
                                                                SvgElement element ->
                                                                    case element.name of
                                                                        "glyph" ->
                                                                            glyphParser element

                                                                        "hkern" ->
                                                                            Err ()

                                                                        _ ->
                                                                            Err ()

                                                                SvgText _ ->
                                                                    Err ()

                                                                SvgComment _ ->
                                                                    Err ()
                                                            )
                                                                |> Result.toMaybe
                                                        )
                                                        glyphs
                                                        |> RegularDict.fromList
                                            in
                                            case ( parseFontFaceAttributes fontFace, missingGlyphParser missingGlyph ) of
                                                ( Ok { ascent, descent, xHeight, capHeight, bboxLeft, bboxRight, bboxTop, bboxBottom }, Ok missingGlyph_ ) ->
                                                    Ok
                                                        { ascent = ascent
                                                        , descent = descent
                                                        , xHeight = xHeight
                                                        , capHeight = capHeight
                                                        , bboxLeft = bboxLeft
                                                        , bboxRight = bboxRight
                                                        , bboxTop = bboxTop
                                                        , bboxBottom = bboxBottom
                                                        , missingGlyph = missingGlyph_
                                                        , glyphs = parsedGlyphs
                                                        }

                                                _ ->
                                                    Err ()

                                        _ ->
                                            Err ()

                                _ ->
                                    Err ()

                        _ ->
                            Err ()

                _ ->
                    Err ()

        Err _ ->
            Err ()


decodeUnicode : String -> Maybe Char
decodeUnicode text =
    if String.startsWith "&#x" text then
        case String.dropLeft 3 text |> String.dropRight 1 |> Hex.fromString of
            Ok ok ->
                Char.fromCode ok |> Just

            Err _ ->
                Nothing

    else
        String.toList text |> List.head


glyphParser : SvgParser.Element -> Result () ( Char, Glyph )
glyphParser node =
    case node.attributes of
        [ _, ( "unicode", unicode ), ( "horiz-adv-x", horizontalAdvance ) ] ->
            case ( decodeUnicode unicode, String.toInt horizontalAdvance ) of
                ( Just unicode_, Just horizontalAdvance_ ) ->
                    ( unicode_
                    , { horizontalAdvance = horizontalAdvance_
                      , path = TriangularMesh.empty
                      }
                    )
                        |> Ok

                _ ->
                    Err ()

        [ _, ( "unicode", unicode ), ( "horiz-adv-x", horizontalAdvance ), ( "d", path ) ] ->
            case ( decodeUnicode unicode, String.toInt horizontalAdvance, Parser.run pathParser path ) of
                ( Just unicode_, Just horizontalAdvance_, Ok segments ) ->
                    ( unicode_
                    , { horizontalAdvance = horizontalAdvance_
                      , path = pathToTriangularMesh segments
                      }
                    )
                        |> Ok

                _ ->
                    Err ()

        _ ->
            Err ()


missingGlyphParser : SvgParser.Element -> Result () Glyph
missingGlyphParser node =
    case node.attributes of
        [ ( "horiz-adv-x", horizontalAdvance ), ( "d", path ) ] ->
            case ( String.toInt horizontalAdvance, Parser.run pathParser path ) of
                ( Just horizontalAdvance_, Ok segments ) ->
                    { horizontalAdvance = horizontalAdvance_
                    , path = pathToTriangularMesh segments
                    }
                        |> Ok

                _ ->
                    Err ()

        _ ->
            Err ()


precision =
    4


pathToTriangularMesh : List Segment -> TriangularMesh GlyphPoint
pathToTriangularMesh path =
    let
        loops : Nonempty (List (Point2d units coordinates))
        loops =
            List.foldl
                (\segment state ->
                    let
                        ( penX, penY ) =
                            state.penPosition

                        currentList =
                            List.Nonempty.head state.loops
                    in
                    case segment of
                        M point ->
                            { penPosition = point
                            , previousPenPosition = point
                            , loops =
                                List.Nonempty.replaceHead
                                    (Point2d.fromTuple Quantity.unsafe (Tuple.mapBoth toFloat toFloat point)
                                        :: currentList
                                    )
                                    state.loops
                            }

                        L point ->
                            { penPosition = point
                            , previousPenPosition = state.penPosition
                            , loops =
                                List.Nonempty.replaceHead
                                    (Point2d.fromTuple Quantity.unsafe (Tuple.mapBoth toFloat toFloat point)
                                        :: currentList
                                    )
                                    state.loops
                            }

                        H x ->
                            { penPosition = ( x, penY )
                            , previousPenPosition = state.penPosition
                            , loops =
                                List.Nonempty.replaceHead
                                    (Point2d.unsafe { x = toFloat x, y = toFloat penY } :: currentList)
                                    state.loops
                            }

                        V y ->
                            { penPosition = ( penX, y )
                            , previousPenPosition = state.penPosition
                            , loops =
                                List.Nonempty.replaceHead
                                    (Point2d.unsafe { x = toFloat penX, y = toFloat y } :: currentList)
                                    state.loops
                            }

                        Z ->
                            { penPosition = state.penPosition
                            , previousPenPosition = state.penPosition
                            , loops = List.Nonempty.cons [] state.loops
                            }

                        C point1 point2 point3 ->
                            let
                                spline =
                                    CubicSpline2d.fromControlPoints
                                        (toPoint state.penPosition)
                                        (toPoint point1)
                                        (toPoint point2)
                                        (toPoint point3)
                            in
                            { penPosition = point3
                            , previousPenPosition = point2
                            , loops =
                                List.range 0 (precision - 1)
                                    |> List.map (\index -> CubicSpline2d.pointOn spline (1 - toFloat index / precision))
                                    |> (\a -> List.Nonempty.replaceHead (a ++ currentList) state.loops)
                            }

                        S _ _ ->
                            state

                        Q _ _ ->
                            state

                        T _ ->
                            state

                        A _ _ _ _ _ ->
                            state

                        Md ( xd, yd ) ->
                            { penPosition = ( penX + xd, penY + yd )
                            , previousPenPosition = ( penX + xd, penY + yd )
                            , loops = state.loops
                            }

                        Ld ( xd, yd ) ->
                            { penPosition = ( penX + xd, penY + yd )
                            , previousPenPosition = state.penPosition
                            , loops =
                                List.Nonempty.replaceHead
                                    (Point2d.unsafe { x = toFloat (penX + xd), y = toFloat (penY + yd) } :: currentList)
                                    state.loops
                            }

                        Hd x ->
                            { penPosition = ( penX + x, penY )
                            , previousPenPosition = state.penPosition
                            , loops =
                                List.Nonempty.replaceHead
                                    (Point2d.unsafe { x = toFloat (penX + x), y = toFloat penY } :: currentList)
                                    state.loops
                            }

                        Vd y ->
                            { penPosition = ( penX, penY + y )
                            , previousPenPosition = state.penPosition
                            , loops =
                                List.Nonempty.replaceHead
                                    (Point2d.unsafe { x = toFloat penX, y = toFloat (penY + y) } :: currentList)
                                    state.loops
                            }

                        Zd ->
                            { penPosition = state.penPosition
                            , previousPenPosition = state.penPosition
                            , loops = List.Nonempty.cons [] state.loops
                            }

                        Cd point1 point2 point3 ->
                            let
                                spline =
                                    CubicSpline2d.fromControlPoints
                                        (toPoint state.penPosition)
                                        (toPoint (addTuples state.penPosition point1))
                                        (toPoint (addTuples state.penPosition point2))
                                        (toPoint (addTuples state.penPosition point3))
                            in
                            { penPosition = addTuples state.penPosition point3
                            , previousPenPosition = addTuples state.penPosition point2
                            , loops =
                                List.range 0 (precision - 1)
                                    |> List.map (\index -> CubicSpline2d.pointOn spline (1 - toFloat index / precision))
                                    |> (\a -> List.Nonempty.replaceHead (a ++ currentList) state.loops)
                            }

                        Sd point1 point2 ->
                            let
                                spline =
                                    CubicSpline2d.fromControlPoints
                                        (toPoint state.penPosition)
                                        (toPoint
                                            (subTuples state.penPosition state.previousPenPosition
                                                |> addTuples state.penPosition
                                            )
                                        )
                                        (toPoint (addTuples state.penPosition point1))
                                        (toPoint (addTuples state.penPosition point2))
                            in
                            { penPosition = addTuples state.penPosition point2
                            , previousPenPosition = addTuples state.penPosition point1
                            , loops =
                                List.range 0 (precision - 1)
                                    |> List.map
                                        (\index ->
                                            CubicSpline2d.pointOn spline (1 - (toFloat index / precision))
                                        )
                                    |> (\a -> List.Nonempty.replaceHead (a ++ currentList) state.loops)
                            }

                        Qd _ _ ->
                            state

                        Td _ ->
                            state

                        Ad _ _ _ _ _ ->
                            state
                )
                { penPosition = ( 0, 0 )
                , previousPenPosition = ( 0, 0 )
                , loops = Nonempty [] []
                }
                path
                |> .loops
                |> List.Nonempty.reverse

        addTuples : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
        addTuples ( x1, y1 ) ( x2, y2 ) =
            ( x1 + x2, y1 + y2 )

        subTuples : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
        subTuples ( x1, y1 ) ( x2, y2 ) =
            ( x1 - x2, y1 - y2 )

        toPoint : ( Int, Int ) -> Point2d units coordinates
        toPoint ( x, y ) =
            Point2d.fromTuple Quantity.unsafe ( toFloat x, toFloat y )

        polygons =
            List.Nonempty.map
                (List.uniqueBy (\p -> Point2d.toTuple Quantity.unwrap p))
                loops
                |> List.Nonempty.toList
                |> buildPolygons

        outlinePolygons :
            Polygon2d Unitless Unitless
            -> TriangularMesh (Point2d Unitless Unitless)
            -> TriangularMesh GlyphPoint
        outlinePolygons polygon mesh =
            case Polygon2d.outerLoop polygon of
                first :: second :: rest ->
                    let
                        dict =
                            List.foldl
                                (\loop dict_ ->
                                    case loop of
                                        first_ :: second_ :: rest_ ->
                                            Dict.union (expandLoop first_ second_ rest_) dict_

                                        _ ->
                                            dict_
                                )
                                (expandLoop first second rest)
                                (Polygon2d.innerLoops polygon)
                    in
                    TriangularMesh.mapVertices
                        (\p ->
                            case Dict.get p dict of
                                Just newP ->
                                    { position = newP, isOutline = True }

                                Nothing ->
                                    { position = p, isOutline = True }
                        )
                        mesh

                _ ->
                    abc True mesh

        abc : Bool -> TriangularMesh (Point2d Unitless Unitless) -> TriangularMesh GlyphPoint
        abc isOutline mesh =
            TriangularMesh.mapVertices (\point -> { isOutline = isOutline, position = point }) mesh
    in
    List.concatMap
        (\polygon ->
            let
                mesh : TriangularMesh (Point2d Unitless Unitless)
                mesh =
                    Polygon2d.triangulate polygon

                outlineMesh : TriangularMesh GlyphPoint
                outlineMesh =
                    outlinePolygons polygon mesh
            in
            [ outlineMesh, abc False mesh ]
        )
        polygons
        |> TriangularMesh.combine


expandLoop :
    Point2d units coordinates
    -> Point2d units coordinates
    -> List (Point2d units coordinates)
    -> Dict (Point2d units coordinates) (Point2d units coordinates)
expandLoop first second rest =
    List.foldl
        (\next state ->
            let
                na : Vector2d units coordinates
                na =
                    Vector2d.from next state.current
                        |> Vector2d.perpendicularTo
                        |> Vector2d.normalize
                        |> Vector2d.unwrap
                        |> Vector2d.unsafe

                nb : Vector2d units coordinates
                nb =
                    Vector2d.from state.current state.previous
                        |> Vector2d.perpendicularTo
                        |> Vector2d.normalize
                        |> Vector2d.unwrap
                        |> Vector2d.unsafe

                bis : Vector2d units coordinates
                bis =
                    Vector2d.plus na nb
                        |> Vector2d.normalize
                        |> Vector2d.unwrap
                        |> Vector2d.unsafe

                d : number
                d =
                    40

                l : Float
                l =
                    d / sqrt ((1 + (Vector2d.dot na nb |> Quantity.unwrap)) / 2)

                p : Point2d units coordinates
                p =
                    Point2d.translateBy (Vector2d.scaleBy l bis) state.current
            in
            { previous = state.current
            , outline = ( state.current, p ) :: state.outline
            , current = next
            }
        )
        { previous = first, current = second, outline = [] }
        (rest ++ [ first, second ])
        |> .outline
        |> Dict.fromList


{-| This does not handle polygons inside other polygon holes
-}
buildPolygons : List (List (Point2d units coordinates)) -> List (Polygon2d units coordinates)
buildPolygons polygons_ =
    let
        contains : List (Point2d units coordinates) -> List (Point2d units coordinates) -> Bool
        contains outer inner =
            Polygon2d.contains
                (inner |> List.head |> Maybe.withDefault Point2d.origin)
                (Polygon2d.singleLoop outer)
    in
    List.filterMap
        (\polygon ->
            let
                otherPolygons =
                    List.remove polygon polygons_
            in
            if List.any (\other -> contains other polygon) otherPolygons then
                Nothing

            else
                Polygon2d.withHoles (List.filter (contains polygon) otherPolygons) polygon |> Just
        )
        polygons_


pathParser : Parser (List Segment)
pathParser =
    Parser.loop
        []
        (\state ->
            spaces
                |> Parser.andThen
                    (\() ->
                        Parser.oneOf
                            [ Parser.map (\_ -> Parser.Done state) Parser.end
                            , Parser.map (\segment -> segment :: state |> Parser.Loop) segmentParser
                            ]
                    )
        )
        |> Parser.map List.reverse


segmentParser : Parser Segment
segmentParser =
    Parser.chompIf (\_ -> True)
        |> Parser.getChompedString
        |> Parser.andThen
            (\text ->
                case text of
                    "M" ->
                        Parser.map M parsePoint

                    "L" ->
                        Parser.map L parsePoint

                    "H" ->
                        Parser.map H parseInt

                    "V" ->
                        Parser.map V parseInt

                    "Z" ->
                        Parser.succeed Z

                    "C" ->
                        Parser.succeed C
                            |= parsePoint
                            |. spaces
                            |= parsePoint
                            |. spaces
                            |= parsePoint

                    "S" ->
                        Parser.succeed S
                            |= parsePoint
                            |. spaces
                            |= parsePoint

                    "Q" ->
                        Parser.succeed Q
                            |= parsePoint
                            |. spaces
                            |= parsePoint

                    "T" ->
                        Parser.map T parsePoint

                    "A" ->
                        Parser.succeed A
                            |= parsePoint
                            |. spaces
                            |= parseInt
                            |. spaces
                            |= parseBool
                            |. spaces
                            |= parseBool
                            |. spaces
                            |= parsePoint

                    "m" ->
                        Parser.map Md parsePoint

                    "l" ->
                        Parser.map Ld parsePoint

                    "h" ->
                        Parser.map Hd parseInt

                    "v" ->
                        Parser.map Vd parseInt

                    "z" ->
                        Parser.succeed Zd

                    "c" ->
                        Parser.succeed Cd
                            |= parsePoint
                            |. spaces
                            |= parsePoint
                            |. spaces
                            |= parsePoint

                    "s" ->
                        Parser.succeed Sd
                            |= parsePoint
                            |. spaces
                            |= parsePoint

                    "q" ->
                        Parser.succeed Qd
                            |= parsePoint
                            |. spaces
                            |= parsePoint

                    "t" ->
                        Parser.map Td parsePoint

                    "a" ->
                        Parser.succeed Ad
                            |= parsePoint
                            |. spaces
                            |= parseInt
                            |. spaces
                            |= parseBool
                            |. spaces
                            |= parseBool
                            |. spaces
                            |= parsePoint

                    _ ->
                        Parser.problem ("'" ++ text ++ "' is not a valid path flag.")
            )


parseInt : Parser Int
parseInt =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.int
        ]


parsePoint : Parser ( Int, Int )
parsePoint =
    Parser.succeed Tuple.pair
        |= parseInt
        |. spaces
        |= parseInt


parseBool : Parser Bool
parseBool =
    Parser.chompIf (\_ -> True)
        |> Parser.getChompedString
        |> Parser.andThen
            (\text ->
                case text of
                    "0" ->
                        Parser.succeed False

                    "1" ->
                        Parser.succeed True

                    _ ->
                        Parser.problem ("Was expecting a 0 or 1 for representing a boolean value but instead found '" ++ text ++ "'.")
            )


spaces : Parser ()
spaces =
    Parser.chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')


parseFontFaceAttributes : SvgParser.Element -> Result () FontFace
parseFontFaceAttributes fontFace =
    case fontFace.attributes of
        [ _, _, _, _, _, ( "ascent", ascent ), ( "descent", descent ), ( "x-height", xHeight ), ( "cap-height", capHeight ), ( "bbox", bbox ), _, _, _, _, _ ] ->
            case
                T5
                    (String.toInt ascent)
                    (String.toInt descent)
                    (String.toInt xHeight)
                    (String.toInt capHeight)
                    (String.split " " bbox |> List.map String.toInt)
            of
                T5 (Just ascent_) (Just descent_) (Just xHeight_) (Just capHeight_) [ Just left, Just bottom, Just right, Just top ] ->
                    Ok
                        { ascent = ascent_
                        , descent = descent_
                        , xHeight = xHeight_
                        , capHeight = capHeight_
                        , bboxLeft = left
                        , bboxRight = right
                        , bboxTop = top
                        , bboxBottom = bottom
                        }

                _ ->
                    Err ()

        _ ->
            Err ()


glyphPointCodec : Codec e GlyphPoint
glyphPointCodec =
    Serialize.record GlyphPoint
        |> Serialize.field .position point2dCodec
        |> Serialize.field .isOutline Serialize.bool
        |> Serialize.finishRecord


glyphCodec : Codec e Glyph
glyphCodec =
    Serialize.record Glyph
        |> Serialize.field .horizontalAdvance Serialize.int
        |> Serialize.field
            .path
            (triangularMeshCodec glyphPointCodec)
        |> Serialize.finishRecord


point2dCodec : Codec e (Point2d units coordinates)
point2dCodec =
    Serialize.map
        Point2d.unsafe
        Point2d.unwrap
        (Serialize.record (\x y -> { x = x, y = y })
            |> Serialize.field .x Serialize.float
            |> Serialize.field .y Serialize.float
            |> Serialize.finishRecord
        )


triangularMeshCodec : Codec e vertex -> Codec e (TriangularMesh vertex)
triangularMeshCodec vertexCodec =
    Serialize.map
        (\( vertices, indices ) -> TriangularMesh.indexed vertices indices)
        (\mesh -> ( TriangularMesh.vertices mesh, TriangularMesh.faceIndices mesh ))
        (Serialize.tuple
            (Serialize.array vertexCodec)
            (Serialize.list (Serialize.triple Serialize.int Serialize.int Serialize.int))
        )
