module Font exposing (Font, FontFace, Glyph, GlyphPoint, codec)

import Dict as RegularDict
import Point2d exposing (Point2d)
import Quantity exposing (Unitless)
import Serialize exposing (Codec)
import TriangularMesh exposing (TriangularMesh)


type alias GlyphPoint =
    { position : Point2d Unitless Unitless, isOutline : Bool }


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


codec : Codec e Font
codec =
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
