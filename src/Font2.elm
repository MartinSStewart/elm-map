module Font2 exposing (..)

import Bytes exposing (Endianness(..))
import Bytes.Decode exposing (Decoder)
import Dict exposing (Dict)


type alias Font =
    { name : String
    , glyphs : Dict Char Glyph
    , lineHeight : Int
    , fontSize : Int
    , base : Int
    }


type alias Glyph =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , xOffset : Int
    , yOffset : Int
    , xAdvance : Int
    }


headerLength : number
headerLength =
    4


decode : Decoder Font
decode =
    Bytes.Decode.map5
        (\_ { fontSize, name } { lineHeight, base } () glyphs ->
            { name = name
            , fontSize = fontSize
            , glyphs = glyphs
            , lineHeight = lineHeight
            , base = base
            }
        )
        (Bytes.Decode.bytes headerLength)
        (decodeBlock decodeBlock1)
        (decodeBlock decodeBlock2)
        (decodeBlock decodeBlock3)
        (decodeBlock decodeBlock4)


decodeBlock1 : Int -> Decoder { fontSize : Int, name : String }
decodeBlock1 _ =
    Bytes.Decode.map3
        (\fontSize _ name -> { fontSize = fontSize, name = name })
        (Bytes.Decode.signedInt16 LE)
        (Bytes.Decode.bytes 12)
        decodeString


decodeBlock2 : Int -> Decoder { lineHeight : Int, base : Int }
decodeBlock2 size =
    Bytes.Decode.map3 (\lineHeight base _ -> { lineHeight = lineHeight, base = base })
        (Bytes.Decode.unsignedInt16 LE)
        (Bytes.Decode.unsignedInt16 LE)
        (Bytes.Decode.bytes (size - 4))


decodeBlock3 : Int -> Decoder ()
decodeBlock3 size =
    Bytes.Decode.bytes size |> Bytes.Decode.map (\_ -> ())


decodeBlock4 : Int -> Decoder (Dict Char Glyph)
decodeBlock4 size =
    Bytes.Decode.loop
        { glyphs = Dict.empty, count = size // 20 }
        (\state ->
            if state.count <= 0 then
                Bytes.Decode.Done state.glyphs |> Bytes.Decode.succeed

            else
                Bytes.Decode.map5
                    (\id ( x, y ) ( width, height ) ( xOffset, yOffset ) xAdvance ->
                        { glyphs =
                            Dict.insert
                                (Char.fromCode id)
                                { x = x
                                , y = y
                                , width = width
                                , height = height
                                , xOffset = xOffset
                                , yOffset = yOffset
                                , xAdvance = xAdvance
                                }
                                state.glyphs
                        , count = state.count - 1
                        }
                            |> Bytes.Decode.Loop
                    )
                    (Bytes.Decode.unsignedInt32 LE)
                    (Bytes.Decode.map2
                        Tuple.pair
                        (Bytes.Decode.unsignedInt16 LE)
                        (Bytes.Decode.unsignedInt16 LE)
                    )
                    (Bytes.Decode.map2
                        Tuple.pair
                        (Bytes.Decode.unsignedInt16 LE)
                        (Bytes.Decode.unsignedInt16 LE)
                    )
                    (Bytes.Decode.map2
                        Tuple.pair
                        (Bytes.Decode.signedInt16 LE)
                        (Bytes.Decode.signedInt16 LE)
                    )
                    (Bytes.Decode.signedInt16 LE)
                    |> Bytes.Decode.andThen (\state2 -> Bytes.Decode.bytes 2 |> Bytes.Decode.map (\_ -> state2))
        )


decodeBlock : (Int -> Decoder b) -> Decoder b
decodeBlock decoder =
    Bytes.Decode.map2
        (\blockType size -> size)
        Bytes.Decode.unsignedInt8
        (Bytes.Decode.unsignedInt32 LE)
        |> Bytes.Decode.andThen (\size -> decoder size)


decodeString : Decoder String
decodeString =
    Bytes.Decode.loop
        []
        (\chars ->
            Bytes.Decode.andThen
                (\value ->
                    if value == 0 then
                        List.reverse chars |> String.fromList |> Bytes.Decode.Done |> Bytes.Decode.succeed

                    else
                        Bytes.Decode.Loop (Char.fromCode value :: chars) |> Bytes.Decode.succeed
                )
                Bytes.Decode.unsignedInt8
        )
