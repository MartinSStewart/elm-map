module ProtobufDecode exposing
    ( DecodeState
    , Decoder
    , FieldDecoder
    , bool
    , double
    , float
    , int32
    , int64
    , message
    , optional
    , sint64
    , stepPackedField
    , string
    , uint32
    , uint64
    , varIntDecoder
    , zigzagDecoder
    )

import AssocList as Dict exposing (Dict)
import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode
import Int64 exposing (Int64)



-- DECODER


{-| Describes how to turn a sequence of Protobuf-encoded bytes into a nice Elm value.

    import Protobuf.Decode as Decode

    type alias Person =
        { age : Int
        , name : String
        }

    personDecoder : Decode.Decoder Person
    personDecoder =
        Decode.message (Person 0 "")
            |> Decode.optional 1 Decode.int32 setAge
            |> Decode.optional 2 Decode.string setName

    -- SETTERS
    setAge : a -> { b | age : a } -> { b | age : a }
    setAge value model =
        { model | age = value }

    setName : a -> { b | name : a } -> { b | name : a }
    setName value model =
        { model | name = value }

-}
type alias Decoder a =
    Int -> Decode.Decoder ( Int, a )


{-| Describes how to decode a certain field in a Protobuf-encoded message and
how to update a record with the new Elm value.
-}
type alias FieldDecoder a =
    List ( Int, Decoder (a -> a) )


type alias DecodeState a =
    { width : Int
    , dict : Dict Int (Decoder (a -> a))
    , model : a
    }



-- FIELD DECODERS


{-| Decode **all remaining bytes** into an record. The initial value given here
holds all default values (which cannot be overridden for `proto3`). Each
provided field decoder calls a setter function to update the record when its
field number is encountered on the bytes sequence. _Unknown fields_ that have
no matching field decoder are currently being ignored.

    import Protobuf.Decode as Decode

    type alias Person =
        { name : String
        }

    personDecoder : Decode.Decoder Person
    personDecoder =
        -- Person "John"
        Decode.message (Person "John") []

-}
message : a -> List (FieldDecoder a) -> Decoder a
message v fieldDecoders =
    \width ->
        Decode.loop
            { width = width
            , dict =
                fieldDecoders
                    |> List.concat
                    |> Dict.fromList
            , model = v
            }
            (stepMessage width)



-- INTEGER


{-| Decode an optional field.

    import Protobuf.Decode as Decode

    type alias Person =
        { age : Int -- field number 2
        , name : String -- field number 4
        }

    personDecoder : Decode.Decoder Person
    personDecoder =
        -- <08 21 1A 04 4A 6F 68 6E> == Just (Person 33 "John")
        -- <08 21>                   == Just (Person 33 "")
        -- <>                        == Just (Person 0 "")
        Decode.message (Person 0 "")
            [ Decode.optional 2 int32 setAge
            , Decode.optional 4 string setName
            ]

    -- SETTERS
    setAge : a -> { b | age : a } -> { b | age : a }
    setAge value model =
        { model | age = value }

    setName : a -> { b | name : a } -> { b | name : a }
    setName value model =
        { model | name = value }

-}
optional : Int -> Decoder a -> (a -> b -> b) -> FieldDecoder b
optional fieldNumber decoder set =
    [ ( fieldNumber, map set decoder ) ]


{-| Decode a variable number of bytes into an integer from -9,223,372,036,854,775,808
to 9,223,372,036,854,775,807.
-}
int64 : Decoder Int64
int64 =
    \_ -> Decode.map (Tuple.mapSecond Int64.fromInt) varIntDecoder


{-| Decode a variable number of bytes into an integer from 0 to 18,446,744,073,709,551,615
-}
uint64 : Decoder Int64
uint64 =
    \_ -> Decode.map (Tuple.mapSecond (Int64.fromInt << unsigned64)) varIntDecoder


{-| Decode a variable number of bytes into an integer from -9,223,372,036,854,775,808
to 9,223,372,036,854,775,808.
-}
sint64 : Decoder Int64
sint64 =
    \_ -> Decode.map (Tuple.mapSecond (Int64.fromZigZag << Int64.fromInt)) varIntDecoder


{-| Decode a variable number of bytes into an integer from -2147483648 to 2147483647.
-}
int32 : Decode.Decoder ( Int, Int )
int32 =
    varIntDecoder



-- FLOAT


{-| Decode a variable number of bytes into an integer from 0 to 4294967295.
-}
uint32 : Decode.Decoder ( Int, Int )
uint32 =
    Decode.map (Tuple.mapSecond unsigned) varIntDecoder


{-| Decode eight bytes into a floating point number.
-}
double : Decoder Float
double =
    \_ -> Decode.map (Tuple.pair 8) (Decode.float64 LE)



-- STRING


{-| Decode four bytes into a floating point number.
-}
float : Decoder Float
float =
    \_ -> Decode.map (Tuple.pair 4) (Decode.float32 LE)



-- BOOLEAN


{-| Decode all bytes into a string.
-}
string : Decoder String
string =
    \width ->
        Decode.map (Tuple.pair width) (Decode.string width)



-- MAP


{-| Decode one byte into a boolean.
-}
bool : Decoder Bool
bool =
    \_ -> Decode.map (Tuple.mapSecond ((/=) 0)) varIntDecoder



-- BYTES DECODER


{-| Transform the value produced by a decoder.
This is useful when encoding custom types as an enumeration:

    type Fruit
        = Apple
        | Banana
        | Mango
        | Unrecognized Int

    fruitDecoder : Decoder Fruit
    fruitDecoder =
        Decode.int32
            |> Decode.map
                (\value ->
                    case value of
                        0 ->
                            Apple

                        1 ->
                            Banana

                        2 ->
                            Mango

                        v ->
                            Unrecognized v
                )

`Unrecognized Int` is only used for values that are present but not known. For
`proto2` decoding it is left out and unrecognized values are left out.

-}
map : (a -> b) -> Decoder a -> Decoder b
map fn decoder =
    \wireType -> Decode.map (Tuple.mapSecond fn) (decoder wireType)


stepMessage : Int -> DecodeState a -> Decode.Decoder (Decode.Step (DecodeState a) ( Int, a ))
stepMessage width state =
    if state.width <= 0 then
        Decode.succeed (Decode.Done ( width, state.model ))

    else
        varIntDecoder
            |> Decode.andThen
                (\( usedBytes, value ) ->
                    if Bitwise.and 0x07 value == 2 then
                        varIntDecoder
                            |> Decode.map (\( n, wireType ) -> ( usedBytes + n, ( Bitwise.shiftRightZfBy 3 value, wireType ) ))

                    else
                        Decode.succeed ( usedBytes, ( Bitwise.shiftRightZfBy 3 value, -1 ) )
                )
            |> Decode.andThen
                (\( usedBytes, ( fieldNumber, wireType ) ) ->
                    case Dict.get fieldNumber state.dict of
                        Just decoder ->
                            Decode.map
                                (\( n, fn ) ->
                                    Decode.Loop
                                        { width = state.width - usedBytes - n
                                        , model = fn state.model
                                        , dict = state.dict
                                        }
                                )
                                (decoder wireType)

                        Nothing ->
                            Decode.fail
                )


stepPackedField : Int -> Decode.Decoder ( Int, a ) -> ( Int, List a ) -> Decode.Decoder (Decode.Step ( Int, List a ) ( Int, List a ))
stepPackedField fullWidth decoder ( width, values ) =
    Decode.map
        (\( w, value ) ->
            let
                bytesRemaining =
                    width - w

                values_ =
                    value :: values
            in
            if bytesRemaining <= 0 then
                Decode.Done ( fullWidth, List.reverse values_ )

            else
                Decode.Loop ( bytesRemaining, values_ )
        )
        decoder


varIntDecoder : Decode.Decoder ( Int, Int )
varIntDecoder =
    Decode.unsignedInt8
        |> Decode.andThen
            (\octet ->
                if Bitwise.and 0x80 octet == 0x80 then
                    Decode.map
                        (\( usedBytes, value ) ->
                            ( usedBytes + 1
                            , Bitwise.and 0x7F octet + value * 2 ^ 7
                            )
                        )
                        varIntDecoder

                else
                    Decode.succeed ( 1, octet )
            )



-- VARINT


unsigned : Int -> Int
unsigned value =
    if value < 0 then
        value + 2 ^ 32

    else
        value


unsigned64 : Int -> Int
unsigned64 value =
    if value < 0 then
        value + 2 ^ 64

    else
        value


zigzagDecoder : Int -> Int
zigzagDecoder value =
    Bitwise.xor (Bitwise.shiftRightZfBy 1 value) -(Bitwise.and value 1)
