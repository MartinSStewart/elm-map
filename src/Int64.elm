module Int64 exposing
    ( Int64
    , add
    , and
    , complement
    , decoder
    , encoder
    , fromInt
    , fromInt32s
    , fromZigZag
    , or
    , rotateLeftBy
    , rotateRightBy
    , shiftLeftBy
    , shiftRightZfBy
    , signedCompare
    , subtract
    , toBitString
    , toBits
    , toByteValues
    , toInt
    , toSignedString
    , toUnsignedString
    , toZigZag
    , unsignedCompare
    , xor
    )

{-| Vendored from lauber00/elm-int64/1.2.4 as the package no longer exists due to a github name change
-}

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)


{-| A 64-bit integer type with correct overflow behavior

Internally, the number is stored as two 32-bit integers.

-}
type alias Int64 =
    ( Int, Int )


{-| Convert a `Int` to `Int64`. This is guaranteed to work for integers in the [safe JS range](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER).

    fromInt 42
        |> toSignedString
        --> "42"

-}
fromInt : Int -> Int64
fromInt raw =
    if raw < 0 then
        let
            lower =
                raw
                    |> abs
                    |> Bitwise.complement
                    |> Bitwise.shiftRightZfBy 0
                    |> (+) 1

            upper =
                if lower > 0xFFFFFFFF then
                    raw // (2 ^ 32) - 1 + 1

                else
                    raw // (2 ^ 32) - 1
        in
        ( upper |> Bitwise.shiftRightZfBy 0
        , lower |> Bitwise.shiftRightZfBy 0
        )

    else if raw > 0xFFFFFFFF then
        ( raw // 2 ^ 32, Bitwise.shiftRightZfBy 0 raw )

    else
        ( 0, raw )


{-| Give an Int64 var, corresponding to Int var

    toInt (0, 2)
        --> 2

-}
toInt : Int64 -> Int
toInt ( upper, lower ) =
    upper * (2 ^ 32) + lower


{-| Give two integers, corresponding to the upper and lower 32 bits

    fromInt32s 4 2
        |> toHex
        --> "0000000400000002"

-}
fromInt32s : Int -> Int -> Int64
fromInt32s a b =
    ( Bitwise.shiftRightZfBy 0 a, Bitwise.shiftRightZfBy 0 b )



-- ARITHMETIC


{-| 64-bit addition, with correct overflow

    fromInt32s 0xFFFFFFFF 0xFFFFFFFF
        |> Int64.add (Int64.fromInt 1)
        |> Int64.toUnsignedString
        --> "0"

    fromInt32s 0xFFFFFFFF 0xFFFFFFFF
        |> Int64.add (Int64.fromInt 2)
        |> Int64.toUnsignedString
        --> "1"

-}
add : Int64 -> Int64 -> Int64
add ( a, b ) ( p, q ) =
    let
        lower =
            Bitwise.shiftRightZfBy 0 b + Bitwise.shiftRightZfBy 0 q

        higher =
            Bitwise.shiftRightZfBy 0 a + Bitwise.shiftRightZfBy 0 p
    in
    -- check for overflow in the lower bits
    if lower > 0xFFFFFFFF then
        ( Bitwise.shiftRightZfBy 0 (higher + 1), Bitwise.shiftRightZfBy 0 lower )

    else
        ( Bitwise.shiftRightZfBy 0 higher, Bitwise.shiftRightZfBy 0 lower )


{-| 64-bit subtraction, with correct overflow

    -- equivalent to `0 - 1`
    Int64.subtract  (Int64.fromInt 0) (Int64.fromInt 1)
        |> Int64.toUnsignedString
        --> "18446744073709551615"

    -- equivalent to `0 - 1`
    Int64.subtract  (Int64.fromInt 0) (Int64.fromInt 1)
        |> Int64.toSignedString
        --> "-1"


    -- equivalent to `1 - 0`
    Int64.subtract  (Int64.fromInt 1) (Int64.fromInt 0)
        |> Int64.toUnsignedString
        --> "1"

-}
subtract : Int64 -> Int64 -> Int64
subtract ( a, b ) ( p, q ) =
    let
        lower =
            Bitwise.shiftRightZfBy 0 b - Bitwise.shiftRightZfBy 0 q

        higher =
            Bitwise.shiftRightZfBy 0 a - Bitwise.shiftRightZfBy 0 p
    in
    -- check for overflow in the lower bits
    if lower < 0 then
        ( Bitwise.shiftRightZfBy 0 (higher - 1), Bitwise.shiftRightZfBy 0 lower )

    else
        ( Bitwise.shiftRightZfBy 0 higher, Bitwise.shiftRightZfBy 0 lower )



-- BITWISE


{-| Bitwise and
-}
and : Int64 -> Int64 -> Int64
and ( a, b ) ( p, q ) =
    ( Bitwise.and a p, Bitwise.and b q )


{-| Bitwise complement
-}
complement : Int64 -> Int64
complement ( a, b ) =
    ( Bitwise.complement a |> Bitwise.shiftRightZfBy 0
    , Bitwise.complement b |> Bitwise.shiftRightZfBy 0
    )


{-| Bitwise or
-}
or : Int64 -> Int64 -> Int64
or ( a, b ) ( p, q ) =
    ( Bitwise.or a p, Bitwise.or b q )


{-| Bitwise xor
-}
xor : Int64 -> Int64 -> Int64
xor ( a, b ) ( p, q ) =
    ( Bitwise.xor a p, Bitwise.xor b q )


{-| Left bitwise shift, typically written `<<`

Fills in zeros from the right.

    Int64.fromInt32s 0xDEADBEAF 0xBAAAAAAD
        |> Int64.shiftLeftBy 16
        |> Int64.toHex
        --> "beafbaaaaaad0000"

-}
shiftLeftBy : Int -> Int64 -> Int64
shiftLeftBy n ( higher, lower ) =
    if n > 32 then
        let
            carry =
                Bitwise.shiftLeftBy n lower
        in
        ( carry, 0 )

    else
        let
            carry =
                Bitwise.shiftRightZfBy (32 - n) lower

            newHigher =
                higher
                    |> Bitwise.shiftLeftBy n
                    |> Bitwise.or carry
        in
        ( newHigher, Bitwise.shiftLeftBy n lower )


{-| Right bitwise shift, typically written `>>` (but `>>>` in JavaScript)

Fills in zeros from the left.

    Int64.fromInt32s 0xDEADBEAF 0xBAAAAAAD
        |> Int64.shiftRightZfBy 16
        |> Int64.toHex
        --> "0000deadbeafbaaa"

-}
shiftRightZfBy : Int -> Int64 -> Int64
shiftRightZfBy n ( higher, lower ) =
    if n > 32 then
        ( 0, Bitwise.shiftRightZfBy n higher )

    else if n == 32 then
        ( 0, higher )

    else
        let
            carry =
                Bitwise.shiftLeftBy (32 - n) higher

            newLower =
                lower
                    |> Bitwise.shiftRightZfBy n
                    |> Bitwise.or carry
                    |> Bitwise.shiftRightZfBy 0
        in
        ( Bitwise.shiftRightZfBy n higher, newLower )


{-| Left bitwise rotation

    Int64.fromInt32s 0xDEADBEAF 0xBAAAAAAD
        |> Int64.rotateLeftBy 16
        |> Int64.toHex
        --> "beafbaaaaaaddead"

-}
rotateLeftBy : Int -> Int64 -> Int64
rotateLeftBy n_ (( higher, lower ) as i) =
    let
        n =
            n_ |> modBy 64
    in
    if n == 32 then
        ( lower, higher )

    else if n == 0 then
        ( higher, lower )

    else if n >= 32 then
        let
            -- guaranteed m <= 32
            m =
                64 - n

            carry1 =
                Bitwise.shiftLeftBy (n - 32) lower

            carry2 =
                Bitwise.shiftRightZfBy (32 - (n - 32)) higher

            carry3 =
                Bitwise.shiftLeftBy (n - 32) higher

            carry4 =
                Bitwise.shiftRightZfBy (32 - (n - 32)) lower
        in
        ( Bitwise.or carry1 carry2, Bitwise.or carry3 carry4 )

    else
        -- n <= 32, m > 32
        let
            carry1 =
                Bitwise.shiftLeftBy n lower

            carry2 =
                Bitwise.shiftRightZfBy (32 - n) higher

            carry3 =
                Bitwise.shiftLeftBy n higher

            carry4 =
                Bitwise.shiftRightZfBy (32 - n) lower
        in
        ( Bitwise.or carry3 carry4, Bitwise.or carry1 carry2 )


{-| Right bitwise rotation

    Int64.fromInt32s 0xDEADBEAF 0xBAAAAAAD
        |> Int64.rotateRightBy 16
        |> Int64.toHex
        --> "aaaddeadbeafbaaa"

-}
rotateRightBy : Int -> Int64 -> Int64
rotateRightBy n_ (( higher, lower ) as i) =
    let
        n =
            n_ |> modBy 64
    in
    if n == 32 then
        ( lower, higher )

    else if n == 0 then
        ( higher, lower )

    else if n > 32 then
        let
            -- guaranteed m <= 32
            m =
                64 - n

            carry =
                Bitwise.shiftRightZfBy (32 - m) lower

            p1 =
                higher
                    |> Bitwise.shiftLeftBy m
                    |> Bitwise.or carry

            p2 =
                Bitwise.shiftLeftBy m lower

            q1 =
                0

            q2 =
                Bitwise.shiftRightZfBy n higher
        in
        ( Bitwise.or p1 q1, Bitwise.or p2 q2 )

    else
        let
            -- guaranteed n <= 32, m > 32
            m =
                64 - n

            p1 =
                Bitwise.shiftLeftBy m lower

            p2 =
                0

            carry =
                Bitwise.shiftLeftBy (32 - n) higher

            q1 =
                Bitwise.shiftRightZfBy n higher

            q2 =
                lower
                    |> Bitwise.shiftRightZfBy n
                    |> Bitwise.or carry
        in
        ( Bitwise.or p1 q1, Bitwise.or p2 q2 )



-- COMPARISON


{-| Compare two `Int64` values, intepreting the bits as an unsigned integer.
-}
unsignedCompare : Int64 -> Int64 -> Order
unsignedCompare ( u1, l1 ) ( u2, l2 ) =
    case Basics.compare u1 u2 of
        EQ ->
            Basics.compare l1 l2

        otherwise ->
            otherwise


{-| Compare two `Int64` values, intepreting the bits as a signed integer.
-}
signedCompare : Int64 -> Int64 -> Order
signedCompare ( u1, l1 ) ( u2, l2 ) =
    let
        isPositive1 =
            Bitwise.and 0x80000000 u1 == 0

        isPositive2 =
            Bitwise.and 0x80000000 u2 == 0
    in
    case isPositive1 of
        True ->
            case isPositive2 of
                True ->
                    -- both positive
                    case Basics.compare u1 u2 of
                        EQ ->
                            Basics.compare l1 l2

                        otherwise ->
                            otherwise

                False ->
                    -- 1 is positive, 2 is negative
                    GT

        False ->
            case isPositive2 of
                False ->
                    -- both negative
                    case Basics.compare u1 u2 of
                        EQ ->
                            Basics.compare l1 l2

                        otherwise ->
                            otherwise

                True ->
                    -- 1 is negative, 2 is positive
                    LT



-- STRING CONVERSION


{-| Bits as a string of `0`s and `1`s in big-endian order.

    toBitString (fromInt 42)
        --> "0000000000000000000000000000000000000000000000000000000000101010"

-}
toBitString : Int64 -> String
toBitString input =
    toBits input
        |> List.foldr
            (\b accum ->
                case b of
                    True ->
                        String.cons '1' accum

                    False ->
                        String.cons '0' accum
            )
            ""


{-| Interpret a `Int64` as an unsigned integer, and give its string representation

    toSignedString (fromInt 10)
        --> "10"

    toSignedString (fromInt -10)
        --> "-10"

-}
toSignedString : Int64 -> String
toSignedString (( origUpper, origLower ) as input) =
    let
        isPositive =
            Bitwise.and (Bitwise.shiftLeftBy 31 1) upper /= 0

        ( upper, lower ) =
            complement input

        newLower =
            lower + 1

        newUpper =
            if newLower > 0xFFFFFFFF then
                upper

            else
                upper
    in
    if isPositive then
        toUnsignedStringHelp origUpper origLower ""

    else
        "-" ++ toUnsignedStringHelp newUpper newLower ""


{-| Interpret a `Int64` as an unsigned integer, and give its string representation

    toUnsignedString (fromInt 10)
        --> "10"

    toUnsignedString (fromInt -10)
        --> "18446744073709551606"

-}
toUnsignedString : Int64 -> String
toUnsignedString (( upper, lower ) as input) =
    toUnsignedStringHelp upper lower ""


toUnsignedStringHelp upper lower accum =
    let
        digit =
            ((upper |> modBy 10) * (2 ^ 32) + lower) |> modBy 10

        nextUpper =
            upper // 10

        nextLower =
            ((upper |> modBy 10) * (2 ^ 32) + lower) // 10
    in
    if lower < 10 && upper == 0 then
        String.cons (Char.fromCode (digit + 48)) accum

    else
        toUnsignedStringHelp (Bitwise.shiftRightZfBy 0 nextUpper) (Bitwise.shiftRightZfBy 0 nextLower) (String.cons (Char.fromCode (digit + 48)) accum)



-- Bytes


{-| A `elm/bytes` Decoder for `Int64`
-}
decoder : Endianness -> Decoder Int64
decoder endianness =
    case endianness of
        BE ->
            Decode.map2 (\higher lower -> ( higher, lower ))
                (Decode.unsignedInt32 BE)
                (Decode.unsignedInt32 BE)

        LE ->
            Decode.map2 (\lower higher -> ( higher, lower ))
                (Decode.unsignedInt32 LE)
                (Decode.unsignedInt32 LE)


{-| A `elm/bytes` Encoder for `Int64`
-}
encoder : Endianness -> Int64 -> Encoder
encoder endianness ( higher, lower ) =
    case endianness of
        BE ->
            Encode.sequence
                [ Encode.unsignedInt32 BE higher
                , Encode.unsignedInt32 BE lower
                ]

        LE ->
            Encode.sequence
                [ Encode.unsignedInt32 LE lower
                , Encode.unsignedInt32 LE higher
                ]


{-| Convert an `Int64` to its 8 byte values in big-endian order

    toByteValues  (fromInt 0xDEADBEAF)
        --> [0,0,0,0,222,173,190,175]

-}
toByteValues : Int64 -> List Int
toByteValues ( higher, lower ) =
    wordToBytes higher ++ wordToBytes lower


wordToBytes : Int -> List Int
wordToBytes int =
    [ int |> Bitwise.shiftRightZfBy 0x18 |> Bitwise.and 0xFF
    , int |> Bitwise.shiftRightZfBy 0x10 |> Bitwise.and 0xFF
    , int |> Bitwise.shiftRightZfBy 0x08 |> Bitwise.and 0xFF
    , int |> Bitwise.and 0xFF
    ]


{-| The individual bits as a list of `Bool` in big-endian order.

    toBits (fromInt 10)
        --> [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,False,True,False]

-}
toBits : Int64 -> List Bool
toBits ( upper, lower ) =
    bits32 upper ++ bits32 lower


bits32 : Int -> List Bool
bits32 value =
    bits32Help 32 value []


bits32Help n remaining accum =
    if n > 0 then
        let
            new =
                Bitwise.and 1 remaining == 1
        in
        bits32Help (n - 1) (Bitwise.shiftRightZfBy 1 remaining) (new :: accum)

    else
        accum


{-| Convert an `Int64` to a new `Int64` with zigZag algorithm

    toZigZag (0, 10)
        --> (0, 20)

-}
toZigZag : Int64 -> Int64
toZigZag ( upper, lower ) =
    ( Bitwise.xor (Bitwise.shiftRightBy 31 upper) (Bitwise.shiftLeftBy 1 upper)
    , Bitwise.xor (Bitwise.shiftRightBy 31 lower) (Bitwise.shiftLeftBy 1 lower)
    )


{-| Convert an `Int64` with zigZag algorithm to its original `Int64` value

    fromZigZag (0, 20)
        --> (0, 10)

-}
fromZigZag : Int64 -> Int64
fromZigZag ( upper, lower ) =
    ( Bitwise.xor (Bitwise.shiftRightZfBy 1 upper) (-1 * Bitwise.and 1 upper)
    , Bitwise.xor (Bitwise.shiftRightZfBy 1 lower) (-1 * Bitwise.and 1 lower)
    )
