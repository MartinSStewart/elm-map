module List.Extra exposing (..)

{-| Since `elm-community/list-extra` changes major versions often and we only need a few functions, it's better to copy them here and add a dependency to `elm-community/list-extra`
-}


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) [ 2, 4, 6, 8 ]
    --> Just 6

-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


{-| Split list into groups of length `size`. If there are not enough elements
to completely fill the last group, it will not be included. This is equivalent
to calling `groupsOfWithStep` with the same `size` and `step`.

    groupsOf 3 (List.range 1 10)
    --> [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

-}
groupsOf : Int -> List a -> List (List a)
groupsOf size xs =
    groupsOfWithStep size size xs


{-| Split list into groups of length `size` at offsets `step` apart. If there
are not enough elements to completely fill the last group, it will not be
included. (See `greedyGroupsOfWithStep` if you would like the last group to be
included regardless.)

    groupsOfWithStep 4 4 (List.range 1 10)
    --> [ [ 1, 2, 3, 4 ], [ 5, 6, 7, 8 ] ]

    groupsOfWithStep 3 1 (List.range 1 5)
    --> [ [ 1, 2, 3 ], [ 2, 3, 4 ], [ 3, 4, 5 ] ]

    groupsOfWithStep 3 6 (List.range 1 20)
    --> [ [ 1, 2, 3 ], [ 7, 8, 9 ], [ 13, 14, 15 ] ]

If `step == size`, every element (except for perhaps the last few due to the
non-greedy behavior) will appear in exactly one group. If `step < size`, there
will be an overlap between groups. If `step > size`, some elements will be
skipped and not appear in any groups.

-}
groupsOfWithStep : Int -> Int -> List a -> List (List a)
groupsOfWithStep size step list =
    if size <= 0 || step <= 0 then
        []

    else
        let
            go : List a -> List (List a) -> List (List a)
            go xs acc =
                if List.isEmpty xs then
                    List.reverse acc

                else
                    let
                        thisGroup =
                            List.take size xs
                    in
                    if size == List.length thisGroup then
                        let
                            rest =
                                List.drop step xs
                        in
                        go rest (thisGroup :: acc)

                    else
                        List.reverse acc
        in
        go list []


{-| Group equal elements together. A function is applied to each element of the list
and then the equality check is performed against the results of that function evaluation.
Elements will be grouped in the same order as they appear in the original list. The
same applies to elements within each group.

    gatherEqualsBy .age [{age=25},{age=23},{age=25}]
    --> [({age=25},[{age=25}]),({age=23},[])]

-}
gatherEqualsBy : (a -> b) -> List a -> List ( a, List a )
gatherEqualsBy extract list =
    gatherWith (\a b -> extract a == extract b) list


{-| Group equal elements together using a custom equality function. Elements will be
grouped in the same order as they appear in the original list. The same applies to
elements within each group.

    gatherWith (==) [1,2,1,3,2]
    --> [(1,[1]),(2,[2]),(3,[])]

-}
gatherWith : (a -> a -> Bool) -> List a -> List ( a, List a )
gatherWith testFn list =
    let
        helper : List a -> List ( a, List a ) -> List ( a, List a )
        helper scattered gathered =
            case scattered of
                [] ->
                    List.reverse gathered

                toGather :: population ->
                    let
                        ( gathering, remaining ) =
                            List.partition (testFn toGather) population
                    in
                    helper remaining (( toGather, gathering ) :: gathered)
    in
    helper list []
