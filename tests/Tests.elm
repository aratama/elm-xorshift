module Tests exposing (all)

-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!

import Bitwise
import Expect
import Hex
import List.Extra as List
import Reference exposing (reference)
import Test exposing (..)
import XorShift exposing (Generator, randomint)


hexview : ( Int, Int ) -> String
hexview ( u, l ) =
    String.toUpper <|
        String.padLeft 8 '0' (Hex.toString u)
            ++ String.padLeft 8 '0' (Hex.toString l)


all : Test
all =
    describe "A Test Suite"
        [ test "randomint" <|
            \_ ->
                Expect.equal
                    (XorShift.randomint <| XorShift.Generator { state0U = 0, state0L = 1, state1U = 0, state1L = 2 })
                    ( ( 0, 3 ), XorShift.Generator { state0U = 0, state0L = 2, state1U = 0, state1L = 8388643 } )
        , test "randomint2" <|
            \_ ->
                Expect.equal
                    (XorShift.randomint <| XorShift.Generator { state0U = 0, state0L = 2, state1U = 0, state1L = 8388643 })
                    ( ( 0, 8388645 ), XorShift.Generator { state0U = 0, state0L = 8388643, state1U = 0, state1L = 25428064 } )
        , test "random int 3" <|
            \_ ->
                Expect.equal
                    (XorShift.randomint <| XorShift.Generator { state0U = 182003892, state0L = 1672215412, state1U = 973758608, state1L = 921479410 })
                    ( ( 1155762500, 2593694822 ), XorShift.Generator { state0U = 973758608, state0L = 921479410, state1U = 1798494951, state1L = 1369587883 } )
        , test "random int 4" <|
            \_ ->
                Expect.equal
                    (XorShift.randomint <| XorShift.Generator { state0U = 502995320, state0L = -2047706196, state1U = 1545392436, state1L = -197426 })
                    ( ( 2048387757, 2247063674 ), XorShift.Generator { state0U = 1545392436, state0L = -197426, state1U = -12189802, state1L = 1944073336 } )
        , test "random int 5" <|
            \_ ->
                case XorShift.randomint <| XorShift.Generator { state0U = 502995320, state0L = -2047706196, state1U = 1545392436, state1L = -197426 } of
                    ( r, _ ) ->
                        Expect.equal (hexview r) "7A17EAAD85EF787A"
        , test "hexView" <|
            \_ ->
                Expect.equal (hexview ( 0, 0 )) "0000000000000000"
        , describe "random int"
            [ let
                refs =
                    reference.integer.seed1_2
              in
              case XorShift.randomIntList (List.length refs) <| XorShift.Generator { state0U = 0, state0L = 1, state1U = 0, state1L = 2 } of
                ( rs, _ ) ->
                    test "randomInt int list with seed [0, 1, 0, 2]" <|
                        \_ ->
                            Expect.equal (List.map hexview rs) refs
            , let
                refs =
                    reference.integer.seed3_4
              in
              case XorShift.randomIntList (List.length refs) <| XorShift.Generator { state0U = 0, state0L = 3, state1U = 0, state1L = 4 } of
                ( rs, _ ) ->
                    test "randomInt int list with seed [0, 3, 0, 4]" <|
                        \_ ->
                            Expect.equal (List.map hexview rs) refs
            ]
        , describe "random double"
            [ let
                refs =
                    reference.double.seed1_2
              in
              case XorShift.randomList (List.length refs) <| XorShift.Generator { state0U = 0, state0L = 1, state1U = 0, state1L = 2 } of
                ( rs, _ ) ->
                    let
                        expects =
                            List.filterMap identity <| List.map String.toFloat refs
                    in
                    Test.concat <|
                        List.indexedMap
                            (\i ( r, expect ) ->
                                test ("random double list with seed [0, 1, 0, 2] (" ++ String.fromInt i ++ ")") <|
                                    \_ ->
                                        Expect.within (Expect.Absolute 0.00000001) r expect
                            )
                            (List.zip rs expects)
            , let
                refs =
                    reference.double.seed3_4
              in
              case XorShift.randomList (List.length refs) <| XorShift.Generator { state0U = 0, state0L = 3, state1U = 0, state1L = 4 } of
                ( rs, _ ) ->
                    let
                        expects =
                            List.filterMap identity <| List.map String.toFloat refs
                    in
                    Test.concat <|
                        List.indexedMap
                            (\i ( r, expect ) ->
                                test ("random double list with seed [0, 3, 0, 4] (" ++ String.fromInt i ++ ")") <|
                                    \_ ->
                                        Expect.within (Expect.Absolute 0.00000001) r expect
                            )
                            (List.zip rs expects)
            ]
        ]
