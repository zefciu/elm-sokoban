module UtilTest exposing (padTests, padToLongestTests)

import Expect
import Test
import Util


padTests : Test.Test
padTests =
    Test.describe "Tests for list padding"
        [ Test.test "Pad a list that is too short"
            (\_ -> Expect.equal (Util.padList 6 0 [ 1, 2, 3, 4 ]) [ 1, 2, 3, 4, 0, 0 ])
        , Test.test "Pad a list that is right length"
            (\_ -> Expect.equal (Util.padList 4 0 [ 1, 2, 3, 4 ]) [ 1, 2, 3, 4 ])
        , Test.test "Pad a list that is too long"
            (\_ -> Expect.equal (Util.padList 3 0 [ 1, 2, 3, 4 ]) [ 1, 2, 3, 4 ])
        ]


padToLongestTests : Test.Test
padToLongestTests =
    Test.describe "Tests for padToLongest"
        [ Test.test "Pad a list that is too short"
            (\_ ->
                Expect.equal
                    (Util.padToLongest 0
                        [ [ 1, 2, 3 ]
                        , [ 1, 2 ]
                        , [ 1, 2, 3, 4 ]
                        ]
                    )
                    [ [ 1, 2, 3, 0 ]
                    , [ 1, 2, 0, 0 ]
                    , [ 1, 2, 3, 4 ]
                    ]
            )
        , Test.test "Pad an empty list"
            (\_ ->
                Expect.equal
                    (Util.padToLongest 0 [])
                    []
            )
        ]
