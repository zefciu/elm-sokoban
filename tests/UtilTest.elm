module UtilTest exposing (padTests)

import Expect
import Test
import Util


padTests : Test.Test
padTests =
    Test.describe "Tests for lists"
        [ Test.test "Pad a list that is too short"
            (\_ -> Expect.equal (Util.padList 6 0 [ 1, 2, 3, 4 ]) [ 1, 2, 3, 4, 0, 0 ])
        , Test.test "Pad a list that is right length"
            (\_ -> Expect.equal (Util.padList 4 0 [ 1, 2, 3, 4 ]) [ 1, 2, 3, 4 ])
        ]
