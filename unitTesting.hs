import GameState
import ReadState
import WriteState

import Test.HUnit

testState = TestList ["Correct Reading 4x4" ~: "a.bb\na.cc\ndddd" ~=? writeState (readState "a.bb\na.cc\ndddd" ),
                      "Correct Reading 5x4" ~: "a..b\na..b\nad..\n.d..\nccc." ~=? writeState (readState "a..b\na..b\nad..\n.d..\nccc." )]
