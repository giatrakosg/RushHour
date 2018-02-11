import GameState
import ReadState
import WriteState
import Test.HUnit

testState = TestList ["Correct Reading 4x4" ~: "a.bb\na.cc\ndddd" ~=? writeState (readState "a.bb\na.cc\ndddd" ),
                      "Correct Reading 5x4" ~: "a..b\na..b\nad..\n.d..\nccc." ~=? writeState (readState "a..b\na..b\nad..\n.d..\nccc." )]

testMove = TestList ["Down" ~: "...\n..a\n..a" ~=? writeState (makeMove (readState "..a\n..a\n...") ('a',South)),
                     "Right" ~: "...\n.aa\n..." ~=? writeState (makeMove (readState "...\naa.\n...") ('a',East)),
                     "Left" ~: "...\naa.\n..." ~=? writeState (makeMove (readState "...\n.aa\n...") ('a',West)),
                     "Up" ~: ".a.\n.a.\n..." ~=? writeState (makeMove (readState "...\n.a.\n.a.") ('a',North))]

testEmpty = TestList ["Is Empty" ~: True ~=? isempty (readState "a.bb\na...") 2,
                      "Has Element" ~: False ~=? isempty (readState "a.bb\na...") 3]
