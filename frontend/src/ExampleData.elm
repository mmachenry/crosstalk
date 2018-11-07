module ExampleData exposing (..)

import Model exposing (..)
import Matrix exposing (Matrix)

puzzle1 = {
  squares = parseCrossword (
      "      __ _ _ " ++
      " __ _        " ++
      " _    __ _ _ " ++
      "      _      " ++
      " _   _ _     " ++
      "    _     _ _" ++
      "_ __     __ _" ++
      "_ _     _    " ++
      "     _ _   _ " ++
      "      _      " ++
      " _ _ __    _ " ++
      "        _ __ " ++
      " _ _ __      "),
  acrossClues = [
    (1, "Cover (6)")
    ],
  downClues = [
    (1, "Slim (6)")
    ]
  }

geoCachePuzzle = {
  squares = parseCrossword (
      "   _ _ "++
      " _ _ _ "++
      "__     "++
      "   _ _ "++
      " _ _ _ "++
      " ___ __"++
      "       "
      ),
  acrossClues = [
    (1, "poet Pound"),
    (5, "1936 olympics venue"),
    (6, "depend"),
    (7, "96706")
    ],
  downClues = [
    (1, "pitcher stat"),
    (2, "monks house"),
    (3, "island nation"),
    (4, "elbow injuring sport"),
    (6, "save")
    ]
  }

testInput2 =
  "_GRATEFUL_B__" ++
  "____H_U__HARD" ++
  "R_C_I_L_E_D_E" ++
  "ETERNAL_P___T" ++
  "A_R_____I_B_E" ++
  "S_TECHNICAL_R" ++
  "O_A_______A_M" ++
  "N_IMPORTANT_I" ++
  "A_N_O_____A_N" ++
  "B___O_STRANGE" ++
  "L_B_R_L_E_T_D" ++
  "EVIL__I_A____" ++
  "  G_HOMELESS_"

