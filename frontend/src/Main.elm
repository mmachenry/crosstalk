module Main exposing (..)

import Model exposing (..)
import View
import WebSocket
import Html
import List
import Matrix exposing (Matrix)
import Exts.List

main =
  Html.program {
    init = init,
    view = View.view,
    update = update,
    subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init = ({crossword = parseCrossword testInput}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update message model = case message of
  WebSocketMessage str -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen "ws://127.0.0.1:9160" WebSocketMessage

testInput =
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
  " _ _ __      "

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

parseCrossword : String -> Crossword
parseCrossword str =
     String.toList str
  |> List.map charToSquare
  |> squareList
  |> Matrix.fromList
  |> (\b->{ squares = b })

charToSquare : Char -> Square
charToSquare c = case c of
  '_' -> Blank
  ' ' -> EmptyBox
  _ -> Box c

squareList : List a -> List (List a)
squareList aList =
  let size = truncate <| sqrt <| toFloat <| List.length aList
  in Exts.List.chunk size aList
