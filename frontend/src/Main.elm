module Main exposing (..)

import Model exposing (..)
import View
import WebSocket
import Html
import List
import Matrix exposing (Matrix)
import ExampleData

main =
  Html.program {
    init = init,
    view = View.view,
    update = update,
    subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init = ({crossword = ExampleData.geoCachePuzzle}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update message model = case message of
  WebSocketMessage str -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen "ws://127.0.0.1:9160" WebSocketMessage

