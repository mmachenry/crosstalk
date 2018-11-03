import Html exposing (Html, p, text)
import WebSocket

main =
  Html.program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
    }

type alias Model = String

type Msg = WebSocketMessage String

init : (Model, Cmd Msg)
init = ("No messages received yet.", Cmd.none)

view : Model -> Html Msg
view model = p [] [ text model ]

update : Msg -> Model -> (Model, Cmd Msg)
update message model = case message of
  WebSocketMessage str -> (str, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen "ws://127.0.0.1:9160" WebSocketMessage

