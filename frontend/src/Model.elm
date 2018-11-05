module Model exposing (..)

import Matrix exposing (Matrix)

type alias Model = {
  crossword : Crossword
  }

type alias Crossword = {
  squares : Matrix Square
  }

type Square = Blank | Box Char

type Msg = WebSocketMessage String

