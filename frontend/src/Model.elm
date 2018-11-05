module Model exposing (..)

import Matrix exposing (Matrix)
import Exts.List

type alias Model = {
  crossword : Crossword
  }

type alias Crossword = {
  squares : Matrix Square,
  labels : List (Int, (Int, Int)),
  acrossClues : List (Int, String),
  downClues : List (Int, String)
  }

type Square = Blank | EmptyBox | Box Char

type Msg = WebSocketMessage String

parseCrossword : String -> Matrix Square
parseCrossword str =
     String.toList str
  |> List.map charToSquare
  |> squareList
  |> Matrix.fromList

charToSquare : Char -> Square
charToSquare c = case c of
  '_' -> Blank
  ' ' -> EmptyBox
  _ -> Box c

squareList : List a -> List (List a)
squareList aList =
  let size = truncate <| sqrt <| toFloat <| List.length aList
  in Exts.List.chunk size aList

