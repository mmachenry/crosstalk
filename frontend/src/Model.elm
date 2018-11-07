module Model exposing (..)

import Matrix exposing (Matrix)
import Exts.List
import Exts.Maybe
import List.Extra

type alias Model = {
  crossword : Crossword
  }

type alias Crossword = {
  squares : Matrix Square,
  labels : List Label,
  acrossClues : List Clue,
  downClues : List Clue
  }

type Square = Blank | EmptyBox | Box Char
type alias Squares = Matrix Square
type alias Clue = (Int, String)
type alias Label = (Int, (Int, Int))

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

getLabels : Crossword -> List Label
getLabels crossword =
  let squares = crossword.squares
      startSquares =
           Matrix.mapWithLocation (makeLabel squares) squares
        |> Matrix.flatten
        |> Exts.Maybe.catMaybes
  in List.Extra.zip (List.range 1 (List.length startSquares)) startSquares

makeLabel : Squares -> Matrix.Location -> Square -> Maybe Matrix.Location
makeLabel squares location square = case square of
  Blank -> Nothing
  _ -> if startsAcross squares location || startsDown squares location
       then Just location
       else Nothing

startsAcross : Squares -> Matrix.Location -> Bool
startsAcross squares (row, col) =
  case Matrix.get (row,col-1) squares of
    Just (EmptyBox) -> False
    Just (Box _) -> False
    _ -> case Matrix.get (row,col+1) squares of
           Just (EmptyBox) -> True
           Just (Box _) -> True
           _ -> False

startsDown : Squares -> Matrix.Location -> Bool
startsDown squares (row, col) =
  case Matrix.get (row-1,col) squares of
    Just (EmptyBox) -> False
    Just (Box _) -> False
    _ -> case Matrix.get (row+1,col) squares of
           Just (EmptyBox) -> True
           Just (Box _) -> True
           _ -> False

