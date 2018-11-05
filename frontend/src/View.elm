module View exposing (view)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Matrix exposing (Matrix)

view : Model -> Html Msg
view model = div [] [
  node "style" [type_ "text/css"] [ text myCss ],
  viewNew model
  ]

viewNew : Model -> Html Msg
viewNew model =
  div [class "crossword-board-container"] [
    node "style" [type_ "text/css"] [ text myCss ],
    div [class "crossword-board"] (
      squaresToHtml model.crossword.squares
      ++ [
      div [class "crossword-board",
           style [("position", "absolute"),
                  ("z-index", "60")]]
        (List.map makeLabel model.crossword.labels),
      div [ class "crossword-clues" ] [
        dl [class "crossword-clues__list"]
          (clueTitle "Across"
          :: (List.map clue model.crossword.acrossClues)),
        dl [class "crossword-clues__list"]
          (clueTitle "Down"
          :: (List.map clue model.crossword.downClues))
      ]
    ])
  ]

clueTitle title =
  dt [style [("font-weight", "bold"), ("padding", "4px")]] [text title]

clue : (Int, String) -> Html Msg
clue (num, phrase) =
  dd [style [("margin", "0"), ("padding", "4px")]]
     [ text ((toString num) ++ ". " ++ phrase) ]

squaresToHtml : Matrix Square -> List (Html Msg)
squaresToHtml board =
  Matrix.flatten (Matrix.mapWithLocation squareToHtml board)

squareToHtml : Matrix.Location -> Square -> Html Msg
squareToHtml (row, col) sq = case sq of
  Blank -> span [style [("background", "#000000"),
                        ("border", "1px solid #000000"),
                        ("outline", "1px solid #000000")]] []
  EmptyBox -> makeInput ""
  Box c -> makeInput (String.fromChar c)

makeInput : String -> Html Msg
makeInput val =
  input [class "crossword-board__item",
         type_ "text",
         minlength 1,
         maxlength 1,
         required True,
         value val] []

makeLabel : (Int, (Int, Int)) -> Html Msg
makeLabel (num, (row, col)) =
  span [style [("position", "relative"),
               ("grid-column", toString col),
               ("grid-row", toString row)]] [
   span [style [("position", "absolute"),
                ("top", "2px"),
                ("left", "2px"),
                ("font-size", "14px"),
                ("line-height", "1")]]
     [ text (toString num) ]]

myCss = """
.crossword-board-container {
  position: relative;
  background: #FFFFFF;
}

.crossword-board {
  position: absolute;
  z-index: 1;
  background: transparent;
  border: 1px solid #000000;
  width: 650px;
  height: 650px;
  display: grid;
  grid-template: repeat(13, 7.6923076923%)/repeat(13, 7.6923076923%);
  list-style-type: none;
  padding: 0;
  margin: 0 auto;
}

.crossword-board__item {
  border: 1px solid #000000;
  background: transparent;
  position: relative;
  z-index: 100;
  text-align: center;
  font-size: 20px;
  font-weight: bold;
  text-transform: uppercase;
}

.crossword-board__item:active, .crossword-board__item:focus {
  background: #FFFF74;
  border: 1px solid #000000;
  outline: 1px solid #000000;
}

.crossword-clues {
  position: absolute;
  top: 0;
  left: 650px;
  width: 650px;
}

.crossword-clues__list {
  margin: 0 0 0 60px;
  padding: 0;
  display: inline-block;
  vertical-align: top;
}
"""
