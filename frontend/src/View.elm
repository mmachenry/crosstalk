module View exposing (view)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Matrix exposing (Matrix)

view : Model -> Html Msg
view model = div [] [
  node "style" [type_ "text/css"] [ text myCss ],
  --myHtml
  viewNew model
  ]

viewNew : Model -> Html Msg
viewNew model =
  div [class "crossword-board-container"] [
    node "style" [type_ "text/css"] [ text myCss ],
    div [class "crossword-board"] (
      squaresToHtml model.crossword.squares
      ++ [
      div [class "crossword-board crossword-board--labels"] [
      ],
      dl [class "crossword-clues__list crossword-clues__list--across"]
        ((dt [class "crossword-clues__list-title"] [ text "Across" ])
        :: acrossClues)
      ,
      dl [class "crossword-clues__list crossword-clues__list--down"]
        ((dt [class "crossword-clues__list-title"] [ text "Down" ])
        :: downClues)
      
    ])
  ]

acrossClues = List.map clue [
  ("across", 1, "Cover (6)")
  ]

downClues = List.map clue [
  ("down", 1, "Slim (6)")
  ]

clue : (String, Int, String) -> Html Msg
clue (dir, num, phrase) =
  dd [class ("crossword-clues__list-item crossword-clues__list-item--" ++ dir ++ "-" ++ toString num)
      --, data-number (toString num)
      ]
     [ text phrase ]

squaresToHtml : Matrix Square -> List (Html Msg)
squaresToHtml board =
  Matrix.flatten (Matrix.mapWithLocation squareToHtml board)

squareToHtml : Matrix.Location -> Square -> Html Msg
squareToHtml (row, col) sq =
  let idStr = "item" ++ toString row ++ "-" ++ toString col
  in case sq of
       Blank -> span [class "crossword-board__item--blank", id idStr] []
       Box c ->
         input [id idStr,
                class "crossword-board__item",
                type_ "text",
                minlength 1,
                maxlength 1,
                required True,
                value (String.fromChar c)] []

makeLabel : Int -> Int -> Html Msg
makeLabel row col =
  span [
    id "label-1",
    class "crossword-board__item-label crossword-board__item-label--1"]
    [span [class "crossword-board__item-label-text"] [ text "1" ]]

myHtml = div [ class "crossword-board-container" ]
    [ div [ class "crossword-board" ]
        [ text "        "
        , input [ class "crossword-board__item", id "item1-1", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[sS]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item1-2", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[hH]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item1-3", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[rR]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item1-4", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[oO]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item1-5", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[uU]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item1-6", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[dD]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item1-7" ]
            []
        , span [ class "crossword-board__item--blank", id "item1-8" ]
            []
        , input [ class "crossword-board__item", id "item1-9", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item1-10" ]
            []
        , input [ class "crossword-board__item", id "item1-11", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[lL]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item1-12" ]
            []
        , input [ class "crossword-board__item", id "item1-13", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[fF]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , text "        "
        , text "        "
        , input [ class "crossword-board__item", id "item2-1", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[lL]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item2-2" ]
            []
        , span [ class "crossword-board__item--blank", id "item2-3" ]
            []
        , input [ class "crossword-board__item", id "item2-4", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[rR]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item2-5" ]
            []
        , input [ class "crossword-board__item", id "item2-6", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item2-7", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[dD]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item2-8", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item2-9", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[mM]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item2-10", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[sS]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item2-11", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item2-12", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[lL]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item2-13", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , text "        "
        , text "        "
        , input [ class "crossword-board__item", id "item3-1", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[iI]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item3-2" ]
            []
        , input [ class "crossword-board__item", id "item3-3", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[sS]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item3-4", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item3-5", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[rR]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item3-6", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[iI]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item3-7" ]
            []
        , span [ class "crossword-board__item--blank", id "item3-8" ]
            []
        , input [ class "crossword-board__item", id "item3-9", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item3-10" ]
            []
        , input [ class "crossword-board__item", id "item3-11", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[tT]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item3-12" ]
            []
        , input [ class "crossword-board__item", id "item3-13", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[iI]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , text "        "
        , text "        "
        , input [ class "crossword-board__item", id "item4-1", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[gG]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item4-2", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[rR]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item4-3", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item4-4", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[tT]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item4-5", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[iI]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item4-6", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[sS]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item4-7" ]
            []
        , input [ class "crossword-board__item", id "item4-8", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[oO]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item4-9", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[rR]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item4-10", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[pP]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item4-11", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[hH]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item4-12", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item4-13", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[nN]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , text "        "
        , text "        "
        , input [ class "crossword-board__item", id "item5-1", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[hH]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item5-2" ]
            []
        , input [ class "crossword-board__item", id "item5-3", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[nN]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item5-4", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[oO]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item5-5", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[dD]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item5-6" ]
            []
        , input [ class "crossword-board__item", id "item5-7", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[fF]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item5-8" ]
            []
        , input [ class "crossword-board__item", id "item5-9", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item5-10", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[lL]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item5-11", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item5-12", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[rR]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item5-13", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[tT]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , text "        "
        , text "        "
        , input [ class "crossword-board__item", id "item6-1", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[tT]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item6-2", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[iI]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item6-3", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item6-4", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[rR]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item6-5" ]
            []
        , input [ class "crossword-board__item", id "item6-6", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[jJ]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item6-7", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item6-8", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[lL]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item6-9", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[lL]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item6-10", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[yY]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item6-11" ]
            []
        , input [ class "crossword-board__item", id "item6-12", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item6-13" ]
            []
        , text "        "
        , span [ class "crossword-board__item--blank", id "item7-1" ]
            []
        , input [ class "crossword-board__item", id "item7-2", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[nN]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item7-3" ]
            []
        , span [ class "crossword-board__item--blank", id "item7-4" ]
            []
        , input [ class "crossword-board__item", id "item7-5", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[dD]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item7-6", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item7-7", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[tT]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item7-8", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item7-9", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[dD]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item7-10" ]
            []
        , span [ class "crossword-board__item--blank", id "item7-11" ]
            []
        , input [ class "crossword-board__item", id "item7-12", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item7-13" ]
            []
        , text "        "
        , span [ class "crossword-board__item--blank", id "item8-1" ]
            []
        , input [ class "crossword-board__item", id "item8-2", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item8-3" ]
            []
        , input [ class "crossword-board__item", id "item8-4", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[hH]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item8-5", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item8-6", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[bB]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item8-7", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[iI]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item8-8", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[tT]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item8-9" ]
            []
        , input [ class "crossword-board__item", id "item8-10", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[vV]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item8-11", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item8-12", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[sS]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item8-13", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[tT]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , text "        "
        , text "        "
        , input [ class "crossword-board__item", id "item9-1", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[iI]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item9-2", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[nN]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item9-3", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[fF]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item9-4", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[uU]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item9-5", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[nN]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item9-6" ]
            []
        , input [ class "crossword-board__item", id "item9-7", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[dD]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item9-8" ]
            []
        , input [ class "crossword-board__item", id "item9-9", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[bB]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item9-10", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[iI]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item9-11", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[dD]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item9-12" ]
            []
        , input [ class "crossword-board__item", id "item9-13", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[wW]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , text "        "
        , text "        "
        , input [ class "crossword-board__item", id "item10-1", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[sS]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item10-2", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item10-3", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[lL]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item10-4", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item10-5", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[cC]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item10-6", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[tT]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item10-7" ]
            []
        , input [ class "crossword-board__item", id "item10-8", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[nN]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item10-9", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item10-10", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[gG]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item10-11", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item10-12", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[tT]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item10-13", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , text "        "
        , text "        "
        , input [ class "crossword-board__item", id "item11-1", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[lL]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item11-2" ]
            []
        , input [ class "crossword-board__item", id "item11-3", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[oO]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item11-4" ]
            []
        , input [ class "crossword-board__item", id "item11-5", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item11-6" ]
            []
        , span [ class "crossword-board__item--blank", id "item11-7" ]
            []
        , input [ class "crossword-board__item", id "item11-8", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item11-9", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[tT]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item11-10", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[oO]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item11-11", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[mM]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item11-12" ]
            []
        , input [ class "crossword-board__item", id "item11-13", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[lL]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , text "        "
        , text "        "
        , input [ class "crossword-board__item", id "item12-1", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item12-2", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[cC]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item12-3", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[cC]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item12-4", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[uU]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item12-5", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[rR]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item12-6", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[aA]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item12-7", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[cC]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item12-8", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[yY]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item12-9" ]
            []
        , input [ class "crossword-board__item", id "item12-10", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[uU]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item12-11" ]
            []
        , span [ class "crossword-board__item--blank", id "item12-12" ]
            []
        , input [ class "crossword-board__item", id "item12-13", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[vV]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , text "        "
        , text "        "
        , input [ class "crossword-board__item", id "item13-1", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[mM]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item13-2" ]
            []
        , input [ class "crossword-board__item", id "item13-3", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[kK]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item13-4" ]
            []
        , input [ class "crossword-board__item", id "item13-5", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[sS]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , span [ class "crossword-board__item--blank", id "item13-6" ]
            []
        , span [ class "crossword-board__item--blank", id "item13-7" ]
            []
        , input [ class "crossword-board__item", id "item13-8", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[sS]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item13-9", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[oO]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item13-10", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[rR]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item13-11", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[tT]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item13-12", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[iI]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , text "        "
        , input [ class "crossword-board__item", id "item13-13", attribute "maxlength" "1", attribute "minlength" "1", pattern "^[eE]{1}$", attribute "required" "required", type_ "text", value "" ]
            []
        , div [ class "crossword-board crossword-board--highlight crossword-board--highlight--across" ]
            [ span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-1", id "across-1" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-7", id "across-7" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-8", id "across-8" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-10", id "across-10" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-11", id "across-11" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-14", id "across-14" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-16", id "across-16" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-17", id "across-17" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-19", id "across-19" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-21", id "across-21" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-22", id "across-22" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-23", id "across-23" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-26", id "across-26" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-28", id "across-28" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-29", id "across-29" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-30", id "across-30" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-31", id "across-31" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-32", id "across-32" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--across-33", id "across-33" ]
                []
            ]
        , div [ class "crossword-board crossword-board--highlight crossword-board--highlight-down" ]
            [ span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-1", id "down-1" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-2", id "down-2" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-3", id "down-3" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-4", id "down-4" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-5", id "down-5" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-6", id "down-6" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-8", id "down-8" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-9", id "down-9" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-12", id "down-12" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-13", id "down-13" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-15", id "down-15" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-18", id "down-18" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-19", id "down-19" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-20", id "down-20" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-21", id "down-21" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-22", id "down-22" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-23", id "down-23" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-24", id "down-24" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-25", id "down-25" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-26", id "down-26" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-27", id "down-27" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-28", id "down-28" ]
                []
            , span [ class "crossword-board__item-highlight crossword-board__item-highlight--down-30", id "down-30" ]
                []
            ]
        , div [ class "crossword-board crossword-board--labels" ]
            [ span [ class "crossword-board__item-label crossword-board__item-label--1", id "label-1" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "1" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--2", id "label-2" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "2" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--3", id "label-3" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "3" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--4", id "label-4" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "4" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--5", id "label-5" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "5" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--6", id "label-6" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "6" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--7", id "label-7" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "7" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--8", id "label-8" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "8" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--9", id "label-9" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "9" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--10", id "label-10" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "10" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--11", id "label-11" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "11" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--12", id "label-12" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "12" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--13", id "label-13" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "13" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--14", id "label-14" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "14" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--15", id "label-15" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "15" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--16", id "label-16" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "16" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--17", id "label-17" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "17" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--18", id "label-18" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "18" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--19", id "label-19" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "19" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--20", id "label-20" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "20" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--21", id "label-21" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "21" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--22", id "label-22" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "22" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--23", id "label-23" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "23" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--24", id "label-24" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "24" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--25", id "label-25" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "25" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--26", id "label-26" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "26" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--27", id "label-27" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "27" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--28", id "label-28" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "28" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--29", id "label-29" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "29" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--30", id "label-30" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "30" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--31", id "label-31" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "31" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--32", id "label-32" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "32" ]
                ]
            , span [ class "crossword-board__item-label crossword-board__item-label--33", id "label-33" ]
                [ span [ class "crossword-board__item-label-text" ]
                    [ text "33" ]
                ]
            ]
        , div [ class "crossword-clues" ]
            [ dl [ class "crossword-clues__list crossword-clues__list--across" ]
                [ dt [ class "crossword-clues__list-title" ]
                    [ text "Across" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-1", attribute "data-number" "1" ]
                    [ text "Cover (6)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-7", attribute "data-number" "7" ]
                    [ text "Water (5,3)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-8", attribute "data-number" "8" ]
                    [ text "Indian Dress (4)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-10", attribute "data-number" "10" ]
                    [ text "Without payment (6)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-11", attribute "data-number" "11" ]
                    [ text "Parentless child (6)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-14", attribute "data-number" "14" ]
                    [ text "Signal agreement (3)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-16", attribute "data-number" "16" ]
                    [ text "Vigilant (5)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-17", attribute "data-number" "17" ]
                    [ text "Row (4)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-19", attribute "data-number" "19" ]
                    [ text "Wobbly dessert (5)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-21", attribute "data-number" "21" ]
                    [ text "Unfashionable (5)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-22", attribute "data-number" "22" ]
                    [ text "Nun's outfit (5)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-23", attribute "data-number" "23" ]
                    [ text "Singlet (4)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-26", attribute "data-number" "26" ]
                    [ text "As a joke (2,3)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-28", attribute "data-number" "28" ]
                    [ text "Offer (3)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-29", attribute "data-number" "29" ]
                    [ text "Choose (6)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-30", attribute "data-number" "30" ]
                    [ text "Nullify (6)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-31", attribute "data-number" "31" ]
                    [ text "Particle of matter (4)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-32", attribute "data-number" "32" ]
                    [ text "Precision (8)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--across-33", attribute "data-number" "33" ]
                    [ text "A sally (6)" ]
                ]
            , dl [ class "crossword-clues__list crossword-clues__list--down" ]
                [ dt [ class "crossword-clues__list-title" ]
                    [ text "Down" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-1", attribute "data-number" "1" ]
                    [ text "Slim (6)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-2", attribute "data-number" "2" ]
                    [ text "Public speaker (6)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-3", attribute "data-number" "3" ]
                    [ text "Podium (4)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-4", attribute "data-number" "4" ]
                    [ text "Gemstone (7)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-5", attribute "data-number" "5" ]
                    [ text "Turning tool (5)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-6", attribute "data-number" "6" ]
                    [ text "Mock attack (5)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-8", attribute "data-number" "8" ]
                    [ text "Sound of mind (4)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-9", attribute "data-number" "9" ]
                    [ text "To free (3)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-12", attribute "data-number" "12" ]
                    [ text "Fold (3)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-13", attribute "data-number" "13" ]
                    [ text "Regions (5)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-15", attribute "data-number" "15" ]
                    [ text "Stinking (5)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-18", attribute "data-number" "18" ]
                    [ text "Senseless (5)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-19", attribute "data-number" "19" ]
                    [ text "Quick short punch (3)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-20", attribute "data-number" "20" ]
                    [ text "Permit (3)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-21", attribute "data-number" "21" ]
                    [ text "Ballerinas (7)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-22", attribute "data-number" "22" ]
                    [ text "Shade (3)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-23", attribute "data-number" "23" ]
                    [ text "Vitality (6)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-24", attribute "data-number" "24" ]
                    [ text "Dutch cheese (4)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-25", attribute "data-number" "25" ]
                    [ text "Dozen (6)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-26", attribute "data-number" "26" ]
                    [ text "Muslim religion (5)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-27", attribute "data-number" "27" ]
                    [ text "Group of sheep (5)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-28", attribute "data-number" "28" ]
                    [ text "Wager (3)" ]
                , dd [ class "crossword-clues__list-item crossword-clues__list-item--down-30", attribute "data-number" "30" ]
                    [ text "No votes (4)" ]
                ]
            ]
        , div [ class "crossword-complete" ]
            [ text "well done, champ!       " ]
        ]
    ]

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

.crossword-board__item--blank {
  background: #000000;
  border: 1px solid #000000;
  outline: 1px solid #000000;
}

.crossword-board--labels {
  position: absolute;
  z-index: 60;
}

.crossword-board__item-label {
  position: relative;
}

.crossword-board__item-label-text {
  position: absolute;
  top: 2px;
  left: 2px;
  font-size: 14px;
  line-height: 1;
}

.crossword-board__item-label--1 {
  grid-column: 1/1;
}

.crossword-board__item-label--2 {
  grid-column: 4/4;
}

.crossword-board__item-label--3 {
  grid-column: 6/6;
}

.crossword-board__item-label--4 {
  grid-column: 9/9;
}

.crossword-board__item-label--5 {
  grid-column: 11/11;
}

.crossword-board__item-label--6 {
  grid-column: 13/13;
}

.crossword-board__item-label--7 {
  grid-column: 6/6;
  grid-row: 2/2;
}

.crossword-board__item-label--8 {
  grid-column: 3/3;
  grid-row: 3/3;
}

.crossword-board__item-label--9 {
  grid-column: 5/5;
  grid-row: 3/3;
}

.crossword-board__item-label--10 {
  grid-column: 1/1;
  grid-row: 4/4;
}

.crossword-board__item-label--11 {
  grid-column: 8/8;
  grid-row: 4/4;
}

.crossword-board__item-label--12 {
  grid-column: 10/10;
  grid-row: 4/4;
}

.crossword-board__item-label--13 {
  grid-column: 12/12;
  grid-row: 4/4;
}

.crossword-board__item-label--14 {
  grid-column: 3/3;
  grid-row: 5/5;
}

.crossword-board__item-label--15 {
  grid-column: 7/7;
  grid-row: 5/5;
}

.crossword-board__item-label--16 {
  grid-column: 9/9;
  grid-row: 5/5;
}

.crossword-board__item-label--17 {
  grid-column: 1/1;
  grid-row: 6/6;
}

.crossword-board__item-label--18 {
  grid-column: 2/2;
  grid-row: 6/6;
}

.crossword-board__item-label--19 {
  grid-column: 6/6;
  grid-row: 6/6;
}

.crossword-board__item-label--20 {
  grid-column: 8/8;
  grid-row: 6/6;
}

.crossword-board__item-label--21 {
  grid-column: 5/5;
  grid-row: 7/7;
}

.crossword-board__item-label--22 {
  grid-column: 4/4;
  grid-row: 8/8;
}

.crossword-board__item-label--23 {
  grid-column: 10/10;
  grid-row: 8/8;
}

.crossword-board__item-label--24 {
  grid-column: 11/11;
  grid-row: 8/8;
}

.crossword-board__item-label--25 {
  grid-column: 13/13;
  grid-row: 8/8;
}

.crossword-board__item-label--26 {
  grid-column: 1/1;
  grid-row: 9/9;
}

.crossword-board__item-label--27 {
  grid-column: 3/3;
  grid-row: 9/9;
}

.crossword-board__item-label--28 {
  grid-column: 9/9;
  grid-row: 9/9;
}

.crossword-board__item-label--29 {
  grid-column: 1/1;
  grid-row: 10/10;
}

.crossword-board__item-label--30 {
  grid-column: 8/8;
  grid-row: 10/10;
}

.crossword-board__item-label--31 {
  grid-column: 8/8;
  grid-row: 11/11;
}

.crossword-board__item-label--32 {
  grid-column: 1/1;
  grid-row: 12/12;
}

.crossword-board__item-label--33 {
  grid-column: 8/8;
  grid-row: 13/13;
}

.crossword-board--highlight {
  position: absolute;
  z-index: 50;
}

.crossword-board__item-highlight {
  background: #9AFF67;
  display: grid;
  opacity: 0;
  transition: opacity 0.3s linear;
}

/***********************************************************/
/** ACROSS ANSWERS HIGHLIGHTING START                      */
/***********************************************************/
.crossword-board__item-highlight--across-1 {
  grid-column: 1/7;
}

.crossword-board__item-highlight--across-7 {
  grid-column: 6/14;
  grid-row: 2;
}

.crossword-board__item-highlight--across-8 {
  grid-column: 3/7;
  grid-row: 3;
}

.crossword-board__item-highlight--across-10 {
  grid-column: 1/7;
  grid-row: 4;
}

.crossword-board__item-highlight--across-11 {
  grid-column: 8/14;
  grid-row: 4;
}

.crossword-board__item-highlight--across-14 {
  grid-column: 3/6;
  grid-row: 5;
}

.crossword-board__item-highlight--across-16 {
  grid-column: 9/14;
  grid-row: 5;
}

.crossword-board__item-highlight--across-17 {
  grid-column: 1/5;
  grid-row: 6;
}

.crossword-board__item-highlight--across-19 {
  grid-column: 6/11;
  grid-row: 6;
}

.crossword-board__item-highlight--across-21 {
  grid-column: 5/10;
  grid-row: 7;
}

.crossword-board__item-highlight--across-22 {
  grid-column: 4/9;
  grid-row: 8;
}

.crossword-board__item-highlight--across-23 {
  grid-column: 10/14;
  grid-row: 8;
}

.crossword-board__item-highlight--across-26 {
  grid-column: 1/6;
  grid-row: 9;
}

.crossword-board__item-highlight--across-28 {
  grid-column: 9/12;
  grid-row: 9;
}

.crossword-board__item-highlight--across-29 {
  grid-column: 1/7;
  grid-row: 10;
}

.crossword-board__item-highlight--across-30 {
  grid-column: 8/14;
  grid-row: 10;
}

.crossword-board__item-highlight--across-31 {
  grid-column: 8/12;
  grid-row: 11;
}

.crossword-board__item-highlight--across-32 {
  grid-column: 1/9;
  grid-row: 12;
}

.crossword-board__item-highlight--across-33 {
  grid-column: 8/14;
  grid-row: 13;
}

.crossword-board__item-highlight--across-33 {
  grid-column: 8/14;
  grid-row: 13;
}

#item1-1:valid ~ #item1-2:valid ~ #item1-3:valid ~ #item1-4:valid ~ #item1-5:valid ~ #item1-6:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-1 {
  opacity: 1;
}

#item2-6:valid ~ #item2-7:valid ~ #item2-8:valid ~ #item2-9:valid ~ #item2-10:valid ~ #item2-11:valid ~ #item2-12:valid ~ #item2-13:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-7 {
  opacity: 1;
}

#item3-3:valid ~ #item3-4:valid ~ #item3-5:valid ~ #item3-6:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-8 {
  opacity: 1;
}

#item4-1:valid ~ #item4-2:valid ~ #item4-3:valid ~ #item4-4:valid ~ #item4-5:valid ~ #item4-6:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-10 {
  opacity: 1;
}

#item4-8:valid ~ #item4-9:valid ~ #item4-10:valid ~ #item4-11:valid ~ #item4-12:valid ~ #item4-13:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-11 {
  opacity: 1;
}

#item5-3:valid ~ #item5-4:valid ~ #item5-5:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-14 {
  opacity: 1;
}

#item5-9:valid ~ #item5-10:valid ~ #item5-11:valid ~ #item5-12:valid ~ #item5-13:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-16 {
  opacity: 1;
}

#item6-1:valid ~ #item6-2:valid ~ #item6-3:valid ~ #item6-4:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-17 {
  opacity: 1;
}

#item6-6:valid ~ #item6-7:valid ~ #item6-8:valid ~ #item6-9:valid ~ #item6-10:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-19 {
  opacity: 1;
}

#item7-5:valid ~ #item7-6:valid ~ #item7-7:valid ~ #item7-8:valid ~ #item7-9:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-21 {
  opacity: 1;
}

#item8-4:valid ~ #item8-5:valid ~ #item8-6:valid ~ #item8-7:valid ~ #item8-8:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-22 {
  opacity: 1;
}

#item8-10:valid ~ #item8-11:valid ~ #item8-12:valid ~ #item8-13:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-23 {
  opacity: 1;
}

#item9-1:valid ~ #item9-2:valid ~ #item9-3:valid ~ #item9-4:valid ~ #item9-5:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-26 {
  opacity: 1;
}

#item9-9:valid ~ #item9-10:valid ~ #item9-11:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-28 {
  opacity: 1;
}

#item10-1:valid ~ #item10-2:valid ~ #item10-3:valid ~ #item10-4:valid ~ #item10-5:valid ~ #item10-6:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-29 {
  opacity: 1;
}

#item10-8:valid ~ #item10-9:valid ~ #item10-10:valid ~ #item10-11:valid ~ #item10-12:valid ~ #item10-13:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-30 {
  opacity: 1;
}

#item11-8:valid ~ #item11-9:valid ~ #item11-10:valid ~ #item11-11:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-31 {
  opacity: 1;
}

#item12-1:valid ~ #item12-2:valid ~ #item12-3:valid ~ #item12-4:valid ~ #item12-5:valid ~ #item12-6:valid ~ #item12-7:valid ~ #item12-8:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-32 {
  opacity: 1;
}

#item13-8:valid ~ #item13-9:valid ~ #item13-10:valid ~ #item13-11:valid ~ #item13-12:valid ~ #item13-13:valid
~ .crossword-board--highlight .crossword-board__item-highlight--across-33 {
  opacity: 1;
}

/***********************************************************/
/** ACROSS ANSWERS HIGHLIGHTING END                        */
/***********************************************************/
/***********************************************************/
/** DOWN ANSWERS HIGHLIGHTING START                        */
/***********************************************************/
.crossword-board__item-highlight--down-1 {
  grid-column: 1;
  grid-row: 1/7;
}

.crossword-board__item-highlight--down-2 {
  grid-column: 4;
  grid-row: 1/7;
}

.crossword-board__item-highlight--down-3 {
  grid-column: 6;
  grid-row: 1/5;
}

.crossword-board__item-highlight--down-4 {
  grid-column: 9;
  grid-row: 1/8;
}

.crossword-board__item-highlight--down-5 {
  grid-column: 11;
  grid-row: 1/6;
}

.crossword-board__item-highlight--down-6 {
  grid-column: 13;
  grid-row: 1/6;
}

.crossword-board__item-highlight--down-8 {
  grid-column: 3;
  grid-row: 3/7;
}

.crossword-board__item-highlight--down-9 {
  grid-column: 5;
  grid-row: 3/6;
}

.crossword-board__item-highlight--down-12 {
  grid-column: 10;
  grid-row: 4/7;
}

.crossword-board__item-highlight--down-13 {
  grid-column: 12;
  grid-row: 4/9;
}

.crossword-board__item-highlight--down-15 {
  grid-column: 7;
  grid-row: 5/10;
}

.crossword-board__item-highlight--down-18 {
  grid-column: 2;
  grid-row: 6/11;
}

.crossword-board__item-highlight--down-19 {
  grid-column: 6;
  grid-row: 6/9;
}

.crossword-board__item-highlight--down-20 {
  grid-column: 8;
  grid-row: 6/9;
}

.crossword-board__item-highlight--down-21 {
  grid-column: 5;
  grid-row: 7/14;
}

.crossword-board__item-highlight--down-22 {
  grid-column: 4;
  grid-row: 8/11;
}

.crossword-board__item-highlight--down-23 {
  grid-column: 10;
  grid-row: 8/14;
}

.crossword-board__item-highlight--down-24 {
  grid-column: 11;
  grid-row: 8/12;
}

.crossword-board__item-highlight--down-25 {
  grid-column: 13;
  grid-row: 8/14;
}

.crossword-board__item-highlight--down-26 {
  grid-column: 1;
  grid-row: 9/14;
}

.crossword-board__item-highlight--down-27 {
  grid-column: 3;
  grid-row: 9/14;
}

.crossword-board__item-highlight--down-28 {
  grid-column: 9;
  grid-row: 9/12;
}

.crossword-board__item-highlight--down-30 {
  grid-column: 8;
  grid-row: 10/14;
}

#item1-1:valid ~ #item2-1:valid ~ #item3-1:valid ~ #item4-1:valid ~ #item5-1:valid ~ #item6-1:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-1 {
  opacity: 1;
}

#item1-4:valid ~ #item2-4:valid ~ #item3-4:valid ~ #item4-4:valid ~ #item5-4:valid ~ #item6-4:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-2 {
  opacity: 1;
}

#item1-6:valid ~ #item2-6:valid ~ #item3-6:valid ~ #item4-6:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-3 {
  opacity: 1;
}

#item1-9:valid ~ #item2-9:valid ~ #item3-9:valid ~ #item4-9:valid ~ #item5-9:valid ~ #item6-9:valid ~ #item7-9:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-4 {
  opacity: 1;
}

#item1-11:valid ~ #item2-11:valid ~ #item3-11:valid ~ #item4-11:valid ~ #item5-11:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-5 {
  opacity: 1;
}

#item1-13:valid ~ #item2-13:valid ~ #item3-13:valid ~ #item4-13:valid ~ #item5-13:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-6 {
  opacity: 1;
}

#item3-3:valid ~ #item4-3:valid ~ #item5-3:valid ~ #item6-3:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-8 {
  opacity: 1;
}

#item3-5:valid ~ #item4-5:valid ~ #item5-5:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-9 {
  opacity: 1;
}

#item4-10:valid ~ #item5-10:valid ~ #item6-10:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-12 {
  opacity: 1;
}

#item4-12:valid ~ #item5-12:valid ~ #item6-12:valid ~ #item7-12:valid ~ #item8-12:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-13 {
  opacity: 1;
}

#item5-7:valid ~ #item6-7:valid ~ #item7-7:valid ~ #item8-7:valid ~ #item9-7:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-15 {
  opacity: 1;
}

#item6-2:valid ~ #item7-2:valid ~ #item8-2:valid ~ #item9-2:valid ~ #item10-2:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-18 {
  opacity: 1;
}

#item6-6:valid ~ #item7-6:valid ~ #item8-6:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-19 {
  opacity: 1;
}

#item6-8:valid ~ #item7-8:valid ~ #item8-8:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-20 {
  opacity: 1;
}

#item7-5:valid ~ #item8-5:valid ~ #item9-5:valid ~ #item10-5:valid ~ #item11-5:valid ~ #item12-5:valid ~ #item13-5:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-21 {
  opacity: 1;
}

#item8-4:valid ~ #item9-4:valid ~ #item10-4:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-22 {
  opacity: 1;
}

#item8-10:valid ~ #item9-10:valid ~ #item10-10:valid ~ #item11-10:valid ~ #item12-10:valid ~ #item13-10:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-23 {
  opacity: 1;
}

#item8-11:valid ~ #item9-11:valid ~ #item10-11:valid ~ #item11-11:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-24 {
  opacity: 1;
}

#item8-13:valid ~ #item9-13:valid ~ #item10-13:valid ~ #item11-13:valid ~ #item12-13:valid ~ #item13-13:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-25 {
  opacity: 1;
}

#item9-1:valid ~ #item10-1:valid ~ #item11-1:valid ~ #item12-1:valid ~ #item13-1:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-26 {
  opacity: 1;
}

#item9-3:valid ~ #item10-3:valid ~ #item11-3:valid ~ #item12-3:valid ~ #item13-3:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-27 {
  opacity: 1;
}

#item9-9:valid ~ #item10-9:valid ~ #item11-9:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-28 {
  opacity: 1;
}

#item10-8:valid ~ #item11-8:valid ~ #item12-8:valid ~ #item13-8:valid
~ .crossword-board--highlight .crossword-board__item-highlight--down-30 {
  opacity: 1;
}

/***********************************************************/
/** DOWN ANSWERS HIGHLIGHTING END                          */
/***********************************************************/
#checkvaliditems:checked ~ .crossword-board-container .crossword-board__item:valid {
  background: #9AFF67;
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

.crossword-clues__list-title {
  font-weight: bold;
  padding: 4px;
}

.crossword-clues__list-item {
  margin: 0;
  padding: 4px;
}
.crossword-clues__list-item:before {
  content: attr(data-number) ". ";
}

#item1-1:active ~ .crossword-clues .crossword-clues__list-item--across-1,
#item1-1:focus ~ .crossword-clues .crossword-clues__list-item--across-1,
#item1-1:hover ~ .crossword-clues .crossword-clues__list-item--across-1 {
  background: #FFFF74;
}

#item1-2:active ~ .crossword-clues .crossword-clues__list-item--across-1,
#item1-2:focus ~ .crossword-clues .crossword-clues__list-item--across-1,
#item1-2:hover ~ .crossword-clues .crossword-clues__list-item--across-1 {
  background: #FFFF74;
}

#item1-3:active ~ .crossword-clues .crossword-clues__list-item--across-1,
#item1-3:focus ~ .crossword-clues .crossword-clues__list-item--across-1,
#item1-3:hover ~ .crossword-clues .crossword-clues__list-item--across-1 {
  background: #FFFF74;
}

#item1-4:active ~ .crossword-clues .crossword-clues__list-item--across-1,
#item1-4:focus ~ .crossword-clues .crossword-clues__list-item--across-1,
#item1-4:hover ~ .crossword-clues .crossword-clues__list-item--across-1 {
  background: #FFFF74;
}

#item1-5:active ~ .crossword-clues .crossword-clues__list-item--across-1,
#item1-5:focus ~ .crossword-clues .crossword-clues__list-item--across-1,
#item1-5:hover ~ .crossword-clues .crossword-clues__list-item--across-1 {
  background: #FFFF74;
}

#item1-6:active ~ .crossword-clues .crossword-clues__list-item--across-1,
#item1-6:focus ~ .crossword-clues .crossword-clues__list-item--across-1,
#item1-6:hover ~ .crossword-clues .crossword-clues__list-item--across-1 {
  background: #FFFF74;
}

#item2-6:active ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-6:focus ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-6:hover ~ .crossword-clues .crossword-clues__list-item--across-7 {
  background: #FFFF74;
}

#item2-7:active ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-7:focus ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-7:hover ~ .crossword-clues .crossword-clues__list-item--across-7 {
  background: #FFFF74;
}

#item2-8:active ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-8:focus ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-8:hover ~ .crossword-clues .crossword-clues__list-item--across-7 {
  background: #FFFF74;
}

#item2-9:active ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-9:focus ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-9:hover ~ .crossword-clues .crossword-clues__list-item--across-7 {
  background: #FFFF74;
}

#item2-10:active ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-10:focus ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-10:hover ~ .crossword-clues .crossword-clues__list-item--across-7 {
  background: #FFFF74;
}

#item2-11:active ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-11:focus ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-11:hover ~ .crossword-clues .crossword-clues__list-item--across-7 {
  background: #FFFF74;
}

#item2-12:active ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-12:focus ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-12:hover ~ .crossword-clues .crossword-clues__list-item--across-7 {
  background: #FFFF74;
}

#item2-13:active ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-13:focus ~ .crossword-clues .crossword-clues__list-item--across-7,
#item2-13:hover ~ .crossword-clues .crossword-clues__list-item--across-7 {
  background: #FFFF74;
}

#item3-3:active ~ .crossword-clues .crossword-clues__list-item--across-8,
#item3-3:focus ~ .crossword-clues .crossword-clues__list-item--across-8,
#item3-3:hover ~ .crossword-clues .crossword-clues__list-item--across-8 {
  background: #FFFF74;
}

#item3-4:active ~ .crossword-clues .crossword-clues__list-item--across-8,
#item3-4:focus ~ .crossword-clues .crossword-clues__list-item--across-8,
#item3-4:hover ~ .crossword-clues .crossword-clues__list-item--across-8 {
  background: #FFFF74;
}

#item3-5:active ~ .crossword-clues .crossword-clues__list-item--across-8,
#item3-5:focus ~ .crossword-clues .crossword-clues__list-item--across-8,
#item3-5:hover ~ .crossword-clues .crossword-clues__list-item--across-8 {
  background: #FFFF74;
}

#item3-6:active ~ .crossword-clues .crossword-clues__list-item--across-8,
#item3-6:focus ~ .crossword-clues .crossword-clues__list-item--across-8,
#item3-6:hover ~ .crossword-clues .crossword-clues__list-item--across-8 {
  background: #FFFF74;
}

#item4-1:active ~ .crossword-clues .crossword-clues__list-item--across-10,
#item4-1:focus ~ .crossword-clues .crossword-clues__list-item--across-10,
#item4-1:hover ~ .crossword-clues .crossword-clues__list-item--across-10 {
  background: #FFFF74;
}

#item4-2:active ~ .crossword-clues .crossword-clues__list-item--across-10,
#item4-2:focus ~ .crossword-clues .crossword-clues__list-item--across-10,
#item4-2:hover ~ .crossword-clues .crossword-clues__list-item--across-10 {
  background: #FFFF74;
}

#item4-3:active ~ .crossword-clues .crossword-clues__list-item--across-10,
#item4-3:focus ~ .crossword-clues .crossword-clues__list-item--across-10,
#item4-3:hover ~ .crossword-clues .crossword-clues__list-item--across-10 {
  background: #FFFF74;
}

#item4-4:active ~ .crossword-clues .crossword-clues__list-item--across-10,
#item4-4:focus ~ .crossword-clues .crossword-clues__list-item--across-10,
#item4-4:hover ~ .crossword-clues .crossword-clues__list-item--across-10 {
  background: #FFFF74;
}

#item4-5:active ~ .crossword-clues .crossword-clues__list-item--across-10,
#item4-5:focus ~ .crossword-clues .crossword-clues__list-item--across-10,
#item4-5:hover ~ .crossword-clues .crossword-clues__list-item--across-10 {
  background: #FFFF74;
}

#item4-6:active ~ .crossword-clues .crossword-clues__list-item--across-10,
#item4-6:focus ~ .crossword-clues .crossword-clues__list-item--across-10,
#item4-6:hover ~ .crossword-clues .crossword-clues__list-item--across-10 {
  background: #FFFF74;
}

#item4-8:active ~ .crossword-clues .crossword-clues__list-item--across-11,
#item4-8:focus ~ .crossword-clues .crossword-clues__list-item--across-11,
#item4-8:hover ~ .crossword-clues .crossword-clues__list-item--across-11 {
  background: #FFFF74;
}

#item4-9:active ~ .crossword-clues .crossword-clues__list-item--across-11,
#item4-9:focus ~ .crossword-clues .crossword-clues__list-item--across-11,
#item4-9:hover ~ .crossword-clues .crossword-clues__list-item--across-11 {
  background: #FFFF74;
}

#item4-10:active ~ .crossword-clues .crossword-clues__list-item--across-11,
#item4-10:focus ~ .crossword-clues .crossword-clues__list-item--across-11,
#item4-10:hover ~ .crossword-clues .crossword-clues__list-item--across-11 {
  background: #FFFF74;
}

#item4-11:active ~ .crossword-clues .crossword-clues__list-item--across-11,
#item4-11:focus ~ .crossword-clues .crossword-clues__list-item--across-11,
#item4-11:hover ~ .crossword-clues .crossword-clues__list-item--across-11 {
  background: #FFFF74;
}

#item4-12:active ~ .crossword-clues .crossword-clues__list-item--across-11,
#item4-12:focus ~ .crossword-clues .crossword-clues__list-item--across-11,
#item4-12:hover ~ .crossword-clues .crossword-clues__list-item--across-11 {
  background: #FFFF74;
}

#item4-13:active ~ .crossword-clues .crossword-clues__list-item--across-11,
#item4-13:focus ~ .crossword-clues .crossword-clues__list-item--across-11,
#item4-13:hover ~ .crossword-clues .crossword-clues__list-item--across-11 {
  background: #FFFF74;
}

#item5-3:active ~ .crossword-clues .crossword-clues__list-item--across-14,
#item5-3:focus ~ .crossword-clues .crossword-clues__list-item--across-14,
#item5-3:hover ~ .crossword-clues .crossword-clues__list-item--across-14 {
  background: #FFFF74;
}

#item5-4:active ~ .crossword-clues .crossword-clues__list-item--across-14,
#item5-4:focus ~ .crossword-clues .crossword-clues__list-item--across-14,
#item5-4:hover ~ .crossword-clues .crossword-clues__list-item--across-14 {
  background: #FFFF74;
}

#item5-5:active ~ .crossword-clues .crossword-clues__list-item--across-14,
#item5-5:focus ~ .crossword-clues .crossword-clues__list-item--across-14,
#item5-5:hover ~ .crossword-clues .crossword-clues__list-item--across-14 {
  background: #FFFF74;
}

#item5-9:active ~ .crossword-clues .crossword-clues__list-item--across-16,
#item5-9:focus ~ .crossword-clues .crossword-clues__list-item--across-16,
#item5-9:hover ~ .crossword-clues .crossword-clues__list-item--across-16 {
  background: #FFFF74;
}

#item5-10:active ~ .crossword-clues .crossword-clues__list-item--across-16,
#item5-10:focus ~ .crossword-clues .crossword-clues__list-item--across-16,
#item5-10:hover ~ .crossword-clues .crossword-clues__list-item--across-16 {
  background: #FFFF74;
}

#item5-10:active ~ .crossword-clues .crossword-clues__list-item--across-16,
#item5-10:focus ~ .crossword-clues .crossword-clues__list-item--across-16,
#item5-10:hover ~ .crossword-clues .crossword-clues__list-item--across-16 {
  background: #FFFF74;
}

#item5-11:active ~ .crossword-clues .crossword-clues__list-item--across-16,
#item5-11:focus ~ .crossword-clues .crossword-clues__list-item--across-16,
#item5-11:hover ~ .crossword-clues .crossword-clues__list-item--across-16 {
  background: #FFFF74;
}

#item5-12:active ~ .crossword-clues .crossword-clues__list-item--across-16,
#item5-12:focus ~ .crossword-clues .crossword-clues__list-item--across-16,
#item5-12:hover ~ .crossword-clues .crossword-clues__list-item--across-16 {
  background: #FFFF74;
}

#item5-13:active ~ .crossword-clues .crossword-clues__list-item--across-16,
#item5-13:focus ~ .crossword-clues .crossword-clues__list-item--across-16,
#item5-13:hover ~ .crossword-clues .crossword-clues__list-item--across-16 {
  background: #FFFF74;
}

#item6-1:active ~ .crossword-clues .crossword-clues__list-item--across-17,
#item6-1:focus ~ .crossword-clues .crossword-clues__list-item--across-17,
#item6-1:hover ~ .crossword-clues .crossword-clues__list-item--across-17 {
  background: #FFFF74;
}

#item6-2:active ~ .crossword-clues .crossword-clues__list-item--across-17,
#item6-2:focus ~ .crossword-clues .crossword-clues__list-item--across-17,
#item6-2:hover ~ .crossword-clues .crossword-clues__list-item--across-17 {
  background: #FFFF74;
}

#item6-3:active ~ .crossword-clues .crossword-clues__list-item--across-17,
#item6-3:focus ~ .crossword-clues .crossword-clues__list-item--across-17,
#item6-3:hover ~ .crossword-clues .crossword-clues__list-item--across-17 {
  background: #FFFF74;
}

#item6-4:active ~ .crossword-clues .crossword-clues__list-item--across-17,
#item6-4:focus ~ .crossword-clues .crossword-clues__list-item--across-17,
#item6-4:hover ~ .crossword-clues .crossword-clues__list-item--across-17 {
  background: #FFFF74;
}

#item6-6:active ~ .crossword-clues .crossword-clues__list-item--across-19,
#item6-6:focus ~ .crossword-clues .crossword-clues__list-item--across-19,
#item6-6:hover ~ .crossword-clues .crossword-clues__list-item--across-19 {
  background: #FFFF74;
}

#item6-7:active ~ .crossword-clues .crossword-clues__list-item--across-19,
#item6-7:focus ~ .crossword-clues .crossword-clues__list-item--across-19,
#item6-7:hover ~ .crossword-clues .crossword-clues__list-item--across-19 {
  background: #FFFF74;
}

#item6-8:active ~ .crossword-clues .crossword-clues__list-item--across-19,
#item6-8:focus ~ .crossword-clues .crossword-clues__list-item--across-19,
#item6-8:hover ~ .crossword-clues .crossword-clues__list-item--across-19 {
  background: #FFFF74;
}

#item6-9:active ~ .crossword-clues .crossword-clues__list-item--across-19,
#item6-9:focus ~ .crossword-clues .crossword-clues__list-item--across-19,
#item6-9:hover ~ .crossword-clues .crossword-clues__list-item--across-19 {
  background: #FFFF74;
}

#item6-10:active ~ .crossword-clues .crossword-clues__list-item--across-19,
#item6-10:focus ~ .crossword-clues .crossword-clues__list-item--across-19,
#item6-10:hover ~ .crossword-clues .crossword-clues__list-item--across-19 {
  background: #FFFF74;
}

#item7-5:active ~ .crossword-clues .crossword-clues__list-item--across-21,
#item7-5:focus ~ .crossword-clues .crossword-clues__list-item--across-21,
#item7-5:hover ~ .crossword-clues .crossword-clues__list-item--across-21 {
  background: #FFFF74;
}

#item7-6:active ~ .crossword-clues .crossword-clues__list-item--across-21,
#item7-6:focus ~ .crossword-clues .crossword-clues__list-item--across-21,
#item7-6:hover ~ .crossword-clues .crossword-clues__list-item--across-21 {
  background: #FFFF74;
}

#item7-7:active ~ .crossword-clues .crossword-clues__list-item--across-21,
#item7-7:focus ~ .crossword-clues .crossword-clues__list-item--across-21,
#item7-7:hover ~ .crossword-clues .crossword-clues__list-item--across-21 {
  background: #FFFF74;
}

#item7-8:active ~ .crossword-clues .crossword-clues__list-item--across-21,
#item7-8:focus ~ .crossword-clues .crossword-clues__list-item--across-21,
#item7-8:hover ~ .crossword-clues .crossword-clues__list-item--across-21 {
  background: #FFFF74;
}

#item7-9:active ~ .crossword-clues .crossword-clues__list-item--across-21,
#item7-9:focus ~ .crossword-clues .crossword-clues__list-item--across-21,
#item7-9:hover ~ .crossword-clues .crossword-clues__list-item--across-21 {
  background: #FFFF74;
}

#item8-4:active ~ .crossword-clues .crossword-clues__list-item--across-22,
#item8-4:focus ~ .crossword-clues .crossword-clues__list-item--across-22,
#item8-4:hover ~ .crossword-clues .crossword-clues__list-item--across-22 {
  background: #FFFF74;
}

#item8-5:active ~ .crossword-clues .crossword-clues__list-item--across-22,
#item8-5:focus ~ .crossword-clues .crossword-clues__list-item--across-22,
#item8-5:hover ~ .crossword-clues .crossword-clues__list-item--across-22 {
  background: #FFFF74;
}

#item8-6:active ~ .crossword-clues .crossword-clues__list-item--across-22,
#item8-6:focus ~ .crossword-clues .crossword-clues__list-item--across-22,
#item8-6:hover ~ .crossword-clues .crossword-clues__list-item--across-22 {
  background: #FFFF74;
}

#item8-7:active ~ .crossword-clues .crossword-clues__list-item--across-22,
#item8-7:focus ~ .crossword-clues .crossword-clues__list-item--across-22,
#item8-7:hover ~ .crossword-clues .crossword-clues__list-item--across-22 {
  background: #FFFF74;
}

#item8-8:active ~ .crossword-clues .crossword-clues__list-item--across-22,
#item8-8:focus ~ .crossword-clues .crossword-clues__list-item--across-22,
#item8-8:hover ~ .crossword-clues .crossword-clues__list-item--across-22 {
  background: #FFFF74;
}

#item8-10:active ~ .crossword-clues .crossword-clues__list-item--across-23,
#item8-10:focus ~ .crossword-clues .crossword-clues__list-item--across-23,
#item8-10:hover ~ .crossword-clues .crossword-clues__list-item--across-23 {
  background: #FFFF74;
}

#item8-11:active ~ .crossword-clues .crossword-clues__list-item--across-23,
#item8-11:focus ~ .crossword-clues .crossword-clues__list-item--across-23,
#item8-11:hover ~ .crossword-clues .crossword-clues__list-item--across-23 {
  background: #FFFF74;
}

#item8-12:active ~ .crossword-clues .crossword-clues__list-item--across-23,
#item8-12:focus ~ .crossword-clues .crossword-clues__list-item--across-23,
#item8-12:hover ~ .crossword-clues .crossword-clues__list-item--across-23 {
  background: #FFFF74;
}

#item8-13:active ~ .crossword-clues .crossword-clues__list-item--across-23,
#item8-13:focus ~ .crossword-clues .crossword-clues__list-item--across-23,
#item8-13:hover ~ .crossword-clues .crossword-clues__list-item--across-23 {
  background: #FFFF74;
}

#item9-1:active ~ .crossword-clues .crossword-clues__list-item--across-26,
#item9-1:focus ~ .crossword-clues .crossword-clues__list-item--across-26,
#item9-1:hover ~ .crossword-clues .crossword-clues__list-item--across-26 {
  background: #FFFF74;
}

#item9-2:active ~ .crossword-clues .crossword-clues__list-item--across-26,
#item9-2:focus ~ .crossword-clues .crossword-clues__list-item--across-26,
#item9-2:hover ~ .crossword-clues .crossword-clues__list-item--across-26 {
  background: #FFFF74;
}

#item9-3:active ~ .crossword-clues .crossword-clues__list-item--across-26,
#item9-3:focus ~ .crossword-clues .crossword-clues__list-item--across-26,
#item9-3:hover ~ .crossword-clues .crossword-clues__list-item--across-26 {
  background: #FFFF74;
}

#item9-4:active ~ .crossword-clues .crossword-clues__list-item--across-26,
#item9-4:focus ~ .crossword-clues .crossword-clues__list-item--across-26,
#item9-4:hover ~ .crossword-clues .crossword-clues__list-item--across-26 {
  background: #FFFF74;
}

#item9-5:active ~ .crossword-clues .crossword-clues__list-item--across-26,
#item9-5:focus ~ .crossword-clues .crossword-clues__list-item--across-26,
#item9-5:hover ~ .crossword-clues .crossword-clues__list-item--across-26 {
  background: #FFFF74;
}

#item9-9:active ~ .crossword-clues .crossword-clues__list-item--across-28,
#item9-9:focus ~ .crossword-clues .crossword-clues__list-item--across-28,
#item9-9:hover ~ .crossword-clues .crossword-clues__list-item--across-28 {
  background: #FFFF74;
}

#item9-10:active ~ .crossword-clues .crossword-clues__list-item--across-28,
#item9-10:focus ~ .crossword-clues .crossword-clues__list-item--across-28,
#item9-10:hover ~ .crossword-clues .crossword-clues__list-item--across-28 {
  background: #FFFF74;
}

#item9-11:active ~ .crossword-clues .crossword-clues__list-item--across-28,
#item9-11:focus ~ .crossword-clues .crossword-clues__list-item--across-28,
#item9-11:hover ~ .crossword-clues .crossword-clues__list-item--across-28 {
  background: #FFFF74;
}

#item10-1:active ~ .crossword-clues .crossword-clues__list-item--across-29,
#item10-1:focus ~ .crossword-clues .crossword-clues__list-item--across-29,
#item10-1:hover ~ .crossword-clues .crossword-clues__list-item--across-29 {
  background: #FFFF74;
}

#item10-2:active ~ .crossword-clues .crossword-clues__list-item--across-29,
#item10-2:focus ~ .crossword-clues .crossword-clues__list-item--across-29,
#item10-2:hover ~ .crossword-clues .crossword-clues__list-item--across-29 {
  background: #FFFF74;
}

#item10-3:active ~ .crossword-clues .crossword-clues__list-item--across-29,
#item10-3:focus ~ .crossword-clues .crossword-clues__list-item--across-29,
#item10-3:hover ~ .crossword-clues .crossword-clues__list-item--across-29 {
  background: #FFFF74;
}

#item10-4:active ~ .crossword-clues .crossword-clues__list-item--across-29,
#item10-4:focus ~ .crossword-clues .crossword-clues__list-item--across-29,
#item10-4:hover ~ .crossword-clues .crossword-clues__list-item--across-29 {
  background: #FFFF74;
}

#item10-5:active ~ .crossword-clues .crossword-clues__list-item--across-29,
#item10-5:focus ~ .crossword-clues .crossword-clues__list-item--across-29,
#item10-5:hover ~ .crossword-clues .crossword-clues__list-item--across-29 {
  background: #FFFF74;
}

#item10-6:active ~ .crossword-clues .crossword-clues__list-item--across-29,
#item10-6:focus ~ .crossword-clues .crossword-clues__list-item--across-29,
#item10-6:hover ~ .crossword-clues .crossword-clues__list-item--across-29 {
  background: #FFFF74;
}

#item10-8:active ~ .crossword-clues .crossword-clues__list-item--across-30,
#item10-8:focus ~ .crossword-clues .crossword-clues__list-item--across-30,
#item10-8:hover ~ .crossword-clues .crossword-clues__list-item--across-30 {
  background: #FFFF74;
}

#item10-9:active ~ .crossword-clues .crossword-clues__list-item--across-30,
#item10-9:focus ~ .crossword-clues .crossword-clues__list-item--across-30,
#item10-9:hover ~ .crossword-clues .crossword-clues__list-item--across-30 {
  background: #FFFF74;
}

#item10-10:active ~ .crossword-clues .crossword-clues__list-item--across-30,
#item10-10:focus ~ .crossword-clues .crossword-clues__list-item--across-30,
#item10-10:hover ~ .crossword-clues .crossword-clues__list-item--across-30 {
  background: #FFFF74;
}

#item10-11:active ~ .crossword-clues .crossword-clues__list-item--across-30,
#item10-11:focus ~ .crossword-clues .crossword-clues__list-item--across-30,
#item10-11:hover ~ .crossword-clues .crossword-clues__list-item--across-30 {
  background: #FFFF74;
}

#item10-12:active ~ .crossword-clues .crossword-clues__list-item--across-30,
#item10-12:focus ~ .crossword-clues .crossword-clues__list-item--across-30,
#item10-12:hover ~ .crossword-clues .crossword-clues__list-item--across-30 {
  background: #FFFF74;
}

#item10-13:active ~ .crossword-clues .crossword-clues__list-item--across-30,
#item10-13:focus ~ .crossword-clues .crossword-clues__list-item--across-30,
#item10-13:hover ~ .crossword-clues .crossword-clues__list-item--across-30 {
  background: #FFFF74;
}

#item11-8:active ~ .crossword-clues .crossword-clues__list-item--across-31,
#item11-8:focus ~ .crossword-clues .crossword-clues__list-item--across-31,
#item11-8:hover ~ .crossword-clues .crossword-clues__list-item--across-31 {
  background: #FFFF74;
}

#item11-9:active ~ .crossword-clues .crossword-clues__list-item--across-31,
#item11-9:focus ~ .crossword-clues .crossword-clues__list-item--across-31,
#item11-9:hover ~ .crossword-clues .crossword-clues__list-item--across-31 {
  background: #FFFF74;
}

#item11-10:active ~ .crossword-clues .crossword-clues__list-item--across-31,
#item11-10:focus ~ .crossword-clues .crossword-clues__list-item--across-31,
#item11-10:hover ~ .crossword-clues .crossword-clues__list-item--across-31 {
  background: #FFFF74;
}

#item11-11:active ~ .crossword-clues .crossword-clues__list-item--across-31,
#item11-11:focus ~ .crossword-clues .crossword-clues__list-item--across-31,
#item11-11:hover ~ .crossword-clues .crossword-clues__list-item--across-31 {
  background: #FFFF74;
}

#item12-1:active ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-1:focus ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-1:hover ~ .crossword-clues .crossword-clues__list-item--across-32 {
  background: #FFFF74;
}

#item12-2:active ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-2:focus ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-2:hover ~ .crossword-clues .crossword-clues__list-item--across-32 {
  background: #FFFF74;
}

#item12-3:active ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-3:focus ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-3:hover ~ .crossword-clues .crossword-clues__list-item--across-32 {
  background: #FFFF74;
}

#item12-4:active ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-4:focus ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-4:hover ~ .crossword-clues .crossword-clues__list-item--across-32 {
  background: #FFFF74;
}

#item12-5:active ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-5:focus ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-5:hover ~ .crossword-clues .crossword-clues__list-item--across-32 {
  background: #FFFF74;
}

#item12-6:active ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-6:focus ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-6:hover ~ .crossword-clues .crossword-clues__list-item--across-32 {
  background: #FFFF74;
}

#item12-7:active ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-7:focus ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-7:hover ~ .crossword-clues .crossword-clues__list-item--across-32 {
  background: #FFFF74;
}

#item12-8:active ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-8:focus ~ .crossword-clues .crossword-clues__list-item--across-32,
#item12-8:hover ~ .crossword-clues .crossword-clues__list-item--across-32 {
  background: #FFFF74;
}

#item13-8:active ~ .crossword-clues .crossword-clues__list-item--across-33,
#item13-8:focus ~ .crossword-clues .crossword-clues__list-item--across-33,
#item13-8:hover ~ .crossword-clues .crossword-clues__list-item--across-33 {
  background: #FFFF74;
}

#item13-9:active ~ .crossword-clues .crossword-clues__list-item--across-33,
#item13-9:focus ~ .crossword-clues .crossword-clues__list-item--across-33,
#item13-9:hover ~ .crossword-clues .crossword-clues__list-item--across-33 {
  background: #FFFF74;
}

#item13-10:active ~ .crossword-clues .crossword-clues__list-item--across-33,
#item13-10:focus ~ .crossword-clues .crossword-clues__list-item--across-33,
#item13-10:hover ~ .crossword-clues .crossword-clues__list-item--across-33 {
  background: #FFFF74;
}

#item13-11:active ~ .crossword-clues .crossword-clues__list-item--across-33,
#item13-11:focus ~ .crossword-clues .crossword-clues__list-item--across-33,
#item13-11:hover ~ .crossword-clues .crossword-clues__list-item--across-33 {
  background: #FFFF74;
}

#item13-12:active ~ .crossword-clues .crossword-clues__list-item--across-33,
#item13-12:focus ~ .crossword-clues .crossword-clues__list-item--across-33,
#item13-12:hover ~ .crossword-clues .crossword-clues__list-item--across-33 {
  background: #FFFF74;
}

#item13-13:active ~ .crossword-clues .crossword-clues__list-item--across-33,
#item13-13:focus ~ .crossword-clues .crossword-clues__list-item--across-33,
#item13-13:hover ~ .crossword-clues .crossword-clues__list-item--across-33 {
  background: #FFFF74;
}

#item1-1:active ~ .crossword-clues .crossword-clues__list-item--down-1,
#item1-1:focus ~ .crossword-clues .crossword-clues__list-item--down-1,
#item1-1:hover ~ .crossword-clues .crossword-clues__list-item--down-1 {
  background: #FFFF74;
}

#item2-1:active ~ .crossword-clues .crossword-clues__list-item--down-1,
#item2-1:focus ~ .crossword-clues .crossword-clues__list-item--down-1,
#item2-1:hover ~ .crossword-clues .crossword-clues__list-item--down-1 {
  background: #FFFF74;
}

#item3-1:active ~ .crossword-clues .crossword-clues__list-item--down-1,
#item3-1:focus ~ .crossword-clues .crossword-clues__list-item--down-1,
#item3-1:hover ~ .crossword-clues .crossword-clues__list-item--down-1 {
  background: #FFFF74;
}

#item4-1:active ~ .crossword-clues .crossword-clues__list-item--down-1,
#item4-1:focus ~ .crossword-clues .crossword-clues__list-item--down-1,
#item4-1:hover ~ .crossword-clues .crossword-clues__list-item--down-1 {
  background: #FFFF74;
}

#item5-1:active ~ .crossword-clues .crossword-clues__list-item--down-1,
#item5-1:focus ~ .crossword-clues .crossword-clues__list-item--down-1,
#item5-1:hover ~ .crossword-clues .crossword-clues__list-item--down-1 {
  background: #FFFF74;
}

#item6-1:active ~ .crossword-clues .crossword-clues__list-item--down-1,
#item6-1:focus ~ .crossword-clues .crossword-clues__list-item--down-1,
#item6-1:hover ~ .crossword-clues .crossword-clues__list-item--down-1 {
  background: #FFFF74;
}

#item1-4:active ~ .crossword-clues .crossword-clues__list-item--down-2,
#item1-4:focus ~ .crossword-clues .crossword-clues__list-item--down-2,
#item1-4:hover ~ .crossword-clues .crossword-clues__list-item--down-2 {
  background: #FFFF74;
}

#item2-4:active ~ .crossword-clues .crossword-clues__list-item--down-2,
#item2-2:focus ~ .crossword-clues .crossword-clues__list-item--down-2,
#item2-4:hover ~ .crossword-clues .crossword-clues__list-item--down-2 {
  background: #FFFF74;
}

#item3-4:active ~ .crossword-clues .crossword-clues__list-item--down-2,
#item3-4:focus ~ .crossword-clues .crossword-clues__list-item--down-2,
#item3-4:hover ~ .crossword-clues .crossword-clues__list-item--down-2 {
  background: #FFFF74;
}

#item4-4:active ~ .crossword-clues .crossword-clues__list-item--down-2,
#item4-4:focus ~ .crossword-clues .crossword-clues__list-item--down-2,
#item4-4:hover ~ .crossword-clues .crossword-clues__list-item--down-2 {
  background: #FFFF74;
}

#item5-4:active ~ .crossword-clues .crossword-clues__list-item--down-2,
#item5-4:focus ~ .crossword-clues .crossword-clues__list-item--down-2,
#item5-4:hover ~ .crossword-clues .crossword-clues__list-item--down-2 {
  background: #FFFF74;
}

#item6-4:active ~ .crossword-clues .crossword-clues__list-item--down-2,
#item6-4:focus ~ .crossword-clues .crossword-clues__list-item--down-2,
#item6-4:hover ~ .crossword-clues .crossword-clues__list-item--down-2 {
  background: #FFFF74;
}

#item1-6:active ~ .crossword-clues .crossword-clues__list-item--down-3,
#item1-6:focus ~ .crossword-clues .crossword-clues__list-item--down-3,
#item1-6:hover ~ .crossword-clues .crossword-clues__list-item--down-3 {
  background: #FFFF74;
}

#item2-6:active ~ .crossword-clues .crossword-clues__list-item--down-3,
#item2-6:focus ~ .crossword-clues .crossword-clues__list-item--down-3,
#item2-6:hover ~ .crossword-clues .crossword-clues__list-item--down-3 {
  background: #FFFF74;
}

#item3-6:active ~ .crossword-clues .crossword-clues__list-item--down-3,
#item3-6:focus ~ .crossword-clues .crossword-clues__list-item--down-3,
#item3-6:hover ~ .crossword-clues .crossword-clues__list-item--down-3 {
  background: #FFFF74;
}

#item4-6:active ~ .crossword-clues .crossword-clues__list-item--down-3,
#item4-6:focus ~ .crossword-clues .crossword-clues__list-item--down-3,
#item4-6:hover ~ .crossword-clues .crossword-clues__list-item--down-3 {
  background: #FFFF74;
}

#item1-9:active ~ .crossword-clues .crossword-clues__list-item--down-4,
#item1-9:focus ~ .crossword-clues .crossword-clues__list-item--down-4,
#item1-9:hover ~ .crossword-clues .crossword-clues__list-item--down-4 {
  background: #FFFF74;
}

#item2-9:active ~ .crossword-clues .crossword-clues__list-item--down-4,
#item2-9:focus ~ .crossword-clues .crossword-clues__list-item--down-4,
#item2-9:hover ~ .crossword-clues .crossword-clues__list-item--down-4 {
  background: #FFFF74;
}

#item3-9:active ~ .crossword-clues .crossword-clues__list-item--down-4,
#item3-9:focus ~ .crossword-clues .crossword-clues__list-item--down-4,
#item3-9:hover ~ .crossword-clues .crossword-clues__list-item--down-4 {
  background: #FFFF74;
}

#item4-9:active ~ .crossword-clues .crossword-clues__list-item--down-4,
#item4-9:focus ~ .crossword-clues .crossword-clues__list-item--down-4,
#item4-9:hover ~ .crossword-clues .crossword-clues__list-item--down-4 {
  background: #FFFF74;
}

#item5-9:active ~ .crossword-clues .crossword-clues__list-item--down-4,
#item5-9:focus ~ .crossword-clues .crossword-clues__list-item--down-4,
#item5-9:hover ~ .crossword-clues .crossword-clues__list-item--down-4 {
  background: #FFFF74;
}

#item6-9:active ~ .crossword-clues .crossword-clues__list-item--down-4,
#item6-9:focus ~ .crossword-clues .crossword-clues__list-item--down-4,
#item6-9:hover ~ .crossword-clues .crossword-clues__list-item--down-4 {
  background: #FFFF74;
}

#item7-9:active ~ .crossword-clues .crossword-clues__list-item--down-4,
#item7-9:focus ~ .crossword-clues .crossword-clues__list-item--down-4,
#item7-9:hover ~ .crossword-clues .crossword-clues__list-item--down-4 {
  background: #FFFF74;
}

#item1-11:active ~ .crossword-clues .crossword-clues__list-item--down-5,
#item1-11:focus ~ .crossword-clues .crossword-clues__list-item--down-5,
#item1-11:hover ~ .crossword-clues .crossword-clues__list-item--down-5 {
  background: #FFFF74;
}

#item2-11:active ~ .crossword-clues .crossword-clues__list-item--down-5,
#item2-11:focus ~ .crossword-clues .crossword-clues__list-item--down-5,
#item2-11:hover ~ .crossword-clues .crossword-clues__list-item--down-5 {
  background: #FFFF74;
}

#item3-11:active ~ .crossword-clues .crossword-clues__list-item--down-5,
#item3-11:focus ~ .crossword-clues .crossword-clues__list-item--down-5,
#item3-11:hover ~ .crossword-clues .crossword-clues__list-item--down-5 {
  background: #FFFF74;
}

#item4-11:active ~ .crossword-clues .crossword-clues__list-item--down-5,
#item4-11:focus ~ .crossword-clues .crossword-clues__list-item--down-5,
#item4-11:hover ~ .crossword-clues .crossword-clues__list-item--down-5 {
  background: #FFFF74;
}

#item5-11:active ~ .crossword-clues .crossword-clues__list-item--down-5,
#item5-11:focus ~ .crossword-clues .crossword-clues__list-item--down-5,
#item5-11:hover ~ .crossword-clues .crossword-clues__list-item--down-5 {
  background: #FFFF74;
}

#item1-13:active ~ .crossword-clues .crossword-clues__list-item--down-6,
#item1-13:focus ~ .crossword-clues .crossword-clues__list-item--down-6,
#item1-13:hover ~ .crossword-clues .crossword-clues__list-item--down-6 {
  background: #FFFF74;
}

#item2-13:active ~ .crossword-clues .crossword-clues__list-item--down-6,
#item2-13:focus ~ .crossword-clues .crossword-clues__list-item--down-6,
#item2-13:hover ~ .crossword-clues .crossword-clues__list-item--down-6 {
  background: #FFFF74;
}

#item3-13:active ~ .crossword-clues .crossword-clues__list-item--down-6,
#item3-13:focus ~ .crossword-clues .crossword-clues__list-item--down-6,
#item3-13:hover ~ .crossword-clues .crossword-clues__list-item--down-6 {
  background: #FFFF74;
}

#item4-13:active ~ .crossword-clues .crossword-clues__list-item--down-6,
#item4-13:focus ~ .crossword-clues .crossword-clues__list-item--down-6,
#item4-13:hover ~ .crossword-clues .crossword-clues__list-item--down-6 {
  background: #FFFF74;
}

#item5-13:active ~ .crossword-clues .crossword-clues__list-item--down-6,
#item5-13:focus ~ .crossword-clues .crossword-clues__list-item--down-6,
#item5-13:hover ~ .crossword-clues .crossword-clues__list-item--down-6 {
  background: #FFFF74;
}

#item3-3:active ~ .crossword-clues .crossword-clues__list-item--down-8,
#item3-3:focus ~ .crossword-clues .crossword-clues__list-item--down-8,
#item3-3:hover ~ .crossword-clues .crossword-clues__list-item--down-8 {
  background: #FFFF74;
}

#item4-3:active ~ .crossword-clues .crossword-clues__list-item--down-8,
#item4-3:focus ~ .crossword-clues .crossword-clues__list-item--down-8,
#item4-3:hover ~ .crossword-clues .crossword-clues__list-item--down-8 {
  background: #FFFF74;
}

#item5-3:active ~ .crossword-clues .crossword-clues__list-item--down-8,
#item5-3:focus ~ .crossword-clues .crossword-clues__list-item--down-8,
#item5-3:hover ~ .crossword-clues .crossword-clues__list-item--down-8 {
  background: #FFFF74;
}

#item6-3:active ~ .crossword-clues .crossword-clues__list-item--down-8,
#item6-3:focus ~ .crossword-clues .crossword-clues__list-item--down-8,
#item6-3:hover ~ .crossword-clues .crossword-clues__list-item--down-8 {
  background: #FFFF74;
}

#item3-5:active ~ .crossword-clues .crossword-clues__list-item--down-9,
#item3-5:focus ~ .crossword-clues .crossword-clues__list-item--down-9,
#item3-5:hover ~ .crossword-clues .crossword-clues__list-item--down-9 {
  background: #FFFF74;
}

#item4-5:active ~ .crossword-clues .crossword-clues__list-item--down-9,
#item4-5:focus ~ .crossword-clues .crossword-clues__list-item--down-9,
#item4-5:hover ~ .crossword-clues .crossword-clues__list-item--down-9 {
  background: #FFFF74;
}

#item5-5:active ~ .crossword-clues .crossword-clues__list-item--down-9,
#item5-5:focus ~ .crossword-clues .crossword-clues__list-item--down-9,
#item5-5:hover ~ .crossword-clues .crossword-clues__list-item--down-9 {
  background: #FFFF74;
}

#item4-10:active ~ .crossword-clues .crossword-clues__list-item--down-12,
#item4-10:focus ~ .crossword-clues .crossword-clues__list-item--down-12,
#item4-10:hover ~ .crossword-clues .crossword-clues__list-item--down-12 {
  background: #FFFF74;
}

#item5-10:active ~ .crossword-clues .crossword-clues__list-item--down-12,
#item5-10:focus ~ .crossword-clues .crossword-clues__list-item--down-12,
#item5-10:hover ~ .crossword-clues .crossword-clues__list-item--down-12 {
  background: #FFFF74;
}

#item6-10:active ~ .crossword-clues .crossword-clues__list-item--down-12,
#item6-10:focus ~ .crossword-clues .crossword-clues__list-item--down-12,
#item6-10:hover ~ .crossword-clues .crossword-clues__list-item--down-12 {
  background: #FFFF74;
}

#item4-12:active ~ .crossword-clues .crossword-clues__list-item--down-13,
#item4-12:focus ~ .crossword-clues .crossword-clues__list-item--down-13,
#item4-12:hover ~ .crossword-clues .crossword-clues__list-item--down-13 {
  background: #FFFF74;
}

#item5-12:active ~ .crossword-clues .crossword-clues__list-item--down-13,
#item5-12:focus ~ .crossword-clues .crossword-clues__list-item--down-13,
#item5-12:hover ~ .crossword-clues .crossword-clues__list-item--down-13 {
  background: #FFFF74;
}

#item6-12:active ~ .crossword-clues .crossword-clues__list-item--down-13,
#item6-12:focus ~ .crossword-clues .crossword-clues__list-item--down-13,
#item6-12:hover ~ .crossword-clues .crossword-clues__list-item--down-13 {
  background: #FFFF74;
}

#item7-12:active ~ .crossword-clues .crossword-clues__list-item--down-13,
#item7-12:focus ~ .crossword-clues .crossword-clues__list-item--down-13,
#item7-12:hover ~ .crossword-clues .crossword-clues__list-item--down-13 {
  background: #FFFF74;
}

#item8-12:active ~ .crossword-clues .crossword-clues__list-item--down-13,
#item8-12:focus ~ .crossword-clues .crossword-clues__list-item--down-13,
#item8-12:hover ~ .crossword-clues .crossword-clues__list-item--down-13 {
  background: #FFFF74;
}

#item5-7:active ~ .crossword-clues .crossword-clues__list-item--down-15,
#item5-7:focus ~ .crossword-clues .crossword-clues__list-item--down-15,
#item5-7:hover ~ .crossword-clues .crossword-clues__list-item--down-15 {
  background: #FFFF74;
}

#item6-7:active ~ .crossword-clues .crossword-clues__list-item--down-15,
#item6-7:focus ~ .crossword-clues .crossword-clues__list-item--down-15,
#item6-7:hover ~ .crossword-clues .crossword-clues__list-item--down-15 {
  background: #FFFF74;
}

#item7-7:active ~ .crossword-clues .crossword-clues__list-item--down-15,
#item7-7:focus ~ .crossword-clues .crossword-clues__list-item--down-15,
#item7-7:hover ~ .crossword-clues .crossword-clues__list-item--down-15 {
  background: #FFFF74;
}

#item8-7:active ~ .crossword-clues .crossword-clues__list-item--down-15,
#item8-7:focus ~ .crossword-clues .crossword-clues__list-item--down-15,
#item8-7:hover ~ .crossword-clues .crossword-clues__list-item--down-15 {
  background: #FFFF74;
}

#item9-7:active ~ .crossword-clues .crossword-clues__list-item--down-15,
#item9-7:focus ~ .crossword-clues .crossword-clues__list-item--down-15,
#item9-7:hover ~ .crossword-clues .crossword-clues__list-item--down-15 {
  background: #FFFF74;
}

#item6-2:active ~ .crossword-clues .crossword-clues__list-item--down-18,
#item6-2:focus ~ .crossword-clues .crossword-clues__list-item--down-18,
#item6-2:hover ~ .crossword-clues .crossword-clues__list-item--down-18 {
  background: #FFFF74;
}

#item7-2:active ~ .crossword-clues .crossword-clues__list-item--down-18,
#item7-2:focus ~ .crossword-clues .crossword-clues__list-item--down-18,
#item7-2:hover ~ .crossword-clues .crossword-clues__list-item--down-18 {
  background: #FFFF74;
}

#item8-2:active ~ .crossword-clues .crossword-clues__list-item--down-18,
#item8-2:focus ~ .crossword-clues .crossword-clues__list-item--down-18,
#item8-2:hover ~ .crossword-clues .crossword-clues__list-item--down-18 {
  background: #FFFF74;
}

#item9-2:active ~ .crossword-clues .crossword-clues__list-item--down-18,
#item9-2:focus ~ .crossword-clues .crossword-clues__list-item--down-18,
#item9-2:hover ~ .crossword-clues .crossword-clues__list-item--down-18 {
  background: #FFFF74;
}

#item10-2:active ~ .crossword-clues .crossword-clues__list-item--down-18,
#item10-2:focus ~ .crossword-clues .crossword-clues__list-item--down-18,
#item10-2:hover ~ .crossword-clues .crossword-clues__list-item--down-18 {
  background: #FFFF74;
}

#item6-6:active ~ .crossword-clues .crossword-clues__list-item--down-19,
#item6-6:focus ~ .crossword-clues .crossword-clues__list-item--down-19,
#item6-6:hover ~ .crossword-clues .crossword-clues__list-item--down-19 {
  background: #FFFF74;
}

#item7-6:active ~ .crossword-clues .crossword-clues__list-item--down-19,
#item7-6:focus ~ .crossword-clues .crossword-clues__list-item--down-19,
#item7-6:hover ~ .crossword-clues .crossword-clues__list-item--down-19 {
  background: #FFFF74;
}

#item7-6:active ~ .crossword-clues .crossword-clues__list-item--down-19,
#item7-6:focus ~ .crossword-clues .crossword-clues__list-item--down-19,
#item7-6:hover ~ .crossword-clues .crossword-clues__list-item--down-19 {
  background: #FFFF74;
}

#item6-8:active ~ .crossword-clues .crossword-clues__list-item--down-20,
#item6-8:focus ~ .crossword-clues .crossword-clues__list-item--down-20,
#item6-8:hover ~ .crossword-clues .crossword-clues__list-item--down-20 {
  background: #FFFF74;
}

#item7-8:active ~ .crossword-clues .crossword-clues__list-item--down-20,
#item7-8:focus ~ .crossword-clues .crossword-clues__list-item--down-20,
#item7-8:hover ~ .crossword-clues .crossword-clues__list-item--down-20 {
  background: #FFFF74;
}

#item8-8:active ~ .crossword-clues .crossword-clues__list-item--down-20,
#item8-8:focus ~ .crossword-clues .crossword-clues__list-item--down-20,
#item8-8:hover ~ .crossword-clues .crossword-clues__list-item--down-20 {
  background: #FFFF74;
}

#item7-5:active ~ .crossword-clues .crossword-clues__list-item--down-21,
#item7-5:focus ~ .crossword-clues .crossword-clues__list-item--down-21,
#item7-5:hover ~ .crossword-clues .crossword-clues__list-item--down-21 {
  background: #FFFF74;
}

#item8-5:active ~ .crossword-clues .crossword-clues__list-item--down-21,
#item8-5:focus ~ .crossword-clues .crossword-clues__list-item--down-21,
#item8-5:hover ~ .crossword-clues .crossword-clues__list-item--down-21 {
  background: #FFFF74;
}

#item9-5:active ~ .crossword-clues .crossword-clues__list-item--down-21,
#item9-5:focus ~ .crossword-clues .crossword-clues__list-item--down-21,
#item9-5:hover ~ .crossword-clues .crossword-clues__list-item--down-21 {
  background: #FFFF74;
}

#item10-5:active ~ .crossword-clues .crossword-clues__list-item--down-21,
#item10-5:focus ~ .crossword-clues .crossword-clues__list-item--down-21,
#item10-5:hover ~ .crossword-clues .crossword-clues__list-item--down-21 {
  background: #FFFF74;
}

#item11-5:active ~ .crossword-clues .crossword-clues__list-item--down-21,
#item11-5:focus ~ .crossword-clues .crossword-clues__list-item--down-21,
#item11-5:hover ~ .crossword-clues .crossword-clues__list-item--down-21 {
  background: #FFFF74;
}

#item12-5:active ~ .crossword-clues .crossword-clues__list-item--down-21,
#item12-5:focus ~ .crossword-clues .crossword-clues__list-item--down-21,
#item12-5:hover ~ .crossword-clues .crossword-clues__list-item--down-21 {
  background: #FFFF74;
}

#item13-5:active ~ .crossword-clues .crossword-clues__list-item--down-21,
#item13-5:focus ~ .crossword-clues .crossword-clues__list-item--down-21,
#item13-5:hover ~ .crossword-clues .crossword-clues__list-item--down-21 {
  background: #FFFF74;
}

#item8-4:active ~ .crossword-clues .crossword-clues__list-item--down-22,
#item8-4:focus ~ .crossword-clues .crossword-clues__list-item--down-22,
#item8-4:hover ~ .crossword-clues .crossword-clues__list-item--down-22 {
  background: #FFFF74;
}

#item9-4:active ~ .crossword-clues .crossword-clues__list-item--down-22,
#item9-4:focus ~ .crossword-clues .crossword-clues__list-item--down-22,
#item9-4:hover ~ .crossword-clues .crossword-clues__list-item--down-22 {
  background: #FFFF74;
}

#item10-4:active ~ .crossword-clues .crossword-clues__list-item--down-22,
#item10-4:focus ~ .crossword-clues .crossword-clues__list-item--down-22,
#item10-4:hover ~ .crossword-clues .crossword-clues__list-item--down-22 {
  background: #FFFF74;
}

#item8-10:active ~ .crossword-clues .crossword-clues__list-item--down-23,
#item8-10:focus ~ .crossword-clues .crossword-clues__list-item--down-23,
#item8-10:hover ~ .crossword-clues .crossword-clues__list-item--down-23 {
  background: #FFFF74;
}

#item9-10:active ~ .crossword-clues .crossword-clues__list-item--down-23,
#item9-10:focus ~ .crossword-clues .crossword-clues__list-item--down-23,
#item9-10:hover ~ .crossword-clues .crossword-clues__list-item--down-23 {
  background: #FFFF74;
}

#item10-10:active ~ .crossword-clues .crossword-clues__list-item--down-23,
#item10-10:focus ~ .crossword-clues .crossword-clues__list-item--down-23,
#item10-10:hover ~ .crossword-clues .crossword-clues__list-item--down-23 {
  background: #FFFF74;
}

#item11-10:active ~ .crossword-clues .crossword-clues__list-item--down-23,
#item11-10:focus ~ .crossword-clues .crossword-clues__list-item--down-23,
#item11-10:hover ~ .crossword-clues .crossword-clues__list-item--down-23 {
  background: #FFFF74;
}

#item12-10:active ~ .crossword-clues .crossword-clues__list-item--down-23,
#item12-10:focus ~ .crossword-clues .crossword-clues__list-item--down-23,
#item12-10:hover ~ .crossword-clues .crossword-clues__list-item--down-23 {
  background: #FFFF74;
}

#item13-10:active ~ .crossword-clues .crossword-clues__list-item--down-23,
#item13-10:focus ~ .crossword-clues .crossword-clues__list-item--down-23,
#item13-10:hover ~ .crossword-clues .crossword-clues__list-item--down-23 {
  background: #FFFF74;
}

#item8-11:active ~ .crossword-clues .crossword-clues__list-item--down-24,
#item8-11:focus ~ .crossword-clues .crossword-clues__list-item--down-24,
#item8-11:hover ~ .crossword-clues .crossword-clues__list-item--down-24 {
  background: #FFFF74;
}

#item9-11:active ~ .crossword-clues .crossword-clues__list-item--down-24,
#item9-11:focus ~ .crossword-clues .crossword-clues__list-item--down-24,
#item9-11:hover ~ .crossword-clues .crossword-clues__list-item--down-24 {
  background: #FFFF74;
}

#item10-11:active ~ .crossword-clues .crossword-clues__list-item--down-24,
#item10-11:focus ~ .crossword-clues .crossword-clues__list-item--down-24,
#item10-11:hover ~ .crossword-clues .crossword-clues__list-item--down-24 {
  background: #FFFF74;
}

#item11-11:active ~ .crossword-clues .crossword-clues__list-item--down-24,
#item11-11:focus ~ .crossword-clues .crossword-clues__list-item--down-24,
#item11-11:hover ~ .crossword-clues .crossword-clues__list-item--down-24 {
  background: #FFFF74;
}

#item8-13:active ~ .crossword-clues .crossword-clues__list-item--down-25,
#item8-13:focus ~ .crossword-clues .crossword-clues__list-item--down-25,
#item8-13:hover ~ .crossword-clues .crossword-clues__list-item--down-25 {
  background: #FFFF74;
}

#item9-13:active ~ .crossword-clues .crossword-clues__list-item--down-25,
#item9-13:focus ~ .crossword-clues .crossword-clues__list-item--down-25,
#item9-13:hover ~ .crossword-clues .crossword-clues__list-item--down-25 {
  background: #FFFF74;
}

#item10-13:active ~ .crossword-clues .crossword-clues__list-item--down-25,
#item10-13:focus ~ .crossword-clues .crossword-clues__list-item--down-25,
#item10-13:hover ~ .crossword-clues .crossword-clues__list-item--down-25 {
  background: #FFFF74;
}

#item11-13:active ~ .crossword-clues .crossword-clues__list-item--down-25,
#item11-13:focus ~ .crossword-clues .crossword-clues__list-item--down-25,
#item11-13:hover ~ .crossword-clues .crossword-clues__list-item--down-25 {
  background: #FFFF74;
}

#item12-13:active ~ .crossword-clues .crossword-clues__list-item--down-25,
#item12-13:focus ~ .crossword-clues .crossword-clues__list-item--down-25,
#item12-13:hover ~ .crossword-clues .crossword-clues__list-item--down-25 {
  background: #FFFF74;
}

#item13-13:active ~ .crossword-clues .crossword-clues__list-item--down-25,
#item13-13:focus ~ .crossword-clues .crossword-clues__list-item--down-25,
#item13-13:hover ~ .crossword-clues .crossword-clues__list-item--down-25 {
  background: #FFFF74;
}

#item9-1:active ~ .crossword-clues .crossword-clues__list-item--down-26,
#item9-1:focus ~ .crossword-clues .crossword-clues__list-item--down-26,
#item9-1:hover ~ .crossword-clues .crossword-clues__list-item--down-26 {
  background: #FFFF74;
}

#item10-1:active ~ .crossword-clues .crossword-clues__list-item--down-26,
#item10-1:focus ~ .crossword-clues .crossword-clues__list-item--down-26,
#item10-1:hover ~ .crossword-clues .crossword-clues__list-item--down-26 {
  background: #FFFF74;
}

#item11-1:active ~ .crossword-clues .crossword-clues__list-item--down-26,
#item11-1:focus ~ .crossword-clues .crossword-clues__list-item--down-26,
#item11-1:hover ~ .crossword-clues .crossword-clues__list-item--down-26 {
  background: #FFFF74;
}

#item12-1:active ~ .crossword-clues .crossword-clues__list-item--down-26,
#item12-1:focus ~ .crossword-clues .crossword-clues__list-item--down-26,
#item12-1:hover ~ .crossword-clues .crossword-clues__list-item--down-26 {
  background: #FFFF74;
}

#item13-1:active ~ .crossword-clues .crossword-clues__list-item--down-26,
#item13-1:focus ~ .crossword-clues .crossword-clues__list-item--down-26,
#item13-1:hover ~ .crossword-clues .crossword-clues__list-item--down-26 {
  background: #FFFF74;
}

#item9-3:active ~ .crossword-clues .crossword-clues__list-item--down-27,
#item9-3:focus ~ .crossword-clues .crossword-clues__list-item--down-27,
#item9-3:hover ~ .crossword-clues .crossword-clues__list-item--down-27 {
  background: #FFFF74;
}

#item10-3:active ~ .crossword-clues .crossword-clues__list-item--down-27,
#item10-3:focus ~ .crossword-clues .crossword-clues__list-item--down-27,
#item10-3:hover ~ .crossword-clues .crossword-clues__list-item--down-27 {
  background: #FFFF74;
}

#item11-3:active ~ .crossword-clues .crossword-clues__list-item--down-27,
#item11-3:focus ~ .crossword-clues .crossword-clues__list-item--down-27,
#item11-3:hover ~ .crossword-clues .crossword-clues__list-item--down-27 {
  background: #FFFF74;
}

#item12-3:active ~ .crossword-clues .crossword-clues__list-item--down-27,
#item12-3:focus ~ .crossword-clues .crossword-clues__list-item--down-27,
#item12-3:hover ~ .crossword-clues .crossword-clues__list-item--down-27 {
  background: #FFFF74;
}

#item13-3:active ~ .crossword-clues .crossword-clues__list-item--down-27,
#item13-3:focus ~ .crossword-clues .crossword-clues__list-item--down-27,
#item13-3:hover ~ .crossword-clues .crossword-clues__list-item--down-27 {
  background: #FFFF74;
}

#item9-9:active ~ .crossword-clues .crossword-clues__list-item--down-28,
#item9-9:focus ~ .crossword-clues .crossword-clues__list-item--down-28,
#item9-9:hover ~ .crossword-clues .crossword-clues__list-item--down-28 {
  background: #FFFF74;
}

#item10-9:active ~ .crossword-clues .crossword-clues__list-item--down-28,
#item10-9:focus ~ .crossword-clues .crossword-clues__list-item--down-28,
#item10-9:hover ~ .crossword-clues .crossword-clues__list-item--down-28 {
  background: #FFFF74;
}

#item11-9:active ~ .crossword-clues .crossword-clues__list-item--down-28,
#item11-9:focus ~ .crossword-clues .crossword-clues__list-item--down-28,
#item11-9:hover ~ .crossword-clues .crossword-clues__list-item--down-28 {
  background: #FFFF74;
}

#item10-8:active ~ .crossword-clues .crossword-clues__list-item--down-30,
#item10-8:focus ~ .crossword-clues .crossword-clues__list-item--down-30,
#item10-8:hover ~ .crossword-clues .crossword-clues__list-item--down-30 {
  background: #FFFF74;
}

#item11-8:active ~ .crossword-clues .crossword-clues__list-item--down-30,
#item11-8:focus ~ .crossword-clues .crossword-clues__list-item--down-30,
#item11-8:hover ~ .crossword-clues .crossword-clues__list-item--down-30 {
  background: #FFFF74;
}

#item12-8:active ~ .crossword-clues .crossword-clues__list-item--down-30,
#item12-8:focus ~ .crossword-clues .crossword-clues__list-item--down-30,
#item12-8:hover ~ .crossword-clues .crossword-clues__list-item--down-30 {
  background: #FFFF74;
}

#item13-8:active ~ .crossword-clues .crossword-clues__list-item--down-30,
#item13-8:focus ~ .crossword-clues .crossword-clues__list-item--down-30,
#item13-8:hover ~ .crossword-clues .crossword-clues__list-item--down-30 {
  background: #FFFF74;
}

#item1-1:valid ~ #item1-2:valid ~ #item1-3:valid ~ #item1-4:valid ~ #item1-5:valid ~ #item1-6:valid
~ .crossword-clues .crossword-clues__list-item--across-1 {
  background: #9AFF67;
}

#item2-6:valid ~ #item2-7:valid ~ #item2-8:valid ~ #item2-9:valid ~ #item2-10:valid ~ #item2-11:valid ~ #item2-12:valid ~ #item2-13:valid
~ .crossword-clues .crossword-clues__list-item--across-7 {
  background: #9AFF67;
}

#item3-3:valid ~ #item3-4:valid ~ #item3-5:valid ~ #item3-6:valid
~ .crossword-clues .crossword-clues__list-item--across-8 {
  background: #9AFF67;
}

#item4-1:valid ~ #item4-2:valid ~ #item4-3:valid ~ #item4-4:valid ~ #item4-5:valid ~ #item4-6:valid
~ .crossword-clues .crossword-clues__list-item--across-10 {
  background: #9AFF67;
}

#item4-8:valid ~ #item4-9:valid ~ #item4-10:valid ~ #item4-11:valid ~ #item4-12:valid ~ #item4-13:valid
~ .crossword-clues .crossword-clues__list-item--across-11 {
  background: #9AFF67;
}

#item5-3:valid ~ #item5-4:valid ~ #item5-5:valid
~ .crossword-clues .crossword-clues__list-item--across-14 {
  background: #9AFF67;
}

#item5-9:valid ~ #item5-10:valid ~ #item5-11:valid ~ #item5-12:valid ~ #item5-13:valid
~ .crossword-clues .crossword-clues__list-item--across-16 {
  background: #9AFF67;
}

#item6-1:valid ~ #item6-2:valid ~ #item6-3:valid ~ #item6-4:valid
~ .crossword-clues .crossword-clues__list-item--across-17 {
  background: #9AFF67;
}

#item6-6:valid ~ #item6-7:valid ~ #item6-8:valid ~ #item6-9:valid ~ #item6-10:valid
~ .crossword-clues .crossword-clues__list-item--across-19 {
  background: #9AFF67;
}

#item7-5:valid ~ #item7-6:valid ~ #item7-7:valid ~ #item7-8:valid ~ #item7-9:valid
~ .crossword-clues .crossword-clues__list-item--across-21 {
  background: #9AFF67;
}

#item8-4:valid ~ #item8-5:valid ~ #item8-6:valid ~ #item8-7:valid ~ #item8-8:valid
~ .crossword-clues .crossword-clues__list-item--across-22 {
  background: #9AFF67;
}

#item8-10:valid ~ #item8-11:valid ~ #item8-12:valid ~ #item8-13:valid
~ .crossword-clues .crossword-clues__list-item--across-23 {
  background: #9AFF67;
}

#item9-1:valid ~ #item9-2:valid ~ #item9-3:valid ~ #item9-4:valid ~ #item9-5:valid
~ .crossword-clues .crossword-clues__list-item--across-26 {
  background: #9AFF67;
}

#item9-9:valid ~ #item9-10:valid ~ #item9-11:valid
~ .crossword-clues .crossword-clues__list-item--across-28 {
  background: #9AFF67;
}

#item10-1:valid ~ #item10-2:valid ~ #item10-3:valid ~ #item10-4:valid ~ #item10-5:valid ~ #item10-6:valid
~ .crossword-clues .crossword-clues__list-item--across-29 {
  background: #9AFF67;
}

#item10-8:valid ~ #item10-9:valid ~ #item10-10:valid ~ #item10-11:valid ~ #item10-12:valid ~ #item10-13:valid
~ .crossword-clues .crossword-clues__list-item--across-30 {
  background: #9AFF67;
}

#item11-8:valid ~ #item11-9:valid ~ #item11-10:valid ~ #item11-11:valid
~ .crossword-clues .crossword-clues__list-item--across-31 {
  background: #9AFF67;
}

#item12-1:valid ~ #item12-2:valid ~ #item12-3:valid ~ #item12-4:valid ~ #item12-5:valid ~ #item12-6:valid ~ #item12-7:valid ~ #item12-8:valid
~ .crossword-clues .crossword-clues__list-item--across-32 {
  background: #9AFF67;
}

#item13-8:valid ~ #item13-9:valid ~ #item13-10:valid ~ #item13-11:valid ~ #item13-12:valid ~ #item13-13:valid
~ .crossword-clues .crossword-clues__list-item--across-33 {
  background: #9AFF67;
}

#item1-1:valid ~ #item2-1:valid ~ #item3-1:valid ~ #item4-1:valid ~ #item5-1:valid ~ #item6-1:valid
~ .crossword-clues .crossword-clues__list-item--down-1 {
  background: #9AFF67;
}

#item1-4:valid ~ #item2-4:valid ~ #item3-4:valid ~ #item4-4:valid ~ #item5-4:valid ~ #item6-4:valid
~ .crossword-clues .crossword-clues__list-item--down-2 {
  background: #9AFF67;
}

#item1-6:valid ~ #item2-6:valid ~ #item3-6:valid ~ #item4-6:valid
~ .crossword-clues .crossword-clues__list-item--down-3 {
  background: #9AFF67;
}

#item1-9:valid ~ #item2-9:valid ~ #item3-9:valid ~ #item4-9:valid ~ #item5-9:valid ~ #item6-9:valid ~ #item7-9:valid
~ .crossword-clues .crossword-clues__list-item--down-4 {
  background: #9AFF67;
}

#item1-11:valid ~ #item2-11:valid ~ #item3-11:valid ~ #item4-11:valid ~ #item5-11:valid
~ .crossword-clues .crossword-clues__list-item--down-5 {
  background: #9AFF67;
}

#item1-13:valid ~ #item2-13:valid ~ #item3-13:valid ~ #item4-13:valid ~ #item5-13:valid
~ .crossword-clues .crossword-clues__list-item--down-6 {
  background: #9AFF67;
}

#item3-3:valid ~ #item4-3:valid ~ #item5-3:valid ~ #item6-3:valid
~ .crossword-clues .crossword-clues__list-item--down-8 {
  background: #9AFF67;
}

#item3-5:valid ~ #item4-5:valid ~ #item5-5:valid
~ .crossword-clues .crossword-clues__list-item--down-9 {
  background: #9AFF67;
}

#item4-10:valid ~ #item5-10:valid ~ #item6-10:valid
~ .crossword-clues .crossword-clues__list-item--down-12 {
  background: #9AFF67;
}

#item4-12:valid ~ #item5-12:valid ~ #item6-12:valid ~ #item7-12:valid ~ #item8-12:valid
~ .crossword-clues .crossword-clues__list-item--down-13 {
  background: #9AFF67;
}

#item5-7:valid ~ #item6-7:valid ~ #item7-7:valid ~ #item8-7:valid ~ #item9-7:valid
~ .crossword-clues .crossword-clues__list-item--down-15 {
  background: #9AFF67;
}

#item6-2:valid ~ #item7-2:valid ~ #item8-2:valid ~ #item9-2:valid ~ #item10-2:valid
~ .crossword-clues .crossword-clues__list-item--down-18 {
  background: #9AFF67;
}

#item6-6:valid ~ #item7-6:valid ~ #item8-6:valid
~ .crossword-clues .crossword-clues__list-item--down-19 {
  background: #9AFF67;
}

#item6-8:valid ~ #item7-8:valid ~ #item8-8:valid
~ .crossword-clues .crossword-clues__list-item--down-20 {
  background: #9AFF67;
}

#item7-5:valid ~ #item8-5:valid ~ #item9-5:valid ~ #item10-5:valid ~ #item11-5:valid ~ #item12-5:valid ~ #item13-5:valid
~ .crossword-clues .crossword-clues__list-item--down-21 {
  background: #9AFF67;
}

#item8-4:valid ~ #item9-4:valid ~ #item10-4:valid
~ .crossword-clues .crossword-clues__list-item--down-22 {
  background: #9AFF67;
}

#item8-10:valid ~ #item9-10:valid ~ #item10-10:valid ~ #item11-10:valid ~ #item12-10:valid ~ #item13-10:valid
~ .crossword-clues .crossword-clues__list-item--down-23 {
  background: #9AFF67;
}

#item8-11:valid ~ #item9-11:valid ~ #item10-11:valid ~ #item11-11:valid
~ .crossword-clues .crossword-clues__list-item--down-24 {
  background: #9AFF67;
}

#item8-13:valid ~ #item9-13:valid ~ #item10-13:valid ~ #item11-13:valid ~ #item12-13:valid ~ #item13-13:valid
~ .crossword-clues .crossword-clues__list-item--down-25 {
  background: #9AFF67;
}

#item9-1:valid ~ #item10-1:valid ~ #item11-1:valid ~ #item12-1:valid ~ #item13-1:valid
~ .crossword-clues .crossword-clues__list-item--down-26 {
  background: #9AFF67;
}

#item9-3:valid ~ #item10-3:valid ~ #item11-3:valid ~ #item12-3:valid ~ #item13-3:valid
~ .crossword-clues .crossword-clues__list-item--down-27 {
  background: #9AFF67;
}

#item9-9:valid ~ #item10-9:valid ~ #item11-9:valid
~ .crossword-clues .crossword-clues__list-item--down-28 {
  background: #9AFF67;
}

#item10-8:valid ~ #item11-8:valid ~ #item12-8:valid ~ #item13-8:valid
~ .crossword-clues .crossword-clues__list-item--down-30 {
  background: #9AFF67;
}

.crossword-complete {
  position: absolute;
  z-index: 1000;
  top: 50%;
  left: 200%;
  font-size: 100px;
  line-height: 1;
  color: red;
  width: 100%;
  opacity: 0;
  transition: opacity 0.3s ease-in-out;
}

#item1-1:valid ~ #item1-2:valid ~ #item1-3:valid ~ #item1-4:valid ~ #item1-5:valid ~ #item1-6:valid ~ #item1-9:valid ~ #item1-11:valid ~ #item1-13:valid ~
#item2-1:valid ~ #item2-4:valid ~ #item2-6:valid ~ #item2-7:valid ~ #item2-8:valid ~ #item2-9:valid ~ #item2-10:valid ~ #item2-11:valid ~ #item2-12:valid ~ #item2-13:valid ~
#item3-1:valid ~ #item3-3:valid ~ #item3-4:valid ~ #item3-5:valid ~ #item3-6:valid ~ #item3-9:valid ~ #item3-11:valid ~ #item3-13:valid ~
#item4-1:valid ~ #item4-2:valid ~ #item4-3:valid ~ #item4-4:valid ~ #item4-5:valid ~ #item4-6:valid ~ #item4-8:valid ~ #item4-9:valid ~ #item4-10:valid ~ #item4-11:valid ~ #item4-12:valid ~ #item4-13:valid ~
#item5-1:valid ~ #item5-3:valid ~ #item5-4:valid ~ #item5-5:valid ~ #item5-7:valid ~ #item5-9:valid ~ #item5-10:valid ~ #item5-11:valid ~ #item5-12:valid ~ #item5-13:valid ~
#item6-1:valid ~ #item6-2:valid ~ #item6-3:valid ~ #item6-4:valid ~ #item6-6:valid ~ #item6-7:valid ~ #item6-8:valid ~ #item6-9:valid ~ #item6-10:valid ~ #item6-12:valid ~
#item7-2:valid ~ #item7-5:valid ~ #item7-6:valid ~ #item7-7:valid ~ #item7-8:valid ~ #item7-9:valid ~ #item7-12:valid ~
#item8-2:valid ~ #item8-4:valid ~ #item8-5:valid ~ #item8-6:valid ~ #item8-7:valid ~ #item8-8:valid ~ #item8-10:valid ~ #item8-11:valid ~ #item8-12:valid ~ #item8-13:valid ~
#item9-1:valid ~ #item9-2:valid ~ #item9-3:valid ~ #item9-4:valid ~ #item9-5:valid ~ #item9-7:valid ~ #item9-9:valid ~ #item9-10:valid ~ #item9-11:valid ~ #item9-13:valid ~
#item10-1:valid ~ #item10-2:valid ~ #item10-3:valid ~ #item10-4:valid ~ #item10-5:valid ~ #item10-6:valid ~ #item10-8:valid ~ #item10-9:valid ~ #item10-10:valid ~ #item10-11:valid ~ #item10-12:valid ~ #item10-13:valid ~
#item11-1:valid ~ #item11-3:valid ~ #item11-5:valid ~ #item11-8:valid ~ #item11-9:valid ~ #item11-10:valid ~ #item11-11:valid ~ #item11-13:valid ~
#item12-1:valid ~ #item12-2:valid ~ #item12-3:valid ~ #item12-4:valid ~ #item12-5:valid ~ #item12-6:valid ~ #item12-7:valid ~ #item12-8:valid ~ #item12-10:valid ~ #item12-13:valid ~
#item13-1:valid ~ #item13-3:valid ~ #item13-5:valid ~ #item13-8:valid ~ #item13-9:valid ~ #item13-10:valid ~ #item13-11:valid ~ #item13-12:valid ~ #item13-13:valid ~ .crossword-complete {
  opacity: 1;
}

#checkvaliditems {
  background: #9AFF67;
  cursor: pointer;
  position: absolute;
  top: 35px;
  left: 37px;
}

[for="checkvaliditems"] {
  padding: 10px 10px 10px 40px;
  margin: 20px;
  display: inline-block;
  background: #9AFF67;
  cursor: pointer;
}
"""
