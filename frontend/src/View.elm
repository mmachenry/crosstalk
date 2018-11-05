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
      div [class "crossword-board crossword-board--labels"] labels,
      div [ class "crossword-clues" ] [
        dl [class "crossword-clues__list crossword-clues__list--across"]
          ((dt [class "crossword-clues__list-title"] [ text "Across" ])
          :: acrossClues),
        dl [class "crossword-clues__list crossword-clues__list--down"]
          ((dt [class "crossword-clues__list-title"] [ text "Down" ])
          :: downClues)
      ]
    ])
  ]

labels = List.map makeLabel [
  (1,1,"1"),
  (1,4,"2"),
  (1,6,"3")
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
      , attribute "data-number" (toString num)
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

makeLabel : (Int, Int, String) -> Html Msg
makeLabel (row, col, str) =
  span [
   id ("label-" ++ str),
   class "crossword-board__item-label",
   style [("grid-column", toString col), ("grid-row", toString row)]
  ] [ span [class "crossword-board__item-label-text"] [ text str ]]

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
