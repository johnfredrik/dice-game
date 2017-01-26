import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random exposing (..)
import Platform.Cmd exposing (batch)
import List.Extra exposing(group)

import Components.Player as Ply exposing (Player, new, diceView)

main =
  Html.program
    { init = init "John Fredrik"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Msg 
 = Roll
 | NewFace1 Int
 | NewFace2 Int
 | NewFace3 Int
 | NewFace4 Int
 | NewFace5 Int

update : Msg -> Ply.Player -> (Ply.Player, Cmd Msg)
update msg model = 
 case msg of 
  Roll ->
   (model, batch [ Random.generate NewFace1 (Random.int 1 6)
                 , Random.generate NewFace2 (Random.int 1 6)
                 , Random.generate NewFace3 (Random.int 1 6)
                 , Random.generate NewFace4 (Random.int 1 6)
                 , Random.generate NewFace5 (Random.int 1 6)])

  NewFace1 newFace1 ->
   ({model | dieFace1 = newFace1}, Cmd.none)

  NewFace2 newFace2 ->
   ({model | dieFace2 = newFace2}, Cmd.none)

  NewFace3 newFace3 ->
   ({model | dieFace3 = newFace3}, Cmd.none)

  NewFace4 newFace4 ->
   ({model | dieFace3 = newFace4}, Cmd.none)
  
  NewFace5 newFace5 ->
   ({model | dieFace5 = newFace5}, Cmd.none)

subscriptions : Ply.Player -> Sub Msg
subscriptions model =
  Sub.none

view : Ply.Player -> Html Msg
view model = 
    div []
    [ (Ply.diceView model)
    , button [ onClick Roll ] [ text "Roll" ]
    , div [] [ text (validate model)]
    ]
  

init : String -> (Ply.Player, Cmd Msg)
init name = 
    let 
      player = Ply.new name
    in
      (player, Cmd.none)



validate: Player -> String
validate model =
  let
    groupedDies = List.Extra.group (List.sort [model.dieFace1, model.dieFace2, model.dieFace3, model.dieFace4, model.dieFace5])
  in
    if (List.any isYatzy groupedDies) then
      "Yatzy"
    else if (List.any isFourOfaKind groupedDies) then 
      "Four of a Kind"
    else if  (List.any isThreeOfaKind groupedDies) && (List.any isPair groupedDies) then
      "House"
    else if  (List.any isThreeOfaKind groupedDies) then
      "Three of a Kind"
    else if List.length (List.filter isPair groupedDies) == 2 then
      "Two Pair"
    else if (List.any isPair groupedDies) then
      "Pair"
    else
      ""

isYatzy : List Int -> Bool
isYatzy a =
  List.length a == 5

isFourOfaKind : List Int -> Bool
isFourOfaKind a =
  List.length a == 4

isThreeOfaKind : List Int -> Bool
isThreeOfaKind a =
  List.length a == 3


isPair : List Int -> Bool
isPair a =
  List.length a == 2
