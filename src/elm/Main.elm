import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random exposing (..)
import Platform.Cmd exposing (batch)
import List.Extra exposing(group)

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
    | Select1
    | Select2
    | Select3
    | Select4
    | Select5


update : Msg -> Player -> (Player, Cmd Msg)
update msg model = 
 case msg of 
  Roll ->
   ({model | rollNumber = model.rollNumber + 1}, batch [ Random.generate NewFace1 (Random.int 1 6)
                 , Random.generate NewFace2 (Random.int 1 6)
                 , Random.generate NewFace3 (Random.int 1 6)
                 , Random.generate NewFace4 (Random.int 1 6)
                 , Random.generate NewFace5 (Random.int 1 6)])

  NewFace1 newFace1 ->
     if not model.die1.saved then
      ({model | die1 = (newFace model.die1 newFace1) }, Cmd.none)
     else
      (model, Cmd.none)

  NewFace2 newFace2 ->
     if not model.die2.saved then
      ({model | die2 = (newFace model.die2 newFace2) }, Cmd.none)
     else
      (model, Cmd.none)

  NewFace3 newFace3 ->
     if not model.die3.saved then
      ({model | die3 = (newFace model.die3 newFace3) }, Cmd.none)
     else
      (model, Cmd.none)

  NewFace4 newFace4 ->
    if not model.die4.saved then
      ({model | die4 = (newFace model.die4 newFace4) }, Cmd.none)
    else
      (model, Cmd.none)
  
  NewFace5 newFace5 ->
    if not model.die5.saved then
      ({model | die5 = (newFace model.die5 newFace5) }, Cmd.none)
    else
      (model, Cmd.none)
  Select1 ->
   let 
    die1 = model.die1
    newDie1 = { face = die1.face, saved = (not die1.saved)}
   in
    ({model | die1 = newDie1}, Cmd.none)

  Select2 ->
   let 
    die2 = model.die2
    newDie2 = { face = die2.face, saved = (not die2.saved)}
   in
    ({model | die2 = newDie2}, Cmd.none)

  Select3 ->
   let 
    die3 = model.die3
    newDie3 = { face = die3.face, saved = (not die3.saved)}
   in
    ({model | die3 = newDie3}, Cmd.none)

  Select4 ->
   let 
    die4 = model.die4
    newDie4 = { face = die4.face, saved = (not die4.saved)}
   in
    ({model | die4 = newDie4}, Cmd.none)

  Select5 ->
   let 
    die5 = model.die5
    newDie5 = { face = die5.face, saved = (not die5.saved)}
   in
    ({model | die5 = newDie5}, Cmd.none)

subscriptions : Player -> Sub Msg
subscriptions model =
  Sub.none

view : Player -> Html Msg
view model = 
    div []
    [ (diceView model)
    , div [] [ text (validate model)]
    ]
  

init : String -> (Player, Cmd Msg)
init name = 
    let 
      player = new name
    in
      (player, Cmd.none)



validate: Player -> String
validate model =
  let
    groupedDies = List.Extra.group (List.sort [model.die1.face, model.die2.face, model.die3.face, model.die4.face, model.die5.face])
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

type alias Player = 
    {   username : String
    ,   rollNumber : Int
    ,   die1 : Dice
    ,   die2 : Dice
    ,   die3 : Dice
    ,   die4 : Dice
    ,   die5 : Dice
    }
new : String -> Player
new name = 
    { username = name, rollNumber = 0, die1 = (newDice 1), die2 = (newDice 1), die3 = (newDice 1), die4 = (newDice 1), die5 = (newDice 1)}

diceView: Player -> Html Msg
diceView player = 
    div []
    [ div [] [ text (player.username ++ "Roll number: " ++ toString player.rollNumber)]
    , img [ src (getUrl player.die1.face), onClick Select1,  style (styles player.die1.saved).img] []
    , img [ src (getUrl player.die2.face), onClick Select2 , style (styles player.die2.saved).img] []
    , img [ src (getUrl player.die3.face), onClick Select3 , style (styles player.die3.saved).img] []
    , img [ src (getUrl player.die4.face), onClick Select4 , style (styles player.die4.saved).img] []
    , img [ src (getUrl player.die5.face), onClick Select5 , style (styles player.die5.saved).img] []
    , rollResetButton player
    ]

getUrl: Int -> String
getUrl dieFace =
 case dieFace of
  1 -> "https://upload.wikimedia.org/wikipedia/commons/1/1b/Dice-1-b.svg"
  2 -> "https://upload.wikimedia.org/wikipedia/commons/5/5f/Dice-2-b.svg"
  3 -> "https://upload.wikimedia.org/wikipedia/commons/b/b1/Dice-3-b.svg"
  4 -> "https://upload.wikimedia.org/wikipedia/commons/f/fd/Dice-4-b.svg"
  5 -> "https://upload.wikimedia.org/wikipedia/commons/0/08/Dice-5-b.svg"
  6 -> "https://upload.wikimedia.org/wikipedia/commons/2/26/Dice-6-b.svg"
  _ -> "https://upload.wikimedia.org/wikipedia/commons/1/1b/Dice-1-b.svg"

  --CSS STYLES
styles: Bool -> { img : List ( String, String ) }
styles saved =
    if (saved) then
      {
        img =
          [ ( "width", "5%" )
          , ( "margin", "10px")
          , ( "border", "1px solid red")
          , ( "border-radius", "7px")
          ]
      }
    else
      {
        img =
          [ ( "width", "5%" )
          , ( "margin", "10px")
          ]
      }

type alias Dice =
    { face : Int
    , saved : Bool
    }

newDice : Int -> Dice 
newDice number =
    {face = number, saved = False}


newFace: Dice -> Int -> Dice 
newFace dice number =
  { face = number, saved = dice.saved}

rollResetButton: Player -> Html Msg
rollResetButton player =
  if player.rollNumber < 3 then 
    button [ onClick Roll ] [ text "Roll" ]
  else 
    button [ ] [ text "Reset" ]
