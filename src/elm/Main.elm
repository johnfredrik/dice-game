port module Main exposing(..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random exposing (..)
import Platform.Cmd exposing (batch)
import List.Extra exposing(group)
import Svg
import Svg.Attributes
import Debug exposing(log)

port rerollDice : (() -> msg) -> Sub msg

main : Program Never Player Msg
main =
  Html.program
    { init = init "John Fredrik"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Msg 
    = Roll
    | ResetDice
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
    | Test String


update : Msg -> Player -> (Player, Cmd Msg)
update msg model = 
 case msg of 
  Test name ->
    let
      _ = log "Name" name
    in
      (model, Cmd.none)
  Roll ->
   ({model | rollNumber = model.rollNumber + 1}, batch [ Random.generate NewFace1 (Random.int 1 6)
                 , Random.generate NewFace2 (Random.int 1 6)
                 , Random.generate NewFace3 (Random.int 1 6)
                 , Random.generate NewFace4 (Random.int 1 6)
                 , Random.generate NewFace5 (Random.int 1 6)])
  
  ResetDice ->
    let 
      player = resetDice model
    in
      (player, Cmd.none)

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
  rerollDice (always Roll)

view : Player -> Html Msg
view model = 
    div []
    [ (diceView model) ]
  

init : String -> (Player, Cmd Msg)
init name = 
    let 
      player = newPlayer name
    in
      (player, Cmd.none)



validate: Player -> String
validate model =
  let
    groupedDies = List.Extra.group (List.sort [model.die1.face, model.die2.face, model.die3.face, model.die4.face, model.die5.face])
  in
    if (model.die1.face < 1 || model.die1.face > 6) then
      ""
    else if (List.any isYatzy groupedDies) then
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
    ,   previousScore : List (List Dice)
    ,   die1 : Dice
    ,   die2 : Dice
    ,   die3 : Dice
    ,   die4 : Dice
    ,   die5 : Dice
    }
newPlayer : String -> Player
newPlayer name = 
    { username = name, rollNumber = 0, previousScore = [], die1 = (newDice 0), die2 = (newDice 0), die3 = (newDice 0), die4 = (newDice 0), die5 = (newDice 0) }

resetDice : Player -> Player
resetDice player =
  let 
    new = (newPlayer player.username)
    previousScore = [player.die1, player.die2, player.die3, player.die4, player.die5]
  in
    {new | previousScore = previousScore :: player.previousScore}
    

diceView: Player -> Html Msg
diceView player = 
    div []
    [ div [] [ text player.username ]
    , div [] [ text ("Roll number: " ++ toString player.rollNumber)]
    , getDieFace player.die1.face
    , getDieFace player.die2.face
    , getDieFace player.die3.face
    , getDieFace player.die4.face
    , getDieFace player.die5.face
    , rollResetButton player
    , div [] [ text (validate player)]
    , previousScoreView player.previousScore
    ]

getUrl: Int -> String
getUrl dieFace =
 case dieFace of
  1 -> "static/img/dice-1.svg"
  2 -> "static/img/dice-2.svg"
  3 -> "static/img/dice-3.svg"
  4 -> "static/img/dice-4.svg"
  5 -> "static/img/dice-5.svg"
  6 -> "static/img/dice-6.svg"
  _ -> "static/img/dice-0.svg"

  --CSS STYLES
mainDiceImageStyles: Bool -> { img : List ( String, String ) }
mainDiceImageStyles saved =
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

previousDiceImageStyles: { img : List ( String, String ) }
previousDiceImageStyles =
  {
    img =
      [ ( "width", "2%" )
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
    button [ onClick ResetDice ] [ text "Reset" ]

diceScores: List Dice -> Html Msg
diceScores dices = 
  if (List.length dices) > 0 then 
    div [] 
      [ div [] [text (toString (List.Extra.elemIndex dices)) ]
      , div [] (List.map diceScore dices)
      ]
  else 
    div [] []

diceScore: Dice -> Html Msg
diceScore dice =
  img [ src (getUrl dice.face), onClick Select1,  style previousDiceImageStyles.img] []

previousScoreView: List (List Dice) -> Html Msg
previousScoreView scores = 
  if (List.length scores) > 0 then 
    div [] 
    [ div [] [text "Previous Score"]
    , div [] (List.map diceScores scores)
    ]
  else 
    div [] []

getDieFace : Int -> Html Msg
getDieFace number =
  Svg.svg 
  [ Svg.Attributes.width "557"
  , Svg.Attributes.height "557"
  ] 
  [ Svg.rect 
      [ Svg.Attributes.x "4"
      , Svg.Attributes.y "4"
      , Svg.Attributes.width "549"
      , Svg.Attributes.height "549"
      , Svg.Attributes.rx "68"
      , Svg.Attributes.fill "white"
      , Svg.Attributes.stroke "#000"
      , Svg.Attributes.strokeWidth "7"
      ][]
  , getDie number
  ]
    
getDie : Int -> Html Msg
getDie number =
  let
    _ = log "number" number
  in
    case number of 
      1 ->
        getDie1
      2 -> 
        getDie2
      3 ->
        getDie3
      4 ->
        getDie4
      5 ->
        getDie5
      6 ->
        getDie6
      _ ->
        getDie1

getDie1 : Html Msg
getDie1 =
  Svg.circle 
    [ Svg.Attributes.cx "278"
    , Svg.Attributes.cy "278"
    , Svg.Attributes.r "70"
    , Svg.Attributes.fill "black"
    , Svg.Attributes.stroke "black"
    , Svg.Attributes.strokeWidth "1"
    ] []

getDie2 : Html Msg
getDie2 =
  Svg.g [] 
    [ Svg.circle 
      [ Svg.Attributes.cx "439.9746094"
      , Svg.Attributes.cy "439.9736328"
      , Svg.Attributes.r "70"
      ] []
    , Svg.circle 
      [ Svg.Attributes.cx "117.0258789"
      , Svg.Attributes.cy "117.0263672"
      , Svg.Attributes.r "70"
      ] []
    ]

getDie3 : Html Msg
getDie3 =
  Svg.g [] 
    [ Svg.circle 
      [ Svg.Attributes.cx "439.9746094"
      , Svg.Attributes.cy "439.9736328"
      , Svg.Attributes.r "70"
      ] []
    , Svg.circle 
      [ Svg.Attributes.cx "117.0258789"
      , Svg.Attributes.cy "117.0263672"
      , Svg.Attributes.r "70"
      ] []
        , Svg.circle 
      [ Svg.Attributes.cx "278.5"
      , Svg.Attributes.cy "278.5"
      , Svg.Attributes.r "70"
      ] []
    ]

getDie4 : Html Msg
getDie4 =
  Svg.g 
    [ Svg.Attributes.strokeDasharray "0,323"
    , Svg.Attributes.strokeLinecap "round"
    ] 
    [ Svg.path 
      [ Svg.Attributes.stroke "#000"
      , Svg.Attributes.strokeWidth "140"
      , Svg.Attributes.d "M117,117v325m323-2V11"
      ][] 
    ]

getDie5 : Html Msg
getDie5 =
  Svg.g 
    [ Svg.Attributes.strokeDasharray "0,228.4"
    , Svg.Attributes.strokeLinecap "round"
    ] 
    [ Svg.path 
      [ Svg.Attributes.stroke "#000"
      , Svg.Attributes.strokeWidth "140"
      , Svg.Attributes.d "m440,440-325-325m2,325 325-325"
      ][] 
    ]

getDie6 : Html Msg
getDie6 =
  Svg.g 
    [ Svg.Attributes.strokeDasharray "0,159"
    , Svg.Attributes.strokeLinecap "round"
    ] 
    [ Svg.path 
      [ Svg.Attributes.stroke "#000"
      , Svg.Attributes.strokeWidth "140"
      , Svg.Attributes.d "M437,117H0M437,440H0"
      ][] 
    ]

