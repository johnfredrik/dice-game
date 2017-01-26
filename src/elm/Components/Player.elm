module Components.Player exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

type alias Player = 
    {   username : String
    ,   dieFace1 : Int
    ,   dieFace2 : Int
    ,   dieFace3 : Int
    ,   dieFace4 : Int
    ,   dieFace5 : Int
    }
new : String -> Player
new name = 
    { username = name, dieFace1 = 1, dieFace2 = 1, dieFace3 = 1, dieFace4 = 1, dieFace5 = 1}

diceView: Player -> Html a
diceView player = 
    div []
    [ div [] [ text player.username]
    , img [ src (getUrl player.dieFace1), style styles.img] []
    , img [ src (getUrl player.dieFace2), style styles.img] []
    , img [ src (getUrl player.dieFace3), style styles.img] []
    , img [ src (getUrl player.dieFace4), style styles.img] []
    , img [ src (getUrl player.dieFace5), style styles.img] []
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
styles : { img : List ( String, String ) }
styles =
  {
    img =
      [ ( "width", "5%" )
      , ( "margin", "10px")
      ]
  }

