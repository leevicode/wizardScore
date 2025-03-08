module Main exposing (..)

import Browser
import Dict exposing (..)
import Html exposing (Attribute, Html, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (singleton)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Score =
    { total : Int
    , guesses : Maybe Int
    , tricks : Maybe Int
    }


type alias Model =
    { round : Int
    , players : Dict String Score
    , playerInput : String
    }


initScore : Score
initScore =
    { total = 0
    , guesses = Nothing
    , tricks = Nothing
    }


init : Model
init =
    { round = 1
    , players = Dict.singleton "Leevi" <| { initScore | total = 1 }
    , playerInput = ""
    }



-- UPDATE


type Msg
    = NewPlayer
    | ChangeNewPlayer String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewPlayer ->
            { model
                | players =
                    model.players
                        |> Dict.insert model.playerInput initScore
                , playerInput = ""
            }

        ChangeNewPlayer name ->
            { model
                | playerInput = name
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.text "hellos"
        , model.players
            |> Dict.toList
            |> List.sortBy (\( a, b ) -> b.total * -1)
            |> List.map (\( a, b ) -> renderPlayer a b)
            |> Html.div []
        , Html.input [ type_ "text", onInput ChangeNewPlayer, value model.playerInput ] []
        , Html.button [ onClick NewPlayer ] [ text "hi" ]
        ]


renderPlayer : String -> Score -> Html Msg
renderPlayer key score =
    Html.div []
        [ textElem p [] key
        , textElem p [] <| "Total score: " ++ String.fromInt score.total
        ]


textElem : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List (Html.Attribute msg) -> String -> Html msg
textElem element attributes text =
    text
        |> Html.text
        |> List.singleton
        |> element attributes
