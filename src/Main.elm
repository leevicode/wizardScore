module Main exposing (..)

import Browser
import Dict exposing (..)
import Enter exposing (onEnter)
import Html exposing (Attribute, Html, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import List exposing (singleton)
import Maybe as M



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
    , errors : Maybe String
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

    --, players = Dict.singleton "Leevi" <| { initScore | total = 10 }
    , players = Dict.empty
    , playerInput = ""
    , errors = Nothing
    }



-- UPDATE


type Msg
    = NewPlayer
    | ChangeNewPlayer String
    | ChangePlayerGuesses String (Maybe Int)
    | ChangePlayerTricks String (Maybe Int)
    | NextRound


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewPlayer ->
            case Dict.get model.playerInput model.players of
                Nothing ->
                    case model.playerInput of
                        "" ->
                            { model | errors = Just "Give a name to the new player" }

                        _ ->
                            { model
                                | players =
                                    model.players
                                        |> Dict.insert model.playerInput initScore
                                , playerInput = ""
                                , errors = Nothing
                            }

                Just _ ->
                    { model | errors = Just <| "Player " ++ model.playerInput ++ " already exists!" }

        ChangeNewPlayer name ->
            { model
                | playerInput = name
            }

        ChangePlayerGuesses key guessMaybe ->
            model
                |> updatePlayers key (\score -> { score | guesses = guessMaybe |> M.andThen checkPositive })

        ChangePlayerTricks key tricksMaybe ->
            model
                |> updatePlayers key (\score -> { score | tricks = tricksMaybe |> M.andThen checkPositive })

        {-
               ChangePlayerGuesses key guessMaybe ->
                   guessMaybe
                   |> M.andThen checkPositive
                   |> M.map
                       (\guesses ->
                           model
                               |> updatePlayers key (\score -> { score | guesses = Just guesses })
                       )
                   |> M.withDefault model

           ChangePlayerTricks key trickMaybe ->
               trickMaybe
                   |> M.andThen checkPositive
                   |> M.map
                       (\tricks ->
                           model
                               |> updatePlayers key (\score -> { score | tricks = Just tricks })
                       )
                   |> M.withDefault model
        -}
        NextRound ->
            let
                getTotal f =
                    model.players
                        |> Dict.foldl
                            (\_ score maybeTotal ->
                                f score
                                    |> M.andThen checkPositive
                                    |> M.map2 (+) maybeTotal
                            )
                            (Just 0)

                totalGuesses =
                    getTotal .guesses

                totalTricks =
                    getTotal .tricks

                errors =
                    case totalGuesses of
                        Nothing ->
                            Just "Input a guess for each player"

                        Just _ ->
                            case totalTricks of
                                Nothing ->
                                    Just "Input tricks for each player"

                                Just tricks ->
                                    case compare tricks model.round of
                                        LT ->
                                            Just "Too little amount of tricks"

                                        GT ->
                                            Just "Too many tricks"

                                        EQ ->
                                            Nothing

                newRound =
                    case errors of
                        Nothing ->
                            model.round + 1

                        Just _ ->
                            model.round

                validRound =
                    case errors of
                        Nothing ->
                            Just ()

                        Just _ ->
                            Nothing
            in
            { model
                | errors = errors
                , round = newRound
                , players =
                    model.players
                        |> Dict.map
                            (\_ score ->
                                M.map3
                                    (\_ guesses tricks ->
                                        case compare guesses tricks of
                                            EQ ->
                                                updateTotal score <| 20 + 10 * tricks

                                            _ ->
                                                updateTotal score <| -10 * (abs <| tricks - guesses)
                                    )
                                    validRound
                                    score.guesses
                                    score.tricks
                                    |> M.withDefault score
                            )
            }


checkPositive : number -> Maybe number
checkPositive x =
    if x < 0 then
        Nothing

    else
        Just x


updateTotal : Score -> Int -> Score
updateTotal score difference =
    { total = score.total + difference
    , guesses = Nothing
    , tricks = Nothing
    }


updatePlayers : String -> (Score -> Score) -> Model -> Model
updatePlayers key f model =
    { model
        | players =
            model.players
                |> Dict.update key (M.map f)

        --, errors = Nothing
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textElem Html.h1 [] "Wizard score keeper"
        , renderError model.errors
        , Html.form [ onSubmit NextRound ]
            [ model.players
                |> Dict.toList
                |> List.sortBy (\( a, b ) -> b.total * -1)
                |> List.map (\( a, b ) -> renderPlayer a b)
                |> Html.div [ class "players" ]
            , textElem p [ class "roundText" ] <| "current round: " ++ String.fromInt model.round
            , Dict.keys model.players
                |> List.head
                |> maybeElem (\_ -> textElem Html.input [ type_ "submit", value "Next round" ] "")
            ]

        --, textElem Html.button [ onClick NextRound ] "next round"
        , textElem Html.h3 [] "Add players"
        , Html.input [ type_ "text", onInput ChangeNewPlayer, value model.playerInput, placeholder "Player name", onEnter NewPlayer ] []
        , textElem Html.button [ onClick NewPlayer ] "Add player"
        ]


renderPlayer : String -> Score -> Html Msg
renderPlayer key score =
    Html.div [ class "player" ]
        [ textElem p [ class "playerName" ] key
        , textElem p [ class "playerScore" ] <| "Total score: " ++ String.fromInt score.total
        , inputElem "guess amount of tricks" key score.guesses ChangePlayerGuesses
        , inputElem "input actual amount " key score.tricks ChangePlayerTricks
        ]


renderError : Maybe String -> Html Msg
renderError err =
    case err of
        Nothing ->
            Html.text ""

        Just e ->
            textElem Html.p [ class "error" ] e


inputElem : String -> String -> Maybe Int -> (String -> Maybe Int -> Msg) -> Html Msg
inputElem placeholderText key valueMaybe onInputMsg =
    Html.input
        [ type_ "number"
        , placeholder placeholderText
        , valueMaybe
            |> M.map String.fromInt
            |> M.withDefault ""
            |> value
        , onInput
            (\s ->
                s
                    |> String.toInt
                    |> onInputMsg key
            )
        ]
        []


textElem : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List (Html.Attribute msg) -> String -> Html msg
textElem element attributes text =
    text
        |> Html.text
        |> List.singleton
        |> element attributes


maybeElem elem maybe =
    case maybe of
        Nothing ->
            Html.text ""

        Just a ->
            elem a
