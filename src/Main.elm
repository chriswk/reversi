module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button, tr, td, table)
import Html.Attributes exposing (src, id, class)
import Html.Events exposing (onClick)
import Reversi exposing (createGame, Game, Turn(..), GameStatus(..), tilesForPlayer, unclaimedTiles, claimPiece, decideWinner, Player(..), Tile)
import Utils exposing (partitionByN)


---- MODEL ----


type alias Model =
    { game : Game
    }


init : ( Model, Cmd Msg )
init =
    ( { game = createGame 8 }, Cmd.none )



---- UPDATE ----


type Msg
    = NewGame
    | ClaimPiece Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            init

        ClaimPiece id ->
            let
                game =
                    model.game

                player =
                    case model.game.turn of
                        BlackTurn ->
                            Black

                        WhiteTurn ->
                            White

                tiles =
                    claimPiece id player game.tiles

                status =
                    decideWinner game

                turn =
                    case game.turn of
                        BlackTurn ->
                            WhiteTurn

                        WhiteTurn ->
                            BlackTurn

                updatedGame =
                    { game | tiles = tiles, status = status, turn = turn }
            in
                ( { model | game = updatedGame }, Cmd.none )



---- VIEW ----


scoreText : Game -> Html Msg
scoreText game =
    let
        blackScore =
            toString (tilesForPlayer game Black)

        whiteScore =
            toString (tilesForPlayer game White)

        remaining =
            toString (unclaimedTiles game)
    in
        div []
            [ div [] [ text "playing" ]
            , div [] [ text ("Black pieces: " ++ blackScore) ]
            , div [] [ text ("White pieces: " ++ whiteScore) ]
            , div [] [ text ("Unclaimed: " ++ remaining) ]
            ]


winningText : Game -> Player -> Html Msg
winningText game player =
    let
        score =
            toString (tilesForPlayer game player)

        t =
            case player of
                Black ->
                    "Black wins with " ++ score

                White ->
                    "White wins with " ++ score
    in
        div [] [ text t ]


statusView : Game -> Html Msg
statusView game =
    let
        ( status, cssClass ) =
            case game.status of
                Playing ->
                    ( scoreText game, "playing" )

                BlackWin ->
                    ( winningText game Black, "blackwon" )

                WhiteWin ->
                    ( winningText game White, "whitewon" )

                Draw ->
                    let
                        t =
                            div [] [ text "Draw" ]
                    in
                        ( t, "draw" )
    in
        div [ class ("status " ++ cssClass) ] [ status ]


drawTile : Tile -> Html Msg
drawTile tile =
    let
        colorClass =
            case tile.owner of
                Just Black ->
                    "black"

                Just White ->
                    "white"

                Nothing ->
                    ""
    in
        td [ class ("cell " ++ colorClass), onClick (ClaimPiece tile.id) ] [ div [ class "piece" ] [ text ((toString tile.id) ++ " " ++ (toString tile.owner)) ] ]


drawRow : List Tile -> Html Msg
drawRow tiles =
    let
        t =
            List.map drawTile tiles
    in
        tr [ class "row" ] t


drawRows : Game -> List (Html Msg)
drawRows game =
    partitionByN game.rows game.tiles
        |> List.map drawRow


gameButtons : Html Msg
gameButtons =
    div []
        [ button [ class "button", onClick NewGame ] [ text "New Game" ]
        ]


view : Model -> Html Msg
view model =
    let
        rows =
            drawRows model.game
    in
        div [ id "main" ]
            [ h1 [] [ text "Reversi", statusView model.game ]
            , div [ class "board" ] [ table [] rows ]
            , div [ class "buttons" ] [ gameButtons ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
