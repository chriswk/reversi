module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button)
import Html.Attributes exposing (src, id, class)
import Html.Events exposing (onClick)
import Reversi exposing (createGame, Game, GameStatus(..), tilesForPlayer, unclaimedTiles, Player(..), Tile)
import Utils exposing (partitionByN)
import List.extra exposing (getAt, setAt)


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
    | ClaimPiece Player Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            init

        ClaimPiece player id ->
            let
                game = model.game
                tiles = claimPiece game.tiles player id
                unclaimed = unclaimedTiles game
                gameStatus = if unclaimed == 0 then decideWinner game else Playing
                updatedGame = { game | tiles = tiles, status = gameStatus } 
            in
                ({ model | game = updatedGame}, Cmd.None



---- VIEW ----


scoreText : Game -> Html Msg
scoreText game =
    let
        blackScore =
            tilesForPlayer game Black

        whiteScore =
            tilesForPlayer game White

        remaining =
            unclaimedTiles game
    in
        div []
            [ div [] text "playing"
            , div [] text "Black pieces: " ++ toString blackScore
            , div [] text "White pieces: " ++ toString whiteScore
            , div [] text "Unclaimed: " ++ toString remaining
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
        div [] text t


statusView : Game -> Html Msg
statusView game =
    let
        ( status, cssClass ) =
            case game.status of
                Playing ->
                    ( scoreText game, "playing" )

                BlackWin ->
                    ( winningText Black, "blackwon" )

                WhiteWin ->
                    ( winningText White, "whitewon" )
    in
        div [ class "status " ++ cssClass ] status


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
        div [ class "cell " ++ colorClass ] [ div [ class "piece" ] [] ]


drawRow : List Tile -> Html Msg
drawRow tiles =
    div [ class "row" ] List.map drawTile tiles


drawRows : Game -> Html Msg
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
    div [ id "main" ]
        [ h1 [] [ text "Reversi", statusView model.game ]
        , div [ class "board" ] drawRows model.game.tiles
        , div [ class "buttons" ] gameButtons
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
