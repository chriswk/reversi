module Reversi exposing (createGame, unclaimedTiles, tilesForPlayer, Game, GameStatus(..), Player(..), Turn(..), Tile)

import List.Extra exposing (getAt, setAt)


type Player
    = Black
    | White


type alias Tile =
    { id : Int
    , owner : Maybe Player
    }


type GameStatus
    = Playing
    | BlackWin
    | WhiteWin


type Turn
    = BlackTurn
    | WhiteTurn


type alias Game =
    { rows : Int
    , cols : Int
    , turn : Turn
    , status : GameStatus
    , tiles : List Tile
    }


claimPiece : List Tile -> Player -> Int -> List Tile
claimPiece tiles player tileId =
    let
        oldTile =
            getAt tileId tiles

        newTile =
            case oldTile of
                Just t ->
                    Just { t | owner = Just player }

                Nothing ->
                    Nothing
    in
        case newTile of
            Just t ->
                setAt tileId t tiles

            Nothing ->
                tiles


unclaimedTiles : Game -> Int
unclaimedTiles game =
    game.tiles
        |> List.filter (\t -> t.owner == Nothing)
        |> List.length


tilesForPlayer : Game -> Player -> Int
tilesForPlayer game player =
    game.tiles
        |> List.filter (\t -> t.owner == Just player)
        |> List.length


createTile : Int -> Tile
createTile id =
    Tile id Nothing


initialTiles : Int -> List Tile
initialTiles squareSize =
    let
        maxL =
            (squareSize * squareSize) - 1
    in
        List.range 0 maxL
            |> List.map createTile


createGame : Int -> Game
createGame rowSize =
    Game rowSize rowSize BlackTurn Playing (initialTiles rowSize)
