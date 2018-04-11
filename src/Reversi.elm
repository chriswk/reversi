module Reversi exposing (createGame, unclaimedTiles, tilesForPlayer, Game, GameStatus(..), Player(..), Turn(..), Tile, claimPiece, decideWinner)

import List.Extra exposing (getAt, setAt)


type Player
    = Black
    | White


type alias Claim =
    { player : Player
    , id : Int
    }


type alias Tile =
    { id : Int
    , owner : Maybe Player
    }


type GameStatus
    = Playing
    | BlackWin
    | WhiteWin
    | Draw


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


decideWinner : Game -> GameStatus
decideWinner game =
    let
        undecided =
            unclaimedTiles game

        whiteScore =
            tilesForPlayer game White

        blackScore =
            tilesForPlayer game Black
    in
        if undecided > 0 then
            Playing
        else if whiteScore > blackScore then
            WhiteWin
        else if whiteScore == blackScore then
            Draw
        else
            BlackWin


claimPiece : Int -> Player -> List Tile -> List Tile
claimPiece tileId player tiles =
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


leftMidTop : Game -> Int
leftMidTop game =
    game.cols * ((game.cols // 2) - 1) + (game.cols // 2 - 1)


leftMidBot : Game -> Int
leftMidBot game =
    game.cols * (game.cols // 2) + (game.cols // 2 - 1)


rightMidTop : Game -> Int
rightMidTop game =
    game.cols * ((game.cols // 2) - 1) + (game.cols // 2)


rightMidBot : Game -> Int
rightMidBot game =
    game.cols * ((game.cols // 2)) + (game.cols // 2)


claimInitialPieces : Game -> Game
claimInitialPieces game =
    let
        initialBoard =
            game.tiles
                |> claimPiece (leftMidTop game) Black
                |> claimPiece (leftMidBot game) White
                |> claimPiece (rightMidTop game) White
                |> claimPiece (rightMidBot game) Black
    in
        { game | tiles = initialBoard }


createGame : Int -> Game
createGame rowSize =
    Game rowSize rowSize BlackTurn Playing (initialTiles rowSize)
        |> claimInitialPieces
