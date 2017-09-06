-- TODO init config: adjust player count
-- TODO init config: adjust teams
-- TODO init config: adjust field height
-- TODO detect GAME OVER
-- TODO detect START GAME
-- TODO ghost piece
-- TODO hold piece
-- TODO show next 3 pieces
-- TODO better rewards for combos (timer)
-- TODO better reward for special moves (T flips)
-- TODO generate random tetrominos using 7system (bags of 7)
-- TODO speed up pieces as free space shrinks
-- TODO improve buttons (how to detect HTML button hold?)
-- TODO use gamepads as controllers
-- TODO hard drop (find maximum y, then translate by that much)
-- TODO CSS3 animations
-- TODO good tetris AI


module Main exposing (main)

import Html exposing (Html, Attribute, Attribute, main_, button, div, text, h3, h2, span)
import Html.Events exposing (on, keyCode, onMouseDown)
import Html.Attributes exposing (attribute, class, id)
import Dom exposing (focus)
import Task
import Time exposing (Time, second)
import Array
import Random.Pcg
import Dict exposing (Dict)
import Array2D exposing (Array2D, initialize)
import Json.Decode as Json


main =
    Html.program
        { init = init numPlayers -- TODO more configuration options
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


unwrapMaybeWith default f v =
    {- Convert `v = Just a` to `f a`
       Convert `v = Nothing` to `default`
    -}
    case v of
        Just x ->
            f x

        Nothing ->
            default


addPos pos1 pos2 =
    { pos1 | x = pos1.x + pos2.x, y = pos1.y + pos2.y }


type Block
    = Edge
    | Full
    | Empty


type alias Grid =
    Array2D Block


type alias Pos =
    { x : Int, y : Int }


type alias ShapeName =
    -- elm does not support user-defined typeclasses, no ShapeName ADT in a dictionary
    String


type alias Tetromino =
    { name : ShapeName, degrees : Int, shape : Grid, pos : Pos }


type DropSpeed
    = Fast
    | Normal


type alias FrameNumber =
    Int


type alias Board =
    { score : Int
    , gameOver : Bool
    , clearedRows : List Int
    , tetris : Maybe FrameNumber
    , speed : ( FrameNumber, DropSpeed ) -- track when speed changed
    , grid : Grid
    , tetromino : Maybe Tetromino -- should replace with a list of tetrominos
    , nextTetromino : Maybe ShapeName
    }


type alias Model =
    { frame : FrameNumber, paused : Bool, boards : List Board }


type Dir
    = Left
    | Right


type PlayerInput
    = Translate Dir
    | Rotate Dir
    | SoftDrop
    | HardDrop


type alias BoardID =
    Int


type Input
    = PlayerInputs ( BoardID, PlayerInput )
    | Tick Time
    | Pause
    | AddTetrominos (List (Maybe ShapeName))
    | None


boardHeight =
    -- TODO make height configurable
    -- note top 2 rows must be hidden
    22 + boardMargin * 2


boardWidth =
    10 + boardMargin * 2


boardMargin =
    1


numPlayers =
    -- TODO make this configurable
    2


fps =
    12


normalSpeed =
    -- soft drops move `normalSpeed` times faster than normal speed
    fps // 4


init nPlayers =
    let
        boards =
            initBoards nPlayers
    in
        ( { frame = 0, paused = False, boards = boards }
        , Cmd.batch <|
            [ -- give the board the browser's focus
              -- ignore errors from Dom.focus
              Task.attempt (\_ -> None) <| Dom.focus "tetris"

            -- generate enough tetrominos for all players
            , randTetrominos boards
            , randTetrominos boards
            ]
        )


initBoards nPlayers =
    List.repeat nPlayers
        { score = 0
        , clearedRows = []
        , gameOver = False
        , tetris = Nothing
        , speed = ( 0, Normal )
        , grid =
            initialize boardHeight
                boardWidth
                (\row col ->
                    if row == boardHeight - 1 || col == 0 || col == boardWidth - 1 then
                        Edge
                    else
                        Empty
                )
        , tetromino = Nothing
        , nextTetromino = Nothing
        }


shapeIndexGenerator nPlayers =
    Random.Pcg.list nPlayers <|
        Random.Pcg.map (Maybe.withDefault "I") <|
            Random.Pcg.sample shapeNames


randTetrominos boards =
    Random.Pcg.generate
        (\names ->
            AddTetrominos <|
                List.map2
                    (\name board ->
                        if board.tetromino == Nothing then
                            Just name
                        else
                            Nothing
                    )
                    names
                    boards
        )
        (shapeIndexGenerator <| List.length boards)


makeTetromino : ShapeName -> Int -> Maybe Tetromino
makeTetromino name degrees =
    unwrapMaybeWith Nothing
        (\shape ->
            Just
                { name = name
                , degrees = degrees
                , shape = shape
                , pos =
                    { x = boardWidth // 2 - (Array2D.columns shape // 2) -- TODO must conform to standard
                    , y = boardMargin
                    }
                }
        )
    <|
        getShape ( name, degrees )


subscriptions : Model -> Sub Input
subscriptions model =
    Time.every (second / fps) Tick


onKeyUp : (Int -> Input) -> Attribute Input
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


onKeyDown : (Int -> Input) -> Attribute Input
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


toTagger cs key =
    let
        -- convert [ ( [a, b], c ) ]
        -- to [ (a, (0, c)), (b, (1, c)) ]
        process =
            Dict.fromList
                << List.foldr
                    (\( orderedControls, c ) controlDict ->
                        (List.indexedMap
                            (\playerNum control ->
                                ( control, ( playerNum, c ) )
                            )
                            orderedControls
                        )
                            ++ controlDict
                    )
                    []

        controls =
            process cs
    in
        unwrapMaybeWith None PlayerInputs (Dict.get key controls)


holdControls =
    toTagger
        [ ( [ 38, 87 ]
          , -- up arrow
            HardDrop
          )
        , ( [ 40, 83 ]
          , -- down arrow
            SoftDrop
          )
        ]


upControls key =
    if key == 80 then
        Pause
    else
        toTagger
            [ ( [ 37, 65 ]
              , -- left arrow
                Translate Left
              )
            , ( [ 39, 68 ]
              , -- right arrow
                Translate Right
              )
            , ( [ 190, 88 ]
              , -- period
                Rotate Right
              )
            , ( [ 188, 90 ]
              , -- comma
                Rotate Left
              )
            ]
            key


view : Model -> Html Input
view model =
    Html.body []
        [ Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "style.css"
            ]
            []
        , main_
            [ id "tetris"
            , attribute "tabindex" "0"
            , onKeyDown holdControls
            , onKeyUp upControls
            ]
          <|
            List.indexedMap viewBoard model.boards
        ]


viewBoard playerNum board =
    let
        grid =
            -- deleteEdges <|
            Array2D.indexedMap
                (\row col c ->
                    let
                        has =
                            case board.tetromino of
                                Just tetr ->
                                    -- if a tetromino exists
                                    -- and it is located at this position, then draw it
                                    case Array2D.get (row - tetr.pos.y) (col - tetr.pos.x) tetr.shape of
                                        Just cell ->
                                            cell /= Empty

                                        _ ->
                                            False

                                _ ->
                                    False
                    in
                        span []
                            [ text
                                << (\t -> t ++ " ")
                              <|
                                if row < 2 then
                                    "-"
                                else if c == Edge then
                                    "•"
                                else if c == Empty then
                                    if has then
                                        "o"
                                    else
                                        "·"
                                else if has then
                                    -- GAME OVER!!!
                                    "G"
                                else
                                    "X"
                            ]
                )
                board.grid

        makeButtons =
            List.map2
                (\action buttonText ->
                    button
                        [ onMouseDown <| PlayerInputs ( playerNum, action ) ]
                        [ text buttonText ]
                )
    in
        div
            [ id <| "player" ++ toString (1 + playerNum)
            , class "board"
            ]
        <|
            [ h2 []
                [ text <|
                    "Player "
                        ++ (toString <| 1 + playerNum)
                        ++ if unwrapMaybeWith False (\t -> True) board.tetris then
                            ": TETRIS!"
                           else
                            ""
                ]
            , h3 [] [ text <| "Score " ++ toString board.score ]
            ]
                ++ [ div [] <|
                        Array.toList <|
                            Array.map (\row -> div [] <| Array.toList row) grid.data
                   , div [] <|
                        makeButtons
                            [ Translate Left, Rotate Left, Rotate Right, Translate Right ]
                            [ "↼", "⤺", "⤻", "⇀" ]
                   , div [] <|
                        makeButtons
                            [ SoftDrop, HardDrop ]
                            [ "fast", "drop" ]
                   ]


update : Input -> Model -> ( Model, Cmd Input )
update input ({ boards, frame } as m) =
    let
        newModel =
            case input of
                -- add a tetromino given a random name
                -- let player see the next tetromino
                -- if no next, add next
                -- if no current and next, then add next, replace next with new
                -- if current and next, do nothing
                AddTetrominos newNexts ->
                    { m | boards = List.map2 (addTetromino) newNexts boards }

                Tick tNew ->
                    if not m.paused then
                        { m
                            | frame = frame + 1
                            , boards = List.map (updateScore frame << dropTetromino frame) boards
                        }
                    else
                        m

                Pause ->
                    { m | paused = not m.paused }

                PlayerInputs ( n, ps ) ->
                    { m
                        | boards =
                            List.indexedMap
                                (\playerNum board ->
                                    if playerNum == n then
                                        processInput frame ps board
                                    else
                                        board
                                )
                                boards
                    }

                None ->
                    m

        anyNeedTetrominos =
            List.any
                (\b -> not b.gameOver && b.tetromino == Nothing)
                newModel.boards
    in
        ( newModel
        , if anyNeedTetrominos then
            randTetrominos newModel.boards
          else
            Cmd.none
        )


tetrisCounter =
    -- show "Tetris!" for 5 seconds
    5


updateScore frame board =
    if unwrapMaybeWith False (\t -> frame - t > tetrisCounter * fps) board.tetris then
        { board | tetris = Nothing }
    else
        board


addTetromino newNext board =
    if board.gameOver || newNext == Nothing then
        board
    else
        let
            add newTetrName =
                if board.tetromino /= Nothing then
                    board
                else
                    let
                        newTetr =
                            makeTetromino newTetrName 0
                    in
                        -- do not add if it will collide
                        -- TODO game over!
                        if unwrapMaybeWith True (checkCollision board.grid) newTetr then
                            { board | gameOver = True }
                        else
                            { board | tetromino = newTetr, nextTetromino = newNext }
        in
            case board.nextTetromino of
                Nothing ->
                    { board | nextTetromino = newNext }

                Just newTetrName ->
                    if board.tetromino /= Nothing then
                        board
                    else
                        add newTetrName


checkCollision : Grid -> Tetromino -> Bool
checkCollision grid { shape, pos } =
    let
        mask =
            -- check all cells of the shape for collision with the grid
            Array2D.indexedMap
                (\row col c ->
                    (c /= Empty)
                        && ((Maybe.withDefault Empty <|
                                Array2D.get (row + pos.y) (col + pos.x) grid
                            )
                                /= Empty
                           )
                )
                shape
    in
        -- check if any cells collide with the non-empty blocks of the grid
        Array.foldl (||) False <| Array.map (Array.foldl (||) False) mask.data


merge frame { shape, pos } ({ grid } as board) =
    clearRows frame
        { board
            | grid =
                Array2D.indexedMap
                    -- merge shape with grid
                    (\row col gridCell ->
                        if
                            -- check if this is an area of interest
                            not <|
                                (row >= pos.y && row < pos.y + Array2D.rows shape)
                                    && (col >= pos.x && col < pos.x + Array2D.columns shape)
                        then
                            gridCell
                        else
                            -- get the part
                            let
                                shapeCell =
                                    Array2D.get (row - pos.y) (col - pos.x) shape
                            in
                                -- add any full blocks in the part to the grid
                                unwrapMaybeWith Empty
                                    (\c ->
                                        if c == Empty then
                                            gridCell
                                        else
                                            c
                                    )
                                    shapeCell
                    )
                    grid
        }


clearRows : Int -> Board -> Board
clearRows frame ({ grid } as board) =
    -- check each row, starting with row 0
    -- if row is all non-empty
    -- then delete it
    -- and append row to top
    let
        emptyRow =
            Array.set 0 Edge <| Array.set (boardWidth - 1) Edge <| Array.repeat boardWidth Empty

        addEmptyRow =
            Array2D.fromArray << Array.append (Array.repeat 1 emptyRow) << .data

        hasEmptyCell =
            Array.foldr
                (\cell hasEmpty ->
                    if hasEmpty then
                        True
                    else
                        cell == Empty
                )
                False

        go row b =
            if row >= boardHeight - boardMargin then
                b
            else
                go (row + 1) <|
                    case
                        Array2D.getRow row b.grid
                    of
                        Just arr ->
                            if hasEmptyCell arr then
                                b
                            else
                                { b
                                    | grid = addEmptyRow <| Array2D.deleteRow row b.grid
                                    , clearedRows = row :: b.clearedRows
                                }

                        Nothing ->
                            b
    in
        addScore frame <| go 0 { board | clearedRows = [] }


addScore frame ({ clearedRows, tetris } as board) =
    let
        gotTetris =
            List.length clearedRows >= 4
    in
        { board
            | score =
                board.score
                    + (if gotTetris then
                        8
                       else
                        List.length clearedRows
                      )
            , tetris =
                if gotTetris then
                    Just frame
                else
                    tetris
        }


stopSoftDrop frame board =
    -- only soft drop for a single frame
    if frame > Tuple.first board.speed then
        { board | speed = ( 0, Normal ) }
    else
        board


dropTetromino : Int -> Board -> Board
dropTetromino frame board =
    stopSoftDrop frame <|
        if Tuple.second board.speed == Normal && frame % normalSpeed /= 0 then
            board
        else
            case board.tetromino of
                Just ({ shape, pos } as oldTetromino) ->
                    let
                        collision =
                            checkCollision board.grid newTetromino

                        newTetromino =
                            { oldTetromino
                                | pos = { pos | y = pos.y + 1 }
                            }
                    in
                        if collision then
                            merge frame oldTetromino { board | tetromino = Nothing }
                        else
                            { board | tetromino = Just newTetromino }

                _ ->
                    board


hardDrop board =
    board


processInput frame playerInputs board =
    case playerInputs of
        HardDrop ->
            hardDrop board

        SoftDrop ->
            { board | speed = ( frame, Fast ) }

        Rotate d ->
            let
                newTetromino =
                    unwrapMaybeWith Nothing
                        (\oldT ->
                            unwrapMaybeWith Nothing (\n -> Just { n | pos = oldT.pos }) <|
                                makeTetromino
                                    (oldT.name)
                                    ((oldT.degrees
                                        + if d == Left then
                                            90
                                          else
                                            270
                                     )
                                        % 360
                                    )
                        )
                        board.tetromino
            in
                tryToAdd newTetromino board

        Translate d ->
            let
                newTetromino =
                    unwrapMaybeWith Nothing
                        (\({ shape, pos } as oldTetromino) ->
                            Just
                                { oldTetromino
                                    | pos =
                                        { pos
                                            | x =
                                                (if d == Left then
                                                    pos.x - 1
                                                 else
                                                    pos.x + 1
                                                )
                                        }
                                }
                        )
                        board.tetromino
            in
                tryToAdd newTetromino board


tryToAdd newTetromino board =
    let
        collision =
            unwrapMaybeWith True (checkCollision board.grid) newTetromino
    in
        if collision then
            board
        else
            { board | tetromino = newTetromino }


getShape s =
    Dict.get s shapes


shapes =
    let
        convert c =
            if c == 0 then
                Empty
            else
                Full

        conv =
            List.map (List.map convert)

        primitives =
            -- the blank rows and columns are for the Tetris Super Rotation System
            [ ( "I", conv [ [ 0, 0, 0, 0 ], [ 1, 1, 1, 1 ], [ 0, 0, 0, 0 ], [ 0, 0, 0, 0 ] ] )
            , ( "S", conv [ [ 0, 1, 1 ], [ 1, 1, 0 ], [ 0, 0, 0 ] ] )
            , ( "Z", conv [ [ 1, 1, 0 ], [ 0, 1, 1 ], [ 0, 0, 0 ] ] )
            , ( "L", conv [ [ 0, 0, 1 ], [ 1, 1, 1 ], [ 0, 0, 0 ] ] )
            , ( "J", conv [ [ 1, 0, 0 ], [ 1, 1, 1 ], [ 0, 0, 0 ] ] )
            , ( "T", conv [ [ 0, 1, 0 ], [ 1, 1, 1 ], [ 0, 0, 0 ] ] )
            , ( "O", conv [ [ 1, 1 ], [ 1, 1 ] ] )
            ]

        -- rotate a 2D array
        rotate p =
            let
                newRowLen =
                    unwrapMaybeWith 0 List.length <| List.head p
            in
                -- [a,b], [c,d] -> [c,a], [d, b]
                List.foldl
                    (List.map2 (::))
                    (List.repeat newRowLen [])
                    p
    in
        Dict.fromList <|
            List.foldr
                (\( name, prim ) l ->
                    -- generate all rotated values of each primitive
                    -- inserts duplicates :(
                    let
                        ( _, _, allShapeRotations ) =
                            List.foldr
                                -- add the shape, then rotate it
                                (\_ ( p, degrees, l ) ->
                                    ( rotate p
                                    , degrees - 90
                                    , ( ( name, degrees % 360 ), Array2D.fromList p )
                                        :: l
                                    )
                                )
                                -- start by adding the non-rotated shape
                                ( prim, 360, l )
                                (List.repeat 4 Nothing)
                    in
                        List.append allShapeRotations l
                )
                []
                primitives


shapeNames =
    [ "I", "S", "Z", "L", "J", "T", "O" ]
