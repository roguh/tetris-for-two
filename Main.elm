-- TODO: strip empty rows from the held and next piece previews
-- TODO fix cleared animation (fancier transitions)
-- TODO make tetrominos look prettier (gradients, ghostier ghost piece)
-- docs:
-- TODO instructions, with nice keyboard picture; write about page reloading
-- TODO take screenshots
-- speed:
-- TODO have less code (half the number of code lines?)
-- config:
-- read from inputs when in gamestart or gameover
-- 3 TODO init config: adjust player count
-- 3 TODO init config: adjust teams
-- 3 TODO init config: adjust field height
-- scoring:
-- 3 TODO detect GAME OVER (offer to restart)
-- 3 TODO detect START GAME
-- 3 TODO better rewards for combos (timer)
-- TODO better reward for special moves (T flips)
-- tetromino generation:
-- TODO replace holdTimer with drop detector, do not allow another hold until a merge
-- 1 TODO show next 3 pieces
-- 1 TODO generate random tetrominos using 7system (bags of 7)
-- controls:
-- 2 TODO BIG EASY TO USE TOUCH CONTROLS (swipe horizontally, swipe up, swipe down, tap)
-- use mpizenberg/elm-touch-events
-- competition
-- TODO different number of team members on each team (single player, two player coop or compete)
-- TODO if competing, add nuisance row to all opponents when a tetris is hit
{-
   special move: combos

   2 or more tetris in a row (less than X seconds)
-}
{-
   special move: 4-wide

   4 wide gap, start clearing it quickly

   XX    XX
   XX    XX
   XX    XX
   XX    XX
   XX    XX
   XX    XX
   XX    XX
   XX    XX


-}
{-
   special move: T-spin


   from here:

    -
   --X
    -
   X X

   to here:

     X
   ---
   X-X

   on rotate with tap: if failed try the other direction

   must clear rows! (max 2)

   clear all (after > 4)
-}


module Main exposing (main)

import Window exposing (Size)
import Html exposing (Html, Attribute, Attribute, main_, button, div, text, h3, h2, span)
import Html.Attributes exposing (attribute, class, id, style)
import Time exposing (Time, second)
import Array
import Random.Pcg
import Dict exposing (Dict)
import Array2D exposing (Array2D, initialize)
import Task
import Keyboard
import AnimationFrame


main =
    Html.program
        { init = init numPlayers -- TODO more configuration options
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


isNothing x =
    case x of
        Nothing ->
            True

        _ ->
            False


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
    | Full ShapeName
    | Empty


type alias Grid =
    Array2D Block


type alias Pos =
    { x : Float, y : Float }


type alias ShapeName =
    -- elm does not support user-defined typeclasses, no ShapeName ADT in a dictionary
    String


type alias Tetromino =
    { name : ShapeName, degrees : Int, shape : Grid, pos : Pos }


type DropSpeed
    = Fast
    | Normal


tetrisBonusName =
    "tetris"


clearedBonusName =
    "cleared board"


rowClearedBonusName =
    "row cleared"


bonuses =
    [ rowClearedBonusName, tetrisBonusName, clearedBonusName ]


type alias BonusName =
    String


type alias Board =
    { score : Int
    , gameOver : Bool
    , clearedRows : List Int
    , bonuses : Dict BonusName Time -- bonus name, when was the last time this bonus was achieved?
    , speed : DropSpeed
    , grid : Grid
    , translate : Maybe Dir
    , ghost : Maybe Tetromino
    , tetromino : Maybe Tetromino -- should replace with a list of tetrominos
    , held : Maybe ShapeName
    , holdTimer : Time
    , nextTetromino : Maybe ShapeName
    }


type alias Model =
    { windowSize : Size, dt : Time, tPrev : Maybe Time, paused : Bool, boards : List Board }


type Dir
    = Left
    | Right


type PlayerInput
    = Translate Dir
    | StopTranslate
    | Rotate Dir
    | SoftDrop
    | StopSoftDrop
    | HardDrop
    | Hold


type alias BoardID =
    Int


type Input
    = PlayerInputs BoardID PlayerInput
    | Tick Time
    | Resize Size
    | Pause
    | AddTetrominos (List (Maybe ShapeName))
    | None


boardHeight =
    -- TODO make height configurable
    -- note top 2 rows must be hidden
    30 + boardMargin * 2


boardWidth =
    10 + boardMargin * 2


boardMargin =
    1


numPlayers =
    -- TODO make this configurable
    2


init nPlayers =
    let
        boards =
            initBoards nPlayers
    in
        ( { windowSize = { width = 200, height = 200 }, dt = 0.1, tPrev = Nothing, paused = False, boards = boards }
        , Cmd.batch <|
            [ -- generate enough tetrominos for all players
              randTetrominos boards
            , randTetrominos boards
            , Task.attempt
                (\res ->
                    case res of
                        Ok sz ->
                            Resize sz

                        _ ->
                            None
                )
                Window.size
            ]
        )


initBoards nPlayers =
    List.repeat nPlayers
        { score = 0
        , clearedRows = []
        , gameOver = False
        , bonuses = Dict.empty
        , speed = Normal
        , grid =
            initialize boardHeight
                boardWidth
                (\row col ->
                    if row == boardHeight - 1 || col == 0 || col == boardWidth - 1 then
                        Edge
                    else
                        Empty
                )
        , translate = Nothing
        , ghost = Nothing
        , tetromino = Nothing
        , held = Nothing
        , holdTimer = holdLimit * 2
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
                    { x = toFloat <| boardWidth // 2 - (Array2D.columns shape // 2) -- TODO must conform to standard
                    , y = toFloat <| boardMargin
                    }
                }
        )
    <|
        getShape ( name, degrees )


subscriptions : Model -> Sub Input
subscriptions model =
    Sub.batch
        [ AnimationFrame.times Tick
        , Window.resizes Resize
        , Keyboard.downs downControls
        , Keyboard.ups upControls
        ]


keyboardHandler array key =
    List.foldl
        (\( orderedControls, act ) action ->
            if action == None then
                Tuple.first <|
                    List.foldl
                        (\control ( prev, playerNum ) ->
                            if key == control then
                                ( PlayerInputs playerNum act
                                , playerNum
                                )
                            else
                                ( prev, playerNum - 1 )
                        )
                        ( None, (List.length orderedControls) - 1 )
                        orderedControls
            else
                action
        )
        None
        array


upControls =
    keyboardHandler
        [ ( [ 40, 83 ]
          , -- down arrow
            StopSoftDrop
          )
        , ( [ 37, 65 ]
          , -- left arrow
            StopTranslate
          )
        , ( [ 39, 68 ]
          , -- right arrow
            StopTranslate
          )
        ]


downControls key =
    if key == 80 then
        Pause
    else
        keyboardHandler
            [ ( [ 37, 65 ]
              , -- left arrow, W
                Translate Left
              )
            , ( [ 39, 68 ]
              , -- right arrow, D
                Translate Right
              )
            , ( [ 190, 88 ]
              , -- period, X
                Rotate Right
              )
            , ( [ 188, 90 ]
              , -- comma, Z
                Rotate Left
              )
            , ( [ 40, 83 ]
              , -- down arrow, S
                SoftDrop
              )
            , ( [ 38, 87 ]
              , -- up arrow, W
                HardDrop
              )
            , ( [ 76, 84 ]
                -- L, T
              , Hold
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
            , class
                (if model.paused then
                    "paused"
                 else
                    ""
                )
            ]
          <|
            [ div []
                [ text <| "FPS: " ++ toString (round <| 1 / (Time.inSeconds model.dt)) ]
            , div [ class "messages" ]
                [ if model.paused then
                    text "Paused!"
                  else
                    text ""
                ]
            ]
                ++ (List.indexedMap (viewBoard <| min model.windowSize.width model.windowSize.height) model.boards)
        ]


cellToAttrs cell =
    "cell"
        :: case cell of
            Empty ->
                [ "empty" ]

            Edge ->
                [ "edge" ]

            Full f ->
                [ "tetromino", "tetromino" ++ f ]


viewBoard minSize playerNum board =
    let
        drawTetromino i =
            case i of
                Nothing ->
                    []

                Just name ->
                    case makeTetromino name 0 of
                        Just tetrs ->
                            Array.toList <|
                                Array.map (div [] << Array.toList)
                                    (Array2D.map (\cell -> div (List.map class <| cellToAttrs cell) []) tetrs.shape).data

                        Nothing ->
                            []

        grid =
            -- deleteEdges <|
            Array2D.indexedMap
                (\row col cell ->
                    let
                        isFull =
                            cellName /= Nothing

                        thisCellName =
                            case cell of
                                Full cellName ->
                                    Just cellName

                                _ ->
                                    Nothing

                        cellName =
                            unwrapMaybeWith thisCellName
                                (\tetr ->
                                    -- if a tetromino exists
                                    -- and it is located at this position, then draw it
                                    case Array2D.get (row - pos2int tetr.pos.y) (col - pos2int tetr.pos.x) tetr.shape of
                                        -- either there's a Full piece at this location
                                        Just (Full shapeName) ->
                                            Just shapeName

                                        _ ->
                                            thisCellName
                                )
                                board.tetromino

                        isGhost =
                            unwrapMaybeWith False
                                (\ghost ->
                                    case Array2D.get (row - pos2int ghost.pos.y) (col - pos2int ghost.pos.x) ghost.shape of
                                        Just (Full _) ->
                                            True

                                        _ ->
                                            False
                                )
                                board.ghost

                        attrs =
                            List.map class <|
                                (if row < 2 then
                                    [ "topMargin" ]
                                 else if isGhost then
                                    [ "ghost", unwrapMaybeWith "" (\t -> "tetromino" ++ t.name) board.ghost ]
                                 else
                                    cellToAttrs cell
                                )
                                    ++ cellToAttrs cell
                    in
                        div attrs []
                )
                board.grid

        pi =
            PlayerInputs playerNum
    in
        div
            [ id <| "team" ++ toString (1 + playerNum % 2)
            , class "board"
            ]
        <|
            [ h2 []
                [ text <|
                    "Player "
                        ++ (toString <| 1 + playerNum)
                        ++ (List.foldl (\s t -> s ++ " " ++ t) "" <| Dict.keys board.bonuses)
                ]
            , h3 [] [ text <| "Score " ++ toString board.score ]
            , div
                [ class "grid"
                , style [ ( "font-size", (toString <| minSize // boardHeight // 2) ++ "px" ) ]
                ]
              <|
                Array.toList <|
                    Array.indexedMap
                        (\rowIndex row ->
                            div
                                [ if List.member rowIndex board.clearedRows then
                                    class "clearedRow"
                                  else
                                    class ""
                                ]
                            <|
                                Array.toList row
                        )
                        grid.data
            , div [ class "info" ] <|
                [ div [] [ text "Next:" ]
                , div [ class "grid", class "next" ] <| drawTetromino board.nextTetromino
                ]
                    ++ (if isNothing board.held then
                            []
                        else
                            [ div [] [ text "Holding: " ]
                            , div [ class "grid", class "held" ] <| drawTetromino board.held
                            ]
                       )

            {-
               , div []
                   [ button [ onMouseDown (pi <| Translate Left), onMouseUp (pi StopTranslate) ] [ text "<" ]
                   , button [ onMouseDown (pi <| Translate Right), onMouseUp (pi StopTranslate) ] [ text ">" ]
                   , button [ onMouseDown (pi <| Rotate Left) ] [ text "r" ]
                   ]
               , div []
                   [ button [ onMouseDown (pi SoftDrop), onMouseUp (pi StopSoftDrop) ] [ text "v" ]
                   , button [ onMouseDown (pi HardDrop) ] [ text "V" ]
                   ]
            -}
            ]


update : Input -> Model -> ( Model, Cmd Input )
update input ({ boards } as m) =
    let
        newModel =
            case input of
                Resize windowSize ->
                    { m | windowSize = windowSize }

                -- add a tetromino given a random name
                -- let player see the next tetromino
                -- if no next, add next
                -- if no current and next, then add next, replace next with new
                -- if current and next, do nothing
                AddTetrominos newNexts ->
                    { m | boards = List.map2 (addTetromino) newNexts boards }

                Tick tNew ->
                    if not m.paused then
                        let
                            newdt =
                                unwrapMaybeWith 0.01 (\tPrev -> tNew - tPrev) m.tPrev
                        in
                            { m
                                | dt = newdt
                                , tPrev = Just tNew
                                , boards = List.map ((\b -> { b | holdTimer = b.holdTimer + Time.inSeconds newdt }) << updateScore tNew << updateGhost << dropTetromino newdt << translateTetromino newdt << (\b -> { b | clearedRows = [] })) boards
                            }
                    else
                        m

                Pause ->
                    { m | paused = not m.paused }

                PlayerInputs n ps ->
                    { m
                        | boards =
                            List.indexedMap
                                (\playerNum board ->
                                    if playerNum == n then
                                        processInput m.dt ps board
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


bonusCounter =
    -- show "Tetris!" for 5 seconds
    5


updateScore t board =
    addScore t <|
        -- remove bonuses that have been around for more than bonusCounter seconds
        { board | bonuses = Dict.filter (\_ time -> Time.inSeconds time + bonusCounter > Time.inSeconds t) board.bonuses }


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


finalPosition : Grid -> Tetromino -> Tetromino
finalPosition grid ({ shape, pos } as tetr) =
    -- scan from current row downward until the shape collides
    let
        go row =
            if row > boardHeight then
                0
            else if checkCollision grid { tetr | pos = { x = pos.x, y = row } } then
                row
            else
                go (row + 1)
    in
        { tetr | pos = { pos | y = go pos.y - 1 } }


updateGhost : Board -> Board
updateGhost board =
    let
        newGhost =
            unwrapMaybeWith Nothing (\tetr -> Just <| finalPosition board.grid tetr) board.tetromino
    in
        { board | ghost = newGhost }


pos2int =
    truncate


checkCollision : Grid -> Tetromino -> Bool
checkCollision grid { shape, pos } =
    let
        mask =
            -- check all cells of the shape for collision with the grid
            Array2D.indexedMap
                (\row col c ->
                    (c /= Empty)
                        && ((Maybe.withDefault Empty <|
                                Array2D.get (row + pos2int pos.y) (col + pos2int pos.x) grid
                            )
                                /= Empty
                           )
                )
                shape
    in
        -- check if any cells collide with the non-empty blocks of the grid
        Array.foldl (||) False <| Array.map (Array.foldl (||) False) mask.data


merge { shape, pos } ({ grid } as board) =
    clearRows
        { board
            | grid =
                Array2D.indexedMap
                    -- merge shape with grid
                    (\row col gridCell ->
                        let
                            x =
                                pos2int pos.x

                            y =
                                pos2int pos.y
                        in
                            if
                                -- check if this is an area of interest
                                not <|
                                    (row >= y && row < y + Array2D.rows shape)
                                        && (col >= x && col < x + Array2D.columns shape)
                            then
                                gridCell
                            else
                                -- add any full blocks in the part to the grid
                                unwrapMaybeWith Empty
                                    (\shapeCell ->
                                        if shapeCell == Empty then
                                            gridCell
                                        else
                                            shapeCell
                                    )
                                    -- get the part
                                    (Array2D.get (row - y) (col - x) shape)
                    )
                    grid
        }


clearRows : Board -> Board
clearRows ({ grid } as board) =
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
        go 0 { board | clearedRows = [] }


addScore t ({ clearedRows, bonuses } as board) =
    let
        gotTetris =
            List.length clearedRows >= 4
    in
        { board
            | score =
                board.score
                    + (if gotTetris then
                        4
                       else
                        1
                      )
                    * List.length clearedRows
            , bonuses =
                (if List.length clearedRows > 0 then
                    Dict.insert rowClearedBonusName t
                 else
                    \t -> t
                )
                <|
                    if gotTetris then
                        Dict.insert tetrisBonusName t bonuses
                    else
                        bonuses
        }


translateTetromino : Time -> Board -> Board
translateTetromino dt board =
    case board.translate of
        Nothing ->
            board

        Just d ->
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
                                                    pos.x - tetrominoHorizSpeed dt
                                                 else
                                                    pos.x + tetrominoHorizSpeed dt
                                                )
                                        }
                                }
                        )
                        board.tetromino
            in
                tryToAdd newTetromino board


tetrominoHorizSpeed dt =
    -- a tetromino can horizontally travel the board in 1 second
    (Time.inSeconds dt) * (toFloat boardWidth)


tetrominoSpeed speed dt =
    let
        tSpeed =
            case speed of
                Fast ->
                    -- a tetromino should travel 10 blocks in 1 second
                    vertFastSpeed

                Normal ->
                    vertSpeed
    in
        (Time.inSeconds dt) * tSpeed


vertSpeed =
    3


vertFastSpeed =
    12


horizSpeed =
    toFloat boardWidth


holdLimit =
    -- TODO change this mechanism
    5


dropTetromino : Float -> Board -> Board
dropTetromino dt board =
    case board.tetromino of
        Just ({ shape, pos } as oldTetromino) ->
            let
                collision =
                    checkCollision board.grid newTetromino

                newTetromino =
                    { oldTetromino
                        | pos = { pos | y = pos.y + tetrominoSpeed board.speed dt }
                    }
            in
                if collision then
                    merge oldTetromino { board | tetromino = Nothing }
                else
                    { board | tetromino = Just newTetromino }

        _ ->
            board


hardDrop board =
    unwrapMaybeWith board
        (\tetr ->
            { board
                | ghost = Nothing
                , tetromino = Just <| finalPosition board.grid tetr
            }
        )
        board.tetromino


processInput dt playerInputs board =
    case playerInputs of
        HardDrop ->
            hardDrop board

        SoftDrop ->
            { board | speed = Fast }

        StopSoftDrop ->
            { board | speed = Normal }

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

        Hold ->
            if board.holdTimer < holdLimit then
                board
            else
                let
                    b =
                        case board.tetromino of
                            Nothing ->
                                board

                            Just tetr ->
                                { board
                                    | held = Just tetr.name
                                    , tetromino = unwrapMaybeWith Nothing (\h -> makeTetromino h 0) board.held
                                }
                in
                    { b | holdTimer = 0 }

        StopTranslate ->
            { board | translate = Nothing }

        Translate d ->
            { board | translate = Just d }


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
        convert name c =
            if c == 0 then
                Empty
            else
                Full name

        conv ( name, grid ) =
            ( name, List.map (List.map (convert name)) grid )

        primitives =
            -- the blank rows and columns are for the Tetris Super Rotation System
            [ conv ( "I", [ [ 0, 0, 0, 0 ], [ 1, 1, 1, 1 ], [ 0, 0, 0, 0 ], [ 0, 0, 0, 0 ] ] )
            , conv ( "S", [ [ 0, 1, 1 ], [ 1, 1, 0 ], [ 0, 0, 0 ] ] )
            , conv ( "Z", [ [ 1, 1, 0 ], [ 0, 1, 1 ], [ 0, 0, 0 ] ] )
            , conv ( "L", [ [ 0, 0, 1 ], [ 1, 1, 1 ], [ 0, 0, 0 ] ] )
            , conv ( "J", [ [ 1, 0, 0 ], [ 1, 1, 1 ], [ 0, 0, 0 ] ] )
            , conv ( "T", [ [ 0, 1, 0 ], [ 1, 1, 1 ], [ 0, 0, 0 ] ] )
            , conv ( "O", [ [ 1, 1 ], [ 1, 1 ] ] )
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
