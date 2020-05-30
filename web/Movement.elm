module Movement exposing (Direction(..), Movement, add, default, isMotion, remove, setPosition, updatePosition)

import Force exposing (Force)
import Maybe.Extra exposing (isJust, orList)
import Position exposing (Pixel)
import Time exposing (Posix)


type alias Movement =
    { position : Pixel
    , keys :
        { up : Maybe Force
        , down : Maybe Force
        , left : Maybe Force
        , right : Maybe Force
        }
    }


default : Movement
default =
    { position =
        { x = 0.0
        , y = 0.0
        }
    , keys =
        { up = Nothing
        , down = Nothing
        , left = Nothing
        , right = Nothing
        }
    }


type Direction
    = Up
    | Down
    | Left
    | Right


add : Posix -> Direction -> Movement -> Movement
add posix direction { position, keys } =
    case direction of
        Up ->
            case keys.up of
                Just _ ->
                    { position = position, keys = keys }

                Nothing ->
                    { position = position, keys = { keys | up = Just <| Force.new posix } }

        Down ->
            case keys.down of
                Just _ ->
                    { position = position, keys = keys }

                Nothing ->
                    { position = position, keys = { keys | down = Just <| Force.new posix } }

        Left ->
            case keys.left of
                Just _ ->
                    { position = position, keys = keys }

                Nothing ->
                    { position = position, keys = { keys | left = Just <| Force.new posix } }

        Right ->
            case keys.right of
                Just _ ->
                    { position = position, keys = keys }

                Nothing ->
                    { position = position, keys = { keys | right = Just <| Force.new posix } }


remove : Direction -> Movement -> Movement
remove direction { position, keys } =
    case direction of
        Up ->
            { position = position, keys = { keys | up = Nothing } }

        Down ->
            { position = position, keys = { keys | down = Nothing } }

        Left ->
            { position = position, keys = { keys | left = Nothing } }

        Right ->
            { position = position, keys = { keys | right = Nothing } }


setPosition : Pixel -> Movement -> Movement
setPosition pos movement =
    { movement | position = pos }


updatePosition : Float -> Posix -> Movement -> Movement
updatePosition size posix { position, keys } =
    let
        upMove =
            Maybe.map (Force.value posix) keys.up
                |> Maybe.withDefault 0.0

        downMove =
            Maybe.map (Force.value posix) keys.down
                |> Maybe.withDefault 0.0

        leftMove =
            Maybe.map (Force.value posix) keys.left
                |> Maybe.withDefault 0.0

        rightMove =
            Maybe.map (Force.value posix) keys.right
                |> Maybe.withDefault 0.0

        pos =
            { x = position.x + (size / 10) * (leftMove - rightMove)
            , y = position.y + (size / 10) * (upMove - downMove)
            }
    in
    { position = { x = pos.x, y = pos.y }
    , keys = keys
    }


isMotion : Movement -> Bool
isMotion movement =
    orList
        [ movement.keys.up
        , movement.keys.down
        , movement.keys.left
        , movement.keys.right
        ]
        |> isJust
