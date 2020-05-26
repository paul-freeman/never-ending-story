module Main2 exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp)
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Border as Border
import Html.Events.Extra.Wheel as Wheel exposing (onWheel)
import Json.Decode as Decode
import List.Extra exposing (lift3)
import Movement exposing (Movement)
import Svg exposing (Svg, svg)
import Svg.Attributes as SA
import Task
import Time exposing (Posix)


type alias Model =
    { map : Dict ( Int, Int, Int ) Content
    , movement : Movement
    , width : Int
    , height : Int
    , size : Float
    , minSize : Float
    , maxSize : Float
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Sub.map (KeyDown Nothing) (onKeyDown keyDecoder)
                    , Sub.map KeyUp (onKeyUp keyDecoder)
                    , onAnimationFrame Tick
                    ]
        , view = \model -> E.layout [] (mainLayout model)
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { movement = Movement.default
      , map = Dict.empty
      , width = 480
      , height = 480
      , size = 20.784
      , minSize = 20
      , maxSize = 100
      }
    , Cmd.none
    )


type Msg
    = Tick Posix
    | KeyDown (Maybe Posix) Direction
    | KeyUp Direction
    | Zoom Wheel.Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | movement = Movement.updatePosition model.size time model.movement }, Cmd.none )

        KeyDown mTime key ->
            case mTime of
                Nothing ->
                    ( model, Task.perform (\posix -> KeyDown (Just posix) key) Time.now )

                Just posix ->
                    case key of
                        Up ->
                            ( { model | movement = Movement.add posix Movement.Up model.movement }, Cmd.none )

                        Down ->
                            ( { model | movement = Movement.add posix Movement.Down model.movement }, Cmd.none )

                        Left ->
                            ( { model | movement = Movement.add posix Movement.Left model.movement }, Cmd.none )

                        Right ->
                            ( { model | movement = Movement.add posix Movement.Right model.movement }, Cmd.none )

                        Other ->
                            ( model, Cmd.none )

        KeyUp key ->
            case key of
                Up ->
                    ( { model | movement = Movement.remove Movement.Up model.movement }, Cmd.none )

                Down ->
                    ( { model | movement = Movement.remove Movement.Down model.movement }, Cmd.none )

                Left ->
                    ( { model | movement = Movement.remove Movement.Left model.movement }, Cmd.none )

                Right ->
                    ( { model | movement = Movement.remove Movement.Right model.movement }, Cmd.none )

                Other ->
                    ( model, Cmd.none )

        Zoom e ->
            let
                d =
                    model.size - e.deltaY / 40

                newSize =
                    min (max d model.minSize) model.maxSize
            in
            ( { model
                | movement =
                    Movement.setPosition
                        (positionAtNewSize model.size newSize model.movement.position)
                        model.movement
                , size = newSize
              }
            , Cmd.none
            )


mainLayout : Model -> Element Msg
mainLayout model =
    E.column
        [ E.width E.fill
        , E.height E.fill
        ]
        [ E.row
            [ E.centerX
            ]
            [ E.el
                [ Border.width 3
                , E.htmlAttribute (Wheel.onWheel Zoom)
                ]
                (E.html <|
                    svg
                        [ SA.width (String.fromInt model.width)
                        , SA.height (String.fromInt model.height)
                        , SA.viewBox <|
                            String.join " "
                                [ "-" ++ String.fromInt (model.width // 2)
                                , "-" ++ String.fromInt (model.height // 2)
                                , String.fromInt model.width
                                , String.fromInt model.height
                                ]
                        ]
                        [ hexgrid model model.width model.height model.size
                        , Svg.circle [ SA.cx "0", SA.cy "0", SA.r "3", SA.fill "black" ] []
                        ]
                )
            ]
        ]


hexgrid : Model -> Int -> Int -> Float -> Svg Msg
hexgrid model w h r =
    let
        ( x0, y0, z0 ) =
            cubeRound model.movement.position.x model.movement.position.y model.size

        currRow =
            if y0 > z0 then
                abs (y0 - z0)

            else
                0 - abs (y0 - z0)

        w0 =
            round (toFloat w / (3 * r)) + 1

        h0 =
            round (sqrt 3 * (toFloat h / (3 * r))) + 2
    in
    Svg.g
        []
        (List.concat <|
            lift3
                (\x y z ->
                    let
                        mContent =
                            Dict.get ( x, y, z ) model.map

                        cx =
                            r * ((3 / 2) * toFloat x)

                        cy =
                            r * ((sqrt 3 / 2) * toFloat x + sqrt 3 * toFloat y)

                        rowNum =
                            if y > z then
                                abs (y - z)

                            else
                                0 - abs (y - z)
                    in
                    if x + y + z == 0 && rowNum >= (currRow - h0) && rowNum <= (currRow + h0) then
                        [ hexagon
                            { x = x
                            , y = y
                            , z = z
                            , cx = model.movement.position.x - cx
                            , cy = model.movement.position.y - cy
                            , size = 0.95 * r
                            , content = mContent
                            , color =
                                if x == x0 && y == y0 && z == z0 then
                                    "pink"

                                else
                                    "grey"
                            }
                        ]

                    else
                        []
                )
                (List.range (x0 - w0) (x0 + w0))
                (List.range (y0 - h0) (y0 + h0))
                (List.range (z0 - h0) (z0 + h0))
        )


type alias HexagonParams =
    { x : Int
    , y : Int
    , z : Int
    , cx : Float
    , cy : Float
    , size : Float
    , color : String
    , content : Maybe Content
    }


hexagon : HexagonParams -> Svg msg
hexagon p =
    let
        a =
            String.fromFloat (p.size + p.cx) ++ "," ++ String.fromFloat p.cy

        b =
            String.fromFloat (p.size / 2 + p.cx) ++ "," ++ String.fromFloat (sqrt 3 * p.size / 2 + p.cy)

        c =
            String.fromFloat (-p.size / 2 + p.cx) ++ "," ++ String.fromFloat (sqrt 3 * p.size / 2 + p.cy)

        d =
            String.fromFloat (0 - p.size + p.cx) ++ "," ++ String.fromFloat p.cy

        e =
            String.fromFloat (-p.size / 2 + p.cx) ++ "," ++ String.fromFloat (-(sqrt 3) * p.size / 2 + p.cy)

        f =
            String.fromFloat (p.size / 2 + p.cx) ++ "," ++ String.fromFloat (-(sqrt 3) * p.size / 2 + p.cy)

        points =
            SA.points <| String.join " " [ a, b, c, d, e, f ]

        ( stroke, fill ) =
            case p.content of
                Nothing ->
                    ( "gray", p.color )

                Just content ->
                    if content.x == 0 && content.y == 0 then
                        ( "black", "pink" )

                    else if modBy 11 content.r == 0 then
                        ( "black", "lightgray" )

                    else
                        ( "black", "white" )
    in
    Svg.g []
        [ Svg.polygon [ SA.fill fill, SA.stroke stroke, points ] []
        , Svg.text_
            [ SA.x (String.fromFloat (-0.6 * p.size + p.cx))
            , SA.y (String.fromFloat (-0.2 * p.size + p.cy))
            , SA.fontSize (String.fromFloat (0.4 * p.size) ++ "px")
            ]
            [ Svg.text (String.fromInt p.x) ]
        , Svg.text_
            [ SA.x (String.fromFloat (0.2 * p.size + p.cx))
            , SA.y (String.fromFloat (-0.2 * p.size + p.cy))
            , SA.fontSize (String.fromFloat (0.4 * p.size) ++ "px")
            ]
            [ Svg.text (String.fromInt p.y) ]
        , Svg.text_
            [ SA.x (String.fromFloat (-0.2 * p.size + p.cx))
            , SA.y (String.fromFloat (0.6 * p.size + p.cy))
            , SA.fontSize (String.fromFloat (0.4 * p.size) ++ "px")
            ]
            [ Svg.text (String.fromInt p.z) ]
        ]


type alias Content =
    { x : Int
    , y : Int
    , r : Int
    , progression : Float
    }


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        _ ->
            Other


cubeRound : Float -> Float -> Float -> ( Int, Int, Int )
cubeRound x0 y0 r =
    let
        x =
            (2 / 3) * x0 / r

        y =
            ((-1 / 3) * x0 + (sqrt 3 / 3) * y0) / r

        z =
            0 - x - y

        rx =
            toFloat (round x)

        ry =
            toFloat (round y)

        rz =
            toFloat (round z)

        xDiff =
            abs (rx - x)

        yDiff =
            abs (ry - y)

        zDiff =
            abs (rz - z)
    in
    if xDiff > yDiff && xDiff > zDiff then
        ( round (0 - ry - rz), round ry, round rz )

    else if yDiff > zDiff then
        ( round rx, round (0 - rx - rz), round rz )

    else
        ( round rx, round ry, round (0 - rx - ry) )


posToHex : { x : Float, y : Float } -> Float -> { x : Float, y : Float, z : Float }
posToHex pos size =
    let
        x =
            (2 / 3) * pos.x / size

        y =
            ((-1 / 3) * pos.x + (sqrt 3 / 3) * pos.y) / size
    in
    { x = x
    , y = y
    , z = 0 - x - y
    }


hexToPos : { x : Float, y : Float, z : Float } -> Float -> { x : Float, y : Float }
hexToPos hex size =
    { x = size * (3 / 2) * hex.x
    , y = size * ((sqrt 3 / 2) * hex.x + sqrt 3 * hex.y)
    }


positionAtNewSize : Float -> Float -> { x : Float, y : Float } -> { x : Float, y : Float }
positionAtNewSize r0 r1 pos =
    hexToPos (posToHex pos r0) r1
