module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp)
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Border as Border
import Html.Events.Extra.Wheel as Wheel exposing (onWheel)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as E exposing (Value)
import List.Extra exposing (filterNot)
import Movement exposing (Movement)
import Position exposing (RoundedHex)
import Svg exposing (Svg, svg)
import Svg.Attributes as SA
import Task
import Time exposing (Posix)


type alias Model =
    { map : Dict ( Int, Int ) Content
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
            \model ->
                Sub.batch
                    [ Sub.map (KeyDown Nothing) (onKeyDown keyDecoder)
                    , Sub.map KeyUp (onKeyUp keyDecoder)
                    , if Movement.isMotion model.movement then
                        onAnimationFrame Tick

                      else
                        Sub.none
                    ]
        , view = \model -> E.layout [] (mainLayout model)
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        movement =
            Movement.default

        map =
            Dict.empty

        size =
            30

        width =
            480

        height =
            480
    in
    ( { movement = movement
      , map = map
      , width = width
      , height = height
      , size = size
      , minSize = 20
      , maxSize = 100
      }
    , (\l -> requestHexes l map) <|
        Position.visibility movement.position size width height
    )


type Msg
    = Tick Posix
    | GotContent (Result Http.Error (List Content))
    | KeyDown (Maybe Posix) Direction
    | KeyUp Direction
    | Zoom Wheel.Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                newMovement =
                    Movement.updatePosition model.size time model.movement

                oldHex =
                    Position.roundHex model.movement.position model.size

                newHex =
                    Position.roundHex newMovement.position model.size

                hasMoved =
                    oldHex.q == newHex.q && oldHex.r == newHex.r

                loadList =
                    if hasMoved then
                        Position.visibility newMovement.position model.size model.width model.height

                    else
                        []
            in
            ( { model | movement = newMovement }, requestHexes loadList model.map )

        GotContent res ->
            case res of
                Err _ ->
                    ( model, Cmd.none )

                Ok content ->
                    let
                        newMap =
                            List.map (\c -> ( ( c.q, c.r ), c )) content
                                |> Dict.fromList
                                |> Dict.union model.map
                    in
                    ( { model | map = newMap }, Cmd.none )

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
                        (Position.pixelAtNewSize model.size newSize model.movement.position)
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
                        [ hexgrid model
                        , Svg.circle [ SA.cx "0", SA.cy "0", SA.r "3", SA.fill "black" ] []
                        ]
                )
            ]
        ]


hexgrid : Model -> Svg Msg
hexgrid model =
    Svg.g
        []
        (List.concatMap
            (\hex ->
                let
                    mContent =
                        Dict.get ( hex.q, hex.r ) model.map

                    cx =
                        model.size * ((3 / 2) * toFloat hex.q)

                    cy =
                        model.size * ((sqrt 3 / 2) * toFloat hex.q + sqrt 3 * toFloat hex.r)
                in
                hexagon
                    { cx = model.movement.position.x - cx
                    , cy = model.movement.position.y - cy
                    , size = model.size
                    , content = mContent
                    }
            )
            (Position.visibility { x = model.movement.position.x, y = model.movement.position.y } model.size model.width model.height)
        )


hexagon : { cx : Float, cy : Float, size : Float, content : Maybe Content } -> List (Svg msg)
hexagon p =
    case p.content of
        Nothing ->
            []

        Just content ->
            case content.shape of
                Circle ->
                    [ Svg.circle
                        [ SA.cx (String.fromFloat p.cx)
                        , SA.cy (String.fromFloat p.cy)
                        , SA.r (String.fromFloat (p.size / 2))
                        , SA.stroke "black"
                        , SA.strokeWidth "3"
                        , SA.fill "red"
                        ]
                        []
                    ]

                Square ->
                    [ Svg.rect
                        [ SA.x (String.fromFloat (p.cx - p.size / 4))
                        , SA.y (String.fromFloat (p.cy - p.size / 4))
                        , SA.width (String.fromFloat (p.size / 2))
                        , SA.height (String.fromFloat (p.size / 2))
                        , SA.stroke "black"
                        , SA.strokeWidth "3"
                        , SA.fill "green"
                        ]
                        []
                    ]

                Triangle ->
                    []

                _ ->
                    []


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


debugHexagon : HexagonParams -> Svg msg
debugHexagon p =
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
                    if content.q == 0 && content.r == 0 then
                        ( "black", "pink" )

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
    { q : Int
    , r : Int
    , shape : Shape
    }


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


keyDecoder : Decoder Direction
keyDecoder =
    D.map toDirection (D.field "key" D.string)


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


requestHexes : List RoundedHex -> Dict ( Int, Int ) Content -> Cmd Msg
requestHexes loadList map =
    postHexesRequest <|
        filterNot (\k -> Dict.member ( k.q, k.r ) map) loadList


postHexesRequest : List RoundedHex -> Cmd Msg
postHexesRequest hexes =
    Http.post
        { url = "locations"
        , body = Http.jsonBody (E.list hexEncoder hexes)
        , expect = Http.expectJson GotContent (D.list contentDecoder)
        }


hexEncoder : RoundedHex -> Value
hexEncoder hex =
    E.object
        [ ( "Q", E.int hex.q )
        , ( "R", E.int hex.r )
        ]


contentDecoder : Decoder Content
contentDecoder =
    D.succeed Content
        |> optional "Q" D.int 0
        |> optional "R" D.int 0
        |> optional "Shape" (D.map shapeFromInt D.int) Circle


type Shape
    = Empty
    | Circle
    | Triangle
    | Square


shapeFromInt : Int -> Shape
shapeFromInt n =
    case n of
        0 ->
            Empty

        1 ->
            Circle

        2 ->
            Triangle

        3 ->
            Square

        _ ->
            Empty
