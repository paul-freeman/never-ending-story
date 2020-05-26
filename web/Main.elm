module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background
import Element.Border as Border
import Element.Input exposing (button, slider)
import Random exposing (Generator, Seed)
import Svg exposing (Svg, svg)
import Svg.Attributes as SA
import Time


type Msg
    = Step
    | ChangeRadius Float


type Act
    = One Float
    | Two Float
    | Three Float


type alias Character =
    { x : Int
    , y : Int
    , color : String
    , lastStep : Maybe Direction
    }


type alias Model =
    { story : Act
    , characters : Dict String Character
    , mainCharacter : Maybe String
    , map : Dict ( Int, Int ) Content
    , seed : Seed
    , width : Int
    , height : Int
    , radius : Float
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions =
            \model ->
                case model.story of
                    One x ->
                        if x < 1.0 then
                            Time.every 200.0 (\_ -> Step)

                        else
                            Sub.none

                    _ ->
                        Sub.none
        , view = \model -> E.layout [] (mainLayout model)
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        seed0 =
            Random.initialSeed 10001

        ( initialContent, seed1 ) =
            generateContent 0 0 ( Dict.empty, seed0 )

        ( initialCharacters, seed2 ) =
            generateCharacter 0 0 "black" ( Dict.empty, seed1 )

        mainCharacter =
            List.head (Dict.keys initialCharacters)
    in
    ( { story = One 0.0
      , characters = initialCharacters
      , mainCharacter = mainCharacter
      , map = initialContent
      , seed = seed2
      , width = 480
      , height = 480
      , radius = 10
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step ->
            let
                model1 =
                    Dict.foldl stepCharacters { model | characters = Dict.empty } model.characters

                ( newX, newY ) =
                    model1.mainCharacter
                        |> Maybe.andThen (\n -> Dict.get n model1.characters)
                        |> Maybe.map (\c -> ( c.x, c.y ))
                        |> Maybe.withDefault ( 0, 0 )

                ( characters1, seed1 ) =
                    generateNewCharacter newX newY "red" ( model1.characters, model1.seed )

                story1 =
                    case Dict.get ( newX, newY ) model1.map of
                        Just c ->
                            case model1.story of
                                One p ->
                                    One (min 1.0 (p + c.progression))

                                Two p ->
                                    Two (min 1.0 (p + c.progression))

                                Three p ->
                                    Three (min 1.0 (p + c.progression))

                        Nothing ->
                            One 0
            in
            ( { model1
                | story = story1
                , characters = characters1
                , seed = seed1
              }
            , Cmd.none
            )

        ChangeRadius newRadius ->
            ( { model | radius = newRadius }, Cmd.none )


mainLayout : Model -> Element Msg
mainLayout model =
    E.column
        [ E.width E.fill
        , E.height E.fill
        ]
        [ E.row
            [ E.centerX
            ]
            [ E.el [ Border.width 3 ]
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
                        (hexgrid model model.width model.height model.radius
                            ++ drawPawns model
                        )
                )
            , E.html <|
                svg
                    [ SA.width (String.fromInt (model.width // 20))
                    , SA.height (String.fromInt model.height)
                    , SA.viewBox <|
                        String.join " "
                            [ "0"
                            , "0"
                            , String.fromInt (model.width // 20)
                            , String.fromInt model.height
                            ]
                    ]
                    [ Svg.rect
                        [ SA.x "0"
                        , SA.y (String.fromFloat (toFloat model.height - toFloat model.height * progressToFloat model))
                        , SA.width (String.fromInt (model.width // 20))
                        , SA.height (String.fromInt model.height)
                        , SA.fill "pink"
                        ]
                        []
                    ]
            ]
        , E.el
            [ E.width (E.px model.width)
            , E.centerX
            ]
            (slider
                [ E.width E.fill
                , E.height (E.px 30)
                ]
                { onChange = ChangeRadius
                , label = Element.Input.labelAbove [] (E.text "Zoom")
                , min = 5
                , max = 60
                , step = Nothing
                , value = model.radius
                , thumb = Element.Input.defaultThumb
                }
            )
        , E.row
            [ E.width (E.px model.width)
            , E.height E.fill
            , E.centerX
            ]
            [ E.column [ E.height E.fill, E.width E.fill ]
                [ E.paragraph []
                    [ E.text <|
                        case model.mainCharacter of
                            Just name ->
                                case Dict.get name model.characters of
                                    Just mainCharacter ->
                                        String.concat
                                            [ name
                                            , " travels "
                                            , directionToString (mainCharacterLastStep model)
                                            , " and arrives at "
                                            , String.fromInt mainCharacter.x
                                            , ","
                                            , String.fromInt mainCharacter.y
                                            ]

                                    Nothing ->
                                        "Nothing happened."

                            Nothing ->
                                "Nothing happened."
                    ]
                ]
            , button [ E.height (E.px 100), E.alignTop ]
                { onPress = Just Step
                , label = E.text "Next"
                }
            ]
        ]


drawPawns : Model -> List (Svg msg)
drawPawns model =
    case model.mainCharacter |> Maybe.andThen (\n -> Dict.get n model.characters) of
        Nothing ->
            []

        Just mainCharacter ->
            Dict.values model.characters
                |> List.indexedMap
                    (\n c ->
                        let
                            ( x, y ) =
                                calcHexPosition ( mainCharacter.x, mainCharacter.y )
                                    model.radius
                                    ( c.x - mainCharacter.x, c.y - mainCharacter.y )
                        in
                        Svg.circle
                            [ SA.cx (String.fromFloat (x + toFloat n * (model.radius / 4)))
                            , SA.cy (String.fromFloat y)
                            , SA.r (String.fromFloat (model.radius / 8))
                            , SA.fill c.color
                            ]
                            []
                    )


hexgrid : Model -> Int -> Int -> Float -> List (Svg msg)
hexgrid model w h r =
    let
        d =
            sqrt 3 / 2

        numRows =
            toFloat h / (d * r)
    in
    List.range -(ceiling (numRows / 2)) (ceiling (numRows / 2))
        |> List.map
            (\y ->
                hexRow model w y r (modBy 2 y /= 0)
            )
        |> List.concat


hexRow : Model -> Int -> Int -> Float -> Bool -> List (Svg msg)
hexRow model w y r oddRow =
    case model.mainCharacter |> Maybe.andThen (\n -> Dict.get n model.characters) of
        Nothing ->
            []

        Just mainCharacter ->
            let
                hexesPerRow =
                    toFloat w / (3 * r)

                d =
                    sqrt 3 / 2

                range =
                    List.range -(ceiling (hexesPerRow / 2)) (ceiling (hexesPerRow / 2))
            in
            range
                |> List.concatMap
                    (\x ->
                        let
                            mContent =
                                Dict.get ( mainCharacter.x + x, mainCharacter.y + y ) model.map

                            offset =
                                if oddRow then
                                    if modBy 2 mainCharacter.y == 0 then
                                        1.5 * r

                                    else
                                        -1.5 * r

                                else
                                    0.0
                        in
                        hexagon (3 * r * toFloat x + offset) (r * toFloat y * d) r mContent
                    )


calcHexPosition : ( Int, Int ) -> Float -> ( Int, Int ) -> ( Float, Float )
calcHexPosition ( _, n ) r ( x, y ) =
    let
        d =
            sqrt 3 / 2

        offset =
            if modBy 2 y /= 0 then
                if modBy 2 n /= 0 then
                    -1.5 * r

                else
                    1.5 * r

            else
                0.0
    in
    ( 3 * r * toFloat x + offset, r * toFloat y * d )


hexagon : Float -> Float -> Float -> Maybe Content -> List (Svg msg)
hexagon cx cy r mContent =
    let
        a =
            String.fromFloat (r + cx) ++ "," ++ String.fromFloat (0 + cy)

        b =
            String.fromFloat (r / 2 + cx) ++ "," ++ String.fromFloat (sqrt 3 * r / 2 + cy)

        c =
            String.fromFloat (-r / 2 + cx) ++ "," ++ String.fromFloat (sqrt 3 * r / 2 + cy)

        d =
            String.fromFloat (-r + cx) ++ "," ++ String.fromFloat (0 + cy)

        e =
            String.fromFloat (-r / 2 + cx) ++ "," ++ String.fromFloat (-(sqrt 3) * r / 2 + cy)

        f =
            String.fromFloat (r / 2 + cx) ++ "," ++ String.fromFloat (-(sqrt 3) * r / 2 + cy)

        points =
            SA.points <| String.join " " [ a, b, c, d, e, f ]

        ( stroke, fill ) =
            case mContent of
                Nothing ->
                    ( "gray", "gray" )

                Just content ->
                    if content.x == 0 && content.y == 0 then
                        ( "black", "pink" )

                    else if modBy 11 content.r == 0 then
                        ( "black", "lightgray" )

                    else
                        ( "none", "white" )
    in
    [ Svg.polygon
        [ SA.fill fill, SA.stroke stroke, points ]
        []
    ]


type alias Content =
    { x : Int
    , y : Int
    , r : Int
    , progression : Float
    }


generateContent : Int -> Int -> ( Dict ( Int, Int ) Content, Seed ) -> ( Dict ( Int, Int ) Content, Seed )
generateContent x y state =
    let
        center =
            ( x, y )

        up =
            ( x, y - 2 )

        down =
            ( x, y + 2 )

        upToLeft =
            ( if modBy 2 y == 0 then
                x - 1

              else
                x
            , y - 1
            )

        upToRight =
            ( if modBy 2 y == 1 then
                x + 1

              else
                x
            , y - 1
            )

        downToLeft =
            ( if modBy 2 y == 0 then
                x - 1

              else
                x
            , y + 1
            )

        downToRight =
            ( if modBy 2 y == 1 then
                x + 1

              else
                x
            , y + 1
            )
    in
    [ center, up, down, upToLeft, upToRight, downToLeft, downToRight ]
        |> List.foldl
            (\a ( d, seed0 ) ->
                case Dict.get a d of
                    Nothing ->
                        let
                            ( progression, seed1 ) =
                                Random.step (Random.float 0 (1 / 500)) seed0

                            ( c, seed2 ) =
                                Random.step
                                    (Random.int 0 Random.maxInt
                                        |> Random.map
                                            (\r ->
                                                { x = Tuple.first a
                                                , y = Tuple.second a
                                                , r = r
                                                , progression = progression
                                                }
                                            )
                                    )
                                    seed1
                        in
                        ( Dict.insert a c d, seed2 )

                    Just _ ->
                        ( d, seed0 )
            )
            state


generateCharacter : Int -> Int -> String -> ( Dict String Character, Seed ) -> ( Dict String Character, Seed )
generateCharacter x y color ( characters, seed0 ) =
    let
        ( a, seed1 ) =
            Random.step (Random.uniform 'B' (String.toList "CDFGHJKLMNPRSTVWZ")) seed0

        ( b, seed2 ) =
            Random.step (Random.uniform 'a' (String.toList "eiou")) seed1

        ( c, seed3 ) =
            Random.step (Random.uniform 'b' (String.toList "cdfghjklmnprstvwz")) seed2

        name =
            String.fromList [ a, b, c ]
    in
    if Dict.member name characters then
        generateCharacter x y color ( characters, seed3 )

    else
        ( Dict.insert name { x = x, y = y, color = color, lastStep = Nothing } characters, seed3 )


generateNewCharacter : Int -> Int -> String -> ( Dict String Character, Seed ) -> ( Dict String Character, Seed )
generateNewCharacter x y color ( characters0, seed0 ) =
    if Dict.size characters0 >= 2 then
        ( characters0, seed0 )

    else
        let
            ( r, seed1 ) =
                Random.step (Random.float 0 1) seed0
        in
        if r > 0.1 then
            ( characters0, seed1 )

        else
            generateCharacter x y color ( characters0, seed1 )


type Direction
    = Up
    | Down
    | UpToLeft
    | UpToRight
    | DownToLeft
    | DownToRight


generateDirection : Generator Direction
generateDirection =
    Random.uniform Up [ Down, UpToLeft, UpToRight, DownToLeft, DownToRight ]


directionToString : Maybe Direction -> String
directionToString m =
    case m of
        Nothing ->
            "nowhere"

        Just d ->
            case d of
                Up ->
                    "north"

                Down ->
                    "south"

                UpToLeft ->
                    "northwest"

                UpToRight ->
                    "northeast"

                DownToLeft ->
                    "southwest"

                DownToRight ->
                    "southeast"


calcNewXY : Character -> Direction -> ( Int, Int )
calcNewXY character direction =
    let
        x =
            character.x

        y =
            character.y
    in
    case direction of
        Up ->
            ( x, y - 2 )

        Down ->
            ( x, y + 2 )

        UpToLeft ->
            ( if modBy 2 y == 0 then
                x - 1

              else
                x
            , y - 1
            )

        UpToRight ->
            ( if modBy 2 y == 1 then
                x + 1

              else
                x
            , y - 1
            )

        DownToLeft ->
            ( if modBy 2 y == 0 then
                x - 1

              else
                x
            , y + 1
            )

        DownToRight ->
            ( if modBy 2 y == 1 then
                x + 1

              else
                x
            , y + 1
            )


mainCharacterLastStep : Model -> Maybe Direction
mainCharacterLastStep model =
    model.mainCharacter
        |> Maybe.andThen (\n -> Dict.get n model.characters)
        |> Maybe.andThen .lastStep


progressToFloat : Model -> Float
progressToFloat model =
    case model.story of
        One f ->
            f

        Two f ->
            f

        Three f ->
            f


stepCharacters : String -> Character -> Model -> Model
stepCharacters name character model =
    let
        ( direction, seed1 ) =
            Random.step generateDirection model.seed

        ( newX, newY ) =
            calcNewXY character direction

        ( map1, seed2 ) =
            generateContent newX newY ( model.map, seed1 )

        characters1 =
            Dict.insert name
                { x = newX
                , y = newY
                , color = character.color
                , lastStep = Just direction
                }
                model.characters
    in
    { model | characters = characters1, map = map1, seed = seed2 }
