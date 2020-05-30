module Position exposing (Hex, Pixel, RoundedHex, hexToPixel, pixelAtNewSize, pixelToHex, roundHex, visibility)

import List
import List.Extra exposing (lift3)


type alias Pixel =
    { x : Float
    , y : Float
    }


type alias Hex =
    { q : Float
    , r : Float
    }


type alias RoundedHex =
    { q : Int
    , r : Int
    }


type alias Size =
    Float


pixelToHex : Pixel -> Size -> Hex
pixelToHex pos size =
    let
        x =
            (2 / 3) * pos.x / size

        y =
            ((-1 / 3) * pos.x + (sqrt 3 / 3) * pos.y) / size
    in
    { q = x
    , r = y
    }


hexToPixel : Hex -> Size -> Pixel
hexToPixel hex size =
    { x = size * (3 / 2) * hex.q
    , y = size * ((sqrt 3 / 2) * hex.q + sqrt 3 * hex.r)
    }


pixelAtNewSize : Size -> Size -> Pixel -> Pixel
pixelAtNewSize s1 s2 pos =
    hexToPixel (pixelToHex pos s1) s2


roundHex : Pixel -> Size -> RoundedHex
roundHex pixel size =
    let
        hex =
            pixelToHex pixel size

        z =
            0 - hex.q - hex.r

        rx =
            toFloat (round hex.q)

        ry =
            toFloat (round hex.r)

        rz =
            toFloat (round z)

        xDiff =
            abs (rx - hex.q)

        yDiff =
            abs (ry - hex.r)

        zDiff =
            abs (rz - z)
    in
    if xDiff > yDiff && xDiff > zDiff then
        { q = round (0 - ry - rz), r = round ry }

    else if yDiff > zDiff then
        { q = round rx, r = round (0 - rx - rz) }

    else
        { q = round rx, r = round ry }


visibility : Pixel -> Size -> Int -> Int -> List RoundedHex
visibility pixel size w h =
    let
        roundedHex =
            roundHex pixel size

        z0 =
            0 - roundedHex.q - roundedHex.r

        currRow =
            if roundedHex.r > z0 then
                abs (roundedHex.r - z0)

            else
                0 - abs (roundedHex.r - z0)

        w0 =
            round (toFloat w / (3 * size)) + 1

        h0 =
            round (sqrt 3 * (toFloat h / (3 * size))) + 2
    in
    List.concat <|
        lift3
            (\x y z ->
                let
                    rowNum =
                        if y > z then
                            abs (y - z)

                        else
                            0 - abs (y - z)
                in
                if x + y + z == 0 && rowNum >= (currRow - h0) && rowNum <= (currRow + h0) then
                    [ { q = x, r = y } ]

                else
                    []
            )
            (List.range (roundedHex.q - w0) (roundedHex.q + w0))
            (List.range (roundedHex.r - h0) (roundedHex.r + h0))
            (List.range (z0 - h0) (z0 + h0))
