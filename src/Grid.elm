module Grid exposing (render)

import Config exposing (..)
import Element exposing (Element, el, height, none, px, rgb, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Types exposing (..)


-- TYPES

type alias Colour = Element.Color

type alias RawColour = { red : Float, green : Float, blue : Float, alpha : Float }

type alias Source = { srcCoords : Coord, ago : TicksSinceEvent, srcColour : RawColour, angle : Float }

type TicksSinceEvent
    = TicksSinceEvent Int

type Sign
    = Positive
    | Negative


-- CONSTANTS

rawBlack : RawColour
rawBlack = { red = 0, green = 0, blue = 0, alpha = 1 }

row = Element.row [ spacing gapSize ]

column = Element.column [ spacing gapSize ]

intMax = 2147483647


-- GRID

makeNode : List Source -> Coord -> Element msg
makeNode sources coord = el
        [ Background.color (calculatePixelColour sources coord)
        , width (px pixelSize)
        , height (px pixelSize)
        ]
        none

calculatePixelColour : List Source -> Coord -> Colour
calculatePixelColour sources coord = List.foldl
        (calcPixColourForSource coord)
        rawBlack
        sources
        |> Element.fromRgb

calcPixColourForSource : Coord -> Source -> RawColour -> RawColour
calcPixColourForSource pixelCoords { srcCoords, ago, srcColour, angle } accColour =
    let
        nominalDistance = calculateNominalDistance srcCoords pixelCoords angle

        amplitude = getRippleAmplitude nominalDistance ago ripplePropagationSpeed
    in
    { red = min 1 (accColour.red + amplitude * srcColour.red)
    , green = min 1 (accColour.green + amplitude * srcColour.green)
    , blue = min 1 (accColour.blue + amplitude * srcColour.blue)
    , alpha = 1
    }

calculateNominalDistance : Coord -> Coord -> Float -> Float
calculateNominalDistance srcCoords pixelCoords angle = case waveShape of
        Circle -> calculateCircleDistance srcCoords pixelCoords

        PointedStar numPoints -> calculateStarDistance srcCoords pixelCoords angle numPoints

calculateCircleDistance : Coord -> Coord -> Float
calculateCircleDistance ( srcX, srcY ) ( currX, currY ) = ((currX - srcX) ^ 2 + (currY - srcY) ^ 2)
        |> toFloat
        |> sqrt

calculateStarDistance : Coord -> Coord -> Float -> Int -> Float
calculateStarDistance ( srcX, srcY ) ( pixelX, pixelY ) baseAngle numPoints =
    let
        x = toFloat (pixelX - srcX)

        y = toFloat (pixelY - srcY)

        euclideanDistance = sqrt (x ^ 2 + y ^ 2)

        thetaRad = case ( sign x, sign y ) of
                ( Positive, Positive ) -> atan (y / x)

                ( Negative, Positive ) -> (pi / 2) - atan (x / y)

                ( Negative, Negative ) -> pi + atan (y / x)

                ( Positive, Negative ) -> (3 * pi / 2) - atan (x / y)

        thetaRadAdjusted = thetaRad + baseAngle

        angleBetweenPointsRad = (2 * pi) / toFloat numPoints

        thetaPoints = thetaRadAdjusted / angleBetweenPointsRad

        thetaPointsMod1 = floatMod1 thetaPoints

        -- stars are symmetric around their points, with peaks at the points and lows half way between points
        scaleAmount = if thetaPointsMod1 > 0.5 then
                thetaPointsMod1
            else
                1 - thetaPointsMod1
    in
    euclideanDistance * scaleAmount

floatMod1 : Float -> Float
floatMod1 x = x - toFloat (floor x)

sign : Float -> Sign
sign x = if x < 0 then
        Negative
    else
        Positive

getRippleAmplitude : Float -> TicksSinceEvent -> Float -> Float
getRippleAmplitude distance (TicksSinceEvent timeAgo) speed = List.range 0 numAdditionalWaves
        |> List.map (\i -> ( toFloat i, (speed * toFloat timeAgo) - (rippleWidth * toFloat i * 2) ))
        |> List.foldl
            (\( i, wf ) acc -> if wf > 0 then
                    acc + (singlePeakAmplitude distance wf / (waveFadeFactor * (i + 1)))
                else
                    acc
            )
            0

singlePeakAmplitude : Float -> Float -> Float
singlePeakAmplitude distance wavefront = (rippleWidth - abs (distance - wavefront))
        |> min 1
        |> max 0

makeGrid : List Source -> Dimensions -> Element msg
makeGrid sources dimensions =
    let
        ( maxX, maxY ) = dimensions

        makeRow y = row (List.map (\x -> makeNode sources ( x, y )) (List.range 0 maxX))
    in
    column (List.map makeRow (List.range 0 maxY))

eventToSource : Ticks -> Dimensions -> Event -> Source
eventToSource now ( maxX, maxY ) ( index, maybeCoords, eventTickTime ) =
    let
        seed = lehmerNext (modBy intMax (eventTickTime + index))

        coords = Maybe.withDefault ( modBy maxX seed, modBy maxY seed ) maybeCoords

        ticksSinceEvent = TicksSinceEvent (now - eventTickTime)

        eventBaseColour = generateRandomColour seed

        angle = scaleIntToFloat (lehmerNext seed) * (2 * pi)
    in
    { srcCoords = coords, ago = ticksSinceEvent, srcColour = eventBaseColour, angle = angle }

generateRandomColour : Int -> RawColour
generateRandomColour seed =
    let
        redInt = lehmerNext seed

        blueInt = lehmerNext redInt

        greenInt = lehmerNext blueInt
    in
    { red = scaleIntToFloat redInt
    , blue = scaleIntToFloat blueInt
    , green = scaleIntToFloat greenInt
    , alpha = 1
    }

scaleIntToFloat : Int -> Float
scaleIntToFloat i = toFloat (i - 1) / (intMax - 1)

lehmerNext : Int -> Int
lehmerNext curr = modBy intMax (curr * 48271)

render : List Event -> Ticks -> Dimensions -> Bool -> Html.Html msg
render events now dimensions showHelp = makeGrid (List.map (eventToSource now dimensions) events) dimensions
        |> Element.layout
            (if showHelp then
                [ Element.inFront renderHelp ]
             else
                []
            )

renderHelp : Element msg
renderHelp = el
        [ Font.color (rgb 1 1 1)
        , Element.centerX
        , Element.centerY
        ]
        (column
            [ el [ Element.centerX ] (Element.text "ZENPOOL")
            , el [ Element.centerX ] (Element.text "turn the sound on and press some keys")
            ]
        )
