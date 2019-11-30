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

type alias Source = ( Coord, TicksSinceEvent, RawColour )

type TicksSinceEvent
    = TicksSinceEvent Int


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
calcPixColourForSource pixelCoords ( srcCoords, ago, srcColour ) accColour =
    let
        nominalDistance = calculateNominalDistance srcCoords pixelCoords

        amplitude = getRippleAmplitude nominalDistance ago ripplePropagationSpeed
    in
    { red = min 1 (accColour.red + amplitude * srcColour.red)
    , green = min 1 (accColour.green + amplitude * srcColour.green)
    , blue = min 1 (accColour.blue + amplitude * srcColour.blue)
    , alpha = 1
    }

calculateNominalDistance : Coord -> Coord -> Float
calculateNominalDistance srcCoords pixelCoords = case waveShape of
        Circle -> calculateCircleDistance srcCoords pixelCoords


-- PointedStar numPoints -> calculateStarDistance srcCoords pixelCoords numPoints

calculateCircleDistance : Coord -> Coord -> Float
calculateCircleDistance ( srcX, srcY ) ( currX, currY ) = ((currX - srcX) ^ 2 + (currY - srcY) ^ 2)
        |> toFloat
        |> sqrt


-- TODO
-- calculateStarDistance : Coord -> Coord -> Int -> Float
-- calculateStarDistance ( srcX, srcY ) ( currX, currY ) numPoints = ((currX - srcX) ^ 2 + (currY - srcY) ^ 2)
--         |> toFloat
--         |> sqrt

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
    in
    ( coords, ticksSinceEvent, eventBaseColour )

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
