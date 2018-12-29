module Grid exposing (render)

import Types exposing (Viewport, Index, Ticks, Event)
import Html
import Element exposing (Element, el, text, alignRight, fill, width, px, height, rgb, spacing, centerY, padding, none)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

type alias Colour = Element.Color

-- yeah yeah I know

type alias RawColour = { red : Float, green : Float, blue : Float, alpha : Float }

type alias Coord = ( Int, Int )

type alias Source = ( Coord, TicksSinceEvent, RawColour )

type TicksSinceEvent
    = TicksSinceEvent Int

rawBlack : RawColour
rawBlack = { red = 1, green = 0, blue = 0, alpha = 1 }

pixelSize = 10

gapSize = 5

row = Element.row [ spacing gapSize ]

column = Element.column [ spacing gapSize ]

makeNode : List Source -> Coord -> Element msg
makeNode sources coord = el
        [ Background.color (calculatePixelColour sources coord)
        , width (px pixelSize)
        , height (px pixelSize)
        ]
        none

calculatePixelColour : List Source -> Coord -> Colour
calculatePixelColour sources coord = (List.foldl
        (calcPixColourForSource coord)
        rawBlack
        sources
    )
        |> Element.fromRgb

calcPixColourForSource : Coord -> Source -> RawColour -> RawColour
calcPixColourForSource ( currX, currY ) ( ( srcX, srcY ), ago, srcColour ) accColour =
    let
        distanceApart = ((currX - srcX) ^ 2 + (currY - srcY) ^ 2)
                |> toFloat
                |> sqrt

        amplitude = getRippleAmplitude distanceApart ago
    in
        { red = min 1 (accColour.red + amplitude * srcColour.red)
        , green = min 1 (accColour.green + amplitude * srcColour.green)
        , blue = min 1 (accColour.blue + amplitude * srcColour.blue)
        , alpha = 1
        }

-- TODO make more interesting

getRippleAmplitude : Float -> TicksSinceEvent -> Float
getRippleAmplitude distance (TicksSinceEvent timeAgo) = if abs (distance - (toFloat timeAgo)) < 2 then
        1
    else
        0

makeGrid : List Source -> Viewport -> Element msg
makeGrid sources viewport =
    let
        maxX = getDimension viewport.width

        maxY = getDimension viewport.height

        _ = (Debug.log "sources" sources)

        makeRow y = row (List.map (\x -> makeNode sources ( x, y )) (List.range 0 maxX))
    in
        column (List.map makeRow (List.range 0 maxY))

getDimensions : Viewport -> Coord
getDimensions viewport = ( getDimension viewport.width, getDimension viewport.height )

getDimension dimensionSize = ceiling (dimensionSize / (pixelSize + gapSize))

eventToSource : Ticks -> Event -> Source
eventToSource now ( index, eventTickTime ) = ( (calculateEventCoords index eventTickTime), TicksSinceEvent (now - eventTickTime), { red = 0, blue = 0, green = 0.5, alpha = 1 } )

calculateEventCoords : Index -> Ticks -> Coord
calculateEventCoords index eventTickTime = ( 10, 10 )

render : List Event -> Ticks -> Viewport -> Html.Html msg
render events now viewport = makeGrid (List.map (eventToSource now) events) viewport
        |> Element.layout []
