module Grid exposing (render)

import Element exposing (Element, el, text, row, column, alignRight, fill, width, rgb, spacing, centerY, padding, none)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Browser.Dom exposing (getViewport)

max_x = 10

max_y = 10

makeNode = el
        [ Background.color (rgb 0 0 0.5)
        , padding 30
        ]
        none

--makeCoordinates = List.concatMap
--(\i -> List.map
--(\j -> ( toFloat i, toFloat j ))
--(List.range 0 max_x)
--)
--(List.range 0 max_y)
--makeGrid = makeCoordinates
--|> List.map makeAndPlaceNode
--makeAndPlaceNode coord = makeNode
--|> shift coord

makeGrid = getViewport
        |> debug
        |> row [ spacing 10 ]
        |> List.repeat 5
        |> column [ spacing 10 ]

debug grid =
    let
        _ = (Debug.log "grid" grid)
    in
        List.repeat 5 makeNode

render seed viewport = makeGrid
        |> Element.layout []
