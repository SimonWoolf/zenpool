module Grid exposing (render)

import Element exposing (Element, el, text, row, column, alignRight, fill, width, px, height, rgb, spacing, centerY, padding, none)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

pixelSize = 15

gapSize = 5

makeNode = el
        [ Background.color (rgb 0 0 0.5)
        , width (px pixelSize)
        , height (px pixelSize)
        ]
        none

makeGrid viewport = makeNode
        |> repeatForDimension viewport.width
        |> row [ spacing gapSize ]
        |> repeatForDimension viewport.height
        |> column [ spacing gapSize ]

repeatForDimension dimensionSize = (dimensionSize / (pixelSize + gapSize))
        |> ceiling
        |> List.repeat

render seed viewport = makeGrid viewport
        |> Element.layout []
