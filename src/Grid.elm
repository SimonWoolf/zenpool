module Grid exposing (render)

import Collage exposing (circle, filled, rectangle, uniform, shift, group)
import Collage.Render exposing (svg)
import Color

max_x = 10

max_y = 10

makeNode = rectangle 100 100
        |> filled (uniform Color.blue)

makeRawCoordinates = List.concatMap
        (\i -> List.map
                (\j -> ( toFloat i, toFloat j ))
                (List.range 0 max_x)
        )
        (List.range 0 max_y)

-- TODO

rawCoordsToPixelCoords = identity

makeGrid = makeRawCoordinates
        |> List.map rawCoordsToPixelCoords
        |> List.map makeAndPlaceNode

makeAndPlaceNode coord = makeNode
        |> shift coord

--makeGrid = List.repeat 5 makeNode
--|> List.intersperse (spacer 10 0)
--|> horizontal
--|> List.repeat 5
--|> List.intersperse (spacer 0 10)
--|> vertical

plant seed grid =
    let
        _ = (Debug.log "grid" grid)
    in
        grid

render seed = makeGrid
        |> plant seed
        |> group
        |> svg
