module Grid exposing (grid)

import Collage exposing (circle, filled, rectangle, uniform)
import Collage.Layout exposing (horizontal, spacer, vertical)
import Collage.Render exposing (svg)
import Color

node = rectangle 100 100
        |> filled (uniform Color.blue)

row = List.repeat 10 node
        |> List.intersperse (spacer 10 0)
        |> horizontal
        |> List.repeat 10
        |> List.intersperse (spacer 0 10)
        |> vertical

grid key = row
        |> svg
