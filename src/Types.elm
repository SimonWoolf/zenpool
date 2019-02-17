module Types exposing (Coord, Dimensions, Event, Index, Ticks, Viewport, composeTwoArgs)

type alias Viewport = ( Int, Int )

type alias Dimensions = ( Int, Int )

type alias Index = Int

type alias Ticks = Int

type alias Coord = ( Int, Int )

type alias Event = ( Index, Maybe Coord, Ticks )


-- generic helpers

composeTwoArgs = (<<) << (<<)
