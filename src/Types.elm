module Types exposing (Dimensions, Event, Index, Ticks, Viewport, composeTwoArgs)

type alias Viewport = ( Int, Int )

type alias Dimensions = ( Int, Int )

type alias Index = Int

type alias Ticks = Int

type alias Event = ( Index, Ticks )

-- generic helpers

composeTwoArgs = (<<) << (<<)
