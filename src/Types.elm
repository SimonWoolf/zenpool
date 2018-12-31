module Types exposing (Event, Index, Ticks, Viewport)

type alias Viewport = { width : Float, height : Float }

type alias Index = Int

type alias Ticks = Int

type alias Event = ( Index, Ticks )
