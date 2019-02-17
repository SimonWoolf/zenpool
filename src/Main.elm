port module Main exposing (Model, Msg(..), init, keyDecoder, main, subscriptions, update, view)

import Browser
import Browser.Events
import Config exposing (..)
import Grid
import Html exposing (Html, div, h1, table, text)
import Html.Attributes exposing (id)
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode
import Random
import Task
import Time
import Types exposing (..)

main = Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

type Msg
    = KeyPress String
    | Touch TouchCoord
    | TouchWithIndex TouchCoord Index
    | Tick Time.Posix
    | ViewportChange Viewport

type alias Frequency = Float

type alias Model = { events : List Event, dimensions : Dimensions, now : Ticks, maxEventEffectTime : Ticks, showHelp : Bool }

type alias TouchCoord = ( Float, Float )

baseFreq : Frequency
baseFreq = 220

tickLengthMs : Float
tickLengthMs = 50


-- Init

init : Viewport -> ( Model, Cmd Msg )
init viewport =
    let
        dimensions = viewportToDimensions viewport
    in
    ( { events = [ ( 0, Nothing, 0 ) ], dimensions = dimensions, maxEventEffectTime = calculateMaxEventEffectTime dimensions, now = 0, showHelp = True }, Cmd.none )

viewportToDimensions : Viewport -> Dimensions
viewportToDimensions ( width, height ) = ( getDimension width, getDimension height )

getDimension : Int -> Int
getDimension dimensionSize = floor (toFloat dimensionSize / (pixelSize + gapSize))

calculateMaxEventEffectTime : Dimensions -> Ticks
calculateMaxEventEffectTime ( maxX, maxY ) =
    let
        maxDist = (maxX ^ 2 + maxY ^ 2) |> toFloat |> sqrt |> round

        maxDistAllWaves = maxDist + (numAdditionalWaves * rippleWidth * 2)
    in
    round (toFloat maxDistAllWaves / ripplePropagationSpeed)


-- Ports

port playDing : Frequency -> Cmd msg


-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
        KeyPress str -> updateSoundAndGrid str model

        Tick _ -> ( trimEvents { model | now = model.now + 1 }, Cmd.none )

        Touch touchCoords -> ( model, Random.generate (TouchWithIndex touchCoords) randomIndexGenerator )

        TouchWithIndex touchCoords index -> registerTouch touchCoords index model

        ViewportChange viewport ->
            let
                dimensions = viewportToDimensions viewport
            in
            ( { model | dimensions = dimensions, maxEventEffectTime = calculateMaxEventEffectTime dimensions }, Cmd.none )

updateSoundAndGrid : String -> Model -> ( Model, Cmd Msg )
updateSoundAndGrid str model =
    let
        index = str |> keyPressToChar |> charToIndex
    in
    ( { model | events = ( index, Nothing, model.now ) :: model.events, showHelp = False }, ding index )

randomIndexGenerator : Random.Generator Int
randomIndexGenerator = Random.int 0 20

registerTouch : TouchCoord -> Index -> Model -> ( Model, Cmd Msg )
registerTouch touchCoords index model =
    let
        coords = touchCoordsToGridCoords touchCoords
    in
    ( { model | events = ( index, Just coords, model.now ) :: model.events, showHelp = False }, ding index )

trimEvents : Model -> Model
trimEvents model = { model | events = List.filter (isActiveEvent model) model.events }

isActiveEvent : Model -> Event -> Bool
isActiveEvent { now, maxEventEffectTime } ( _, _, eventTime ) = now - eventTime < maxEventEffectTime

keyPressToChar : String -> Char
keyPressToChar = String.uncons
        >> Maybe.map Tuple.first
        >> Maybe.withDefault 'a'

ding : Index -> Cmd msg
ding = indexToFreq >> playDing

indexToFreq : Index -> Frequency
indexToFreq index = baseFreq * 2 ^ (toFloat index / 12)

charToIndex : Char -> Index
charToIndex code = Char.toCode code - 97


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
        [ Browser.Events.onKeyPress keyDecoder
        , Browser.Events.onResize onWindowResize
        , Time.every tickLengthMs Tick
        ]

keyDecoder : Decode.Decoder Msg
keyDecoder = Decode.map KeyPress (Decode.field "key" Decode.string)

onWindowResize : Int -> Int -> Msg
onWindowResize = composeTwoArgs ViewportChange Tuple.pair


-- Handle touch events

touchCoordinates : Touch.Event -> TouchCoord
touchCoordinates touchEvent = List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )

touchCoordsToGridCoords : TouchCoord -> Coord
touchCoordsToGridCoords ( h, v ) = ( round h |> getDimension, round v |> getDimension )


-- View

view : Model -> Html.Html Msg
view model = div [ id "elm", Touch.onEnd (Touch << touchCoordinates) ]
        [ Grid.render model.events model.now model.dimensions model.showHelp
        ]
