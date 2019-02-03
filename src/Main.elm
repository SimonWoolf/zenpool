port module Main exposing (Model, Msg(..), init, keyDecoder, main, subscriptions, update, view)

import Browser
import Browser.Events
import Config exposing (..)
import Grid
import Html exposing (Html, div, h1, table, text)
import Html.Attributes exposing (id)
import Json.Decode as Decode
import Task
import Time
import Types exposing (..)

main = Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

type Msg
    = KeyPress String
    | Tick Time.Posix
    | ViewportChange Viewport

type alias Frequency = Float

type alias Model = { events : List Event, dimensions : Dimensions, now : Ticks, maxEventEffectTime : Ticks, showHelp : Bool }

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
    ( { events = [ ( 0, 0 ) ], dimensions = dimensions, maxEventEffectTime = calculateMaxEventEffectTime dimensions, now = 0, showHelp = True }, Cmd.none )

viewportToDimensions : Viewport -> Dimensions
viewportToDimensions ( width, height ) = ( getDimension width, getDimension height )

getDimension : Int -> Int
getDimension dimensionSize = floor (toFloat dimensionSize / (pixelSize + gapSize))

calculateMaxEventEffectTime : Dimensions -> Ticks
calculateMaxEventEffectTime ( maxX, maxY ) =
    let
        maxDist = (maxX ^ 2 + maxY ^ 2) |> toFloat |> sqrt |> round

        maxDistAllWaves = maxDist + (numAdditionalWaves * rippleWidth * 2)

        _ = Debug.log "calculateMaxEventEffectTime" (round (toFloat maxDistAllWaves / ripplePropagationSpeed))
    in
    round (toFloat maxDistAllWaves / ripplePropagationSpeed)

-- Ports

port playDing : Frequency -> Cmd msg

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
        KeyPress str -> updateSoundAndGrid str model

        Tick time -> ( { model | now = timeToTicks time }, Cmd.none )

        ViewportChange viewport ->
            let
                dimensions = viewportToDimensions viewport
            in
            ( { model | dimensions = dimensions, maxEventEffectTime = calculateMaxEventEffectTime dimensions }, Cmd.none )

timeToTicks : Time.Posix -> Ticks
timeToTicks time = round (toFloat (Time.posixToMillis time) / tickLengthMs)

updateSoundAndGrid : String -> Model -> ( Model, Cmd Msg )
updateSoundAndGrid str model =
    let
        index = str |> keyPressToChar |> charToIndex
    in
    ( trimEvents { model | events = ( index, model.now ) :: model.events, showHelp = False }, ding index )

trimEvents : Model -> Model
trimEvents model = { model | events = List.filter (isActiveEvent model) model.events }

isActiveEvent : Model -> Event -> Bool
isActiveEvent { now, maxEventEffectTime } ( _, eventTime ) = now - eventTime < maxEventEffectTime

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

-- View

view : Model -> Html.Html Msg
view model = div [ id "elm" ]
        [ Grid.render model.events model.now model.dimensions model.showHelp
        ]
