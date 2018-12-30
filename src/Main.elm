port module Main exposing (Model, Msg(..), init, keyDecoder, main, subscriptions, update, view)

import Types exposing (Viewport, Index, Ticks, Event)
import Browser
import Browser.Events
import Grid
import Html exposing (Html, div, h1, table, text)
import Html.Attributes exposing (id)
import Json.Decode as Decode
import Time

main = Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

type Msg
    = KeyPress String
    | Tick Time.Posix

type alias Frequency = Float

type alias Model = { events : List Event, viewport : Viewport, ticks : Ticks }

baseFreq : Frequency
baseFreq = 220

-- Init

init : Viewport -> ( Model, Cmd Msg )
init viewport = ( { events = [], viewport = viewport, ticks = 0 }, Cmd.none )

-- Ports

port playDing : Frequency -> Cmd msg

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
        KeyPress str -> updateSoundAndGrid str model

        Tick time -> ( { model | ticks = (round (toFloat (Time.posixToMillis time) / 10)) }, Cmd.none )

updateSoundAndGrid : String -> Model -> ( Model, Cmd Msg )
updateSoundAndGrid str model =
    let
        index = str |> keyPressToChar |> charToIndex
    in
        ( { model | events = ( index, model.ticks ) :: model.events |> trimEvents }, ding index )

-- TODO filter any events more than a few seconds old

trimEvents : List Event -> List Event
trimEvents = List.take 100

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
        , Time.every 50 Tick
        ]

keyDecoder : Decode.Decoder Msg
keyDecoder = Decode.map KeyPress (Decode.field "key" Decode.string)

-- View

view : Model -> Html.Html Msg
view model = div [ id "elm" ]
        [ Grid.render model.events model.ticks model.viewport
        ]
