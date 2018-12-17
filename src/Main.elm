port module Main exposing (Model, Msg(..), init, keyDecoder, main, subscriptions, update, view)

import Browser
import Browser.Events
import Grid
import Html exposing (Html, div, h1, table, text)
import Html.Attributes exposing (id)
import Json.Decode as Decode

main = Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

type Msg
    = KeyPress String

type alias Model = { latestKeyPress : String, latestIndex : Int }

type alias Frequency = Float

baseFreq : Frequency
baseFreq = 220


-- Init

init : () -> ( Model, Cmd Msg )
init _ = ( { latestKeyPress = "", latestIndex = 0 }, Cmd.none )


-- Ports

port playDing : Frequency -> Cmd msg


-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
        KeyPress str -> updateSoundAndGrid str model

updateSoundAndGrid : String -> Model -> ( Model, Cmd Msg )
updateSoundAndGrid str model =
    let
        index = str |> keyPressToChar |> charToIndex
    in
    ( { model | latestKeyPress = str, latestIndex = index }, ding index )

keyPressToChar : String -> Char
keyPressToChar = String.uncons
        >> Maybe.map Tuple.first
        >> Maybe.withDefault 'a'

ding : Int -> Cmd msg
ding = indexToFreq >> playDing

indexToFreq : Int -> Frequency
indexToFreq index = baseFreq * 2 ^ (toFloat index / 12)

charToIndex : Char -> Int
charToIndex code = Char.toCode code - 97


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Browser.Events.onKeyPress keyDecoder

keyDecoder : Decode.Decoder Msg
keyDecoder = Decode.map KeyPress (Decode.field "key" Decode.string)


-- View

view : Model -> Html.Html Msg
view model = div [ id "elm" ]
        [ h1 [] [ text model.latestKeyPress ]

        --, table [ id "grid" ] []
        , Grid.grid model.latestIndex
        ]
