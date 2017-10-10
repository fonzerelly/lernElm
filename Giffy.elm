import Html exposing (..)
import Html.Events exposing (onClick, onInput, on)
import Html.Attributes exposing (src, selected, value, style)
import Http
import Json.Decode as Decode

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


type alias Model =
    { 
        topic: String,
        gifUrl: String,
        err: String
    }


type Msg = 
    MorePlease |
    UpdateTopic String |
    NewGif (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MorePlease ->
            (model, getRandomGif model.topic)

        NewGif (Ok newUrl) ->
            ({ model | err = "", gifUrl = newUrl }, Cmd.none)

        NewGif (Err error) ->
            ({model | err = (toString error)}, Cmd.none)

        UpdateTopic topic ->
            ({ model | topic = topic }, getRandomGif model.topic)

getRandomGif: String -> Cmd Msg
getRandomGif topic =
    let
        url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
        request = Http.get url decodeGifUrl
    in
        Http.send NewGif request

decodeGifUrl: Decode.Decoder String
decodeGifUrl = Decode.at ["data", "image_url"] Decode.string


view : Model -> Html Msg
view model =
    div [] [
        h1 [] [text "Giffy"],
--        select [onInput UpdateTopic ] [
--            option [] [text "cats"],
--            option [] [text "dogs"],
--            option [] [text "birds"],
--            option [] [text "mice"],
--            option [] [text "elefants"]
--        ],
        input [ onInput UpdateTopic ] [text model.topic],
        if model.err == "" then img [ src model.gifUrl] [] else span [style [("color", "red")]] [text model.err],
        br [] [],
        button [ onClick MorePlease ] [ text "More Please!"]
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : (Model, Cmd Msg)
init = 
    (Model "cats" "waiting.gif" "", Cmd.none)
