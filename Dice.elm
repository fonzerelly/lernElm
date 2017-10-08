import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src)
import Random
import Svg exposing (svg, circle)
import Svg.Attributes exposing (r, cx, cy, x, y, version, viewBox, width, height)

main = Html.program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
 }

type alias Model = {
    dieFace: Int
 }

init: (Model, Cmd Msg)
init = 
    (Model 1, Cmd.none)

dice: Int -> Html Msg
dice num =
    case num of
        1 ->
            svg [ version "1.1", width "60", height "60"] [
                circle [ cx "30", cy "30", r "5" ] []
            ]
        2 ->
            svg [ version "1.1", width "60", height "60"] [
                circle [ cx "10", cy "10", r "5" ] [],
                circle [ cx "50", cy "50", r "5"] []
            ]
        3 ->
            svg [ version "1.1", width "60", height "60"] [
                circle [ cx "10", cy "10", r "5" ] [],
                circle [ cx "30", cy "30", r "5" ] [],
                circle [ cx "50", cy "50", r "5"] []
            ]
        4 ->
            svg [ version "1.1", width "60", height "60"] [
                circle [ cx "10", cy "10", r "5" ] [],
                circle [ cx "10", cy "50", r "5" ] [],
                circle [ cx "50", cy "10", r "5" ] [],
                circle [ cx "50", cy "50", r "5"] []
            ]
        5 ->
            svg [ version "1.1", width "60", height "60"] [
                circle [ cx "10", cy "10", r "5" ] [],
                circle [ cx "10", cy "50", r "5" ] [],
                circle [ cx "30", cy "30", r "5" ] [],
                circle [ cx "50", cy "10", r "5" ] [],
                circle [ cx "50", cy "50", r "5"] []
            ]
        6 ->
            svg [ version "1.1", width "60", height "60"] [
                circle [ cx "10", cy "10", r "5" ] [],
                circle [ cx "10", cy "50", r "5" ] [],
                circle [ cx "10", cy "30", r "5" ] [],
                circle [ cx "50", cy "30", r "5" ] [],
                circle [ cx "50", cy "10", r "5" ] [],
                circle [ cx "50", cy "50", r "5"] []
            ]
        _ -> text <| toString num ++ " is not defined as dice"
            

view: Model -> Html Msg
view model = 
    div [] [
        h1 [] [ text "Würfel" ],
        dice model.dieFace,
        br [] [],  
        button [onClick Roll] [text "Roll"]
    ]

type Msg = 
    Roll |
    NewFace Int

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        _ = Debug.log "####" (msg, model)
    in
        case msg of
            Roll ->
                (model, Random.generate NewFace (Random.int 1 6))

            NewFace newFace ->
                (Model newFace, Cmd.none)

subscriptions: Model -> Sub Msg
subscriptions model = Sub.none
