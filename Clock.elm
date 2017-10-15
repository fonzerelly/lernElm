-- module Name exposing (Model, Msg, update, view, subscriptions, init)


import Html exposing (..)
import String exposing (concat)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second, inSeconds, inMinutes, inHours)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


type alias Model = Time


type Msg
    = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            (time, Cmd.none)


view : Model -> Html Msg
view model =
    let
        angle = 
            turns (Time.inMinutes model)
        handX = 
            toString (50 + 40 * cos angle)
        handY = 
            toString ( 50 + 40 * sin angle)
    in
        div []
            [
                svg [ viewBox "0 0 100 100", width "300px"]
                [circle [ cx "50", cy "50",  r "45", fill "#0B79CE"] []
                , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963"] []  
                ]
            ,    Html.text <| currentTime model
            ]

timeAsInt: (Time -> Float) -> Time -> Int
timeAsInt inFn time = round <| inFn time

secondsAsInt = timeAsInt inSeconds
minutesAsInt = timeAsInt inMinutes
hoursAsInt = timeAsInt inHours

currentSeconds : Model -> Int
currentSeconds time = ((secondsAsInt time)% 60)

currentMinutes : Model -> Int
currentMinutes time = ((secondsAsInt time) % 3600) // 60

currentHours : Model -> Int
currentHours time = (((secondsAsInt time) % 86400 ) // 3600 + 2)

currentTime: Model -> String
currentTime time = concat [ toString <| currentHours time
                          , ":"
                          , toString <| currentMinutes time
                          , ":"
                          , toString <| currentSeconds time
                          ] 

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


init : (Model, Cmd Msg)
init = 
    (0, Cmd.none)
