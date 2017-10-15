-- module Name exposing (Model, Msg, update, view, subscriptions, init)


import Html exposing (..)
import Html.Events exposing (onClick)
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


type alias Model =
    {   time: Time
    ,   pause: Bool
    }


type Msg
    = Tick Time
    | Pause


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            ({model | time = time}, Cmd.none)
        Pause ->
            ({model | pause = not model.pause}, Cmd.none)


view : Model -> Html Msg
view model =
    let
        angle = 
            turns (Time.inMinutes model.time) - (pi/2)
        handX = 
            toString (50 + 40 * cos angle)
        handY = 
            toString ( 50 + 40 * sin angle)
    in
        div []
            [
                svg [ viewBox "0 0 100 100", width "300px"]
                [   circle [ cx "50", cy "50",  r "45", fill "#0B79CE"] []
                ,   hand Second model.time
                ,   hand Minute model.time
                ,   hand Hour model.time
                ]
            ,   button [ onClick Pause ] [Html.text "Pause"]
            ,   Html.text <| currentTime model.time
            ]

type HandType = Second | Minute | Hour

hand : HandType -> Time -> Html Msg
hand handType time =
    let
        inFn = case handType of
            Second -> Time.inMinutes
            Minute -> Time.inHours
            Hour -> \t -> ((/) 24) <| Time.inHours <| t
        angle =
            case handType of
                Hour -> turns (inFn time) - (4*pi/6)
                _ -> turns (inFn time) - (pi/2)

        handX =
            case handType of
            Hour -> toString (50 + 30 * cos angle)
            _ -> toString (50 + 40 * cos angle)
        handY =
            case handType of
            Hour -> toString ( 50 + 30 * sin angle)
            _ -> toString ( 50 + 40 * sin angle)
    in
        case handType of
            Hour -> line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963", strokeWidth "3"] []
            _ -> line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963"] []

timeAsInt: (Time -> Float) -> Time -> Int
timeAsInt inFn time = round <| inFn time

secondsAsInt = timeAsInt inSeconds
minutesAsInt = timeAsInt inMinutes
hoursAsInt = timeAsInt inHours

currentSeconds : Time -> Int
currentSeconds time = ((secondsAsInt time)% 60)

currentMinutes : Time -> Int
currentMinutes time = ((secondsAsInt time) % 3600) // 60

currentHours : Time -> Int
currentHours time = (((secondsAsInt time) % 86400 ) // 3600 + 2)

currentTime: Time -> String
currentTime time = concat [ toString <| currentHours time
                          , ":"
                          , toString <| currentMinutes time
                          , ":"
                          , toString <| currentSeconds time
                          ] 

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.pause then Time.every second Tick else Sub.none


init : (Model, Cmd Msg)
init = 
    ({time = 0, pause = True}, Cmd.none)
