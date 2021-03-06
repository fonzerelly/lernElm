import Html exposing (Html, button, div, text, h1)
import Html.Events exposing(onClick)

main = Html.beginnerProgram {
    model = model,
    view = view,
    update = update
 }

type alias Model = Int

model: Model
model = 0

type Msg = Increment | Decrement | Reset

update: Msg -> Model -> Model 
update msg model = 
    case msg of
        Increment ->
            model + 1
        
        Decrement ->
            model - 1

        Reset ->
            0

view : Model -> Html Msg
view model = 
    div [] [
        h1 [] [ text "Counter" ],
        button [onClick Decrement ] [ text "minus" ],
        div [] [text <| toString model ],
        button [onClick Increment ] [ text "plus"],
        button [onClick Reset ] [ text "reset"]
    ]