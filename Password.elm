import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit, onClick)
import String exposing (..)
--import Regex exposing (..)
import Regex
import ParseInt

main = Html.beginnerProgram {
    view = view,
    update = update,
    model = model
    }

type alias Model = 
    {
        age: String,
        name: String,
        password: String,
        passwordAgain: String,
        validate : Bool
    }

model: Model
model = Model "" "" "" "" False

type Msg =
    Age String |
    Name String |
    Password String |
    PasswordAgain String |
    Validate


update: Msg -> Model -> Model
update msg model =
    let
        _ = Debug.log "#####" model
    in    
        case msg of 
            Age age ->
                { model | age = age, validate = False }

            Name name -> 
                { model | name = name, validate = False }

            Password password ->
                { model | password = password, validate = False }

            PasswordAgain password ->
                { model | passwordAgain = password, validate = False }

            Validate ->
                { model | validate = True }
        
view: Model -> Html Msg
view model =
    div [] [
        h1 [] [text "Password"],
        input [ type_ "text", placeholder "Name", onInput Name ] [],
        input [ type_ "text", placeholder "Age", onInput Age ] [],
        input [ type_ "password", placeholder "Password", onInput Password] [],
        input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain] [],
        input [ type_ "submit", onClick Validate] [ text "Send" ],
        if model.validate then viewValidationModel model else text ""
    ]

checkPasswordTyping: Result.Result String Model -> Result.Result String Model
checkPasswordTyping resultOfModel = case resultOfModel of
    Ok model -> if model.password == model.passwordAgain then Ok model else Err "Password does not match"
    Err e -> Err e

checkPasswordLength: Result.Result String Model -> Result.Result String Model
checkPasswordLength resultOfModel = case resultOfModel of
    Ok model -> if length model.password > 3 then Ok model else Err "Password is too short"
    Err e -> Err e

checkPasswordRule: Regex.Regex -> String -> Result.Result String Model -> Result.Result String Model
checkPasswordRule rule msg resultOfModel = case resultOfModel of
    Ok model -> if Regex.contains rule model.password then Ok model else Err ("Password does not contain " ++ msg)
    Err e -> Err e

checkAge: Result.Result String Model -> Result.Result String Model
checkAge resultOfModel = case resultOfModel of
    Ok model -> 
        let
            resultOfParseInt = ParseInt.parseInt model.age
        in
            case resultOfParseInt of
                Ok age -> Ok { model| age = toString age }
                Err e -> Err "Age is no valid Number"
    Err e -> Err e

viewValidationModel: Model -> Html Msg
viewValidationModel model = 
    let
        validation = (checkPasswordRule (Regex.regex "[A-Z]") "upper case") <|
                     (checkPasswordRule (Regex.regex "[a-z]") "lower case") <|
                     (checkPasswordRule (Regex.regex "\\d") "digit") <|
                     checkPasswordLength <| 
                     checkPasswordTyping <| 
                     checkAge <|
                     Ok model
    in
        case validation of 
            Ok m -> div [ style [("color", "green")]] [ text "Ok" ]
            Err msg -> div [ style [("color", "red")]] [ text msg]