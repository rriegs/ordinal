import Browser
import Html exposing (Html, button, text)
import Html.Events exposing (onClick)


type alias Model = Int

type Msg
  = Click


init : Model
init = 0


view : Model -> Html Msg
view model =
  button [ onClick Click ]
    [ text (String.fromInt model) ]


update : Msg -> Model -> Model
update msg model =
  case msg of
    Click ->
      model + 1


main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
