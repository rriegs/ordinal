import Browser
import Html exposing (Html, ul, li, button, text)
import Html.Events exposing (onClick)
import Time


type alias Model = List Int

type Msg
  = Tick Time.Posix
  | Click Int


init : () -> ( Model, Cmd Msg )
init _ =
  ( [0]
  , Cmd.none
  )


view : Model -> Html Msg
view model =
  ul [] (List.indexedMap viewButton model)

viewButton : Int -> Int -> Html Msg
viewButton index count =
  li []
    [ button [ onClick (Click index) ]
        [ text (String.fromInt count) ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick _ ->
      (updateCounts model, Cmd.none)

    Click index ->
      (incAtIndex index model, Cmd.none)

updateCounts : List Int -> List Int
updateCounts counts =
  case counts of
    (head :: next :: tail) ->
      (head + next :: updateCounts (next :: tail))

    _ -> counts

incAtIndex : Int -> List Int -> List Int
incAtIndex index counts =
  case (index, counts) of
    (0, head :: tail) ->
      (head + 1 :: (if tail == [] then [0] else tail))

    (1, head :: next :: tail) ->
      if head >= 10 then
        (head - 10 :: next + 1 :: (if tail == [] then [0] else tail))
      else
        counts

    (_, head :: tail) ->
      (head :: incAtIndex (index - 1) tail)

    _ -> counts


subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every 1000 Tick


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
