import Browser
import Html exposing (Html, div, ul, li, button, text)
import Html.Events exposing (onClick)
import Time


type alias Model =
  { cash : Int
  , workers : List Int
  }

type Msg
  = Tick Time.Posix
  | ClickCash
  | ClickWorker Int


init : () -> ( Model, Cmd Msg )
init _ =
  ( (Model 0 [0])
  , Cmd.none
  )


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick ClickCash ]
        [ text ("💰 " ++ (String.fromInt model.cash)) ]
    , ul [] (List.indexedMap viewWorker model.workers)
    ]

viewWorker : Int -> Int -> Html Msg
viewWorker index count =
  li []
    [ button [ onClick (ClickWorker index) ]
        [ text ("⛏ " ++ (String.fromInt count))
        , text ("💰 " ++ (String.fromInt (workerCost index)))
        ]
    ]

workerCost : Int -> Int
workerCost index =
  10 * 100 ^ index


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick _ ->
      case updateWorkers model.workers of
        (head :: tail) ->
          ( Model (model.cash + head) (head :: tail)
          , Cmd.none
          )

        [] ->
          (Model model.cash [], Cmd.none)

    ClickCash ->
      ({model | cash = model.cash + 1}, Cmd.none)

    ClickWorker index ->
      let
        cost = workerCost index
      in
        if model.cash >= cost then
          ( Model (model.cash - cost) (incAtIndex index model.workers)
          , Cmd.none
          )

        else (model, Cmd.none)

updateWorkers : List Int -> List Int
updateWorkers workers =
  case workers of
    (head :: next :: tail) ->
      (head + next :: updateWorkers (next :: tail))

    _ -> workers

incAtIndex : Int -> List Int -> List Int
incAtIndex index workers =
  case (index, workers) of
    (0, head :: tail) ->
      (head + 1 :: (if tail == [] then [0] else tail))

    (_, head :: tail) ->
      (head :: incAtIndex (index - 1) tail)

    _ -> workers


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
