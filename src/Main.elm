module Main exposing (main)

import Browser
import Html exposing (Html, br, button, div, li, node, text, ul)
import Html.Attributes exposing (class, href, rel, style)
import Html.Events exposing (onClick)
import Time


type alias Model =
    { cash : Float
    , workers : List Float
    }


type Msg
    = Tick Time.Posix
    | ClickCash
    | ClickWorker Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 [ 0 ]
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ node "link"
            [ rel "stylesheet"
            , href "https://www.w3schools.com/w3css/4/w3.css"
            ]
            []
        , div
            [ class "w3-container w3-white"
            , style "position" "-webkit-sticky"
            , style "position" "sticky"
            , style "top" "0"
            ]
            [ button
                [ class "w3-button w3-block w3-xxlarge"
                , onClick ClickCash
                ]
                [ text ("💰 " ++ scientific model.cash) ]
            ]
        , div [ class "w3-row" ]
            [ ul [ class "w3-ul w3-col m4 l4" ]
                (List.indexedMap viewWorker model.workers)
            ]
        ]


viewWorker : Int -> Float -> Html Msg
viewWorker index count =
    li []
        [ button
            [ class "w3-button w3-block w3-xlarge w3-row"
            , onClick (ClickWorker index)
            ]
            [ div [ class "w3-col s6 m12 l6" ]
                [ text ("⛏ " ++ scientific count) ]
            , div [ class "w3-col s6 m12 l6" ]
                [ text ("💰 " ++ scientific (workerCost index)) ]
            ]
        ]


workerCost : Int -> Float
workerCost index =
    10 * 100 ^ toFloat index


scientific : Float -> String
scientific value =
    let
        exponent =
            if value == 0 then
                0

            else
                floor (logBase 1000 (abs value)) * 3

        significand =
            truncate (value / 10 ^ toFloat (exponent - 1))

        mantissa =
            significand // 10

        decimal =
            significand - mantissa * 10
    in
    if exponent == 0 && decimal == 0 then
        String.fromInt mantissa

    else
        String.fromInt mantissa
            ++ "."
            ++ String.fromInt decimal
            ++ "e"
            ++ String.fromInt exponent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case updateWorkers model.workers of
                head :: tail ->
                    ( Model (model.cash + head) (head :: tail)
                    , Cmd.none
                    )

                [] ->
                    ( Model model.cash [], Cmd.none )

        ClickCash ->
            ( { model | cash = model.cash + 1 }, Cmd.none )

        ClickWorker index ->
            let
                cost =
                    workerCost index
            in
            if model.cash >= cost then
                ( Model (model.cash - cost) (incAtIndex index model.workers)
                , Cmd.none
                )

            else
                ( model, Cmd.none )


updateWorkers : List Float -> List Float
updateWorkers workers =
    case workers of
        head :: next :: tail ->
            head + next :: updateWorkers (next :: tail)

        _ ->
            workers


incAtIndex : Int -> List Float -> List Float
incAtIndex index workers =
    case ( index, workers ) of
        ( 0, head :: tail ) ->
            head
                + 1
                :: (if tail == [] then
                        [ 0 ]

                    else
                        tail
                   )

        ( _, head :: tail ) ->
            head :: incAtIndex (index - 1) tail

        _ ->
            workers


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
