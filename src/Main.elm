module Main exposing (main)

import Browser
import Html exposing (Html, br, button, div, li, node, text, ul)
import Html.Attributes exposing (class, href, rel, style)
import Html.Events exposing (onClick)
import Time


type alias Model =
    { cash : Float
    , workers : List Float
    , clickPower : Float
    , workerRate : Float
    , clickBonus : Float
    }


type Msg
    = Tick Time.Posix
    | ClickCash
    | ClickWorker Int
    | ClickClickPower
    | ClickWorkerRate
    | ClickClickBonus


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cash = 0
      , workers = [ 0 ]
      , clickPower = 1
      , workerRate = 1
      , clickBonus = 0
      }
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
            [ ul
                [ class "w3-ul w3-col m4 l4"
                , style "height" "calc(100vh - 86px)"
                , style "overflow-y" "scroll"
                ]
                (List.indexedMap viewWorker model.workers)
            , ul
                [ class "w3-ul w3-col m4 l4"
                , style "height" "calc(100vh - 86px)"
                , style "overflow-y" "scroll"
                ]
                (viewUpgrades model)
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


viewUpgrades : Model -> List (Html Msg)
viewUpgrades model =
    [ li []
        [ button
            [ class "w3-button w3-block w3-xlarge w3-row"
            , onClick ClickClickPower
            ]
            [ div [ class "w3-col s6 m12 l6" ]
                [ text ("Click power: " ++ scientific model.clickPower) ]
            , div [ class "w3-col s6 m12 l6" ]
                [ text ("💰 " ++ scientific (clickPowerCost model.clickPower)) ]
            ]
        ]
    , li []
        [ button
            [ class "w3-button w3-block w3-xlarge w3-row"
            , onClick ClickWorkerRate
            ]
            [ div [ class "w3-col s6 m12 l6" ]
                [ text ("Worker rate: " ++ scientific model.workerRate) ]
            , div [ class "w3-col s6 m12 l6" ]
                [ text ("💰 " ++ scientific (workerRateCost model.workerRate)) ]
            ]
        ]
    , li []
        [ button
            [ class "w3-button w3-block w3-xlarge w3-row"
            , onClick ClickClickBonus
            ]
            [ div [ class "w3-col s6 m12 l6" ]
                [ text ("Click bonus: " ++ scientific model.clickBonus ++ "%") ]
            , div [ class "w3-col s6 m12 l6" ]
                [ text ("💰 " ++ scientific (clickBonusCost model.clickBonus)) ]
            ]
        ]
    ]


workerCost : Int -> Float
workerCost index =
    10 * 100 ^ toFloat index


clickPowerCost : Float -> Float
clickPowerCost power =
    100 ^ power


workerRateCost : Float -> Float
workerRateCost rate =
    1000 ^ rate


clickBonusCost : Float -> Float
clickBonusCost bonus =
    10 * 1000 ^ (bonus + 1)


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
            abs (significand - mantissa * 10)
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
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        Tick _ ->
            case model.workers of
                head :: tail ->
                    { model
                        | cash = model.cash + head * model.workerRate
                        , workers = updateWorkers model.workerRate model.workers
                    }

                [] ->
                    model

        ClickCash ->
            let
                bonus =
                    model.clickBonus * getBonus 0 model.workers

                clicks =
                    toFloat (floor (model.clickPower * (bonus + 1)))
            in
            { model | cash = model.cash + clicks }

        ClickWorker index ->
            let
                cost =
                    workerCost index

                bonus =
                    model.clickBonus * getBonus (index + 1) model.workers

                clicks =
                    toFloat (floor (model.clickPower * (bonus + 1)))

                maxPurchase =
                    min clicks (toFloat (floor (model.cash / cost)))
            in
            if model.cash >= cost then
                { model
                    | cash = model.cash - cost * maxPurchase
                    , workers = incAtIndex maxPurchase index model.workers
                }

            else
                model

        ClickClickPower ->
            let
                cost =
                    clickPowerCost model.clickPower
            in
            if model.cash >= cost then
                { model
                    | cash = model.cash - cost
                    , clickPower = model.clickPower + 1
                }

            else
                model

        ClickWorkerRate ->
            let
                cost =
                    workerRateCost model.workerRate
            in
            if model.cash >= cost then
                { model
                    | cash = model.cash - cost
                    , workerRate = model.workerRate + 1
                }

            else
                model

        ClickClickBonus ->
            let
                cost =
                    clickBonusCost model.clickBonus
            in
            if model.cash >= cost then
                { model
                    | cash = model.cash - cost
                    , clickBonus = model.clickBonus + 1
                }

            else
                model


getBonus : Int -> List Float -> Float
getBonus index workers =
    case List.drop index workers of
        head :: tail ->
            head / 100

        _ ->
            0


updateWorkers : Float -> List Float -> List Float
updateWorkers rate workers =
    case workers of
        head :: next :: tail ->
            head + next * rate :: updateWorkers rate (next :: tail)

        _ ->
            workers


incAtIndex : Float -> Int -> List Float -> List Float
incAtIndex power index workers =
    case ( index, workers ) of
        ( 0, head :: [] ) ->
            head + power :: [ 0 ]

        ( 0, head :: tail ) ->
            head + power :: tail

        ( _, head :: tail ) ->
            head :: incAtIndex power (index - 1) tail

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
