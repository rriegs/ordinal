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
    , selfGrowth : Float
    }


uClickPower : Float -> Model -> Model
uClickPower v m =
    { m | clickPower = v }


uWorkerRate : Float -> Model -> Model
uWorkerRate v m =
    { m | workerRate = v }


uClickBonus : Float -> Model -> Model
uClickBonus v m =
    { m | clickBonus = v }


uSelfGrowth : Float -> Model -> Model
uSelfGrowth v m =
    { m | selfGrowth = v }


type Msg
    = Tick Time.Posix
    | ClickCash
    | ClickWorker Int
    | ClickClickPower
    | ClickWorkerRate
    | ClickClickBonus
    | ClickSelfGrowth


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cash = 0
      , workers = [ 0 ]
      , clickPower = 1
      , workerRate = 1
      , clickBonus = 0
      , selfGrowth = 0
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
            [ ulClickable (List.indexedMap viewWorker model.workers)
            , ulClickable (viewUpgrades model)
            ]
        ]


viewWorker : Int -> Float -> Html Msg
viewWorker index count =
    liClickable (ClickWorker index)
        ("⛏ " ++ scientific count)
        ("💰 " ++ scientific (workerCost (toFloat index)))


viewUpgrades : Model -> List (Html Msg)
viewUpgrades model =
    [ liClickable ClickClickPower
        ("Click power: " ++ scientific model.clickPower)
        ("💰 " ++ scientific (clickPowerCost model.clickPower))
    , liClickable ClickWorkerRate
        ("Worker rate: " ++ scientific model.workerRate)
        ("💰 " ++ scientific (workerRateCost model.workerRate))
    , liClickable ClickClickBonus
        ("Click bonus: " ++ scientific model.clickBonus ++ "%")
        ("💰 " ++ scientific (clickBonusCost model.clickBonus))
    , liClickable ClickSelfGrowth
        ("Self growth: " ++ scientific model.selfGrowth ++ "%")
        ("💰 " ++ scientific (selfGrowthCost model.selfGrowth))
    ]


ulClickable : List (Html Msg) -> Html Msg
ulClickable =
    ul
        [ class "w3-ul w3-col m4 l4"
        , style "height" "calc(100vh - 86px)"
        , style "overflow-y" "scroll"
        ]


liClickable : Msg -> String -> String -> Html Msg
liClickable msg left right =
    li []
        [ button
            [ class "w3-button w3-block w3-xlarge w3-row"
            , onClick msg
            ]
            [ div [ class "w3-col s6 m12 l6" ]
                [ text left ]
            , div [ class "w3-col s6 m12 l6" ]
                [ text right ]
            ]
        ]


workerCost : Float -> Float
workerCost index =
    10 * 100 ^ index * index ^ index


clickPowerCost : Float -> Float
clickPowerCost power =
    power * 100 ^ power


workerRateCost : Float -> Float
workerRateCost rate =
    10 * rate ^ 2 * 1000 ^ rate


clickBonusCost : Float -> Float
clickBonusCost bonus =
    1000 * (bonus + 1) ^ 3 * 1000 ^ (bonus + 1)


selfGrowthCost : Float -> Float
selfGrowthCost growth =
    100000 * (growth + 1) ^ 4 * 1000 ^ (growth + 1)


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
                        , workers = updateWorkers model model.workers
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
                    workerCost (toFloat index)

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
            buyUpgrade clickPowerCost .clickPower uClickPower model

        ClickWorkerRate ->
            buyUpgrade workerRateCost .workerRate uWorkerRate model

        ClickClickBonus ->
            buyUpgrade clickBonusCost .clickBonus uClickBonus model

        ClickSelfGrowth ->
            buyUpgrade selfGrowthCost .selfGrowth uSelfGrowth model


buyUpgrade :
    (Float -> Float)
    -> (Model -> Float)
    -> (Float -> Model -> Model)
    -> Model
    -> Model
buyUpgrade upgradeCost getUpgrade setUpgrade model =
    let
        level =
            getUpgrade model

        cost =
            upgradeCost level
    in
    if model.cash >= cost then
        setUpgrade (level + 1) { model | cash = model.cash - cost }

    else
        model


getBonus : Int -> List Float -> Float
getBonus index workers =
    case List.drop index workers of
        head :: tail ->
            head / 100

        _ ->
            0


updateWorkers : Model -> List Float -> List Float
updateWorkers model workers =
    let
        growth =
            1 + model.selfGrowth / 100
    in
    case workers of
        head :: next :: tail ->
            (head * growth + next * model.workerRate)
                :: updateWorkers model (next :: tail)

        head :: [] ->
            [ head * growth ]

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
