module ForestFire exposing (..)

import AnimationFrame exposing (..)
import Forest exposing (Forest, new, step, restart, svg)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, id)
import Html.Events exposing (onClick, on)
import Json.Decode exposing (string, int, list, Decoder, at)
import Time exposing (..)


type alias Model =
    { forest : Forest
    , running : Bool
    , clock : Int
    , speed : Int
    , lastDrawTime : Int
    , framerate : Float
    }


type alias Flags =
    { randomInt : Int }


type Msg
    = IncrementForest Time
    | Restart
    | SetBurnRate Int
    | SetGrowthRate Int
    | SetSpeed Int
    | ToggleRunning


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementForest msg ->
            let
                currentTime =
                    (round (inMilliseconds msg))
            in
                if (currentTime - model.lastDrawTime) < model.speed then
                    ( model, Cmd.none )
                else if model.running then
                    ( { model
                        | forest = Forest.step model.forest
                        , clock = model.clock + 1
                        , lastDrawTime = currentTime
                        , framerate = calcFramerate model.lastDrawTime currentTime
                      }
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )

        Restart ->
            ( { model | forest = Forest.restart model.forest }, Cmd.none )

        SetBurnRate sliderVal ->
            let
                burnRate =
                    (toFloat sliderVal) / 100000

                forest =
                    Forest.setBurnRate model.forest burnRate
            in
                ( { model | forest = forest }, Cmd.none )

        SetGrowthRate sliderVal ->
            let
                growthRate =
                    (toFloat sliderVal) / 1000

                forest =
                    Forest.setGrowthRate model.forest growthRate
            in
                ( { model | forest = forest }, Cmd.none )

        SetSpeed sliderVal ->
            let
                newSpeed =
                    floor (clamp 0 1000 (1.05 ^ (toFloat sliderVal) * 10 + 20))
            in
                ( { model | speed = newSpeed }, Cmd.none )

        ToggleRunning ->
            ( { model | running = (not model.running) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times IncrementForest


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text ("Forest Fire (" ++ (toString model.clock) ++ ")") ]
        , div [ class "filters" ]
            [ speedSlider model.speed model.framerate
            , rateSlider "GrowthRate" SetGrowthRate model.forest.growthRate
            , rateSlider "Burnrate" SetBurnRate model.forest.burnRate
            ]
        , toggleButton model
        , button [ onClick Restart ] [ text "Restart" ]
        , Forest.svg model.forest
        ]


toggleButton : Model -> Html Msg
toggleButton model =
    let
        label =
            if model.running then
                "Pause"
            else
                "Start"
    in
        button [ onClick ToggleRunning ] [ text label ]


speedSlider : Int -> Float -> Html Msg
speedSlider speed framerate =
    let
        effectiveFramerate =
            1000 // speed
    in
        div [ class "filter-slider" ]
            [ label [ class "name" ] [ text "Framerate" ]
            , paperSlider [ Attr.max "95", onImmediateValueChange SetSpeed ] []
            , label [ class "val" ] [ text ((toString effectiveFramerate) ++ "(" ++ (toString framerate) ++ ")") ]
            ]


rateSlider : String -> (Int -> Msg) -> Float -> Html Msg
rateSlider name msg rate =
    div [ class "filter-slider" ]
        [ label [ class "name" ] [ text name ]
        , paperSlider [ Attr.max "100", onImmediateValueChange msg ] []
        , label [ class "val" ] [ text (toString rate) ]
        ]



-- Good starting parameters are edgeLength: 100, burnRate: 0.00001, growthRate: 0.01


initModel : Int -> Model
initModel randomInt =
    let
        burnRate =
            0.0001

        growthRate =
            0.01

        edgeLength =
            40

        speed =
            30

        initForest =
            Forest.new burnRate growthRate edgeLength randomInt
    in
        { forest = initForest
        , running = True
        , clock = 0
        , speed = speed
        , lastDrawTime = 0
        , framerate = 0
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel flags.randomInt, Cmd.none )


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider =
    node "paper-slider"


onImmediateValueChange : (Int -> msg) -> Attribute msg
onImmediateValueChange toMsg =
    at [ "target", "immediateValue" ] int
        |> Json.Decode.map toMsg
        |> on "immediate-value-changed"


calcFramerate : Int -> Int -> Float
calcFramerate lastDrawTime currentTime =
    (currentTime - lastDrawTime)
        |> toFloat
        |> (/) 10000
        |> round
        |> toFloat
        |> (flip (/)) 10
