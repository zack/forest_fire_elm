module ForestFire exposing (..)

import AnimationFrame exposing (..)
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, id)
import Html.Events exposing (onClick, on)
import Json.Decode exposing (string, int, list, Decoder, at)
import Random exposing (Generator, Seed, maxInt, minInt)
import Set exposing (..)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (version, viewBox, width, fill, x, y, height)
import Time exposing (..)


type alias Model =
    { forest : Forest
    , running : Bool
    , seed : Seed
    , clock : Int
    , size : Int
    , burnRate : Float
    , growthRate : Float
    , speed : Int
    , lastDrawTime : Int
    , actualFramerate : Float
    }


type alias Flags =
    { randomSeed : Int }


type alias Forest =
    List Tree


type Tree
    = Living
    | Burning
    | Dead


type Msg
    = InitForest Forest
    | IncrementForest Time
    | ToggleRunning
    | Restart
    | SetSpeed Int
    | SetGrowth Int
    | SetBurn Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitForest forest ->
            ( { model | forest = forest }
            , Cmd.none
            )

        IncrementForest msg ->
            let
                time =
                    round (inMilliseconds msg)

                actualFramerate =
                    (1000 / (toFloat (time - model.lastDrawTime)))
                        |> (*) 10
                        |> round
                        |> toFloat
                        |> (flip (/)) 10
            in
                if (time - model.lastDrawTime) < model.speed then
                    ( model, Cmd.none )
                else
                    let
                        ( newSeed, newForest ) =
                            incrementForest model
                    in
                        if model.running then
                            ( { model
                                | forest = newForest
                                , seed = newSeed
                                , clock = model.clock + 1
                                , lastDrawTime = time
                                , actualFramerate = actualFramerate
                              }
                            , Cmd.none
                            )
                        else
                            ( model, Cmd.none )

        SetSpeed sliderVal ->
            let
                newSpeed =
                    floor (clamp 0 1000 (1.05 ^ (toFloat sliderVal) * 10 + 20))
            in
                ( { model | speed = newSpeed }, Cmd.none )

        SetGrowth sliderVal ->
            ( { model | growthRate = ((toFloat sliderVal) / 1000) }, Cmd.none )

        SetBurn sliderVal ->
            ( { model | burnRate = ((toFloat sliderVal) / 100000) }, Cmd.none )

        ToggleRunning ->
            ( { model | running = (not model.running) }, Cmd.none )

        Restart ->
            let
                m =
                    initModel model.seed
            in
                ( { m | running = model.running }
                , Random.generate InitForest (forestGenerator model.size)
                )


incrementForest : Model -> ( Random.Seed, Forest )
incrementForest model =
    let
        getNeighbors : Int -> List Int
        getNeighbors idx =
            let
                leftBorder =
                    idx % model.size == 0

                rightBorder =
                    (idx + 1) % model.size == 0
            in
                [ if leftBorder then
                    -1
                  else
                    idx - 1
                , if rightBorder then
                    -1
                  else
                    idx + 1
                , idx - model.size
                , if leftBorder then
                    -1
                  else
                    idx - model.size - 1
                , if rightBorder then
                    -1
                  else
                    idx - model.size + 1
                , idx + model.size
                , if leftBorder then
                    -1
                  else
                    idx + model.size - 1
                , if rightBorder then
                    -1
                  else
                    idx + model.size + 1
                ]
                    |> List.filter (\idx -> idx >= 0 && idx < (model.size * model.size))

        ignitingTrees : Forest -> Set Int
        ignitingTrees forest =
            forest
                |> List.indexedMap
                    (\idx tree ->
                        if tree == Burning then
                            (getNeighbors idx)
                        else
                            []
                    )
                |> List.concat
                |> Set.fromList

        reductionFunction : Tree -> ( Seed, Int, Forest ) -> ( Seed, Int, Forest )
        reductionFunction tree ( seed, idx, forestSoFar ) =
            case tree of
                Burning ->
                    ( seed, idx + 1, Dead :: forestSoFar )

                Dead ->
                    let
                        ( randFloat, newSeed ) =
                            Random.step (Random.float 0 1) seed

                        newTree =
                            if randFloat < model.growthRate then
                                Living
                            else
                                Dead
                    in
                        ( newSeed, idx + 1, newTree :: forestSoFar )

                Living ->
                    let
                        ( randFloat, newSeed ) =
                            Random.step (Random.float 0 1) seed

                        newTree =
                            if randFloat < model.burnRate || (Set.member idx burningNeighbors) then
                                Burning
                            else
                                Living
                    in
                        ( newSeed, idx + 1, newTree :: forestSoFar )

        burningNeighbors =
            ignitingTrees model.forest

        ( newSeed, _, newForest ) =
            List.foldl reductionFunction ( model.seed, 0, [] ) model.forest
    in
        ( newSeed, (List.reverse newForest) )


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times IncrementForest


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text ("Forest Fire (" ++ (toString model.clock) ++ ")") ]
        , div [ class "filters" ]
            [ speedSlider model.speed model.actualFramerate
            , growthSlider model.growthRate
            , burnSlider model.burnRate
            ]
        , toggleButton model
        , button [ onClick Restart ] [ text "Restart" ]
        , forestSvg model
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
speedSlider speed actualFramerate =
    let
        frameRate =
            1000 // speed
    in
        div [ class "filter-slider" ]
            [ label [ class "name" ] [ text "Framerate" ]
            , paperSlider [ Attr.max "95", onImmediateValueChange SetSpeed ] []
            , label [ class "val" ] [ text ((toString frameRate) ++ "(" ++ (toString actualFramerate) ++ ")") ]
            ]


growthSlider : Float -> Html Msg
growthSlider speed =
    div [ class "filter-slider" ]
        [ label [ class "name" ] [ text "Growth Rate" ]
        , paperSlider [ Attr.max "1000", onImmediateValueChange SetGrowth ] []
        , label [ class "val" ] [ text (toString speed) ]
        ]


burnSlider : Float -> Html Msg
burnSlider speed =
    div [ class "filter-slider" ]
        [ label [ class "name" ] [ text "Burn Rate" ]
        , paperSlider [ Attr.max "1000", onImmediateValueChange SetBurn ] []
        , label [ class "val" ] [ text (toString speed) ]
        ]


forestSvg : Model -> Html Msg
forestSvg model =
    let
        tree xc yc color =
            rect
                [ fill color
                , x (toString xc)
                , y (toString yc)
                , width "1"
                , height "1"
                ]
                []

        burningTree x y =
            tree x y "#F98A15"

        livingTree x y =
            tree x y "#0EA27D"

        drawTree idx type_ =
            let
                y =
                    idx // model.size

                x =
                    idx % model.size
            in
                case type_ of
                    Living ->
                        livingTree x y

                    Burning ->
                        burningTree x y

                    Dead ->
                        Svg.text ""
    in
        svg
            [ version "1.1"
            , width "800"
            , height "800"
            , viewBox ("0 0 " ++ (toString model.size) ++ " " ++ (toString model.size))
            ]
            ((rect
                [ fill "#1A58A3"
                , x "0"
                , y "0"
                , width (toString model.size)
                , height (toString model.size)
                ]
                []
             )
                :: (List.indexedMap drawTree model.forest)
            )


forestGenerator : Int -> Generator Forest
forestGenerator size =
    let
        intToTree : Int -> Tree
        intToTree val =
            if val == 1 then
                Living
            else
                Dead

        treeGenerator : Generator Tree
        treeGenerator =
            Random.map intToTree (Random.int 0 1)
    in
        Random.list (size * size) treeGenerator



-- Good starting parameters are size: 100, burnRate: 0.00001, growthRate: 0.01


initModel : Seed -> Model
initModel seed =
    { forest = []
    , running = True
    , seed = seed
    , clock = 0
    , size = 40
    , burnRate = 0.0001
    , growthRate = 0.01
    , speed = 30
    , lastDrawTime = 0
    , actualFramerate = 0
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            initModel (Random.initialSeed flags.randomSeed)
    in
        ( model
        , Random.generate InitForest (forestGenerator model.size)
        )


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
