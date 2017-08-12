module ForestFire exposing (..)

import Array exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Random exposing (Generator, maxInt, minInt)
import Set exposing (..)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (version, viewBox, width, fill, x, y, height)
import Time exposing (Time, millisecond)


type alias Model =
    { forest : Forest
    , running : Bool
    , seed : Random.Seed
    , clock : Int
    , speed : Float
    , size : Int
    , burnRate : Float
    , growthRate : Float
    , probabilities : List Float
    }


type alias Forest =
    List Tree


type Tree
    = Living
    | Burning
    | Dead


type Msg
    = InitForest Forest
    | SetSeed Int
    | IncrementForest Time
    | ToggleRunning
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( probabilities, seed ) =
            getNewProbabilities model
    in
        case msg of
            InitForest forest ->
                ( { model
                    | forest = forest
                    , probabilities = probabilities
                    , seed = seed
                  }
                , Random.generate SetSeed (Random.int maxInt minInt)
                )

            SetSeed seed ->
                ( { model | seed = Random.initialSeed seed }, Cmd.none )

            IncrementForest _ ->
                if model.running then
                    ( { model
                        | forest = incrementForest model
                        , probabilities = probabilities
                        , seed = seed
                        , clock = model.clock + 1
                      }
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )

            ToggleRunning ->
                ( { model | running = (not model.running) }, Cmd.none )

            Restart ->
                ( { initModel | running = model.running }
                , Random.generate InitForest (forestGenerator initModel)
                )


incrementForest : Model -> Forest
incrementForest model =
    let
        arrProbabilities : Array Float
        arrProbabilities =
            Array.fromList model.probabilities

        getNeighbors : Int -> List Int
        getNeighbors idx =
            [ idx - 1
            , idx + 1
            , idx - model.size
            , idx - model.size - 1
            , idx - model.size + 1
            , idx + model.size
            , idx + model.size - 1
            , idx + model.size + 1
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

        transformTree : Set Int -> Int -> Tree -> Tree
        transformTree igniting idx tree =
            case tree of
                Living ->
                    if Set.member idx igniting then
                        Burning
                    else
                        let
                            prob =
                                Maybe.withDefault 0 (Array.get idx arrProbabilities)
                        in
                            if prob < model.burnRate then
                                Burning
                            else
                                Living

                Burning ->
                    Dead

                Dead ->
                    let
                        prob =
                            Maybe.withDefault 0 (Array.get idx arrProbabilities)
                    in
                        if prob < model.growthRate then
                            Living
                        else
                            Dead
    in
        let
            igniting =
                ignitingTrees model.forest
        in
            List.indexedMap (transformTree igniting) model.forest


getNewProbabilities : Model -> ( List Float, Random.Seed )
getNewProbabilities model =
    Random.step (probabilityGenerator model) model.seed


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (model.speed * millisecond) IncrementForest


view : Model -> Html Msg
view model =
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

        deadTree x y =
            tree x y "#1A58A3"

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
                        deadTree x y
    in
        div [ class "content" ]
            [ h1 [] [ text ("Forest Fire (" ++ (toString model.clock) ++ ")") ]
            , toggleButton model
            , restartButton
            , div [] [ text (toString model.seed) ]
            , div []
                [ svg
                    [ version "1.1"
                    , width "800"
                    , height "800"
                    , viewBox ("0 0 " ++ (toString model.size) ++ " " ++ (toString model.size))
                    ]
                    (List.indexedMap drawTree model.forest)
                ]
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


restartButton : Html Msg
restartButton =
    button [ onClick Restart ] [ text "Restart" ]


forestGenerator : Model -> Generator Forest
forestGenerator model =
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
        Random.list (model.size * model.size) treeGenerator


probabilityGenerator : Model -> Generator (List Float)
probabilityGenerator model =
    Random.list (model.size * model.size) (Random.float 0 1)



-- Good starting parameters are size: 100, burnRate: 0.00001, growthRate: 0.01


initModel : Model
initModel =
    { forest = []
    , running = True
    , seed = Random.initialSeed 0
    , speed = 1
    , clock = 0
    , size = 80
    , burnRate = 0.00001
    , growthRate = 0.01
    , probabilities = []
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Random.generate InitForest (forestGenerator initModel) )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
