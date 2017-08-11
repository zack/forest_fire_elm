module ForestFire exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Random exposing (Generator, maxInt, minInt)
import Time exposing (Time, millisecond)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (version, viewBox, width, fill, x, y, height)


type alias Model =
    { forest : Forest
    , running : Bool
    , seed : Random.Seed
    , speed : Float
    , size : Int
    , burnRate : Float
    , growthRate : Float
    , probabilities : List (List Float)
    }


type alias Forest =
    List (List Tree)


type Tree
    = Living
    | Burning
    | Dead


type Msg
    = InitForest Forest
    | SetSeed Int
    | IncrementForest Time


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
                    , running = True
                  }
                , Random.generate SetSeed (Random.int maxInt minInt)
                )

            SetSeed seed ->
                ( { model | seed = Random.initialSeed seed, running = True }, Cmd.none )

            IncrementForest _ ->
                if model.running then
                    ( { model
                        | forest = incrementForest model
                        , probabilities = probabilities
                        , seed = seed
                      }
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )


incrementForest : Model -> Forest
incrementForest model =
    let
        arrForest : Array (Array Tree)
        arrForest =
            Array.fromList (List.map Array.fromList model.forest)

        arrProbabilities : Array (Array Float)
        arrProbabilities =
            Array.fromList (List.map Array.fromList model.probabilities)

        transformTree : Int -> Int -> Tree -> Tree
        transformTree row_idx col_idx tree =
            case tree of
                Living ->
                    let
                        probability =
                            case Array.get row_idx arrProbabilities of
                                Just row ->
                                    Array.get col_idx row

                                Nothing ->
                                    Nothing
                    in
                        case probability of
                            Just value ->
                                if value < model.burnRate then
                                    Burning
                                else
                                    Living

                            Nothing ->
                                Dead

                Burning ->
                    Dead

                Dead ->
                    let
                        probability =
                            case Array.get row_idx arrProbabilities of
                                Just row ->
                                    Array.get col_idx row

                                Nothing ->
                                    Nothing
                    in
                        case probability of
                            Just value ->
                                if value < model.growthRate then
                                    Living
                                else
                                    Dead

                            Nothing ->
                                Dead

        transformRow : Int -> Array Tree -> List Tree
        transformRow row_idx row =
            Array.toList (Array.indexedMap (transformTree row_idx) row)
    in
        Array.toList (Array.indexedMap transformRow arrForest)


getNewProbabilities : Model -> ( List (List Float), Random.Seed )
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

        drawTree x y type_ =
            case type_ of
                Living ->
                    livingTree x y

                Burning ->
                    burningTree x y

                Dead ->
                    deadTree x y

        drawRow y row =
            List.indexedMap (drawTree y) row
    in
        div [ class "content" ]
            [ h1 [] [ text "Forest Fire" ]
            , div []
                [ svg
                    [ version "1.1"
                    , width "800"
                    , height "800"
                    , viewBox ("0 0 " ++ (toString model.size) ++ " " ++ (toString model.size))
                    ]
                    (List.concat (List.indexedMap drawRow model.forest))
                ]
            ]


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
        Random.list model.size (Random.list model.size treeGenerator)


probabilityGenerator : Model -> Generator (List (List Float))
probabilityGenerator model =
    Random.list model.size (Random.list model.size (Random.float 0 1))


initModel : Model
initModel =
    { forest = []
    , running = False
    , seed = Random.initialSeed 0
    , speed = 10
    , size = 100
    , burnRate = 0.01
    , growthRate = 0.5
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
