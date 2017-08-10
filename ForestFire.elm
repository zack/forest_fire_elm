module ForestFire exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, id)
import Random exposing (Generator)


type alias Model =
    { forest : Forest }


type alias Forest =
    List (List Tree)


type Tree
    = Living
    | Burning
    | None


type Msg
    = InitForest Forest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitForest forest ->
            ( { model | forest = forest }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Forest Fire" ]
        , viewForest model.forest
        ]


viewForest : Forest -> Html Msg
viewForest forest =
    let
        treeToText : Tree -> Html Msg
        treeToText tree =
            case tree of
                Living ->
                    div [ class "tree" ] [ text "T" ]

                Burning ->
                    div [ class "burning" ] [ text "B" ]

                None ->
                    div [ class "none" ] [ text "N" ]

        viewRow : List Tree -> Html Msg
        viewRow row =
            div [ class "row" ] (List.map treeToText row)
    in
        div [ id "forest" ] (List.map viewRow forest)


forestGenerator : Generator Forest
forestGenerator =
    let
        intToTree : Int -> Tree
        intToTree val =
            if val == 1 then
                Living
            else
                None

        treeGenerator : Generator Tree
        treeGenerator =
            Random.map intToTree (Random.int 0 1)
    in
        Random.list 10 (Random.list 20 treeGenerator)


init : ( Model, Cmd Msg )
init =
    ( { forest = [] }, Random.generate InitForest forestGenerator )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
