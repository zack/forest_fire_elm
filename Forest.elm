module Forest exposing (Forest, new, step, svg)

--, step)

import Html exposing (Html)
import Random exposing (Generator, Seed)
import Set exposing (Set)
import Svg
import Svg.Keyed
import Svg.Attributes as SvgAttr


type alias Forest =
    { burnRate : Float
    , growthRate : Float
    , trees : Trees
    , edgeLength : Int
    , seed : Seed
    }


type alias Trees =
    List Tree


type Tree
    = Living
    | Burning
    | Dead


getNeighbors : Int -> Int -> List Int
getNeighbors edgeLength idx =
    let
        leftBorder =
            idx % edgeLength == 0

        rightBorder =
            (idx + 1) % edgeLength == 0
    in
        if leftBorder then
            [ idx + 1
            , idx - edgeLength + 1
            , idx + edgeLength + 1
            ]
        else if rightBorder then
            [ idx - 1
            , idx - edgeLength - 1
            , idx + edgeLength - 1
            ]
        else
            [ idx - 1
            , idx + 1
            , idx - edgeLength - 1
            , idx - edgeLength + 1
            , idx + edgeLength - 1
            , idx + edgeLength + 1
            ]
                |> List.append [ idx + edgeLength, idx - edgeLength ]
                |> List.filter (\idx -> idx >= 0 && idx < (edgeLength * edgeLength))


ignitingTrees : Trees -> Int -> Set Int
ignitingTrees trees edgeLength =
    trees
        |> List.indexedMap
            (\idx tree ->
                if tree == Burning then
                    (getNeighbors edgeLength idx)
                else
                    []
            )
        |> List.concat
        |> Set.fromList


step : Forest -> Forest
step forest =
    let
        reductionFunction : Tree -> ( Seed, Int, Trees ) -> ( Seed, Int, Trees )
        reductionFunction tree ( seed, idx, forestSoFar ) =
            case tree of
                Burning ->
                    ( seed, idx + 1, Dead :: forestSoFar )

                Dead ->
                    let
                        ( randFloat, newSeed ) =
                            Random.step (Random.float 0 1) seed

                        newTree =
                            if randFloat < forest.growthRate then
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
                            if randFloat < forest.burnRate || (Set.member idx burningNeighbors) then
                                Burning
                            else
                                Living
                    in
                        ( newSeed, idx + 1, newTree :: forestSoFar )

        burningNeighbors =
            ignitingTrees forest.trees forest.edgeLength

        ( newSeed, _, newForest ) =
            List.foldl reductionFunction ( forest.seed, 0, [] ) forest.trees
    in
        { forest
            | trees = (List.reverse newForest)
            , seed = newSeed
        }


new : Float -> Float -> Int -> Int -> Forest
new burnRate growthRate edgeLength randomInt =
    let
        initSeed =
            Random.initialSeed randomInt

        ( trees, newSeed ) =
            Random.step (forestGenerator edgeLength) initSeed
    in
        { burnRate = burnRate
        , growthRate = growthRate
        , edgeLength = edgeLength
        , seed = newSeed
        , trees = trees
        }


forestGenerator : Int -> Generator Trees
forestGenerator edgeLength =
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
        Random.list (edgeLength * edgeLength) treeGenerator


svg : Forest -> Html msg
svg forest =
    let
        tree xc yc color =
            Svg.Keyed.node "rect"
                [ SvgAttr.fill color
                , SvgAttr.x (toString xc)
                , SvgAttr.y (toString yc)
                , SvgAttr.width "1"
                , SvgAttr.height "1"
                ]
                []

        burningTree x y =
            tree x y "#F98A15"

        livingTree x y =
            tree x y "#0EA27D"

        deadTree x y =
            Svg.text ""

        drawTree idx type_ =
            let
                y =
                    idx // forest.edgeLength

                x =
                    idx % forest.edgeLength

                key =
                    "(" ++ (toString x) ++ "," ++ (toString y) ++ ")"
            in
                case type_ of
                    Living ->
                        ( key, livingTree x y )

                    Burning ->
                        ( key, burningTree x y )

                    Dead ->
                        ( key, deadTree x y )

        background =
            ( "-1"
            , (Svg.Keyed.node "rect"
                [ SvgAttr.fill "#1A58A3"
                , SvgAttr.x "0"
                , SvgAttr.y "0"
                , SvgAttr.width (toString forest.edgeLength)
                , SvgAttr.height (toString forest.edgeLength)
                ]
                []
              )
            )
    in
        Svg.Keyed.node "svg"
            [ SvgAttr.version "1.1"
            , SvgAttr.width "800"
            , SvgAttr.height "800"
            , SvgAttr.viewBox ("0 0 " ++ (toString forest.edgeLength) ++ " " ++ (toString forest.edgeLength))
            ]
            (background :: (List.indexedMap drawTree forest.trees))
