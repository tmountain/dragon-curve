module Dragon exposing (..)

import Html exposing (beginnerProgram, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    beginnerProgram { model = 1, view = view, update = update }


type Direction
    = Up
    | Right
    | Down
    | Left


type alias Point =
    { x : Int
    , y : Int
    }


type alias Line =
    ( Point, Point )


transition : Direction -> Direction
transition direction =
    case direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


invert : Direction -> Direction
invert direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


move : Direction -> Int -> Point -> Point
move direction distance point =
    case direction of
        Up ->
            { point | y = point.y - distance }

        Right ->
            { point | x = point.x + distance }

        Down ->
            { point | y = point.y + distance }

        Left ->
            { point | x = point.x - distance }


lineToSvg : Line -> Svg msg
lineToSvg points =
    let
        ( p1, p2 ) =
            points

        px1 =
            p1.x

        py1 =
            p1.y

        px2 =
            p2.x

        py2 =
            p2.y
    in
        line [ x1 (toString px1), y1 (toString py1), x2 (toString px2), y2 (toString py2), stroke "#023963" ] []


directionsToPoints : Point -> List Direction -> List Line -> List Line
directionsToPoints point directions newDirections =
    case directions of
        [] ->
            List.reverse newDirections

        dir :: dirRest ->
            let
                transPoint =
                    move dir 5 point
            in
                directionsToPoints transPoint dirRest (( point, transPoint ) :: newDirections)


generate : Int -> Int -> List Direction -> List Direction
generate current max moves =
    if current == max then
        moves
    else
        let
            nextIter =
                current + 1

            nextMove =
                moves
                    |> List.map transition
                    |> List.map invert
                    |> List.reverse
        in
            generate nextIter max (nextMove ++ moves)


aligner : Html.Attribute msg
aligner =
    Html.Attributes.style
        [ ( "display", "flex" )
        , ( "justify-content", "center" )
        , ( "align-items", "center" )
        , ( "flex-direction", "column" )
        , ( "background-color", "#333333" )
        , ( "height", "100vh" )
        , ( "min-height", "100vh" )
        ]


alignerItem : Html.Attribute msg
alignerItem =
    Html.Attributes.style [ ( "max-width", "50%" ) ]


box : Html.Attribute msg
box =
    Html.Attributes.style
        [ ( "width", "80vh" )
        , ( "height", "80vh" )
        , ( "background-color", "#3399FF" )
        , ( "border-radius", "5px" )
        ]


iterButton : Html.Attribute msg
iterButton =
    Html.Attributes.style
        [ ( "background-color", "white" )
        , ( "width", "15px" )
        , ( "margin", "10px 5px 5px 10px" )
        , ( "text-align", "center" )
        , ( "vertical-align", "middle" )
        , ( "display", "inline-block" )
        , ( "border-radius", "5px" )
        , ( "cursor", "pointer" )
        ]


view model =
    div [ aligner ]
        [ div [ alignerItem ]
            [ div [ iterButton, onClick Increment ] [ Html.text "+" ]
            , div [ iterButton, onClick Decrement ] [ Html.text "-" ]
            ]
        , div [ alignerItem, box ]
            [ svg [ viewBox "-300 -300 600 600" ] <|
                List.map lineToSvg <|
                    directionsToPoints (Point 0 0) (generate 0 model [ Up, Right ]) []
            ]
        ]


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            if model - 1 > 0 then
                model - 1
            else
                1
