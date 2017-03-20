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


step : Int
step =
    5


move : Direction -> Line -> Line
move direction ( l1, l2 ) =
    let
        { x, y } =
            l2
    in
        case direction of
            Up ->
                ( Point x y, Point x (y - step) )

            Down ->
                ( Point x y, Point x (y + step) )

            Left ->
                ( Point x y, Point (x - step) y )

            Right ->
                ( Point x y, Point (x + step) y )


lineToSvg : Line -> Svg msg
lineToSvg ( p1, p2 ) =
    let
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


directionsToLines : Point -> List Direction -> List Line
directionsToLines point directions =
    List.scanl move ( point, point ) directions


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


genLines : Int -> List Line
genLines iters =
    directionsToLines (Point 0 0) (generate 0 iters [ Up, Right ])


scaler : Int -> String
scaler iters =
    let
        base =
            iters
                |> toFloat
                |> \y ->
                    y
                        / 5
                        |> floor

        scalingFactor =
            clamp 1 base base

        origin =
            -50 * iters * 2

        -- -50 * scalingFactor
        units =
            -origin * 2
    in
        String.join " " <| List.map toString [ origin, origin, units, units ]


view model =
    let
        lines =
            genLines model
    in
        div [ aligner ]
            [ div [ alignerItem ]
                [ div [ iterButton, onClick Increment ] [ Html.text "+" ]
                , div [ iterButton, onClick Decrement ] [ Html.text "-" ]
                ]
            , div [ alignerItem, box ]
                [ Html.text <| scaler model
                , Html.text <| "iter: " ++ toString model ++ " lines: " ++ (toString <| List.length <| lines)
                , svg [ viewBox (scaler model) ] <| List.map lineToSvg <| lines
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
