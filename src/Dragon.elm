module Dragon exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    beginnerProgram { model = Model 1 0 0 0, view = view, update = update }



-- MODEL


type alias Model =
    { iters : Int, zoom : Int, xoffset : Int, yoffset : Int }


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



-- UPDATE


type Msg
    = Forward
    | Backward
    | ZoomIn
    | ZoomOut
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight


update : Msg -> Model -> Model
update msg model =
    case msg of
        Forward ->
            { model | iters = model.iters + 1 }

        Backward ->
            if model.iters - 1 > 0 then
                { model | iters = model.iters - 1 }
            else
                { model | iters = 1 }

        ZoomIn ->
            { model | zoom = model.zoom + 1 }

        ZoomOut ->
            if model.zoom - 1 > -1 then
                { model | zoom = model.zoom - 1 }
            else
                { model | zoom = 0 }

        MoveLeft ->
            { model | xoffset = model.xoffset - 5 }

        MoveRight ->
            { model | xoffset = model.xoffset + 5 }

        MoveUp ->
            { model | yoffset = model.yoffset - 5 }

        MoveDown ->
            { model | yoffset = model.yoffset + 5 }


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


genLines : Int -> List Line
genLines iters =
    directionsToLines (Point 0 0) (generate 0 iters [ Up, Right ])


originOffset : Int -> Int -> Int
originOffset origin offset =
    origin
        - (origin
            |> toFloat
            |> (\y -> y * ((toFloat offset) / 100.0))
            |> floor
          )


scaler : Model -> String
scaler { iters, zoom, xoffset, yoffset } =
    let
        scalingFactor =
            clamp 1 (iters - zoom) (iters - zoom)

        origin =
            -100 * scalingFactor * 2

        originX =
            originOffset origin xoffset

        originY =
            originOffset origin yoffset

        units =
            -origin * 2
    in
        String.join " " <| List.map toString [ originX, originY, units, units ]



-- VIEW


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


view : Model -> Html Msg
view model =
    let
        lines =
            genLines model.iters
    in
        div [ aligner ]
            [ div [ alignerItem ]
                [ div [ iterButton, onClick Forward ] [ Html.text "F" ]
                , div [ iterButton, onClick Backward ] [ Html.text "B" ]
                , div [ iterButton, onClick ZoomIn ] [ Html.text "+" ]
                , div [ iterButton, onClick ZoomOut ] [ Html.text "-" ]
                , div [ iterButton, onClick MoveUp ] [ Html.text "U" ]
                , div [ iterButton, onClick MoveDown ] [ Html.text "D" ]
                , div [ iterButton, onClick MoveLeft ] [ Html.text "L" ]
                , div [ iterButton, onClick MoveRight ] [ Html.text "R" ]
                ]
            , div [ alignerItem, box ]
                [ Html.text <| "[ " ++ scaler model ++ " ]"
                , Html.text <| "[ iter: " ++ toString model.iters ++ " lines: " ++ (toString <| List.length <| lines) ++ " ]"
                , svg [ viewBox (scaler model) ] <| List.map lineToSvg <| lines
                ]
            ]
