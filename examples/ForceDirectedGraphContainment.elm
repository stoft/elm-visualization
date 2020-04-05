module ForceDirectedGraphContainment exposing (main)

{-| This demonstrates laying out the characters in Les Miserables
based on their co-occurence in a scene. Try dragging the nodes!

@delay 5

-}

import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Browser.Events
import Color
import Force exposing (State)
import Force.Container exposing (Container(..))
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html
import Html.Events exposing (on)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import SampleData exposing (miserablesGraph, smallGraph)
import Time
import TypedSvg exposing (circle, g, line, rect, svg, title)
import TypedSvg.Attributes exposing (class, fill, stroke, viewBox, fillOpacity)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, rx, ry, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (Paint(..), Opacity(..))


w : Float
w =
    990


h : Float
h =
    1200


type Msg
    = DragStart NodeId ( Float, Float )
    | DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | Tick Time.Posix


type alias Model =
    { drag : Maybe Drag
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    }


type alias Drag =
    { start : ( Float, Float )
    , current : ( Float, Float )
    , index : NodeId
    }


type alias Entity =
    Force.Entity NodeId { value : String }


initializeNode : NodeContext String () -> NodeContext Entity ()
initializeNode ctx =
    let
        ent : Int -> { x : Float, y : Float, vx : Float, vy : Float, id : NodeId, value : String }
        ent id =
            { x = 150
            , y = 100 + (toFloat id * 100)
            , vx = 75
            , vy = 0
            , id = id
            , value = String.fromInt id
            }
    in
    ent ctx.node.id
        |> (\entity ->
                { node = { label = entity, id = ctx.node.id }
                , incoming = ctx.incoming
                , outgoing = ctx.outgoing
                }
           )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        graph =
            Graph.mapContexts initializeNode smallGraph

        link { from, to } =
            let
                distance =
                    if from == 0 then
                        150

                    else
                        90
            in
            { source = from
            , target = to
            , distance = distance
            , strength = Nothing
            }

        forces = initForces 
    in
    ( Model Nothing graph (Force.simulation forces), Cmd.none )

initForces =
    [ Force.containment 0.0 0.0 False (boundingBox 0)
    , Force.containment 5.0 0.1 False (boundingBox 1)
    , Force.containment 5.0 0.3 False (boundingBox 2)
    , Force.containment 10.0 0.1 False (boundingBox 3)
    , Force.containment 10.0 0.3 False (boundingBox 4)
    , Force.containment -5.0 0.1 False (boundingBox 5)
    , Force.containment 5.0 0.0 True (boundingBox 6)
    , Force.containment 10.0 0.0 True (boundingBox 7)
    ] 

boundingBox : Float -> BoundingBox2d
boundingBox y =
    BoundingBox2d.fromExtrema { minX = 50, maxX = 250, minY = (y * 100) + 50, maxY = ((y + 1) * 100) + 50 }


updateNode : ( Float, Float ) -> NodeContext Entity () -> NodeContext Entity ()
updateNode ( x, y ) nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
    updateContextWithValue nodeCtx { nodeValue | x = x, y = y }


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


update : Msg -> Model -> Model
update msg ({ drag, graph, simulation } as model) =
    case msg of
        Tick t ->
            let
                ( newState, list ) =
                    Force.tick simulation <| List.map .label <| Graph.nodes graph
            in
            case drag of
                Nothing ->
                    Model drag (updateGraphWithList graph list) newState

                Just { current, index } ->
                    Model drag
                        (Graph.update index
                            (Maybe.map (updateNode current))
                            (updateGraphWithList graph list)
                        )
                        newState

        DragStart index xy ->
            Model (Just (Drag xy xy index)) graph simulation

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    Model (Just (Drag start xy index))
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        (Force.reheat simulation)

                Nothing ->
                    Model Nothing graph simulation

        DragEnd xy ->
            case drag of
                Just { start, index } ->
                    Model Nothing
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        simulation

                Nothing ->
                    Model Nothing graph simulation


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            -- This allows us to save resources, as if the simulation is done, there is no point in subscribing
            -- to the rAF.
            if Force.isCompleted model.simulation then
                Sub.none

            else
                Browser.Events.onAnimationFrame Tick

        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map (.clientPos >> DragAt) Mouse.eventDecoder)
                , Browser.Events.onMouseUp (Decode.map (.clientPos >> DragEnd) Mouse.eventDecoder)
                , Browser.Events.onAnimationFrame Tick
                ]


onMouseDown : NodeId -> Attribute Msg
onMouseDown index =
    Mouse.onDown (.clientPos >> DragStart index)


linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
    line
        [ strokeWidth 1
        , stroke <| Paint <| Color.rgb255 170 170 170
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        ]
        []


nodeElement node =
    g []
        [ circle
            [ fill <| Paint <| Color.white
            , stroke <| Paint <| Color.black
            , strokeWidth 1
            , onMouseDown node.id
            , cx node.label.x
            , cy node.label.y
            , r 10
            ]
            [ title [] [ text node.label.value ] ]
        , TypedSvg.text_
            [ x node.label.x
            , y node.label.y
            , fill <| Paint <| Color.black
            , TypedSvg.Attributes.InPx.fontSize 10
            , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorMiddle
            ]
            [ title [] [ text node.label.value ], text node.label.value ]
        ]


viewForce strength buffer impermeable boundingBox_ =
    let
        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema boundingBox_

        width_ =
            maxX - minX

        height_ =
            maxY - minY
        
        boolToString bool =
            case bool of
                True -> "True"
                False -> "False"
    in
    g [] [
        rect 
            [ x (minX + width_ * buffer / 2)
            , y (minY + height_ * buffer / 2)
            , width (width_ - width_ * buffer)
            , height (height_ - height_ * buffer)
            , stroke <| Paint <| Color.grey
            , strokeWidth 1
            , fill <| Paint <| Color.white
            ]
            []
        , rect
            [ x minX
            , y minY
            , width width_
            , height height_
            , stroke <| Paint <| Color.black
            , strokeWidth 1
            , fill <| Paint <| Color.white
            , fillOpacity <| Opacity <| 0
            ]
            []
        ,TypedSvg.text_
            [ x ((minX + 10))
            , y (minY + 10)
            , fill <| Paint <| Color.black
            , TypedSvg.Attributes.InPx.fontSize 10
            -- , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorMiddle
            ]
            [ text ("Strength: " ++ String.fromFloat strength ++
                    ", Buffer: " ++ String.fromFloat buffer ++
                    ", Impermeable: " ++ boolToString impermeable) ] 
    ]

viewForces : List (Svg Msg)
viewForces =
    List.foldl (\force list -> 
        case force of 
            (Force.Containment strength buffer impermeable (Rectangle bb)) ->
                viewForce strength buffer impermeable bb :: list 
            _ -> list
        ) [] initForces
        |> List.reverse

view : Model -> Svg Msg
view model =
    svg [ viewBox 0 0 w h ]
        (viewForces ++ [ 
         Graph.edges model.graph
            |> List.map (linkElement model.graph)
            |> g [ class [ "links" ] ]
        , Graph.nodes model.graph
            |> List.map nodeElement
            |> g [ class [ "nodes" ] ]
        ])


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }
