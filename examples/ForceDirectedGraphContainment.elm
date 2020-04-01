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
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html
import Html.Events exposing (on)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import SampleData exposing (miserablesGraph, smallGraph)
import Time
import TypedSvg exposing (circle, g, line, rect, svg, title)
import TypedSvg.Attributes exposing (class, fill, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, rx, ry, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (Paint(..))


w : Float
w =
    990


h : Float
h =
    504


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
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


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

        setBodyStrength node =
            if node == 0 then
                ( node, -200 )

            else
                ( node, -200 )

        forces =
            [ --Force.customLinks 50 <| List.map link <| Graph.edges graph
              Force.center (w / 2) (h / 2)

            --, Force.customManyBody -1 <| List.map (.id >> setBodyStrength) <| Graph.nodes graph
            , Force.manyBodyStrength -20 <| (Graph.nodes graph |> List.map .id)
            , Force.containment 1.0 0.1 initContainment

            --, Force.impregnableContainment 5.0 0.05 initContainment
            ]
    in
    ( Model Nothing graph (Force.simulation forces), Cmd.none )


initContainment : BoundingBox2d
initContainment =
    BoundingBox2d.fromExtrema { minX = 100, maxX = w - 100, minY = 100, maxY = h - 100 }


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

                newlist =
                    list
                        |> List.map
                            (\ent ->
                                if ent.id == 0 then
                                    { ent | x = w / 2, y = h / 2 }

                                else
                                    ent
                            )
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


viewBoundingBox =
    let
        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema initContainment

        width_ =
            maxX - minX

        height_ =
            maxY - minY
    in
    rect
        [ x minX
        , y minY
        , width width_
        , height height_
        , stroke <| Paint <| Color.black
        , strokeWidth 1
        , fill <| Paint <| Color.white
        ]
        []


view : Model -> Svg Msg
view model =
    svg [ viewBox 0 0 w h ]
        [ viewBoundingBox
        , Graph.edges model.graph
            |> List.map (linkElement model.graph)
            |> g [ class [ "links" ] ]
        , Graph.nodes model.graph
            |> List.map nodeElement
            |> g [ class [ "nodes" ] ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }
