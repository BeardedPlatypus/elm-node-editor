module Main exposing (main)

import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer


-- Main
main : Program () Model Msg
main = 
  Browser.element { init = init 
                  , update = update
                  , view = view 
                  , subscriptions = subscriptions
                  }

-- Init
init : () -> ( Model, Cmd Msg )
init _ = ( { nodeData = [ constructInputNode "Input:0" 2 2 "Input:0"
                        , constructInputNode "Input:1" 2 52 "Input:1" 
                        , constructInputNode "Input:2" 2 102 "Input:2" 
                        , constructInputNode "Input:3" 2 152 "Input:3" 
                        , constructInputNode "Input:4" 2 202 "Input:4" 
                        , constructInputNode "Input:5" 2 252 "Input:5" 
                        , constructInputNode "Input:6" 2 302 "Input:6"
                        , constructInputNode "Input:7" 2 352 "Input:7"
                        , constructOutputNode "Output:0" 202 102 "Output:0"
                        , constructOutputNode "Output:1" 202 152 "Output:1"
                        , constructOutputNode "Output:2" 202 202 "Output:2"
                        , constructOutputNode "Output:3" 202 252 "Output:3"
                        ]
           , editMode = Selecting Nothing
           }
         , Cmd.none
         )


constructInputNode : String -> Int -> Int -> String -> Node
constructInputNode id x y content =
  Input { id = InputID id
        , info = { position = { x = x, y = y }
                 , content = content 
                 }
        , connection = Nothing
        }


constructOutputNode : String -> Int -> Int -> String -> Node
constructOutputNode id x y content =
  Output { id = OutputID id
         , info = { position = { x = x, y = y }
                 , content = content 
                 }
        , connection = Nothing
        }


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- Model 
type alias Model = { nodeData : NodeData
                   , editMode : EditMode
                   }


type EditMode = Drawing DrawData
              | Selecting SelectData 


type alias SelectData = Maybe NodeID


type alias DrawData = { source : InputID
                      , target : Maybe OutputID
                      , mousePosition : RelativeMousePosition
                      }


type alias ID = String


type NodeID = InputIDVal InputID
            | OutputIDVal OutputID

type InputID = InputID ID 
type OutputID = OutputID ID 

type alias NodeData = List Node 


type Node = Input InputNode 
          | Output OutputNode 


type alias InputNode = { id : InputID  
                       , info : NodeInformation
                       , connection : Maybe OutputID
                       }

type alias OutputNode = { id : OutputID
                        , info : NodeInformation 
                        , connection : Maybe InputID
                        }


type alias NodeInformation = { position : NodePosition 
                             , content : String
                             }

type alias NodePosition = { x : Int 
                          , y : Int
                          }


getNodeID : Node -> NodeID 
getNodeID node = 
  case node of 
    Input inputNode -> InputIDVal inputNode.id
    Output outputNode -> OutputIDVal outputNode.id


getOutputNode : NodeData -> OutputID -> Maybe OutputNode
getOutputNode nodeData outputID =
  case getNode nodeData ( OutputIDVal outputID ) of
    Just node -> 
      case node of 
        Input _ -> Nothing 
        Output outputNode -> Just outputNode
    Nothing -> 
      Nothing


getInputNode : NodeData -> InputID -> Maybe InputNode
getInputNode nodeData inputID = 
  case getNode nodeData ( InputIDVal inputID ) of
    Just node -> 
      case node of 
        Input inputNode -> Just inputNode
        Output _ -> Nothing
    Nothing -> 
      Nothing


getNode : NodeData -> NodeID -> Maybe Node 
getNode nodeData nodeID =
  let
    getItem node acc = if getNodeID node == nodeID then Just node else acc
  in
    List.foldl getItem Nothing nodeData


getNodeInformation : Node -> NodeInformation
getNodeInformation node =
  case node of 
    Input inputNode -> inputNode.info
    Output outputNode -> outputNode.info


type alias RelativeMousePosition = 
  { x : Int 
  , y : Int
  }


-- Update
type Msg = SetSelected ( Maybe NodeID )
         | SetTarget ( Maybe OutputID )
         | StartDrawing 
         | UpdateMousePosition RelativeMousePosition
         | EndDrawing 
         | None 


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of 
    SetSelected nodeID -> 
      ( setSelected model nodeID, Cmd.none )
    SetTarget outputID -> 
      ( setTarget model outputID, Cmd.none )
    StartDrawing -> 
      ( startDrawing model, Cmd.none )
    UpdateMousePosition mousePosition -> 
      ( updateMousePosition model mousePosition, Cmd.none )
    EndDrawing -> 
      ( endDrawing model, Cmd.none )
    None -> 
      ( model, Cmd.none )


setSelected : Model -> Maybe NodeID -> Model 
setSelected model nodeID = 
  case model.editMode of 
    Selecting _ -> 
      { model | editMode = Selecting nodeID }
    _ -> 
      model 


setTarget : Model -> Maybe OutputID -> Model 
setTarget model outputID =
  case model.editMode of 
    Drawing drawData -> 
      let 
        newDrawData = { drawData | target = outputID }
      in 
        { model | editMode = Drawing newDrawData }
    _ -> 
      model


replaceWithNodesByID_ : List Node -> Node -> Node
replaceWithNodesByID_ replacementNodes node = 
  let
    replaceNode newNode oldNode = 
      if getNodeID newNode == getNodeID oldNode then newNode else oldNode
  in
    List.foldl replaceNode node replacementNodes
    

replace : List Node -> List Node -> List Node
replace replacementNodes inputNodes =
  List.map ( replaceWithNodesByID_ replacementNodes ) inputNodes


disconnectNodeID_ : NodeData -> Node -> NodeData
disconnectNodeID_ nodeData node =
  let 
    replacementNode = 
      case node of
        Input inputNode ->
          Input { inputNode | connection = Nothing }
        Output outputNode ->
          Output { outputNode | connection = Nothing }
  in
    replace [ replacementNode ] nodeData

disconnectMultipleNodeID_ : NodeData -> List Node -> NodeData
disconnectMultipleNodeID_ nodeData nodesToDisconnect = 
  let
      disconnectNode node data = 
        disconnectNodeID_ data node
  in
    List.foldl disconnectNode nodeData nodesToDisconnect

removeConnectionsFromInput : NodeData -> InputNode -> NodeData
removeConnectionsFromInput nodeData inputNode = 
  case inputNode.connection of 
    Just outputNodeID ->
      case getOutputNode nodeData outputNodeID of 
        Just outputNode -> 
          disconnectMultipleNodeID_ nodeData [ Input inputNode, Output outputNode ]
        Nothing -> 
          disconnectMultipleNodeID_ nodeData [ Input inputNode ]
    Nothing -> 
      nodeData


removeConnectionsFromOutput : NodeData -> OutputNode -> NodeData
removeConnectionsFromOutput nodeData outputNode =
  case outputNode.connection of 
    Just inputNodeID -> 
      case getInputNode nodeData inputNodeID of 
        Just inputNode -> 
          disconnectMultipleNodeID_ nodeData [ Input inputNode, Output outputNode ]
        Nothing -> 
          disconnectMultipleNodeID_ nodeData [ Output outputNode ]
    Nothing -> 
      nodeData


startDrawing : Model -> Model 
startDrawing model =
  case model.editMode of
    Selecting selectData ->
      case selectData of 
        Just nodeID ->
          case getNode model.nodeData nodeID of 
            Just node -> 
              startDrawingWithNode model node 
            Nothing -> 
              model
        Nothing -> 
          model
    Drawing _ ->
      model


startDrawingWithNode : Model -> Node -> Model
startDrawingWithNode model node = 
  let 
    ( sourceNode, targetNodeID ) = 
      case node of 
        Input inputNode -> 
          ( Just inputNode, Nothing )
        Output outputNode -> 
          case outputNode.connection of 
            Just inputNodeID ->
              ( getInputNode model.nodeData inputNodeID, Just outputNode.id )
            Nothing -> 
              ( Nothing, Just outputNode.id )
  in 
    case sourceNode of 
      Just inputNode -> 
        let
            nodePosition = 
              case node of 
                Input  n -> { x = n.info.position.x + 100, y = n.info.position.y + 20 }
                Output n -> { x = n.info.position.x, y = n.info.position.y + 20 }

            newNodeData = removeConnectionsFromInput model.nodeData inputNode
            newEditMode = Drawing { source = inputNode.id
                                  , target = targetNodeID
                                  , mousePosition = nodePosition
                                  }
        in
          { model | nodeData = newNodeData
                  , editMode = newEditMode
          }
      Nothing -> model
              

updateMousePosition : Model -> RelativeMousePosition -> Model
updateMousePosition model newPosition = 
  case model.editMode of 
    Selecting _ -> model
    Drawing drawData -> 
      let
        newDrawData = { drawData | mousePosition = newPosition }
      in
        { model | editMode = Drawing newDrawData }


addConnections : NodeData -> InputID -> OutputID -> NodeData
addConnections nodeData inputNodeID outputNodeID =
  case ( getInputNode nodeData inputNodeID, getOutputNode nodeData outputNodeID ) of 
    (Just inputNode, Just outputNode ) ->
      let
        replacementNodes = 
          [ Input { inputNode | connection = Just outputNodeID }
          , Output { outputNode | connection = Just inputNodeID }
          ]
      in
        replace replacementNodes nodeData
    _ -> 
      nodeData


endDrawing : Model -> Model
endDrawing model =
  case model.editMode of 
    Selecting _ -> 
      model 
    Drawing drawData -> 
      case drawData.target of 
        Just targetID ->
          let
            newNodeData = 
              let 
                modelDataWithoutConnections =
                  case getOutputNode model.nodeData targetID of 
                    Just outputNode -> 
                      removeConnectionsFromOutput model.nodeData outputNode
                    Nothing -> 
                      model.nodeData
              in 
                addConnections modelDataWithoutConnections drawData.source targetID
          in 
            { model | editMode = Selecting ( Just (OutputIDVal targetID ))
                    , nodeData = newNodeData
            }
        Nothing -> 
          { model | editMode = Selecting Nothing }


-- View

view : Model -> Html Msg
view model =
  let
      viewElements = List.map ( viewNode model.editMode ) model.nodeData
      viewConnections = List.filterMap (viewConnection model.nodeData model.editMode ) model.nodeData
      interactivityElements = backgroundInteractivy :: List.map ( viewNodeInteractivity model.editMode ) model.nodeData

      elements = 
        case drawConnection of 
          Just element -> 
            [ viewConnections, List.singleton element, viewElements, interactivityElements ]
          Nothing -> 
            [ viewConnections, viewElements, interactivityElements ]

      drawConnection = viewDrawConnection model.nodeData model.editMode

      svgAttributes = 
        [ viewBox "0 0 304 404"
        , width "304"
        , height "404"
        , Html.Attributes.id "nodeEditorId"
        ]

      attributes = 
        case model.editMode of 
          Selecting _ -> svgAttributes
          Drawing _ -> List.append 
                         svgAttributes 
                         [ Pointer.onMove onMoveMsg 
                         , Pointer.onLeave ( \_ -> EndDrawing )
                         , Pointer.onUp ( \_ -> EndDrawing )
                         ]

  in
      svg
        attributes
        ( List.concat elements )


viewNode : EditMode -> Node -> Svg msg
viewNode editMode node =
  g []
    [ nodeRect_ editMode node
    , nodeText_ editMode node
    , nodeConnector_ editMode node
    ]


nodeRect_ : EditMode -> Node -> Svg msg
nodeRect_ editMode node = 
  let
    opacityVal = getOpacity editMode node
    nodeInformation = getNodeInformation node
  in
    viewNodeRect_
      nodeInformation.position
      [ fill <| fromColour White
      , rx "12"
      , stroke <| fromColour GreyLight
      , strokeWidth "1"
      , opacity opacityVal
      ]


nodeText_ : EditMode -> Node -> Svg msg
nodeText_ editMode node = 
  let
    opacityVal = getOpacity editMode node
    nodeInformation = getNodeInformation node
  in
    text_ 
      [ x ( String.fromInt (nodeInformation.position.x + 50 ))
      , y ( String.fromInt (nodeInformation.position.y + 25 ))
      , fontSize "20"
      , width "100"
      , height "40"
      , fill "black"
      , textAnchor "middle"
      , opacity opacityVal
      ]
      [ text nodeInformation.content ]


nodeConnector_ : EditMode -> Node -> Svg msg
nodeConnector_ editMode node =
  let
    opacityVal = getOpacity editMode node
    nodeInformation = getNodeInformation node

    xOffset = case node of 
                Input _  -> 100
                Output _ -> 0

    colour = getConnectorColour editMode node
  in
    g []
      [ circle 
          [ cx ( String.fromInt ( nodeInformation.position.x + xOffset ))
          , cy ( String.fromInt ( nodeInformation.position.y + 20 ) )
          , r "8"
          , fill "white"
          ]
          [ ]
      , circle
          [ cx ( String.fromInt ( nodeInformation.position.x + xOffset ))
          , cy ( String.fromInt ( nodeInformation.position.y + 20 ) )
          , r "8"
          , fill <| fromColour colour
          , opacity opacityVal
          ]
          [ ]
      ]


getConnectorColour : EditMode -> Node -> Colour
getConnectorColour editMode node =
  case editMode of 
    Selecting data -> getConnectorColourSelecting data node 
    Drawing data   -> getConnectorColourDrawing   data node


getConnectorColourSelecting : SelectData -> Node -> Colour
getConnectorColourSelecting selectData node =
  if selectData == Just ( getNodeID node ) then 
    case node of 
      Input nodeData -> 
        case nodeData.connection of
          Nothing -> Success
          Just _ -> Warning
      Output nodeData ->
        case nodeData.connection of 
          Nothing -> Success
          Just _ -> Warning
  else
    case node of
      Input nodeData -> 
        case nodeData.connection of
          Just connectedOutputID ->
            if Just ( OutputIDVal connectedOutputID ) == selectData then
              Warning
            else
              Link
          Nothing -> 
            Info
      Output nodeData ->
        case nodeData.connection of 
          Just connectedInputID ->
            if Just ( InputIDVal connectedInputID ) == selectData then
              Warning
            else
              Link
          Nothing -> 
            Info


getConnectorColourDrawing : DrawData -> Node -> Colour
getConnectorColourDrawing drawData node = 
  case node of 
    Input inputNode -> 
      if inputNode.id == drawData.source then 
        Success
      else
        case inputNode.connection of 
          Just connectedOutputID -> 
            if drawData.target == Just connectedOutputID then 
              Danger
            else
              Link
          Nothing -> 
            Info
    Output outputNode -> 
      if Just outputNode.id == drawData.target then
        case outputNode.connection of
          Just _ -> Danger
          Nothing -> Success
      else
        case outputNode.connection of
          Just _ -> Link
          Nothing-> Info


viewNodeRect_ : NodePosition -> List ( Svg.Attribute msg ) -> Svg msg 
viewNodeRect_ nodePosition styleAttributes = 
  let
    xPos = String.fromInt nodePosition.x
    yPos = String.fromInt nodePosition.y

    posAttributes = 
      [ x xPos 
      , y yPos
      , width  "100"
      , height "40"
      ]
  in 
    rect (List.append posAttributes styleAttributes) [ ] 


getOpacity : EditMode -> Node -> String
getOpacity editMode node =
  case editMode of
    Selecting _ -> "1.0"
    Drawing data ->
      case node of
        Input inputNodeData -> if data.source == inputNodeData.id then "1.0" else "0.5"
        Output _ -> "1.0"


viewConnection : NodeData -> EditMode -> Node -> Maybe ( Svg msg )
viewConnection nodeData editMode node =
    case node of 
      Input inputNode ->
        case inputNode.connection of 
          Just outputNodeID ->
            case getOutputNode nodeData outputNodeID of 
              Just outputNode ->
                Just ( nodeConnection_ editMode inputNode outputNode )
              Nothing ->
                Nothing
          Nothing -> 
            Nothing
      _ -> 
        Nothing


viewDrawConnection : NodeData -> EditMode -> Maybe ( Svg msg )
viewDrawConnection nodeData editMode =
  case editMode of 
    Selecting _ -> 
      Nothing
    Drawing drawData -> 
      getInputNode nodeData drawData.source |> Maybe.map ( viewDrawConnection_ nodeData drawData )


viewDrawConnection_ : NodeData -> DrawData -> InputNode -> Svg msg
viewDrawConnection_ nodeData drawData inputNode =
  let
    targetNode = 
      drawData.target |> Maybe.andThen ( getOutputNode nodeData )

    ( xEnd, yEnd ) = 
      case targetNode of 
        Just node -> ( node.info.position.x, node.info.position.y + 20 )
        Nothing   -> ( drawData.mousePosition.x, drawData.mousePosition.y )
    ( xStart, yStart ) = 
      ( inputNode.info.position.x + 100, inputNode.info.position.y + 20 )

    pathString = 
      getPathString ( xStart, yStart )
                    ( xEnd, yEnd )

    colour = 
      case targetNode of 
        Just _ -> Success
        Nothing -> Black
  in
    Svg.path 
        [ fill "transparent"
        , stroke <| fromColour colour
        , strokeWidth "1"
        , d pathString
        ] 
        []


nodeConnection_ : EditMode -> InputNode -> OutputNode -> Svg msg
nodeConnection_ editMode nodeIn nodeOut = 
  let 
    xStart = nodeIn.info.position.x + 100
    yStart = nodeIn.info.position.y + 20

    xEnd = nodeOut.info.position.x
    yEnd = nodeOut.info.position.y + 20

    pathString = getPathString ( xStart, yStart )  ( xEnd, yEnd )

    colour = 
      case editMode of 
        Selecting selectData -> 
          case selectData of 
            Just selectedNodeID -> 
              if selectedNodeID == InputIDVal nodeIn.id || 
                 selectedNodeID == OutputIDVal nodeOut.id then 
                Warning
              else 
                GreyLight
            Nothing -> 
              Black
        Drawing drawData -> 
          if drawData.target == Just nodeOut.id then 
            Danger
          else 
            GreyLight
  in 
    Svg.path 
        [ fill "transparent"
        , stroke <| fromColour colour
        , strokeWidth "1"
        , d pathString
        ] 
        []


getPathString : ( Int, Int )-> ( Int, Int ) -> String
getPathString ( xStart, yStart ) ( xEnd, yEnd ) =
  let
    xDiff = round ( toFloat ( abs ( xStart - xEnd )) / 2)
    xBezierControl = Basics.min xStart xEnd + xDiff
  in
    "M " ++ 
    String.fromInt xStart ++ " " ++ 
    String.fromInt yStart ++ " " ++ 
    "C " ++ 
    String.fromInt xBezierControl ++ " " ++ 
    String.fromInt yStart ++ ", " ++ 
    String.fromInt xBezierControl ++ " " ++ 
    String.fromInt yEnd ++ ", " ++ 
    String.fromInt xEnd ++ " " ++ 
    String.fromInt yEnd


viewNodeInteractivity : EditMode -> Node -> Svg Msg
viewNodeInteractivity editMode node =
  let 
    nodeInformation = getNodeInformation node
    xOffset = case node of 
                Input _  -> 100
                Output _ -> 0
    ( onMouseOverMsg, onMouseOutMsg ) =
      case editMode of 
        Selecting _ -> ( SetSelected <| Just (getNodeID node), SetSelected Nothing )
        Drawing _   -> 
          case node of 
            Input _ -> ( None, None )
            Output outputNode -> ( SetTarget <| Just outputNode.id, SetTarget Nothing )
  in
    circle
      [ cx ( String.fromInt ( nodeInformation.position.x + xOffset ))
      , cy ( String.fromInt ( nodeInformation.position.y + 20 ) )
      , r "20"
      , fill "transparent"
      , onMouseOver onMouseOverMsg
      , onMouseOut  onMouseOutMsg
      , onMouseDown StartDrawing
      , onMouseUp EndDrawing
      ]
      [ ]


onMoveMsg : Pointer.Event -> Msg
onMoveMsg event = 
  let
      ( x, y ) = event.pointer.offsetPos
  in
    UpdateMousePosition { x = round x, y = round y }


backgroundInteractivy : Svg Msg 
backgroundInteractivy =
  rect 
    [ x "0"
    , y "0"
    , width  "304"
    , height "404"
    , fill "transparent"
    , onMouseUp EndDrawing
    ]
    [ ]


-- Colours
type Colour = White
            | Black
            | Light
            | Dark
            | Primary
            | Link
            | Info
            | Success
            | Warning
            | Danger
            | BlackBis
            | BlackTer
            | GreyDarker
            | GreyDark
            | GreyLight
            | GreyLighter
            | WhiteTer
            | WhiteBis


fromColour : Colour -> String
fromColour colour =
  case colour of
    White   -> "hsl(0, 0%, 100%)"
    Black   -> "hsl(0, 0%, 4%)"
    Light   -> "hsl(0, 0%, 96%)"
    Dark    -> "hsl(0, 0%, 21%)"
    Primary -> "hsl(171, 100%, 41%)"
    Link    -> "hsl(217, 71%, 53%)"
    Info    -> "hsl(204, 86%, 53%)"
    Success -> "hsl(141, 53%, 53%)"
    Warning -> "hsl(48, 100%, 67%)"
    Danger  -> "hsl(348, 100%, 61%)"
    BlackBis    -> "hsl(0, 0%, 7%)"
    BlackTer    -> "hsl(0, 0%, 14%)"
    GreyDarker  -> "hsl(0, 0%, 21%)"
    GreyDark    -> "hsl(0, 0%, 29%)"
    GreyLight   -> "hsl(0, 0%, 71%)"
    GreyLighter -> "hsl(0, 0%, 86%)"
    WhiteTer    -> "hsl(0, 0%, 96%)"
    WhiteBis    -> "hsl(0, 0%, 98%)"
