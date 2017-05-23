module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Css
import Array
import Set exposing (..)


type alias Disk =
    Int


type alias Rod =
    Set Disk


type Tower
    = Left
    | Middle
    | Right


type alias Model =
    { title : String
    , left : Rod
    , middle : Rod
    , right : Rod
    , picked : Maybe Tower
    }


defaultState =
    Model
        "Hanoi!"
        (empty |> insert 1 |> insert 2)
        (empty |> insert 3 |> insert 4)
        (empty |> insert 5 |> insert 6)
        Nothing


type Msg
    = SetTitle String
    | SelectRod Int
    | Drop Tower
    | Reset
    | Pick Tower


update : Msg -> Model -> Model
update msg state =
    case msg of
        SetTitle title ->
            state

        SelectRod index ->
            state

        Pick tower ->
            { state | picked = Just tower }

        Drop dropped ->
            case getMinDisk state dropped of
                Just to ->
                    case state.picked of
                        Just picked ->
                            case getMinDisk state picked of
                                Just from ->
                                    if from <= to then
                                        let
                                            left = move state Left picked dropped from to
                                            middle = move state Middle picked dropped from to
                                            right = move state Right picked dropped from to
                                        in
                                            { state
                                                | picked = Nothing
                                                , left = left
                                                , middle = middle
                                                , right = right
                                            }
                                    else
                                        state

                                Nothing ->
                                    state

                        Nothing ->
                            state

                Nothing ->
                    state

        _ ->
            state


move : Model -> Tower -> Tower -> Tower -> Disk -> Disk -> Rod
move state tower picked dropped from to =
    case tower of
        Left -> 
            case picked of
                Left ->
                    case dropped of
                        Left ->
                            state.left

                        _ ->
                            getRod picked state |> remove from

                _ ->
                    case dropped of
                        Left ->
                            getRod picked state |> insert to

                        _ ->
                            state.left
        Middle -> 
            case picked of
                Middle ->
                    case dropped of
                        Middle ->
                            state.middle

                        _ ->
                            getRod picked state |> remove from

                _ ->
                    case dropped of
                        Middle ->
                            getRod picked state |> insert to

                        _ ->
                            state.middle   
        Right -> 
            case picked of
                Right ->
                    case dropped of
                        Right ->
                            state.right

                        _ ->
                            getRod picked state |> remove from

                _ ->
                    case dropped of
                        Right ->
                            getRod picked state |> insert to

                        _ ->
                            state.right

getRod : Tower -> Model -> Rod
getRod tower state =
    case tower of
        Left ->
            state.left

        Middle ->
            state.middle

        Right ->
            state.right


getMinDisk : Model -> Tower -> Maybe Disk
getMinDisk state tower =
    let
        rod =
            getRod tower state
    in
        rod |> toList |> List.head


view : Model -> Html Msg
view state =
    div [ class "hanoi" ]
        [ h1 [] [ text state.title ]
        , span [] [ text <| "Picked: " ++ toString state.picked ]
        , viewRod state Left
        , viewRod state Middle
        , viewRod state Right
        ]


viewRod : Model -> Tower -> Html Msg
viewRod state tower =
    let
        rod =
            getRod tower state

        maybePicked =
            state.picked
    in
        ul []
            [ li []
                [ ul [] <|
                    (rod
                        |> toList
                        |> List.map
                            (\disk ->
                                li [] [ disk |> toString |> text ]
                            )
                    )
                ]
            , li [] [ button [ onClick <| Pick tower, disabled <| maybePicked /= Nothing ] [ text "Pick" ] ]
            , li [] [ button [ onClick <| Drop tower, disabled <| disableDrop state tower ] [ text "Drop" ] ]
            ]


disableDrop : Model -> Tower -> Bool
disableDrop state tower =
    case getMinDisk state tower of
        Just to ->
            case state.picked of
                Just picked ->
                    case getMinDisk state picked of
                        Just from ->
                            let
                                _ =
                                    Debug.log "tower" (toString tower)
                            in
                                Debug.log "from < to" (from > to)

                        Nothing ->
                            False

                Nothing ->
                    True

        Nothing ->
            False


main =
    Html.beginnerProgram
        { model = defaultState
        , view = view
        , update = update
        }
