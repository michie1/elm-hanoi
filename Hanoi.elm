module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Css
import Array


type alias Disk =
    { size : Int
    }


type alias Rod =
    { disks : List Disk
    }


type alias Model =
    { title : String
    , rods : Array.Array Rod
    , selectedRod : Maybe Int
    }


defaultState =
    Model
        "Hanoi!"
        (Array.fromList
            [ Rod [ Disk 1, Disk 2 ]
            , Rod []
            , Rod [ Disk 3 ]
            ]
        )
        Nothing


type Msg
    = SetTitle String
    | SelectRod Int
    | Drop Int
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetTitle title ->
            { model | title = title }

        SelectRod index ->
            { model | selectedRod = Just index }

        Drop index ->
            case model.selectedRod of
                Nothing ->
                    model

                Just selected ->
                    { model
                        | selectedRod = Nothing
                        , rods = move selected index model.rods
                    }

        _ ->
            model


-- TODO: check size of disks and if they can be placed on top

move : Int -> Int -> Array.Array Rod -> Array.Array Rod
move fromIndex toIndex rods =
    case Array.get fromIndex rods of
        Just from ->
            case Array.get toIndex rods of
                Just to ->
                    let
                        nextFrom = { from | disks = from.disks |> List.take ((List.length from.disks) - 1) }
                        nextTo = { to | disks = List.concat [ to.disks, from.disks |> List.drop ((List.length from.disks) - 1) ] }
                    in
                        rods |> Array.set fromIndex nextFrom |> Array.set toIndex nextTo

                Nothing ->
                    rods

        Nothing ->
            rods


view : Model -> Html Msg
view model =
    div [ class "hanoi" ]
        [ h1 [] [ text model.title ]
        , div [ class "rods", styles [] ] [ renderRods model.rods model.selectedRod ]
        ]


renderRods : Array.Array Rod -> Maybe Int -> Html Msg
renderRods rods maybeSelectedRod =
    ul [ styles [ Css.height (Css.px 50), Css.listStyle Css.none ] ]
        (List.indexedMap
            (\index rod ->
                li [ styles [ Css.display Css.inlineBlock ] ] [ (renderRod index rod.disks maybeSelectedRod) ]
            )
            (Array.toList rods)
        )


styles =
    Css.asPairs >> Html.Attributes.style


renderRod : Int -> List Disk -> Maybe Int -> Html Msg
renderRod index disks maybeSelectedRod =
    ul [ class "disks", styles [ Css.verticalAlign Css.bottom, Css.display Css.block, Css.listStyle Css.none ] ]
        ((List.map
            (\disk ->
                li [ styles [ Css.width (Css.px 100) ] ] [ text (toString disk.size) ]
            )
            (List.reverse disks)
         )
            ++ [ li [] [ button [ onClick (SelectRod index), disabled ((List.length disks) == 0) ] [ text "Select" ] ]
               , li [] [ button [ onClick (Drop index), disabled (maybeSelectedRod == Nothing) ] [ text "Drop" ] ]
               ]
        )


main =
    Html.beginnerProgram
        { model = defaultState
        , view = view
        , update = update
        }
