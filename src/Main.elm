module Main exposing (main)

import Browser
import Html exposing (Html, div, form, h2, input, label, text)
import Html.Attributes exposing (attribute, class, id, type_, value)
import Html.Events exposing (onInput)
import Round
import Task exposing (perform, succeed)


type alias Model =
    { original_length : Float
    , alternative_length : Float
    , original_amount : Float
    , alternative_amount : Float
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 0 0 0, Cmd.none )


type Msg
    = OrigLength (Maybe Float)
    | AltLength (Maybe Float)
    | OrigAmount (Maybe Float)
    | ComputeAltAmount


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OrigLength float ->
            ( { model | original_length = Maybe.withDefault 0 float }, send ComputeAltAmount )

        AltLength float ->
            ( { model | alternative_length = Maybe.withDefault 0 float }, send ComputeAltAmount )

        OrigAmount float ->
            ( { model | original_amount = Maybe.withDefault 0 float }, send ComputeAltAmount )

        ComputeAltAmount ->
            ( { model | alternative_amount = model.original_length * model.original_amount / model.alternative_length }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    form [ class "container" ]
        [ div []
            [ h2 [] [ text "Garnet i oppskriften:" ]
            , div [ id "orig_length" ]
                [ label [] [ text "Lengde i meter" ]
                , input
                    [ type_ "number"
                    , value (String.fromFloat model.original_length)
                    , onInput (OrigLength << String.toFloat)

                    --, attribute "data-index" "1"
                    ]
                    []
                ]
            , div [ id "orig_amount" ]
                [ label [] [ text "Antall nøster" ]
                , input
                    [ type_ "number"
                    , value (String.fromFloat model.original_amount)
                    , onInput (OrigAmount << String.toFloat)

                    --, attribute "data-index" "2"
                    ]
                    []
                ]
            ]
        , div []
            [ h2 [] [ text "Garnet som skal byttes til:" ]
            , div [ id "alt_length" ]
                [ label [] [ text "Lengde i meter" ]
                , input
                    [ type_ "number"
                    , value (String.fromFloat model.alternative_length)
                    , onInput (AltLength << String.toFloat)

                    --, attribute "data-index" "3"
                    ]
                    []
                ]
            , div [ id "alt_amount" ]
                [ div [] [ text "Beregnet forbruk" ]
                , div [ class "outputnumber" ] [ text (Round.round 2 model.alternative_amount) ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
