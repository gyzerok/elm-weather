module Main exposing (..)

import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Char
import Http
import Json.Decode as Decode
import Time


-- MODEL


type alias Model =
    { zipInput : String
    , forecasts : List Forecast
    }


type alias Forecast =
    { zipCode : String
    , temperature : Maybe Float
    }


init : ( Model, Cmd Msg )
init =
    ( { zipInput = ""
      , forecasts = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddForecast
    | ZipInputChanged String
    | RefreshForecasts
    | NewForecastLoaded Forecast
    | ExistingForecastUpdated Forecast


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddForecast ->
            let
                isValidZipCode =
                    String.length model.zipInput >= 5 && String.length model.zipInput <= 6
            in
                if isValidZipCode then
                    ( { model | zipInput = "" }
                    , getForecast NewForecastLoaded model.zipInput
                    )
                else
                    ( model, Cmd.none )

        ZipInputChanged input ->
            ( { model
                | zipInput = String.filter Char.isDigit input
              }
            , Cmd.none
            )

        RefreshForecasts ->
            ( model
            , model.forecasts
                |> List.map (.zipCode >> getForecast ExistingForecastUpdated)
                |> Cmd.batch
            )

        NewForecastLoaded forecast ->
            ( { model
                | forecasts = model.forecasts ++ [ forecast ]
              }
            , Cmd.none
            )

        ExistingForecastUpdated refreshedForecast ->
            let
                updateForecast forecast =
                    if forecast.zipCode == refreshedForecast.zipCode then
                        refreshedForecast
                    else
                        forecast
            in
                ( { model
                    | forecasts =
                        model.forecasts
                            |> List.map updateForecast
                  }
                , Cmd.none
                )



-- HTTP


createUrl : String -> List ( String, String ) -> String
createUrl baseUrl args =
    let
        queryPair : ( String, String ) -> String
        queryPair ( key, value ) =
            queryEscape key ++ "=" ++ queryEscape value

        queryEscape : String -> String
        queryEscape string =
            String.join "+" (String.split "%20" (Http.encodeUri string))
    in
        case args of
            [] ->
                baseUrl

            _ ->
                baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)


getForecast : (Forecast -> msg) -> String -> Cmd msg
getForecast msg zipCode =
    let
        url =
            createUrl "http://api.openweathermap.org/data/2.5/weather"
                [ ( "zip", zipCode ++ ",us" )
                , ( "units", "metric" )
                , ( "APPID", "cbbf2cace105ede143a9d7becd38400c" )
                ]
    in
        Http.get url decodeTemperature
            |> Http.send (Result.toMaybe >> Forecast zipCode >> msg)


decodeTemperature : Decode.Decoder Float
decodeTemperature =
    Decode.at [ "main", "temp" ] Decode.float



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if List.isEmpty model.forecasts then
        Sub.none
    else
        Time.every (20 * Time.second) (always RefreshForecasts)



-- VIEW


view : Model -> Html.Html Msg
view { zipInput, forecasts } =
    Html.div
        [ Attributes.id "app-container"
        ]
        [ Html.div
            [ Attributes.id "form"
            ]
            [ Html.label [] [ Html.text "Zip Code:" ]
            , Html.input
                [ Attributes.placeholder "Enter Zip"
                , Attributes.value zipInput
                , Events.onInput ZipInputChanged
                ]
                []
            , Html.button
                [ Events.onClick AddForecast
                ]
                [ Html.text "Add location" ]
            ]
        , forecasts
            |> List.map forecastView
            |> Html.div []
        ]


forecastView : Forecast -> Html.Html msg
forecastView forecast =
    let
        temperature =
            forecast.temperature
                |> Maybe.map toString
                |> Maybe.withDefault "err"
    in
        Html.div [ Attributes.class "location" ]
            [ Html.p [ Attributes.class "zip" ] [ Html.text forecast.zipCode ]
            , Html.p [ Attributes.class "temp" ] [ Html.text temperature, Html.text "℃" ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
