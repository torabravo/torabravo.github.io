module Main exposing (..)

import Html exposing( Html, program )
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Date exposing (Date)
import Date.Extra.Compare exposing (..)


born: Date
born = 
    Date.fromString "1984/09/30" |> Result.withDefault (Date.fromTime 0)

die: Date
die =
    Date.fromString "2064/09/30" |> Result.withDefault (Date.fromTime 0)

today: Date
today =
    Date.fromString "2016/12/23" |> Result.withDefault (Date.fromTime 0)

-- MODEL -----------------------------------------------------

type alias Model =
    {decades: List Decade}

type alias Year =
    {year: Int, days: List Date}

type alias Decade = 
     List Year

type alias Range =
    {from: Date, to: Date}

init: (Model, Cmd Msg)
init = 
    let model =
        {decades = 
        [
            [ Year 1984 [born], Year 1986 [die]],
            [ Year 1984 [born], Year 1986 [die]]
        ]
        }

        
    in 
        (model, Cmd.none)


-- UPDATE ----------------------------------------------------


type Msg =
    Highlight Range



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (model, Cmd.none)


-- VIEW ------------------------------------------------------

viewDay: Date -> Int -> Int -> Svg Msg
viewDay date x y =
    let  
        className = 
            if is After date today then "act" else "psv"
    in
        rect [width "5", height "5", Svg.Attributes.x (toString x), Svg.Attributes.y (toString y), class className] []


viewYearRow: Int -> List Date -> List (Svg Msg)
viewYearRow y days = 
    days |> List.indexedMap (\i day ->  viewDay day (i * 7 + 33) y)


viewYear: Int -> Year -> List (Svg Msg)
viewYear y year = 
    let
        textOffset = y + 10
        fstRow = year.days |> List.take 182 |> viewYearRow y
        sndRow = year.days |> List.drop 182 |> viewYearRow (y + 7)
    in
         Svg.text_ [Svg.Attributes.x "0", Svg.Attributes.y (toString y)] [Svg.text (toString year.year)]  :: fstRow ++ sndRow

viewDecade: Int -> Decade -> List (Svg Msg)
viewDecade y decade =
    decade 
    |> List.indexedMap (\i year -> viewYear (y + i * 33 * 2) year)
    |> List.concat

view : Model -> Html Msg
view model =
    let 
        nodes = model.decades 
        |> List.indexedMap (\i decade -> viewDecade (i * 33 * 2 * 10 + 7) decade)
        |> List.concat
    in 
        svg [ width "1200", height "1400"] nodes
        




-- MAIN -------------------------------------------------

main : Program Never Model Msg
main =
    program 
        { init = init
        , update = update
        , view = view
        , subscriptions = (\model -> Sub.none)
        }