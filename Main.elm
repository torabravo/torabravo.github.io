module Main exposing (..)

import Html exposing (Html, program, div)
import Html.Attributes exposing (class)
import Html.Events
import Svg exposing (..)
import Task
import Svg.Attributes exposing (width, height)
import Date exposing (Date)
import Date.Extra.Compare
import Date.Extra.Create
import Date.Extra.Utils
import Date.Extra.Format
import Date.Extra.Config.Config_lt_lt
import Date.Extra.TimeUnit
import List.Extra


born : Date
born =
    toDate "1984/09/30"


toDate : String -> Date
toDate str =
    Date.fromString str |> Result.withDefault (Date.fromTime 0) |> Date.Extra.TimeUnit.startOfTime Date.Extra.TimeUnit.Day


datetoString : Date -> String
datetoString date =
    Date.Extra.Format.format Date.Extra.Config.Config_lt_lt.config "%Y/%m/%d" date



-- MODEL -----------------------------------------------------


type alias Model =
    { dates : List Date
    , today : Date
    , events : List Event
    , message : Maybe String
    , selectedEvent : Maybe Event
    , selectedDay : Maybe Date
    }


type Event
    = EventList String (List Event)
    | Point String Date
    | Period String Date Date


isDateInEvent : Date -> Event -> Bool
isDateInEvent dateToTest event =
    case event of
        Point name date ->
            Date.Extra.Compare.is Date.Extra.Compare.Same dateToTest date

        Period name from to ->
            Date.Extra.Compare.is3 Date.Extra.Compare.BetweenOpen dateToTest from to

        EventList name list ->
            List.any (isDateInEvent dateToTest) list


getEventInfo : Event -> ( String, String )
getEventInfo event =
    case event of
        Point name date ->
            ( name, datetoString date )

        Period name from to ->
            ( name, (datetoString from) ++ "-" ++ (datetoString to) )

        EventList name list ->
            ( name, "[" ++ (list |> List.length |> toString) ++ "]" )


init : ( Model, Cmd Msg )
init =
    let
        fromDate =
            Date.Extra.Create.dateFromFields (Date.year born) Date.Jan 1 1 0 0 0

        dates =
            fromDate
                |> Date.Extra.Utils.dayList (70 * 365)
                |> List.map (Date.Extra.TimeUnit.startOfTime Date.Extra.TimeUnit.Day)

        model =
            { dates = dates
            , today = Date.fromTime 0
            , events =
                [ Point "born" born
                , Period "school" (toDate "1991/09/01") (toDate "2003/06/01")
                , Period "university" (toDate "2004/09/01") (toDate "2009/07/01")
                , EventList "work"
                    [ Period "KM-soft" (toDate "2008/06/02") (toDate "2008/12/20")
                    , Period "Netwrix" (toDate "2009/03/01") (toDate "2010/06/10")
                    , Period "dataArt" (toDate "2010/07/02") (toDate "2011/10/01")
                    , Period "BRASCO" (toDate "2012/03/01") (toDate "2017/10/01")
                    ]
                , EventList "travel"
                    [ Period "italy" (toDate "2014/09/28") (toDate "2014/10/12")
                    , Period "russia" (toDate "2015/08/01") (toDate "2015/09/01")
                    , Period "netherlands" (toDate "2016/05/02") (toDate "2016/05/07")
                    , Period "berlin" (toDate "2017/05/01") (toDate "2017/05/06")
                    , Period "madrid" (toDate "2017/09/28") (toDate "2017/10/01")
                    ]
                ]
            , message = Nothing
            , selectedEvent = Nothing
            , selectedDay = Nothing
            }
    in
        ( model, Task.perform (Just >> GetToday) Date.now )



-- UPDATE ----------------------------------------------------


type Msg
    = Highlight Event
    | DayClick Date
    | GetToday (Maybe Date)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetToday maybeDate ->
            case maybeDate of
                Nothing ->
                    ( model, Cmd.none )

                Just d ->
                    ( { model | today = d }, Cmd.none )

        Highlight evnt ->
            ( { model | selectedEvent = Just evnt }, Cmd.none )

        DayClick day ->
            ( { model | selectedDay = Just day }, Cmd.none )



-- VIEW ------------------------------------------------------


viewDay : Model -> Date -> Int -> Int -> Svg Msg
viewDay model date x y =
    let
        className =
            if model.selectedDay |> Maybe.map (Date.Extra.Compare.is Date.Extra.Compare.Same date) |> Maybe.withDefault False then
                "selected"
            else if model.selectedEvent |> Maybe.map (isDateInEvent date) |> Maybe.withDefault False then
                "highlight"
            else if Date.Extra.Compare.is3 Date.Extra.Compare.BetweenOpen date born model.today then
                "wasted"
            else
                "default"
    in
        rect [ width "5", height "5", Svg.Attributes.x (toString x), Svg.Attributes.y (toString y), Svg.Attributes.class className, Html.Events.onClick (DayClick date) ] []


viewYearRow : Model -> Int -> List Date -> List (Svg Msg)
viewYearRow model y days =
    days |> List.indexedMap (\i day -> viewDay model day (i * 7 + 33) y)


viewYear : Model -> Int -> List Date -> List (Svg Msg)
viewYear model y year =
    let
        txt =
            case List.head year of
                Nothing ->
                    ""

                Just day ->
                    day |> Date.year |> toString

        fstRow =
            year |> List.take 182 |> viewYearRow model y

        sndRow =
            year |> List.drop 182 |> viewYearRow model (y + 7)
    in
        Svg.text_ [ Svg.Attributes.x "0", Svg.Attributes.y (toString (y + 10)), Svg.Attributes.class "year" ] [ Svg.text (txt) ] :: fstRow ++ sndRow


viewDecade : Model -> Int -> List Date -> List (Svg Msg)
viewDecade model y decade =
    decade
        |> List.Extra.groupWhile (\x y -> Date.year x == Date.year y)
        |> List.indexedMap (\i year -> viewYear model (y + i * 14) year)
        |> List.concat


viewEvent : Model -> Event -> Html Msg
viewEvent model event =
    let
        active =
            Maybe.map (\e -> event == e) model.selectedEvent |> Maybe.withDefault False

        classList =
            [ ( "active", active ), ( "event", True ) ]

        ( name, date ) =
            getEventInfo event

        title =
            div [ Html.Events.onClick (Highlight event) ]
                [ Html.b [] [ Html.text name ], Html.span [] [ Html.text date ] ]

        subEvents =
            case event of
                EventList _ list ->
                    list |> List.map (viewEvent model)

                _ ->
                    []
    in
        Html.div [ Html.Attributes.classList classList ] (title :: subEvents)


view : Model -> Html Msg
view model =
    let
        svgNodes =
            viewDecade model 0 model.dates

        eventsHtml =
            List.map (viewEvent model) model.events

        current =
            model.selectedDay |> Maybe.map datetoString |> Maybe.withDefault ""

        --|> List.Extra.groupWhile (\x y -> (Date.year x) // 10 == (Date.year y) // 10)
        --|> List.indexedMap (\i decade -> viewDecade (i * (List.length decade * 14 + 10)) decade)
        --|> List.concat
    in
        div [ class "columns" ]
            [ div [ class "column is-narrow" ] eventsHtml
            , div [ class "column is-narrow" ] [ svg [ width "1400", height "1000" ] svgNodes ]
            , div [ class "column is-narrow" ] [ Html.text current ]
            ]



-- MAIN -------------------------------------------------


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\model -> Sub.none)
        }
