module PhotoGroove exposing (main)

import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Array exposing (Array)
import Random
import Html exposing (..)


type alias Photo = { url : String }
--type alias Model =
--    { photos: List Photo
--    , selectedUrl: String
--    , chosenSize: ThumbnailSize
--    }

type alias Model =
    { status: Status
    , chosenSize: ThumbnailSize
    }

-- type alias Msg =
--     { description : String
--     , data : String
--     , size: ThumbnailSize
--     }

type Status
    = Loading
    | Loaded (List Photo) String -- String is for the url
    | Errored String

type Msg 
    = ClickedPhoto String
    | GotRandomPhoto Photo
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe

type ThumbnailSize 
    = Small
    | Medium
    | Large


view : Model -> Html.Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded  photos selectedUrl  -> viewLoaded photos selectedUrl model.chosenSize 
            Loading                     -> []
            Errored errorMessage        -> [ text ("Error: " ++ errorMessage)]
        

viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
        [ h1 [] [ text "Photo Groove" ]
        , button 
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise me" ]
        , h3 [] [ text "Thumbnail size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [Small, Medium, Large])
        , div [ id "thumbnails", class (sizeToString chosenSize) ] 
            (List.map (viewThumbnail selectedUrl ) photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ selectedUrl)
            ]
            []
        ]



viewThumbnail : String -> Photo -> Html.Html Msg
viewThumbnail selectedUrl thumb =
        img 
            [ src (urlPrefix ++ thumb.url)
            , classList [ ( "selected", selectedUrl == thumb.url) ]
            , onClick (ClickedPhoto thumb.url)
            ] []

viewSizeChooser : ThumbnailSize -> Html.Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size)] []
        , text (sizeToString size) 
        ]

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small   -> "small"
        Medium  -> "med"
        Large   -> "large"

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }



selectUrl : String -> Status -> Status
selectUrl url status = 
    case status of
        Loaded photos _         -> Loaded photos url
        Loading                 -> status 
        Errored errorMessage    -> status

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
    ClickedPhoto url ->
        ( { model | status = selectUrl url model.status }, Cmd.none )
    GotRandomPhoto photo ->
        ( { model | status = selectUrl photo.url model.status }, Cmd.none )
    ClickedSize size ->
        ( { model | chosenSize = size }, Cmd.none )
    ClickedSurpriseMe ->
        case model.status of 
            Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model
            _ -> (model, Cmd.none)

main : Program () Model Msg
main =
    Browser.element 
        {init = \_ -> (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }