module PhotoGroove exposing (main)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Array exposing (Array)
import Html exposing (button)
import Html exposing (label)
import Html exposing (input)
import Html exposing (h3)
import Random


type alias Photo = { url : String }
type alias Model =
    { photos: List Photo
    , selectedUrl: String
    , chosenSize: ThumbnailSize
    }

-- type alias Msg =
--     { description : String
--     , data : String
--     , size: ThumbnailSize
--     }

type Msg 
    = ClickedPhoto String
    | GotSelectedIndex Int
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe

type ThumbnailSize 
    = Small
    | Medium
    | Large


view : Model -> Html.Html Msg
view model =
    div [ class "content" ]
        
        [ h1 [] [ text "Photo Groove" ]
        , button 
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise me" ]
        , h3 [] [ text "Thumbnail size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [Small, Medium, Large])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ] 
            (List.map (viewThumbnail model.selectedUrl ) model.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]

randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)

getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo  -> photo.url
        Nothing     -> ""

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
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }

photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
    ClickedPhoto url ->
        ( { model | selectedUrl = url }, Cmd.none )
    GotSelectedIndex index ->
        ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )
    ClickedSize size ->
        ( { model | chosenSize = size }, Cmd.none )
    ClickedSurpriseMe ->
        ( model, Random.generate GotSelectedIndex randomPhotoPicker )

main : Program () Model Msg
main =
    Browser.element 
        {init = \flags -> (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }