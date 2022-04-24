module PhotoGroove exposing (main)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Array exposing (Array)

view : { a | selectedUrl : String, photos : List { b | url : String } } -> Html.Html { description : String, data : String }
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ] 
            (List.map
                
                (viewThumbnail model.selectedUrl )
                model.photos
                )
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]

type alias Photo = { url : String }
type alias Model =
    { photos: List Photo
    , selectedUrl: String
    }

viewThumbnail : String -> { a | url : String } -> Html.Html { description : String, data : String }
viewThumbnail selectedUrl thumb =
        img 
            [ src (urlPrefix ++ thumb.url)
            , classList [ ( "selected", selectedUrl == thumb.url) ]
            , onClick { description = "ClickedPhoto", data = thumb.url }
            ] []

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

photoList : String
photoList =
    "http://elm-in-action.com/list-photos"


isEmpty : String -> Bool
isEmpty str = str == ""

initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }

photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos

update : { a | description : String, data : b } -> { c | selectedUrl : b } -> { c | selectedUrl : b }
update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedUrl = msg.data }
    else
        model

main : Program () { photos : List Photo, selectedUrl : String } { description : String, data : String }
main =
    Browser.sandbox 
        {init = initialModel
        , view = view
        , update = update
        }