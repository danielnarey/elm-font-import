import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events
import Markdown
import FontImport


-- Scroll to bottom for `FontImport` use example

main =
  Html.beginnerProgram
    { model = chapter1
    , update = update
    , view = view
    }


-- MODEL

type alias Model =
  { fontFace : FontFace
  , fontNames : FontNames
  , content : String
  }


type FontFace
  = Serif
  | Sans
  | Script


type alias FontNames =
  { serif : String
  , sans : String
  , script : String
  }


fonts : FontNames
fonts =
  { serif = "Gentium Basic"
  , sans = "Josefin Sans"
  , script = "Dancing Script"
  }


chapter1 : Model
chapter1 =
  Model Serif fonts intro


intro : String
intro = """

# Anna Karenina

## Chapter 1

Happy families are all alike; every unhappy family is unhappy in its own way.

Everything was in confusion in the Oblonskys’ house. The wife had discovered
that the husband was carrying on an intrigue with a French girl, who had been
a governess in their family, and she had announced to her husband that she
could not go on living in the same house with him...

"""


-- UPDATE

type Msg
  = SwitchTo FontFace


update : Msg -> Model -> Model
update msg model =
  case msg of
    SwitchTo newFontFace ->
      { model
      | fontFace =
          newFontFace
      }


-- VIEW

view : Model -> Html Msg
view model =
  let
    radioButtons =
      [ (fonts.serif, SwitchTo Serif)
      , (fonts.sans, SwitchTo Sans)
      , (fonts.script, SwitchTo Script)
      ]
        |> List.map radioButton
        |> Html.fieldset []

    content =
      model.content
        |> Markdown.toHtml [ model.fontFace |> getStyles ]

  in
    [ model.fontNames
      |> importFonts
    , radioButtons
    , content
    ]
      |> Html.div []


radioButton : (String, msg) -> Html msg
radioButton (labelString, msg) =
  let
    inputElement =
      []
        |> Html.input
          [ Attributes.type_ "radio"
          , Attributes.name "font-size"
          , Events.onClick msg
          ]

    containerStyle =
      [ ("padding", "20px") ]
        |> Attributes.style

  in
    [ inputElement
    , labelString
      |> Html.text
    ]
      |> Html.label [ containerStyle ]


getStyles : FontFace -> Attribute msg
getStyles fontFace =
  let
    fontFamily =
      case fontFace of
        Serif ->
          fonts.serif

        Sans ->
          fonts.sans

        Script ->
          fonts.script

  in
    [ ("font-family", fontFamily)
    , ("font-size", "1.5em")
    , ("width", "50%")
    , ("padding", "40px")
    ]
      |> Attributes.style


importFonts : FontNames -> Html msg
importFonts fonts =
  [ fonts.serif
  , fonts.sans
  , fonts.script
  ]
    |> List.map FontImport.newFontFamily
    |> List.map (FontImport.withVariants [ "400", "700" ])
    |> FontImport.toStyleNode
