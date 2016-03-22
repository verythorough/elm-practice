module StringMod (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp
import Task exposing (Task)
import Effects exposing (Effects)
import Json.Encode
import Signal exposing (Address)
import String exposing (left, dropLeft, toUpper, split)
import Array exposing (fromList, slice, toList, Array)
import Regex exposing (regex)


main : Signal Html
main =
  app.html


app : StartApp.App Model
app =
  StartApp.start
    { view = view
    , update = update
    , init = ( initialModel, Effects.none )
    , inputs = []
    }


type alias Model =
  { inputStr : String
  , outputStr : String
  }


initialModel : Model
initialModel =
  { inputStr = "this is a sample"
  , outputStr = ""
  }


view : Address Action -> Model -> Html
view address model =
  div
    [ class "content" ]
    [ h2 [] [ text "StringMod" ]
    , p [] [ text "Enter a string and choose a style." ]
    , input
        [ type' "text"
        , onInput address SetInputStr
        , defaultValue model.inputStr
        , autofocus True
        ]
        []
    , div [] (List.map (viewButton address) listConvertBtns)
    , h3 [] [ text "Result:" ]
    , input [ type' "text", readonly True, value (model.outputStr) ] []
    ]


viewButton : Address Action -> ConvertBtn -> Html
viewButton address btn =
  let
    buttonId =
      (String.toLower btn.converter) ++ "-btn"
  in
    label
      [ for buttonId ]
      [ input
          [ id buttonId
            --radio buttons have psudo-classes for easy "selected" styling
            --without extra logic, and keyboard access is built in
          , type' "radio"
          , value btn.converter
          , onClick address (Convert btn.converter)
          , name "converters"
          ]
          []
      , span [] [ text btn.label ]
      ]


onInput address wrap =
  on "input" targetValue (\val -> Signal.message address (wrap val))


defaultValue str =
  property "defaultValue" (Json.Encode.string str)


type Action
  = SetInputStr String
  | Convert Converter


type alias ConvertBtn =
  { converter : Converter
  , label : String
  }


type alias Converter =
  String


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    SetInputStr inputStr ->
      ( { model | inputStr = inputStr }, Effects.none )

    Convert converter ->
      ( { model | outputStr = convertString converter model.inputStr }, Effects.none )



{- -I initially set converters as a union type, but I couldn't treat them as a
list for mapping the button view.  Rather than list them 3 times (type
declaration, case expression, and button list), I saved them as strings.
Bad idea?  I do like that the required wildcard case gives an opportunity to
explain, but it's not as clean as type coverage checking.-
-}


listConvertBtns : List ConvertBtn
listConvertBtns =
  [ ConvertBtn "Lowercase" "lowercase"
  , ConvertBtn "Caps" "ALL CAPS"
  , ConvertBtn "Reverse" "sdrawkcaB"
  , ConvertBtn "ReverseCaps" "SDRAWKCAB"
  , ConvertBtn "Sentence" "Sentence case"
  , ConvertBtn "Initial" "Initial Case"
  , ConvertBtn "Title" "Title Case (AP)"
  , ConvertBtn "Snake" "snake_case"
  , ConvertBtn "ScreamingSnake" "SCREAMING_SNAKE_CASE"
  , ConvertBtn "Kebob" "kebob-case"
  , ConvertBtn "Train" "Train-Case"
  , ConvertBtn "Camel" "camelCase"
  , ConvertBtn "UpperCamel" "UpperCamelCase"
  , ConvertBtn "Leet" "l337 5p34k"
  , ConvertBtn "Studly" "StUdLyCaPs"
  ]


convertString : Converter -> String -> String
convertString converter str =
  let
    stripSplitList =
      encodeStr symbolStripPairs str |> String.words
  in
    case converter of
      "Lowercase" ->
        String.toLower str

      "Caps" ->
        String.toUpper str

      "Reverse" ->
        String.reverse str

      "ReverseCaps" ->
        String.reverse str |> String.toUpper

      "Sentence" ->
        capWord str

      "Initial" ->
        String.split " " str |> List.map (capHyphenated capWord) |> String.join " "

      "Title" ->
        let
          wordList =
            String.split " " str

          lastIndex =
            (List.length wordList) - 1
        in
          List.indexedMap
            (\i s ->
              (if (i == 0 || i == lastIndex) then
                capWord s
               else
                titleCapWord s
              )
            )
            wordList
            |> String.join " "

      "Snake" ->
        stripSplitList |> String.join "_"

      "ScreamingSnake" ->
        stripSplitList |> String.join "_" |> String.toUpper

      "Kebob" ->
        stripSplitList |> String.join "-"

      "Train" ->
        stripSplitList |> List.map capWord |> String.join "-"

      "Camel" ->
        encodeStr symbolStripPairs str
          |> String.words
          |> List.indexedMap
              (\i s ->
                (if i == 0 then
                  String.toLower s
                 else
                  capWord s
                )
              )
          |> String.concat

      --Alternate method:
      -- "Camel" ->
      --   let
      --     head =
      --       List.take 1 stripSplitList |> String.concat |> String.toLower
      --     tail =
      --       List.drop 1 stripSplitList |> List.map capWord
      --   in
      --     String.concat (head :: tail)
      "UpperCamel" ->
        encodeStr symbolStripPairs str
          |> String.words
          |> List.map capWord
          |> String.concat

      "Leet" ->
        String.toLower str |> encodeStr leetPairs

      "Studly" ->
        --could use String.toList but would need to import Char for toUpper
        String.split "" str
          |> List.indexedMap
              (\i s ->
                (if i % 2 == 0 then
                  String.toUpper s
                 else
                  String.toLower s
                )
              )
          |> String.concat

      _ ->
        "This converter is not yet defined in StringMod.convertString."


capWord : String -> String
capWord word =
  --words with caps assumed to be capitalized as desired (e.g. iPad, eBay)
  if Regex.contains (regex "[A-Z]") word then
    word
  else
    --chose this method over String.uncons because it's more concise and the
    --Maybe seemed unnecessary (uncons failure cases don't break the function)
    toUpper (left 1 word) ++ (dropLeft 1 word)


titleCapWord : String -> String
titleCapWord word =
  if List.member word smallWords then
    word
  else
    capWord word


smallWords : List String
smallWords =
  --words not capitalized in AP-style titles
  [ "a", "an", "and", "at", "but", "by", "for", "in", "nor", "of", "on", "or", "so", "the", "to", "up", "yet" ]


capHyphenated : (String -> String) -> String -> String
capHyphenated capFunc word =
  --Used in cases calling for caps after a hyphen. Does not account for commonly
  --hyphenated compound words like Follow-up, which some styles leave lowercase
  if String.contains "-" word then
    String.split "-" word
      |> List.map capFunc
      |> String.join "-"
  else
    capFunc word


encodeStr : List ( String, String ) -> String -> String
encodeStr pairs str =
  List.foldl replaceFromTuple str pairs


replaceFromTuple : ( String, String ) -> String -> String
replaceFromTuple ( old, new ) str =
  Regex.replace Regex.All (regex old) (\_ -> new) str


symbolStripPairs : List ( String, String )
symbolStripPairs =
  --characters that don't usually break words
  [ ( "[`']", "" )
    --all other non-alphanumeric characters
  , ( "[^a-zA-Z\\d]+", " " )
  ]


leetPairs : List ( String, String )
leetPairs =
  [ ( "a", "4" )
  , ( "e", "3" )
  , ( "g", "6" )
  , ( "i", "1" )
  , ( "o", "0" )
  , ( "s", "5" )
  , ( "t", "7" )
  ]
