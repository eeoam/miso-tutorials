{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2026 Eric Macaulay
-- License     : BSD3-style (see the file LICENSE)
-- Maintainer  : Eric Macaulay <eric@whetztone.net>
-- Stability   : experimental
-- Portability : non-portable

module Main where

import GHC.Generics ( Generic )
import Prelude hiding ((.))
import Control.Category ((.))
import Control.Monad ( unless )
import Data.Bool ( bool )
import Data.IntMap ( IntMap )
import Data.IntMap qualified as IMap 


import Miso hiding ( at )
import Miso.Lens
import Miso.Lens.TH ( makeLenses )
import Miso.String qualified as Miso.String
import Miso.Html.Element as H 
import Miso.Html.Event as E 
import Miso.Html.Property hiding ( label_ )  
import Miso.CSS qualified as Css






type State = Model 
data Model
  = Model
  { _entries :: IntMap Entry
  , _field   :: MisoString
  , _uid     :: Int 
  , _visibility :: MisoString
  , _step :: Bool
  } deriving stock (Generic, Show, Eq)

data Entry 
  = Entry
  { _description :: MisoString
  , _completed :: Bool
  , _editing :: Bool
  , _focussed :: Bool
  } deriving stock (Generic, Show, Eq)

$(makeLenses ''Entry)
$(makeLenses ''Model)

emptyModel :: Model
emptyModel = Model
  { _entries = mempty
  , _visibility = "All"
  , _field = mempty
  , _uid = 0
  , _step = False
  }

newEntry :: MisoString -> Entry
newEntry desc = Entry
  { _description = desc 
  , _completed   = False
  , _editing     = False
  , _focussed    = False
  }


type Msg = Action
data Action
  = Skip
  | CurrentTime Int
  | UpdateField MisoString
  | EditingEntry Int Bool
  | UpdateEntry Int MisoString
  | Add
  | Delete Int
  | DeleteComplete
  | Check Int Bool
  | CheckAll Bool
  | ChangeVisibility MisoString
  | FocusOnInput
  deriving stock (Show, Eq)






#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

main :: IO ()
#ifdef INTERACTIVE
main = reload defaultEvents app
#else 
main = startApp (defaultEvents <> keyboardEvents) app 
#endif

app :: App State Action
app = (component init_state updateModel viewModel)
      { mount = Just FocusOnInput
      #ifdef INTERACTIVE
      , styles = 
          [ Href "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.min.css" False
          , Href "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.4.3/index.min.css" False
          ]
      #endif
      }

init_state :: State
init_state = emptyModel

updateModel :: Action -> Effect parent State Action
updateModel= \case 
  Skip -> pure ()
  FocusOnInput -> io_ (focus "input-box")
  CurrentTime time -> io_ $ consoleLog (toMisoString time)
  Add -> do 
    value <- use field 
    unless (Miso.String.null value) $ do 
      field .= mempty
      uid += 1
      nextId <- use uid
      entries %= IMap.insert nextId (newEntry value)
  
  UpdateField str -> field .= str

  EditingEntry idx isEditing ->
    entries . at idx %?= (\e ->
      e & editing .~ isEditing
        & focussed .~ isEditing)

  UpdateEntry idx task ->
    (entries . at idx) %?= (description .~ task)

  Delete idx -> entries . at idx .= Nothing

  DeleteComplete -> 
    entries %= IMap.filter (\entry -> not (entry ^. completed))

  Check idx isCompleted ->
    entries . at idx %?= do completed .~ isCompleted

  CheckAll isCompleted ->
    entries %= IMap.map (\entry -> entry & completed .~ isCompleted)

  ChangeVisibility v -> visibility .= v

---
viewModel :: Model -> View model Action
viewModel model = 
  div_
    [ class_ "todomvc-wrapper" ]
    [ section_
        [ class_ "todoapp"]
        [ viewInput model (model ^. field)
        , viewEntries (model ^. visibility) (IMap.toList $ model ^. entries)
        , viewControls model (model ^. visibility) (IMap.toList $ model ^. entries)
        ]
    , infoFooter
    ]

viewInput :: Model -> MisoString -> View model Action 
viewInput _ task =
  header_
    [ class_ "header" ]
    [ h1_ [] [ text "todos"] 
    , input_
        [ class_ "new-todo" 
        , id_ "input-box"
        , placeholder_ "What needs to be done?"
        , autofocus_ True
        , value_ task
        , name_ "newTodo"
        , onInput UpdateField
        , onEnter Skip Add
        ]
    ]

viewEntries :: MisoString -> [(Int , Entry)] -> View model Action
viewEntries visibility entries =
  section_
    [ class_ "main"
    , Css.style_ [ Css.visibility cssVisibility ]
    ]
    [ input_
        [ class_ "toggle-all" 
        , type_ "checkbox"
        , name_ "toggle"
        , id_ "toggle-all"
        , checked_ allCompleted
        , onClick $ CheckAll (not allCompleted)
        ]
    , label_
        [ for_ "toggle-all" ]
        [ text $ Miso.String.pack "Mark all as complete"]
    , ul_ [ class_ "todo-list" ] $
        filter isVisible entries <&> viewEntry
    ]
  where
    cssVisibility = bool "visible" "hidden" (Prelude.null entries)
    allCompleted = Prelude.all _completed (snd <$> entries)
    isVisible (_, Entry {..}) =
      case visibility of
        "Completed" -> _completed
        "Active" -> not _completed
        _ -> True

viewEntry :: (Int , Entry) -> View model Action
viewEntry (eid , Entry{..}) =
  li_
    [ class_ $
        Miso.String.intercalate " " $ [ "completed" | _completed ] <> ["editing" | _editing ]
    , key_ eid
    ]
    [ div_ [ class_"view" ]
        [ input_
          [ class_ "toggle"
          , type_ "checkbox"
          , checked_ _completed
          , onClick $ Check eid (not _completed)
          ]
        , label_
            [ onDoubleClick (EditingEntry eid True) ]
            [ text _description ]
        , button_
            [ class_ "destroy"
            , onClick $ Delete eid 
            ]
            []
        ]
    , input_ 
        [ class_ "edit"
        , value_ _description
        , name_ "title"
        , id_ ("todo-" <> toMisoString eid)
        , onInput (UpdateEntry eid)
        , onBlur (EditingEntry eid False)
        , onEnter Skip (EditingEntry eid False)
        ]
    ]

viewControls :: Model -> MisoString -> [ (Int , Entry)] -> View model Action
viewControls model visibility entries =
  footer_
    [ class_ "footer"
    , hidden_ (null entries)
    ]
    [ viewControlsCount entriesLeft
    , viewControlsFilter visibility
    , viewControlsClear model entriesCompleted
    ]
  where
    entriesCompleted = length . filter (_completed . snd) $ entries
    entriesLeft = length entries - entriesCompleted

viewControlsCount :: Int -> View model Action
viewControlsCount entriesLeft =
  span_ [ class_ "todo-count"]
    [ strong_ [] [ text $ toMisoString entriesLeft ]
    , text (item_ <> " left")
    ]
  where
    item_ = Miso.String.pack $ bool " items" " item" (entriesLeft == 1)

viewControlsFilter :: MisoString -> View model Action
viewControlsFilter visibility =
  ul_ [ class_ "filters" ]
    [ visibilitySwap "#/" "All" visibility
    , text " "
    , visibilitySwap "#/active" "Active" visibility
    , text " "
    , visibilitySwap "#/completed" "Completed" visibility
    ]

--depth+

visibilitySwap :: MisoString -> MisoString -> MisoString -> View model Action
visibilitySwap uri visibility actualVisibility =
  li_ []
    [ a_ 
        [ href_ uri
        , class_ $ Miso.String.concat [ "selected" | visibility == actualVisibility]
        , onClick $ ChangeVisibility visibility 
        ]
        [ text visibility ]
    ]

--depth

viewControlsClear :: Model -> Int -> View model Action
viewControlsClear _ entriesCompleted =
  button_
    [ class_ "clear-completed"
    , hidden_ (entriesCompleted == 0)
    , onClick DeleteComplete
    ]
    [ text $ "Clear completed (" <> toMisoString entriesCompleted <> ")"]

---
infoFooter :: View model Action
infoFooter =
  footer_ [ class_ "info" ]
  [ p_ [] [ text "Double-click to edit a todo"]
  , p_ [] 
       [ text "Written by "
       , a_ [ href_ "https://github.com/eeoam"] [ text "@eeoam"]
       ]
  , p_ []
       [ text "Part of "
       , a_ [ href_ "http://todomvc.com"] [ text "TodoMVC"]
       ]
  ]