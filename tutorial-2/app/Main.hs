{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Main where

import Control.Monad ( when )

import Miso 
  ( MisoString
  , reload 
  , live 
  , defaultEvents
  , startApp
  , App
  , Component(styles, mount)
  , component
  , vcomp
  , Effect, ROOT
  , noop
  , issue
  , io
  , clearLocalStorage, setLocalStorage, clearSessionStorage
  , removeLocalStorage
  , setSessionStorage
  , removeSessionStorage
  , setValue, getElementById
  , View
  , text
  , (=:)
  , CSS(Sheet))

import Miso.Lens.TH ( makeLenses , Lens, lens )
import Miso.Lens ( (^.) , (.=) , use )
import Miso.Html
import Miso.Html.Property hiding ( label_ )
import Miso.CSS qualified as Css
import Miso.FFI.QQ ( js )





data Action
  = ClearStorage
  | AddLocal
  | AddSession
  | SetLocalKey MisoString
  | SetLocalValue MisoString
  | SetSessionKey MisoString
  | SetSessionValue MisoString
  | GetCurrentLocal
  | SetCurrentLocal MisoString
  | GetCurrentSession
  | SetCurrentSession MisoString
  | RemoveLocal
  | RemoveSession
  | Load
  deriving stock (Show, Eq)

type State = Model
data Model = Model
    { _localKey :: MisoString
    , _localValue :: MisoString
    , _sessionKey :: MisoString
    , _sessionValue :: MisoString
    , _currentLocal :: MisoString
    , _currentSession :: MisoString
    } deriving stock (Show , Eq)

$(makeLenses ''Model)

defaultModel :: Model 
defaultModel = Model mempty mempty mempty mempty mempty mempty

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

main :: IO ()
#ifdef INTERACTIVE
main = live defaultEvents app
#else 
main = startApp defaultEvents app { mount = Just Load }
#endif

getCurrentLocalStorage :: IO MisoString
getCurrentLocalStorage = [js| return JSON.stringify(window.localStorage) |]

getCurrentSessionStorage :: IO MisoString
getCurrentSessionStorage = [js| return JSON.stringify(window.sessionStorage) |]

app :: App State Action
app = vcomp init_state next_state viewModel

init_state :: State
init_state = defaultModel

next_state :: Action -> Effect ROOT State Action
next_state = updateModel

updateModel :: Action -> Effect ROOT State Action
updateModel = \case 
    Load -> do 
        issue GetCurrentLocal
        issue GetCurrentSession

    ClearStorage -> do
        io $ do
            clearLocalStorage
            pure GetCurrentLocal
        io $ do
            clearSessionStorage
            pure GetCurrentSession

    AddLocal -> do
        k <- use localKey
        v <- use localValue
        localKey .= mempty
        localValue .= mempty
        io $ do
            when (k /= "") $ do
                setLocalStorage k v 
                resetLocal
            pure GetCurrentLocal

    AddSession -> do
        k <- use sessionKey
        v <- use sessionValue
        sessionKey .= mempty
        sessionValue .= mempty
        io $ do
            when (k /= "") $ do
                setSessionStorage k v 
                resetSession
            pure GetCurrentSession

    RemoveLocal -> do
        k <- use localKey
        localKey .= mempty
        localValue .= mempty
        io $ do
            removeLocalStorage k 
            resetLocal
            pure GetCurrentLocal

    RemoveSession -> do
        k <- use sessionKey
        sessionKey .= mempty
        sessionValue .= mempty
        io $ do
            removeSessionStorage k 
            resetSession
            pure GetCurrentSession

    SetLocalKey k -> localKey .= k 
    SetLocalValue v -> localValue .= v 

    SetSessionKey k -> sessionKey .= k 
    SetSessionValue v -> sessionValue .= v 

    GetCurrentLocal -> io (SetCurrentLocal <$> getCurrentLocalStorage)
    
    SetCurrentLocal x -> currentLocal .= x

    GetCurrentSession -> io (SetCurrentSession <$> getCurrentSessionStorage)

    SetCurrentSession x -> currentSession .= x

    
resetLocal :: IO ()
resetLocal = do 
    getElementById "localValue" >>= flip setValue mempty
    getElementById "localKey" >>= flip setValue mempty

resetSession :: IO ()
resetSession = do
    getElementById "sessionValue" >>= flip setValue mempty
    getElementById "sessionKey" >>= flip setValue mempty




viewModel :: State -> View State Action
viewModel state = 
    div_ [ class_ "card"]
        [ heading
        , subheading
        , storage_grid state
        , footnote
        , small_hint
        , hr_ []
        , local_n_session
        ]

heading = 
    h1_ []
        [ span_ [] ["🍜 🗂️"] , a_ [ href_ "https://github.com/haskell-miso/miso-storage" ] ["miso-storage"]]

subheading = 
    div_ [ class_ "subhead" ]
        [ "⚡ data survives page reload - local stays, session dies when tab closes" ]

storage_grid state =
    div_ [ class_ "storage-grid" ]
        [ local_panel state, session_panel state ]

local_panel state = 
    div_ [ class_ "panel local local-panel"]
        [ heading2
        , input_group
        , button_cluster
        , display_box state
        ]
        
    where
        heading2 = 
            h2_ []
                [ "📦 local storage"
                , span_ [ class_ "badge" ] [ "persists until cleared "]
                ]
        
        input_group = 
            div_ [ class_ "input-group" ]
                [ div_ [ class_ "row"]
                    [ label_ [] ["Key"] , input_ [ placeholder_ "e.g. key" , id_ "localKey" , type_ "text" , onChange SetLocalKey]]
                , div_ [ class_ "row"]
                    [ label_ [] [ "Value"] , input_ [ placeholder_ "e.g. value" , id_ "localValue", type_ "text", onChange SetLocalValue]]
                ]

        button_cluster =
            div_ [ class_ "button-cluster" ]
                [ button_
                    [ id_ "setLocalBtn" 
                    , class_ "primary"
                    , onClick AddLocal
                    ]
                    ["💾 set item"]
                , button_ [ id_ "removeLocalBtn" , onClick RemoveLocal ] ["❌ remove item"]
                ]

        display_box state =
            div_ [ class_ "display-box" ]
                [ p_ [] ["📋 current local storage"]
                , div_ 
                    [ id_ "localDisplay"
                    , class_ "storage-content"
                    ]
                    [ text (state ^. currentLocal)]
                ]

session_panel state =
    div_ [ class_ "panel session session-panel"]
        [ heading2
        , input_group
        , button_cluster
        , display_box state
        ]
    where
        heading2 = 
            h2_ [] 
                [ "⏳ session storage"
                , span_ [ class_ "badge"] ["cleared on tab close"]]

        input_group = 
            div_ [ class_ "input-group" ]
            [ div_ [ class_ "row"] 
                [ label_ [] [ "Key"]
                , input_
                    [ placeholder_ "e.g. draft"
                    , id_ "sessionKey"
                    , type_ "text"
                    , onChange SetSessionKey
                    ]
                ]
            , div_ [ class_ "row" ]
                [ label_ [] [ "Value" ]
                , input_
                    [ placeholder_ "value"
                    , id_ "sessionValue"
                    , type_ "text"
                    , onChange SetSessionValue
                    ]
                ]
            ]
            

        button_cluster =
            div_ [ class_ "button-cluster" ] 
                [ button_
                    [ id_ "setSessionBtn"
                    , class_ "primary"
                    , onClick AddSession
                    ]
                    [ "💾 set item" ]
                , button_
                    [ id_ "removeSessionBtn" , onClick RemoveSession ]
                    [ "❌ remove item" ]
                ]

        display_box state =
            div_ [ class_ "display-box" ] 
                [ p_ [] ["📋 current session storage"]
                , div_
                    [ id_ "sessionDisplay"
                    , class_ "storage-content"
                    ]
                    [ text (state ^. currentSession) ]
                ]

footnote =
    div_ [ class_ "foot-note" ]
        [ span_ [] [ "🔄 try reloading the page - local stays, session resets (if tab closed)"]
        , button_
            [ id_ "clearAllBtn", class_ "clear-all" , onClick ClearStorage ]
            ["🧹 clear both storages"]
        ]

small_hint =
    div_ [ class_ "small-hint" ]
        [ span_ [] ["✨"] , "keys and values are stored as strings. Use get button to retrieve a single key."]

local_n_session =
    div_ 
        [ Css.style_ 
            [ "color" =: "#4a5f7d"
            , "font-size" =: "0.85rem"
            , "gap" =: "1.5rem"
            , "justify-content" =: "center"
            , "display" =: "flex"
            ]
        ]
        [ span_ [] ["🗂️ local — shared across tabs"]
        , span_ [] ["⏳ session — only current tab"]
        ]

style :: Css.StyleSheet
style = Css.sheet_ []