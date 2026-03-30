{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso
import Miso.Lens
import Miso.String ( toMisoString )
import Miso.Html.Element as H 
import Miso.Html.Event as E 
import Miso.Html.Property as P 
import Miso.CSS qualified as Css

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

main :: IO ()
#ifdef INTERACTIVE
main = reload defaultEvents app
#else 
main = startApp defaultEvents app 
#endif

type State = Int

data Action
  = Increment
  | Decrement
  deriving stock (Show, Eq)

app :: App State Action
app = (component 0 updateModel viewModel)
      { styles = [ Sheet sheet ]
      }

updateModel :: Action -> Effect parent State Action
updateModel Increment = this += 1
updateModel Decrement = this -= 1

viewModel :: State -> View State Action
viewModel state = H.div_
  [ P.class_ "counter-container" ]
  [ H.h1_ [ P.class_ "counter-title" ]
          [ "Counter" ] 
  , H.div_ [ P.class_ "counter-display" ]
           [ text (toMisoString state) ]
  , H.div_ [ P.class_ "buttons-container" ]
           [ H.button_ [ E.onClick Increment , P.class_ "btn-increment" ]
                       [ text "+" ]
           , H.button_ [ E.onClick Decrement , P.class_ "btn-decrement" ]
                       [ text "-" ]
           ]
  ]

sheet :: Css.StyleSheet
sheet = Css.sheet_ 
  [ Css.selector_ ":root"
      [ "--primary-color" =: "#4a6bff"
      , "--primary-hover" =: "#3451d1"
      , "--secondary-color" =: "#ff4a6b"
      , "--secondary-hover" =: "#d13451"
      , "--background" =: "#f7f9fc"
      , "--text-color" =: "#333"
      , "--shadow" =: "0 4px 10px rgba(0, 0, 0, 0.1);"
      , "--transition" =: "all 0.3s ease;"
      ]
  , Css.selector_ "body"
      [ Css.fontFamily "'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"
      , Css.display "flex"
      , Css.justifyContent "center"
      , Css.alignItems "center"
      , Css.height "100vh"
      , Css.margin "0"
      , Css.backgroundColor (Css.var "background")
      , Css.color (Css.var "text-color")
      ]
  , counter_container_style
  , counter_display_style
  , buttons_container_style
  , button_style 
  , increment_btn_style
  , increment_btn_hover_style
  , decrement_btn_style
  , decrement_btn_hover_style
  , keyframes
  , counter_display_animate
  , media
  ]
  where
    counter_container_style = Css.selector_ ".counter-container"
      [ Css.backgroundColor Css.white
      , Css.padding (Css.rem 2)
      , Css.borderRadius (Css.px 12)
      , Css.boxShadow "var(--shadow)"
      , Css.textAlign "center"
      ]
    counter_display_style = Css.selector_ ".counter-display"
      [ Css.fontSize "5rem"
      , Css.fontWeight "bold"
      , Css.margin "1rem 0"
      , Css.transition "var(--transition)"
      ]
    buttons_container_style = Css.selector_ ".buttons-container"
      [ Css.display "flex"
      , Css.gap "1rem"
      , Css.justifyContent "center"
      , Css.marginTop "1.5rem"
      ]
    button_style = Css.selector_ "button"
      [ Css.fontSize "1.5rem"
      , Css.width "3rem"
      , Css.height "3rem"
      , Css.border "none"
      , Css.borderRadius "50%"
      , Css.cursor "pointer"
      , Css.transition "var(--transition)"
      , Css.color Css.white
      , Css.display "flex"
      , Css.alignItems "center"
      , Css.justifyContent "center"
      ]
    increment_btn_style = Css.selector_ ".btn-increment" 
      [ Css.backgroundColor (Css.var "primary-color") ]
    increment_btn_hover_style = Css.selector_ ".btn-increment:hover"
      [ Css.backgroundColor (Css.var "primary-hover")
      , Css.transform "translateY(-2px)"
      ]

    decrement_btn_style = Css.selector_ ".btn-decrement"
      [ Css.backgroundColor $ Css.var "secondary-color"]
    
    decrement_btn_hover_style = Css.selector_ ".btn-decrement:hover"
      [ Css.backgroundColor $ Css.var "secondary-hover"
      , Css.transform "translateY(-2px)"
      ]
    
    keyframes = Css.keyframes_ "pulse"
      [ Css.pct 0 =: [ Css.transform "scale(1)"]
      , Css.pct 50 =: [ Css.transform "scale(1.1)"]
      , Css.pct 100 =: [ Css.transform "scale(1)"]
      ]

    counter_display_animate = Css.selector_ ".counter-display.animate"
      [ Css.animation "pulse 0.3s ease" ]

    media = Css.media_ "(max-width: 480px)"
       [ ".counter-container" =: [ Css.padding $ Css.rem 1.5 ]
       , ".counter-display" =: [ Css.fontSize $ Css.rem 3 ]
       , "button" =: 
            [ Css.fontSize $ Css.rem 1.2
            , Css.width $ Css.rem 2.5
            , Css.width $ Css.rem 2.5
            ] 
       ]