module Tender.View.Button
  ( Variant(..)
  , button
  , linkButton
  , roundButton
  ) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.Types (Url)
import Tender.View.Icon (Icon, icon)

data Variant
  = Primary
  | Secondary
  -- | For use on top of a coloured background
  | OnColour

-- Full class names so Tailwind finds them in the bundle
base :: String
base = "inline-flex items-center justify-center gap-2 rounded-full px-6 py-3 text-lg font-bold transition active:translate-y-0.5 active:shadow-none focus:outline-none focus-visible:ring-4 focus-visible:ring-butter"

variantClasses :: Variant -> String
variantClasses = case _ of
  Primary -> "bg-tomato-500 text-white shadow-pop hover:bg-tomato-600"
  Secondary -> "bg-white text-tomato-600 shadow-pop ring-2 ring-tomato-100 hover:bg-tomato-50"
  OnColour -> "bg-white/20 text-white ring-2 ring-white/60 hover:bg-white/30"

content :: forall a. Maybe Icon -> String -> Array (Html a)
content i label = maybe [] (\x -> [ icon x "w-5 h-5" ]) i <> [ HE.text label ]

button :: forall a. Variant -> Maybe Icon -> String -> a -> Html a
button v i label msg =
  HE.button
    [ HA.class' (base <> " " <> variantClasses v), HA.onClick msg ]
    (content i label)

-- | Looks like a button, opens the url in a new tab
linkButton :: forall a. Variant -> Maybe Icon -> String -> Url -> Html a
linkButton v i label url =
  HE.a
    [ HA.class' (base <> " " <> variantClasses v)
    , HA.href url
    , HA.target "_blank"
    , HA.rel "noopener"
    ]
    (content i label)

-- | Large circular icon button, e.g. like and nope
roundButton :: forall a. { label :: String, icon :: Icon, classes :: String } -> a -> Html a
roundButton r msg =
  HE.button
    [ HA.class' ("grid place-items-center rounded-full shadow-card transition duration-150 hover:scale-105 active:scale-90 focus:outline-none focus-visible:ring-4 focus-visible:ring-butter " <> r.classes)
    , HA.onClick msg
    , HA.title r.label
    , HA.createAttribute "aria-label" r.label
    ]
    [ icon r.icon "w-1/2 h-1/2" ]
