module Tender.View.Button
  ( Variant(..)
  , button
  , linkButton
  , iconButton
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
base = "inline-flex items-center justify-center gap-2 rounded-xl border-[3px] px-6 py-3 text-lg font-bold transition active:translate-y-0.5 active:shadow-none focus:outline-none focus-visible:ring-4 focus-visible:ring-star"

variantClasses :: Variant -> String
variantClasses = case _ of
  Primary -> "border-ink bg-brand-500 text-white shadow-pop hover:bg-brand-600"
  Secondary -> "border-ink bg-white text-ink shadow-pop hover:bg-brand-50"
  OnColour -> "border-white bg-white/20 text-white hover:bg-white/30"

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

-- | Large square icon button, e.g. like and nope
iconButton :: forall a. { label :: String, icon :: Icon, classes :: String } -> a -> Html a
iconButton r msg =
  HE.button
    [ HA.class' ("grid place-items-center rounded-2xl border-[3px] border-ink shadow-pop transition duration-150 hover:scale-105 active:scale-90 focus:outline-none focus-visible:ring-4 focus-visible:ring-star " <> r.classes)
    , HA.onClick msg
    , HA.title r.label
    , HA.createAttribute "aria-label" r.label
    ]
    [ icon r.icon "w-1/2 h-1/2" ]
