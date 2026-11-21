-- | Inline SVG icons, drawn on a 24x24 grid in the style of Lucide (ISC
-- | licensed). Size and colour come from the classes passed in.
module Tender.View.Icon (Icon(..), icon) where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

data Icon
  = Heart
  | Close
  | Star
  | ExternalLink
  | Share
  | Copy
  | Utensils
  | Refresh
  | Check

paths :: Icon -> Array String
paths = case _ of
  Heart -> [ "M19 14c1.49-1.46 3-3.21 3-5.5A5.5 5.5 0 0 0 16.5 3c-1.76 0-3 .5-4.5 2-1.5-1.5-2.74-2-4.5-2A5.5 5.5 0 0 0 2 8.5c0 2.3 1.5 4.05 3 5.5l7 7Z" ]
  Close -> [ "M18 6 6 18", "m6 6 12 12" ]
  Star -> [ "M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 3.25L7 14.14 2 9.27l6.91-1.01L12 2z" ]
  ExternalLink -> [ "M15 3h6v6", "M10 14 21 3", "M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6" ]
  Share -> [ "M4 12v8a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2v-8", "m16 6-4-4-4 4", "M12 2v13" ]
  Copy -> [ "M10 8h10a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2H10a2 2 0 0 1-2-2V10a2 2 0 0 1 2-2z", "M4 16a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2" ]
  Utensils -> [ "M3 2v7c0 1.1.9 2 2 2h4a2 2 0 0 0 2-2V2", "M7 2v20", "M21 15V2a5 5 0 0 0-5 5v6c0 1.1.9 2 2 2h3Zm0 0v7" ]
  Check -> [ "M20 6 9 17l-5-5" ]
  Refresh -> [ "M3 12a9 9 0 0 1 9-9 9.75 9.75 0 0 1 6.74 2.74L21 8", "M21 3v5h-5", "M21 12a9 9 0 0 1-9 9 9.75 9.75 0 0 1-6.74-2.74L3 16", "M8 16H3v5" ]

-- | An icon with the given classes, e.g. `icon Heart "w-6 h-6 fill-current"`.
-- | Hidden from screen readers; label the button around it instead.
icon :: forall a. Icon -> String -> Html a
icon i classes =
  HE.svg
    [ HA.viewBox "0 0 24 24"
    , HA.fill "none"
    , HA.stroke "currentColor"
    , HA.strokeWidth "2.25"
    , HA.strokeLinecap "round"
    , HA.strokeLinejoin "round"
    , HA.class' classes
    , HA.createAttribute "aria-hidden" "true"
    ]
    (map (\d -> HE.path [ HA.d d ] []) (paths i))
