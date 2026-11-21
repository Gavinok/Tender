module Tender.View.Layout (page, centred) where

import Prelude

import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.View.Icon (Icon(..), icon)

wordmark :: forall a. Html a
wordmark =
  HE.div [ HA.class' "flex items-center gap-1.5 text-tomato-500" ]
    [ icon Utensils "w-6 h-6"
    , HE.span [ HA.class' "text-2xl font-black tracking-tight" ] [ HE.text "tender" ]
    ]

-- | Badge with the number of matches so far
matchBadge :: forall a. Int -> Html a
matchBadge n =
  HE.div
    [ HA.class' "flex items-center gap-1 rounded-full bg-white px-3 py-1 font-bold text-tomato-600 shadow-sm ring-1 ring-tomato-100"
    , HA.createAttribute "aria-label" (show n <> if n == 1 then " match" else " matches")
    ]
    [ icon Heart "w-4 h-4 fill-current", HE.text (show n) ]

-- | Full height page with the wordmark at the top, and the match count
-- | once the user is swiping
page :: forall a. Maybe Int -> Array (Html a) -> Html a
page matches content =
  HE.main [ HA.id "main", HA.class' "mx-auto flex min-h-[100dvh] max-w-md flex-col px-4" ]
    [ HE.header [ HA.class' "flex items-center justify-between py-4" ]
        [ wordmark
        , case matches of
            Just n -> matchBadge n
            Nothing -> HE.text ""
        ]
    , HE.div [ HA.class' "flex flex-1 flex-col pb-6" ] content
    ]

-- | Content centred in the remaining space
centred :: forall a. Array (Html a) -> Html a
centred = HE.div [ HA.class' "flex flex-1 flex-col items-center justify-center gap-6 text-center" ]
