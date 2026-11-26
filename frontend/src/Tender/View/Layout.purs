module Tender.View.Layout (page, centred) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.View.Icon (Icon(..), icon)

wordmark :: forall a. Html a
wordmark =
  HE.div [ HA.class' "flex items-center gap-1.5 text-brand-500" ]
    [ icon Utensils "w-6 h-6"
    , HE.span [ HA.class' "font-display text-2xl" ] [ HE.text "tender" ]
    ]

-- | Badge with the number of matches so far, opens the list of them
matchBadge :: forall a. Int -> a -> Html a
matchBadge n open =
  HE.button
    [ HA.class' "flex items-center gap-1 rounded-lg border-2 border-ink bg-white px-3 py-1 font-bold text-brand-600 shadow-pop transition active:translate-y-0.5 active:shadow-none focus:outline-none focus-visible:ring-4 focus-visible:ring-star"
    , HA.onClick open
    , HA.createAttribute "aria-label" (show n <> if n == 1 then " match" else " matches")
    ]
    [ icon Heart "w-4 h-4 fill-current", HE.text (show n) ]

-- | Starts a new session, leaving this one
newSessionButton :: forall a. a -> Html a
newSessionButton start =
  HE.button
    [ HA.class' "flex items-center gap-1 rounded-lg border-2 border-ink bg-white px-3 py-1 font-bold text-ink shadow-pop transition active:translate-y-0.5 active:shadow-none focus:outline-none focus-visible:ring-4 focus-visible:ring-star"
    , HA.onClick start
    , HA.title "Start a new session"
    , HA.createAttribute "aria-label" "Start a new session"
    ]
    [ icon Plus "w-4 h-4", HE.text "New" ]

-- | Full height page with the wordmark at the top, a button for a new
-- | session, and the match count once the user is swiping
page :: forall a. { matches :: Maybe { count :: Int, open :: a }, newSession :: Maybe a } -> Array (Html a) -> Html a
page { matches, newSession } content =
  HE.main [ HA.id "main", HA.class' "mx-auto flex min-h-[100dvh] max-w-md flex-col px-4" ]
    [ HE.header [ HA.class' "flex items-center justify-between py-4" ]
        [ wordmark
        , HE.div [ HA.class' "flex items-center gap-2" ]
            ( maybe [] (pure <<< newSessionButton) newSession
                <> maybe [] (\{ count, open } -> [ matchBadge count open ]) matches
            )
        ]
    , HE.div [ HA.class' "flex flex-1 flex-col pb-6" ] content
    ]

-- | Content centred in the remaining space
centred :: forall a. Array (Html a) -> Html a
centred = HE.div [ HA.class' "flex flex-1 flex-col items-center justify-center gap-6 text-center" ]
