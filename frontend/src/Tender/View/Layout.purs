module Tender.View.Layout (page, centred) where

import Prelude

import Data.Foldable (foldMap)

import Data.Maybe (Maybe)
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
    [ HA.class' "flex min-h-[2.75rem] items-center gap-1 rounded-lg border-2 border-ink bg-white px-3 font-bold text-brand-600 shadow-pop transition active:translate-y-0.5 active:shadow-none focus:outline-none focus-visible:ring-4 focus-visible:ring-star"
    , HA.onClick open
    , HA.createAttribute "aria-label" (show n <> if n == 1 then " match" else " matches")
    ]
    [ icon Heart "w-4 h-4 fill-current", HE.text (show n) ]

-- | Starts a new session, leaving this one
newSessionButton :: forall a. a -> Html a
newSessionButton start =
  HE.button
    [ HA.class' "flex min-h-[2.75rem] items-center gap-1 rounded-lg border-2 border-ink bg-white px-3 font-bold text-ink shadow-pop transition active:translate-y-0.5 active:shadow-none focus:outline-none focus-visible:ring-4 focus-visible:ring-star"
    , HA.onClick start
    , HA.title "Start a new session"
    , HA.createAttribute "aria-label" "Start a new session"
    ]
    [ icon Plus "w-4 h-4", HE.text "New" ]

-- | Full height page with the wordmark at the top, and either a button for
-- | a new session or, once swiping, who's here and the match count. With
-- | `fill` it's exactly the screen's height, so the content can share out
-- | what's left, as the lobby and swiping do.
page :: forall a. { matches :: Maybe { count :: Int, open :: a }, newSession :: Maybe a, session :: Maybe (Html a), fill :: Boolean } -> Array (Html a) -> Html a
page { matches, newSession, session, fill } content =
  HE.main [ HA.id "main", HA.class' ("mx-auto flex max-w-md flex-col px-4 " <> if fill then "h-[100dvh]" else "min-h-[100dvh]") ]
    [ HE.header [ HA.class' "flex items-center justify-between gap-2 py-3" ]
        [ wordmark
        , HE.div [ HA.class' "flex items-center gap-2" ]
            ( foldMap (pure <<< newSessionButton) newSession
                <> foldMap pure session
                <> foldMap (\{ count, open } -> [ matchBadge count open ]) matches
            )
        ]
    , HE.div [ HA.class' ("flex flex-1 flex-col " <> if fill then "min-h-0 pb-3" else "pb-6") ] content
    ]

-- | Content centred in the remaining space
centred :: forall a. Array (Html a) -> Html a
centred = HE.div [ HA.class' "flex flex-1 flex-col items-center justify-center gap-6 text-center" ]
