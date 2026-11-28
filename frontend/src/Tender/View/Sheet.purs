-- | A sheet that slides up over the screen, for what doesn't fit on it.
-- | Tapping outside, the close button and Escape (handled in App) close it,
-- | and Dialog.js keeps focus inside while it's open.
module Tender.View.Sheet (sheet) where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.View.Icon (Icon(..), icon)

sheet :: forall a. { title :: String, close :: a } -> Array (Html a) -> Html a
sheet { title, close } body =
  HE.div [ HA.class' "fixed inset-0 z-10 flex flex-col justify-end" ]
    [ HE.button
        [ HA.class' "absolute inset-0 bg-ink/40"
        , HA.onClick close
        , HA.createAttribute "aria-label" ("Close " <> title)
        , HA.createAttribute "tabindex" "-1"
        ]
        []
    , HE.section
        [ HA.class' "relative mx-auto max-h-[85dvh] w-full max-w-md animate-sheet-up overflow-y-auto rounded-t-[2rem] border-x-[3px] border-t-[3px] border-ink bg-surface p-5 pb-8 shadow-2xl"
        , HA.createAttribute "role" "dialog"
        , HA.createAttribute "aria-modal" "true"
        , HA.createAttribute "aria-label" title
        ]
        -- The close button stays top right beside a title of several lines
        [ HE.div [ HA.class' "mb-4 flex items-start justify-between gap-2" ]
            [ HE.h2 [ HA.class' "text-2xl font-display" ] [ HE.text title ]
            , HE.button
                [ HA.class' "grid h-11 w-11 place-items-center rounded-lg text-ink/60 hover:bg-ink/5"
                , HA.onClick close
                , HA.createAttribute "aria-label" "Close"
                , HA.createAttribute "data-autofocus" ""
                ]
                [ icon Close "h-6 w-6" ]
            ]
        , HE.div_ body
        ]
    ]
