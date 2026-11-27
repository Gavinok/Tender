module Tender.View.Lobby (lobby) where

import Prelude

import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Data.Array (elem)
import Tender.App (Lobby)
import Tender.Vibe (Vibe)
import Tender.QR (qrDataUrl)
import Tender.View.Button (Variant(..), button)
import Tender.View.Icon (Icon(..))
import Tender.View.PlacePicker (placePicker, searchingNear)
import Tender.Types (Found)

-- | Invite people to the session, then start
lobby
  :: forall a
   . Lobby
  -> { share :: a, start :: a, toggle :: String -> a, picker :: { open :: a, clear :: a, query :: String -> a, find :: a, choose :: Found -> a, close :: a } }
  -> Html a
lobby { link, copied, vibe, picker } { share, start, toggle, picker: on } =
  HE.div [ HA.class' "flex flex-1 flex-col items-center justify-center gap-6 text-center" ]
    [ HE.h1 [ HA.class' "font-display text-5xl leading-none" ] [ HE.text "Where should we eat?" ]
    , HE.p [ HA.class' "max-w-xs text-lg text-ink/70" ]
        [ HE.text "Get everyone to scan this, then start swiping. Anything you all like is a match." ]
    -- Before the QR code, so it's seen without scrolling on a phone
    , case picker of
        Just p | not vibe.locked -> placePicker p on
        _ -> searchingNear vibe on
    , HE.figure [ HA.class' "-rotate-2 rounded-3xl border-[3px] border-ink bg-white p-4 shadow-card" ]
        [ HE.img [ HA.src (qrDataUrl link), HA.alt "QR code to join this session", HA.class' "block h-44 w-44" ]
        , HE.p [ HA.class' "w-44 truncate pt-2 font-mono text-xs text-ink/70", HA.title link ] [ HE.text link ]
        ]
    , vibeChips vibe toggle
    , HE.div [ HA.class' "flex flex-col items-center gap-3" ]
        [ if copied then button Secondary (Just Check) "Link copied!" share
          else button Secondary (Just Share) "Share invite" share
        , button Primary (Just Heart) "Start swiping" start
        ]
    ]

-- | Chips anyone in the session can toggle until someone starts swiping
vibeChips :: forall a. Vibe -> (String -> a) -> Html a
vibeChips vibe toggle =
  HE.section [ HA.class' "flex w-full flex-col items-center gap-2" ]
    [ HE.h2 [ HA.class' "font-display text-2xl" ] [ HE.text "What's the vibe?" ]
    , HE.div [ HA.class' "flex flex-wrap justify-center gap-2" ] (map chip vibe.choices)
    , HE.p [ HA.class' "text-sm text-ink/70" ]
        [ HE.text
            if vibe.locked then "Locked in: someone has started swiping."
            else "Everyone in the session sees the same picks."
        ]
    ]
  where
  chip c =
    let
      on = elem c.id vibe.chips
    in
      HE.button
        [ HA.class' $ "rounded-lg border-2 border-ink px-3 py-1.5 text-sm font-bold transition disabled:opacity-60 "
            <> if on then "bg-brand-500 text-white shadow-pop" else "bg-white text-ink hover:bg-brand-50"
        , HA.onClick (toggle c.id)
        , HA.disabled vibe.locked
        , HA.createAttribute "aria-pressed" (if on then "true" else "false")
        ]
        [ HE.text c.label ]
