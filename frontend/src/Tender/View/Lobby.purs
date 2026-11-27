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
import Tender.Types (Found, Url)
import Tender.View.Sheet (sheet)

-- | Invite people to the session, then start. Everything fits on a phone:
-- | the vibe chips are one row that scrolls sideways (wrapping on roomy
-- | screens), the QR code takes the space left, and the buttons stay at the
-- | bottom.
lobby
  :: forall a
   . Lobby
  -> { share :: a, start :: a, toggle :: String -> a, bigQR :: a, picker :: { open :: a, clear :: a, query :: String -> a, find :: a, choose :: Found -> a, close :: a } }
  -> Html a
lobby { link, copied, vibe, picker, bigQR } on =
  HE.div [ HA.class' "flex min-h-0 flex-1 flex-col items-center gap-3 text-center" ]
    ( [ HE.div_
          [ HE.h1 [ HA.class' "font-display text-3xl leading-tight" ] [ HE.text "Where should we eat?" ]
          , HE.p [ HA.class' "text-ink/70" ] [ HE.text "Scan or share to invite your group" ]
          ]
      , searchingNear vibe on.picker
      , vibeChips vibe on.toggle
      -- Tapping it shows it full screen, easier to scan from across a table
      , HE.button
          [ HA.class' "flex min-h-0 flex-1 items-center justify-center pb-4"
          , HA.onClick on.bigQR
          , HA.createAttribute "aria-label" "Show the QR code bigger"
          ]
          [ HE.span [ HA.class' "block h-full max-h-72 -rotate-2 rounded-3xl border-[3px] border-ink bg-white p-3 shadow-card" ]
              [ HE.img [ HA.src (qrDataUrl link), HA.alt "QR code to join this session", HA.class' "block aspect-square h-full max-w-full" ] ]
          ]
      , HE.div [ HA.class' "flex w-full flex-col gap-3 pb-[env(safe-area-inset-bottom)]" ]
          [ if copied then button Secondary (Just Check) "Link copied!" on.share
            else button Secondary (Just Share) "Share invite" on.share
          , button Primary (Just Heart) "Start swiping" on.start
          ]
      ]
        <>
          ( case picker of
              Just p | not vibe.locked -> [ sheet { title: "Where are you eating?", close: on.picker.close } [ placePicker p on.picker ] ]
              _ -> []
          )
        <> if bigQR then [ bigCode link on.bigQR ] else []
    )

-- | The QR code as big as the screen allows
bigCode :: forall a. Url -> a -> Html a
bigCode link close =
  HE.div
    [ HA.class' "fixed inset-0 z-10 grid place-items-center bg-white p-4"
    , HA.createAttribute "role" "dialog"
    , HA.createAttribute "aria-modal" "true"
    , HA.createAttribute "aria-label" "QR code"
    ]
    [ HE.button [ HA.class' "flex h-full w-full flex-col items-center justify-center gap-4", HA.onClick close, HA.createAttribute "data-autofocus" "" ]
        [ HE.img [ HA.src (qrDataUrl link), HA.alt "QR code to join this session", HA.class' "aspect-square w-full max-w-lg" ]
        , HE.span [ HA.class' "font-bold text-ink/70" ] [ HE.text "Tap to close" ]
        ]
    ]

-- | Chips anyone in the session can toggle until someone starts swiping,
-- | in a row that scrolls sideways to the screen's edges, so the cut-off
-- | chip shows there are more. Roomy screens show them all.
vibeChips :: forall a. Vibe -> (String -> a) -> Html a
vibeChips vibe toggle =
  HE.section [ HA.class' "-mx-4 flex w-[calc(100%+2rem)] flex-col gap-1 roomy:mx-0 roomy:w-full", HA.createAttribute "aria-label" "Vibe" ]
    [ HE.p [ HA.class' "text-sm text-ink/70" ]
        [ HE.text
            if vibe.locked then "Locked in: someone has started swiping."
            else "Pick a vibe. Everyone sees the same picks."
        ]
    -- Padded so the chips' taller tap areas aren't clipped by the scrolling row
    , HE.div [ HA.class' "flex gap-2 overflow-x-auto px-4 py-1 roomy:flex-wrap roomy:justify-center roomy:overflow-visible roomy:px-0" ] (map chip vibe.choices)
    ]
  where
  chip c =
    let
      on = elem c.id vibe.chips
    in
      HE.button
        -- 36px to look at, with a 44px tap area that just meets the next row's
        [ HA.class' $ "relative h-9 shrink-0 whitespace-nowrap rounded-lg border-2 border-ink px-3 text-sm font-bold transition before:absolute before:-inset-y-1 before:inset-x-0 before:content-[''] disabled:opacity-60 "
            <> if on then "bg-brand-500 text-white shadow-pop" else "bg-white text-ink hover:bg-brand-50"
        , HA.onClick (toggle c.id)
        , HA.disabled vibe.locked
        , HA.createAttribute "aria-pressed" (if on then "true" else "false")
        ]
        [ HE.text c.label ]
