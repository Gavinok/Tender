module Tender.View.Lobby (lobby) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Data.Array (elem, filter, null)
import Tender.App (Lobby)
import Tender.Vibe (Vibe)
import Tender.QR (qrDataUrl)
import Tender.View.Button (Variant(..), button)
import Tender.View.Icon (Icon(..))
import Tender.View.PlacePicker (placePicker, searchingNear)
import Tender.Types (Found, Url)
import Tender.View.Sheet (sheet)

-- | Invite people to the session, then start. Everything fits on a phone:
-- | the setup is one row that opens a sheet, the QR code takes the space
-- | left, and the buttons stay at the bottom.
lobby
  :: forall a
   . Lobby
  -> { share :: a, start :: a, toggle :: String -> a, setup :: a, bigQR :: a, picker :: { open :: a, clear :: a, query :: String -> a, find :: a, choose :: Found -> a, close :: a } }
  -> Html a
lobby l@{ link, copied, vibe } on =
  HE.div [ HA.class' "flex min-h-0 flex-1 flex-col items-center gap-4 text-center" ]
    ( [ HE.div_
          [ HE.h1 [ HA.class' "font-display text-3xl leading-tight" ] [ HE.text "Where should we eat?" ]
          , HE.p [ HA.class' "text-ink/70" ] [ HE.text "Scan or share to invite your group" ]
          ]
      , setupRow vibe on.setup
      -- Tapping it shows it full screen, easier to scan from across a table
      , HE.button
          [ HA.class' "flex min-h-0 flex-1 items-center justify-center"
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
        <> (if l.setup then [ setupSheet l on ] else [])
        <> if l.bigQR then [ bigCode link on.bigQR ] else []
    )

-- | The vibe and where they're eating as one line, opening the sheet to
-- | change them until someone starts swiping
setupRow :: forall a. Vibe -> a -> Html a
setupRow vibe open =
  HE.button
    [ HA.class' "flex min-h-[2.75rem] w-full items-center gap-2 rounded-xl border-2 border-ink bg-white px-3 py-1 text-left text-sm shadow-pop"
    , HA.onClick open
    ]
    [ HE.span [ HA.class' "flex min-w-0 flex-1 flex-col" ]
        [ HE.span [ HA.class' "truncate" ] [ HE.strong_ [ HE.text "Near: " ], HE.text (maybe "your location" _.name vibe.place) ]
        , HE.span [ HA.class' "truncate" ] [ HE.strong_ [ HE.text "Vibe: " ], HE.text (if null chosen then "anything" else joinWith ", " chosen) ]
        ]
    , HE.span [ HA.class' "font-bold underline" ] [ HE.text (if vibe.locked then "See" else "Edit") ]
    ]
  where
  chosen = map _.label (filter (\c -> elem c.id vibe.chips) vibe.choices)

-- | Choosing the vibe and where they're eating
setupSheet
  :: forall a r
   . Lobby
  -> { toggle :: String -> a, setup :: a, picker :: { open :: a, clear :: a, query :: String -> a, find :: a, choose :: Found -> a, close :: a } | r }
  -> Html a
setupSheet { vibe, picker } on =
  sheet { title: "Your session", close: on.setup }
    [ HE.div [ HA.class' "flex flex-col gap-5 text-left" ]
        [ vibeChips vibe on.toggle
        , case picker of
            Just p | not vibe.locked -> placePicker p on.picker
            _ -> searchingNear vibe on.picker
        ]
    ]

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

-- | Chips anyone in the session can toggle until someone starts swiping
vibeChips :: forall a. Vibe -> (String -> a) -> Html a
vibeChips vibe toggle =
  HE.section [ HA.class' "flex w-full flex-col gap-2" ]
    [ HE.h3 [ HA.class' "font-display text-lg" ] [ HE.text "What's the vibe?" ]
    , HE.div [ HA.class' "flex flex-wrap gap-2" ] (map chip vibe.choices)
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
        [ HA.class' $ "min-h-[2.75rem] rounded-lg border-2 border-ink px-3 text-sm font-bold transition disabled:opacity-60 "
            <> if on then "bg-brand-500 text-white shadow-pop" else "bg-white text-ink hover:bg-brand-50"
        , HA.onClick (toggle c.id)
        , HA.disabled vibe.locked
        , HA.createAttribute "aria-pressed" (if on then "true" else "false")
        ]
        [ HE.text c.label ]
