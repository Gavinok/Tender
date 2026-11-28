module Tender.View.Lobby (lobby) where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), contains, toLower, trim)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Data.Array (elem, filter, null)
import Tender.App (Lobby)
import Tender.QR (qrDataUrl)
import Tender.View.Button (Variant(..), button)
import Tender.View.Icon (Icon(..), icon)
import Tender.View.PlacePicker (placeSearch, searchingNear)
import Tender.Types (Found, Url)
import Tender.View.Sheet (sheet)

-- | Invite people to the session, then start. Everything fits on a phone:
-- | the vibe is one box whose suggestions open on tap, the QR code takes
-- | the space left, and the buttons stay at the bottom.
lobby
  :: forall a
   . Lobby
  -> { share :: a, start :: a, bigQR :: a, vibe :: { toggle :: String -> a, pick :: String -> a, open :: a, close :: a, draft :: String -> a, save :: a }, picker :: { open :: a, clear :: a, query :: String -> a, find :: a, choose :: Found -> a, close :: a } }
  -> Html a
lobby l@{ link, copied, vibe, picker, bigQR, placeChanged } on =
  HE.div [ HA.class' "flex min-h-0 flex-1 flex-col items-center gap-3 text-center" ]
    ( [ HE.div_
          [ HE.h1 [ HA.class' "font-display text-3xl leading-tight" ] [ HE.text "Where should we eat?" ]
          , HE.p [ HA.class' "text-ink/70" ] [ HE.text "Scan or share to invite your group" ]
          ]
      , searchingNear { place: vibe.place, locked: vibe.locked, changed: placeChanged } on.picker
      , vibeBox l on.vibe
      -- Tapping it shows it full screen, easier to scan from across a table.
      -- With a mouse a short window can scroll, so it keeps a scannable size.
      -- The code is placed in the space left rather than sized from its
      -- contents: iOS Safari sized it from the image's natural width, which
      -- an SVG doesn't have, and squashed it into a thin bar. Its bottom
      -- 1rem is left for the card's tilt and shadow.
      , HE.div [ HA.class' "relative min-h-0 w-full flex-1 mouse:min-h-[14rem]" ]
          [ HE.button
              [ HA.class' "absolute left-1/2 top-[calc(50%-0.5rem)] aspect-square h-[min(calc(100%-1rem),18rem)] -translate-x-1/2 -translate-y-1/2 -rotate-2 rounded-3xl border-[3px] border-ink bg-white p-3 shadow-card"
              , HA.onClick on.bigQR
              , HA.createAttribute "aria-label" "Show the QR code bigger"
              ]
              [ HE.img [ HA.src (qrDataUrl link), HA.alt "QR code to join this session", HA.class' "block h-full w-full" ] ]
          ]
      , HE.div [ HA.class' "flex w-full flex-col gap-3 pb-[env(safe-area-inset-bottom)]" ]
          [ if copied then button Secondary (Just Check) "Link copied!" on.share
            else button Secondary (Just Share) "Share invite" on.share
          , button Primary (Just Heart) "Start swiping" on.start
          ]
      ]
        <>
          ( case picker of
              Just p | not vibe.locked ->
                [ sheet { title: "Where are you eating?", close: on.picker.close }
                    -- Going back to everyone's locations lives here, so the lobby has one link
                    ( [ placeSearch p on.picker ]
                        <> if isJust vibe.place then [ HE.div [ HA.class' "mt-4 border-t-2 border-ink/10 pt-4" ] [ button Secondary (Just MapPin) "Use our locations" on.picker.clear ] ] else []
                    )
                ]
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

-- | What the group is in the mood for: a box with the chosen chips in it as
-- | tokens and a craving typed after them. Tapping in shows the other chips
-- | below as suggestions, filtered by what's typed, as shortcuts for typing.
-- | Closed, the box is one line on short phones and wraps on tall ones, so
-- | every pick shows where there's room. Tall phones also show the
-- | suggestions, as one row that scrolls sideways to the screen's edges,
-- | snapping to chips, and roomy phones (iPhone 12 and up, Pixels) show
-- | them all; short ones open them on tap, so the lobby keeps its room.
-- | Mice, which can't swipe a row, see them all.
vibeBox
  :: forall a
   . Lobby
  -> { toggle :: String -> a, pick :: String -> a, open :: a, close :: a, draft :: String -> a, save :: a }
  -> Html a
vibeBox { vibe, vibeOpen, draft } on =
  HE.section [ HA.class' "-mx-4 flex w-[calc(100%+2rem)] flex-col gap-1 mouse:mx-0 mouse:w-full", HA.createAttribute "aria-label" "Vibe" ]
    [ HE.form [ HA.class' "mx-4 mouse:mx-0", HA.onSubmit on.save ]
        -- One line while closed, scrolling sideways if it's full, so the
        -- lobby keeps its room; it wraps while open, when the keyboard is up
        -- A label for the input, so a tap anywhere in it, such as on the
        -- craving, types. Named with `for`, or it would label the first
        -- chip's remove button instead.
        [ HE.label
            [ HA.id "vibe-box"
            , HA.createAttribute "for" "craving"
            , HA.class' $ "flex min-h-[2.75rem] cursor-text items-center gap-1.5 rounded-xl border-2 border-ink bg-white px-2 py-1.5 text-left shadow-pop focus-within:ring-4 focus-within:ring-star "
                <> if vibeOpen then "flex-wrap" else "overflow-x-auto [scrollbar-width:none] [&::-webkit-scrollbar]:hidden tall:flex-wrap tall:overflow-visible"
            ]
            ([ cravingToken ] <> map token chosen <> [ input ])
        ]
    , if vibe.locked then HE.p [ HA.class' "px-4 text-sm text-ink/70 mouse:px-0" ] [ HE.text "Locked in: someone has started swiping." ]
      else HE.text ""
    , if vibe.locked then HE.text ""
      else
        -- Padded so the chips' taller tap areas and shadows aren't clipped by
        -- the scrolling row
        HE.div
          [ HA.class' $ "snap-x snap-mandatory scroll-px-4 gap-2 overflow-x-auto px-4 pb-2 pt-1 [scrollbar-width:none] [&::-webkit-scrollbar]:hidden roomy:flex-wrap roomy:justify-center roomy:overflow-visible mouse:flex-wrap mouse:justify-center mouse:overflow-visible mouse:px-0 "
              <> if vibeOpen then "flex" else "hidden tall:flex mouse:flex"
          , HA.createAttribute "aria-label" "Suggestions"
          ]
          (map suggestion suggested <> if vibeOpen then [ done ] else [])
    ]
  where
  chosen = filter (\c -> elem c.id vibe.chips) vibe.choices
  typed = toLower (trim draft)
  suggested = filter (\c -> not (elem c.id vibe.chips) && (not vibeOpen || contains (Pattern typed) (toLower c.label))) vibe.choices
  -- 32px to look at, with a 44px tap area
  tapArea = "relative shrink-0 before:absolute before:-inset-y-1.5 before:inset-x-0 before:content-[''] "
  token c =
    HE.button
      [ HA.key c.id
      , HA.type' "button"
      , HA.class' (tapArea <> "flex h-8 items-center gap-1 rounded-lg border-2 border-ink bg-brand-500 px-2 text-sm font-bold text-white disabled:opacity-80")
      , HA.onClick (on.toggle c.id)
      , HA.disabled vibe.locked
      , HA.createAttribute "aria-label" ("Remove " <> c.label)
      ]
      ([ HE.text c.label ] <> if vibe.locked then [] else [ icon Close "h-3.5 w-3.5" ])
  -- While closed, the craving leads, quoted, so a full box doesn't hide it.
  -- Everything in the box is keyed, so adding or removing a chip doesn't
  -- rebuild the input and lose what's being typed or its focus.
  cravingToken = case vibe.craving of
    Just c | not vibeOpen -> HE.span [ HA.key "craving", HA.class' "shrink-0 whitespace-nowrap rounded-lg border-2 border-ink px-2 py-0.5 text-sm font-bold" ] [ HE.text ("“" <> c <> "”") ]
    _ -> HE.span [ HA.key "craving", HA.class' "hidden" ] []
  input =
    HE.input
      [ HA.key "input"
      , HA.type' "text"
      , HA.id "craving"
      , HA.class' "h-8 min-w-[8rem] flex-1 bg-transparent px-1 text-sm outline-none placeholder:text-ink/50 disabled:text-ink/70"
      , HA.placeholder (if null chosen && not (isJust vibe.craving) then "What are you in the mood for?" else "Anything else?")
      , HA.value (if vibeOpen then draft else "")
      , HA.onFocus on.open
      , HA.onInput on.draft
      , HA.maxlength 60
      , HA.autocomplete "off"
      , HA.disabled vibe.locked
      , HA.createAttribute "enterkeyhint" "done"
      , HA.createAttribute "aria-label" "What you're in the mood for"
      ]
  -- 36px to look at, with a 44px tap area that just meets the next row's
  suggestion c =
    HE.button
      [ HA.type' "button"
      , HA.class' "relative h-9 shrink-0 snap-start whitespace-nowrap rounded-lg border-2 border-ink bg-white px-3 text-sm font-bold text-ink transition before:absolute before:-inset-y-1 before:inset-x-0 before:content-[''] hover:bg-brand-50"
      , HA.onClick (on.pick c.id)
      ]
      [ HE.text c.label ]
  done =
    HE.button
      [ HA.type' "button", HA.class' "relative h-9 shrink-0 snap-start px-2 text-sm font-bold underline before:absolute before:-inset-y-1 before:inset-x-0 before:content-['']", HA.onClick on.close ]
      [ HE.text "Done" ]
