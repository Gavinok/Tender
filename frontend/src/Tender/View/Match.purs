module Tender.View.Match (announcement, toast, celebration, matchesSheet) where

import Prelude

import Data.Array (length, range)
import Data.Maybe (Maybe(..), maybe)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.Types (Restaurant)
import Tender.View.Button (Variant(..), button, linkButton)
import Tender.View.Icon (Icon(..), icon)
import Tender.View.Photo (menuUrl, photoCredit, restaurantPhoto)
import Tender.View.Sheet (sheet)
import Tender.View.States (matchList)

-- | Pieces of confetti falling across the screen. Positions are spread with
-- | a fixed step rather than randomly so the view stays a pure function.
-- | Left out for people who ask for reduced motion: shortening the animation
-- | would leave the pieces sitting still while they wait for their delay.
confetti :: forall a. Html a
confetti =
  HE.div [ HA.class' "pointer-events-none fixed inset-0 overflow-hidden motion-reduce:hidden", HA.createAttribute "aria-hidden" "true" ]
    (map piece (range 0 27))
  where
  piece i =
    HE.span
      [ HA.class' "absolute -top-4 block h-3 w-2 animate-confetti rounded-sm"
      , HA.style
          { left: show ((i * 37) `mod` 100) <> "%"
          , background: colour i
          , "animation-delay": show (((i * 53) `mod` 100) * 10) <> "ms"
          , "animation-duration": show (2200 + (i `mod` 5) * 300) <> "ms"
          }
      ]
      []
  colour i = case i `mod` 5 of
    0 -> "#ffc94d"
    1 -> "#ffffff"
    2 -> "#ffa089"
    3 -> "#7dd3fc"
    _ -> "#86efac"

-- | Read out by screen readers when a match comes up. The region has to be
-- | on the page before its text changes, so it's always there and empty
-- | until then.
announcement :: forall a. Maybe Restaurant -> Html a
announcement matched =
  HE.p [ HA.class' "sr-only", HA.createAttribute "role" "status", HA.createAttribute "aria-live" "polite" ]
    [ HE.text (maybe "" (\r -> "It's a match! Everyone's hungry for " <> r.name) matched) ]

-- | A match after the first, over the cards so swiping carries on, for a
-- | few seconds. It drops in from the top and slides back up when it goes.
-- | Tapping it opens the list of matches.
toast :: forall a. { match :: Restaurant, leaving :: Boolean } -> { open :: a, dismiss :: a } -> Html a
toast { match: r, leaving } { open, dismiss } =
  HE.div
    -- Keyed so the next match drops in again rather than reusing this one
    [ HA.key (show r.id)
    , HA.class' ("fixed inset-x-4 top-4 z-10 mx-auto flex max-w-md items-center rounded-xl border-2 border-ink bg-star font-bold shadow-pop " <> if leaving then "animate-toast-out" else "animate-toast-in")
    , HA.createAttribute "role" "status"
    ]
    [ HE.button [ HA.type' "button", HA.class' "min-w-0 flex-1 truncate py-3 pl-4 text-left", HA.onClick open ]
        [ HE.text ("It's a match: " <> r.name) ]
    , HE.button [ HA.type' "button", HA.class' "grid h-11 w-11 shrink-0 place-items-center", HA.onClick dismiss, HA.createAttribute "aria-label" "Dismiss" ]
        [ icon Close "h-5 w-5" ]
    ]

-- | Full screen "it's a match" over everything else
celebration :: forall a. Restaurant -> a -> Html a
celebration r continue =
  -- Scrolls if it must, and is centred only when it fits (m-auto below):
  -- centring with justify-center pushed the heading off the top, out of
  -- reach, on a phone with Safari's bars showing
  HE.div
    [ HA.class' "fixed inset-0 z-20 flex overflow-y-auto overflow-x-hidden bg-gradient-to-b from-brand-400 to-brand-600 px-6 text-center text-white"
    , HA.createAttribute "role" "dialog"
    , HA.createAttribute "aria-modal" "true"
    , HA.createAttribute "aria-label" "It's a match"
    ]
    [ confetti
    , HE.div [ HA.class' "m-auto flex flex-col items-center gap-5 py-6 pb-[max(1.5rem,env(safe-area-inset-bottom))]" ]
        -- One line, as big as the width allows
        [ HE.p [ HA.class' "animate-pop whitespace-nowrap text-[min(3.75rem,13vw)] leading-none font-display drop-shadow-lg" ] [ HE.text "It's a match!" ]
        , HE.p [ HA.class' "text-lg font-bold text-white/90" ] [ HE.text "Everyone's hungry for" ]
        -- The tilt is on a wrapper since the entrance animation ends at no transform
        , HE.div [ HA.class' "rotate-2" ]
            [ HE.div [ HA.class' "w-64 animate-card-in rounded-3xl border-[3px] border-ink bg-white p-3 text-ink shadow-card" ]
                -- Shorter on short screens, so it all fits without scrolling
                [ restaurantPhoto r 0 "aspect-[4/3] w-full rounded-2xl tall:aspect-square"
                , HE.p [ HA.class' "line-clamp-2 pt-3 text-xl leading-tight font-display" ] [ HE.text r.name ]
                , photoCredit r 0 "text-ink/70"
                ]
            ]
        , HE.div [ HA.class' "flex flex-col items-center gap-3" ]
            [ HE.div [ HA.class' "flex flex-wrap justify-center gap-3" ]
                [ maybe (HE.text "") (linkButton Secondary (Just ExternalLink) "Let's go") r.url
                , linkButton OnColour (Just ExternalLink) "See the menu" (menuUrl r)
                ]
            , HE.div [ HA.class' "contents", HA.createAttribute "data-autofocus" "" ] [ button OnColour Nothing "Keep swiping" continue ]
            ]
        ]
    ]

-- | Matches so far, sliding up from the bottom
matchesSheet :: forall a. Array Restaurant -> a -> Html a
matchesSheet matches close =
  sheet { title: title (length matches), close }
    [ if length matches == 0 then
        HE.p [ HA.class' "text-ink/70" ] [ HE.text "Nothing yet. When someone else likes a restaurant you liked, it shows up here." ]
      else matchList matches
    ]
  where
  title 1 = "1 match"
  title n = show n <> " matches"
