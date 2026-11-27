module Tender.View.Match (announcement, celebration, matchesSheet) where

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

-- | Full screen "it's a match" over everything else
celebration :: forall a. Restaurant -> a -> Html a
celebration r continue =
  HE.div
    [ HA.class' "fixed inset-0 z-20 flex flex-col items-center justify-center gap-6 overflow-hidden bg-gradient-to-b from-brand-400 to-brand-600 px-6 text-center text-white"
    , HA.createAttribute "role" "dialog"
    , HA.createAttribute "aria-modal" "true"
    , HA.createAttribute "aria-label" "It's a match"
    ]
    [ confetti
    , HE.p [ HA.class' "animate-pop text-6xl font-display drop-shadow-lg" ] [ HE.text "It's a match!" ]
    , HE.p [ HA.class' "text-lg font-bold text-white/90" ] [ HE.text "Everyone's hungry for" ]
    -- The tilt is on a wrapper since the entrance animation ends at no transform
    , HE.div [ HA.class' "rotate-2" ]
        [ HE.div [ HA.class' "w-64 animate-card-in rounded-3xl border-[3px] border-ink bg-white p-3 text-ink shadow-card" ]
            [ restaurantPhoto r 0 "aspect-square w-full rounded-2xl"
            , HE.p [ HA.class' "pt-3 text-2xl font-display" ] [ HE.text r.name ]
            , photoCredit r 0 "text-ink/50"
            ]
        ]
    , HE.div [ HA.class' "flex flex-col items-center gap-3" ]
        [ maybe (HE.text "") (linkButton Secondary (Just ExternalLink) "Let's go") r.url
        , linkButton OnColour (Just ExternalLink) "See the menu" (menuUrl r)
        , HE.div [ HA.class' "contents", HA.createAttribute "data-autofocus" "" ] [ button OnColour Nothing "Keep swiping" continue ]
        ]
    ]

-- | Matches so far, sliding up from the bottom
matchesSheet :: forall a. Array Restaurant -> a -> Html a
matchesSheet matches close =
  HE.div [ HA.class' "fixed inset-0 z-10 flex flex-col justify-end" ]
    [ HE.button
        [ HA.class' "absolute inset-0 bg-ink/40"
        , HA.onClick close
        , HA.createAttribute "aria-label" "Close matches"
        ]
        []
    , HE.section
        [ HA.class' "relative mx-auto max-h-[80dvh] w-full max-w-md animate-card-in overflow-y-auto rounded-t-[2rem] border-x-[3px] border-t-[3px] border-ink bg-surface p-5 pb-8 shadow-2xl"
        , HA.createAttribute "role" "dialog"
        , HA.createAttribute "aria-modal" "true"
        , HA.createAttribute "aria-label" "Matches"
        ]
        [ HE.div [ HA.class' "mb-4 flex items-center justify-between" ]
            [ HE.h2 [ HA.class' "text-2xl font-display" ] [ HE.text (title (length matches)) ]
            , HE.button [ HA.class' "rounded-lg p-2 text-ink/60 hover:bg-ink/5", HA.onClick close, HA.createAttribute "aria-label" "Close", HA.createAttribute "data-autofocus" "" ]
                [ icon Close "h-6 w-6" ]
            ]
        , if length matches == 0 then
            HE.p [ HA.class' "text-ink/70" ] [ HE.text "Nothing yet. When someone else likes a restaurant you liked, it shows up here." ]
          else matchList matches
        ]
    ]
  where
  title 1 = "1 match"
  title n = show n <> " matches"
