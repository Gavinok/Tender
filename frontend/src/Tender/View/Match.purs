module Tender.View.Match (celebration, matchesSheet) where

import Prelude

import Data.Array (length, range)
import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.Types (Restaurant)
import Tender.View.Button (Variant(..), button, linkButton)
import Tender.View.Icon (Icon(..), icon)
import Tender.View.States (matchList)

-- | Pieces of confetti falling across the screen. Positions are spread with
-- | a fixed step rather than randomly so the view stays a pure function.
confetti :: forall a. Html a
confetti =
  HE.div [ HA.class' "pointer-events-none fixed inset-0 overflow-hidden", HA.createAttribute "aria-hidden" "true" ]
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

-- | Full screen "it's a match" over everything else
celebration :: forall a. Restaurant -> a -> Html a
celebration r continue =
  HE.div
    [ HA.class' "fixed inset-0 z-20 flex flex-col items-center justify-center gap-6 overflow-hidden bg-gradient-to-b from-brand-400 to-brand-600 px-6 text-center text-white"
    , HA.createAttribute "role" "dialog"
    , HA.createAttribute "aria-label" "It's a match"
    ]
    [ confetti
    , HE.p [ HA.class' "animate-pop text-6xl font-display drop-shadow-lg" ] [ HE.text "It's a match!" ]
    , HE.p [ HA.class' "text-lg font-bold text-white/90" ] [ HE.text "Everyone's hungry for" ]
    -- The tilt is on a wrapper since the entrance animation ends at no transform
    , HE.div [ HA.class' "rotate-2" ]
        [ HE.div [ HA.class' "w-64 animate-card-in rounded-3xl border-[3px] border-ink bg-white p-3 text-ink shadow-card" ]
            [ HE.img [ HA.src r.imageLink, HA.alt "", HA.class' "aspect-square w-full rounded-2xl object-cover" ]
            , HE.p [ HA.class' "pt-3 text-2xl font-display" ] [ HE.text r.name ]
            ]
        ]
    , HE.div [ HA.class' "flex flex-col items-center gap-3" ]
        [ linkButton Secondary (Just ExternalLink) "Let's go" r.url
        , button OnColour Nothing "Keep swiping" continue
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
        , HA.createAttribute "aria-label" "Matches"
        ]
        [ HE.div [ HA.class' "mb-4 flex items-center justify-between" ]
            [ HE.h2 [ HA.class' "text-2xl font-display" ] [ HE.text (title (length matches)) ]
            , HE.button [ HA.class' "rounded-lg p-2 text-ink/60 hover:bg-ink/5", HA.onClick close, HA.createAttribute "aria-label" "Close" ]
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
