module Tender.View.Card (restaurantCard, ratingStars, actionBar) where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.Types (Restaurant)
import Tender.View.Button (iconButton)
import Tender.View.Icon (Icon(..), icon)
import Tender.View.Photo (mapsLink, photoCredit, restaurantPhoto)

-- | Five stars, filled to the nearest half
ratingStars :: forall a. Number -> Html a
ratingStars rating =
  HE.div
    [ HA.class' "flex items-center gap-0.5"
    , HA.createAttribute "aria-label" ("Rated " <> show rating <> " out of 5")
    , HA.createAttribute "role" "img"
    ]
    (map star (range 1 5))
  where
  star i
    | rating >= toNumber i = filled "w-5 h-5"
    | rating >= toNumber i - 0.5 =
        HE.span [ HA.class' "relative h-5 w-5" ]
          [ empty
          , HE.span [ HA.class' "absolute inset-0 w-1/2 overflow-hidden" ] [ filled "w-5 h-5 max-w-none" ]
          ]
    | otherwise = empty
  filled classes = icon Star (classes <> " fill-star text-star")
  empty = icon Star "w-5 h-5 text-white/50"

chip :: forall a. String -> Html a
chip text =
  HE.span [ HA.class' "rounded-lg bg-white/20 px-3 py-1 text-sm font-bold text-white backdrop-blur" ]
    [ HE.text text ]

stamp :: forall a. String -> String -> String -> Html a
stamp text classes opacity =
  HE.span
    [ HA.class' ("absolute top-16 rounded-xl border-4 px-3 py-1 text-4xl font-display tracking-wider " <> classes)
    , HA.style { opacity }
    , HA.createAttribute "aria-hidden" "true"
    ]
    [ HE.text text ]

-- | The restaurant's photo with its details over the bottom of it. Keyed on
-- | the restaurant so each new one animates in.
restaurantCard :: forall a. Restaurant -> Html a
restaurantCard r =
  HE.article
    [ HA.key (show r.id)
    , HA.createAttribute "data-swipe" ""
    , HA.class' "relative min-h-[22rem] w-full flex-1 animate-card-in cursor-grab touch-none select-none overflow-hidden rounded-[2rem] border-[3px] border-ink bg-brand-200 shadow-card active:cursor-grabbing"
    ]
    [ restaurantPhoto r "pointer-events-none absolute inset-0 h-full w-full"
    , HE.div [ HA.class' "absolute inset-0 bg-gradient-to-t from-black/80 via-black/20 to-transparent" ] []
    -- Fade in while dragging, see Swipe.js
    , stamp "YUM" "left-5 -rotate-12 border-star text-star" "var(--like, 0)"
    , stamp "NOPE" "right-5 rotate-12 border-white text-white" "var(--nope, 0)"
    , mapsLink r "absolute right-4 top-4 rounded-lg bg-white/90 px-3 py-1 text-sm text-[#1F1F1F] shadow"
    , HE.div [ HA.class' "absolute inset-x-0 bottom-0 flex flex-col gap-2 p-5 text-left" ]
        [ HE.h2 [ HA.class' "font-display text-3xl leading-tight text-white drop-shadow" ] [ HE.text r.name ]
        , HE.div [ HA.class' "flex items-center gap-2" ]
            (maybe [] (pure <<< ratingStars) r.rating <> maybe [] (pure <<< chip) r.price)
        , HE.div [ HA.class' "flex flex-wrap gap-2" ] (maybe [] (pure <<< chip) r.category)
        , photoCredit r "text-white/70"
        ]
    ]

-- | Nope and like, sized for thumbs
actionBar :: forall a. { nope :: a, like :: a } -> Html a
actionBar { nope, like } =
  HE.div [ HA.class' "flex items-center justify-center gap-8 py-2" ]
    [ iconButton { label: "Nope", icon: Close, classes: "h-16 w-16 bg-white text-ink/50 hover:text-ink" } nope
    , iconButton { label: "Like", icon: Heart, classes: "h-20 w-20 bg-brand-500 text-white [&_svg]:fill-current" } like
    ]
