module Tender.View.Card (restaurantCard, ratingStars, actionBar) where

import Prelude

import Data.Array (elem, filter, length, range)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.Types (Restaurant, Review)
import Tender.Vibe (Chip)
import Tender.View.Button (iconButton)
import Tender.View.Icon (Icon(..), icon)
import Tender.View.Photo (mapsLink, menuLink, photoCredit, restaurantPhoto)

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

-- | The whole summary over the lower part of the card. Dragging inside it
-- | doesn't swipe the card (see Swipe.js).
summaryBox :: forall a. { text :: String, disclosure :: Maybe String } -> Array Review -> a -> Html a
summaryBox s reviews close =
  HE.section
    [ HA.createAttribute "data-no-swipe" ""
    , HA.class' "absolute inset-x-0 bottom-0 flex max-h-[70%] animate-card-in flex-col gap-3 overflow-y-auto rounded-t-2xl border-t-[3px] border-ink bg-surface p-5 text-left text-ink touch-pan-y"
    ]
    [ HE.div [ HA.class' "flex items-center justify-between" ]
        [ HE.h3 [ HA.class' "font-display text-xl" ] [ HE.text "The lowdown" ]
        , HE.button [ HA.class' "rounded-lg p-1 text-ink/60 hover:bg-ink/5", HA.onClick close, HA.createAttribute "aria-label" "Close summary" ]
            [ icon Close "h-5 w-5" ]
        ]
    , HE.p [ HA.class' "leading-relaxed" ] [ HE.text s.text ]
    , case s.disclosure of
        Just d -> HE.p [ HA.class' "text-xs italic text-ink/50" ] [ HE.text d ]
        Nothing -> HE.text ""
    , reviewList reviews
    ]

-- | What people wrote, with who wrote it as Google requires
reviewList :: forall a. Array Review -> Html a
reviewList [] = HE.text ""
reviewList reviews =
  HE.div [ HA.class' "flex flex-col gap-3 border-t-2 border-ink/10 pt-3" ]
    ( [ HE.h4 [ HA.class' "font-display text-lg" ] [ HE.text "What people said" ] ]
        <> map review reviews
    )
  where
  review r =
    HE.figure [ HA.class' "flex flex-col gap-1" ]
      [ HE.blockquote [ HA.class' "text-sm leading-relaxed" ] [ HE.text r.text ]
      , HE.p [ HA.class' "text-xs text-ink/60" ]
          ( [ case r.author.uri of
                Just uri -> HE.a [ HA.href uri, HA.target "_blank", HA.rel "noopener", HA.class' "font-bold underline" ] [ HE.text r.author.name ]
                Nothing -> HE.span [ HA.class' "font-bold" ] [ HE.text r.author.name ]
            ]
              <> maybe [] (\n -> [ HE.text (" · " <> show n <> " out of 5") ]) r.rating
              <> maybe [] (\w -> [ HE.text (" · " <> w) ]) r.when
          )
      ]

-- | A bar per photo across the top of the card, the one showing filled in.
-- | Tapping the left or right of the card moves between them (see Swipe.js).
photoBars :: forall a. Int -> Int -> Html a
photoBars count showing
  | count < 2 = HE.text ""
  | otherwise =
      HE.div [ HA.class' "absolute inset-x-3 top-2 flex gap-1", HA.createAttribute "aria-label" ("Photo " <> show (showing + 1) <> " of " <> show count) ]
        (map bar (range 0 (count - 1)))
      where
      bar i = HE.span [ HA.class' ("h-1 flex-1 rounded-full " <> if i == showing then "bg-white" else "bg-white/40") ] []

-- | The restaurant's photo with its details over the bottom of it, and a
-- | badge when someone else in the group already liked it. Keyed on the
-- | restaurant so each new one animates in.
restaurantCard :: forall a. { likedByOthers :: Boolean, vibe :: Array Chip, photo :: Int, summaryOpen :: Boolean, toggleSummary :: a } -> Restaurant -> Html a
restaurantCard { likedByOthers, vibe, photo, summaryOpen, toggleSummary } r =
  HE.article
    [ HA.key (show r.id)
    , HA.createAttribute "data-swipe" ""
    , HA.class' "relative min-h-[22rem] w-full flex-1 animate-card-in cursor-grab touch-none select-none overflow-hidden rounded-[2rem] border-[3px] border-ink bg-brand-200 shadow-card active:cursor-grabbing"
    ]
    [ restaurantPhoto r photo "pointer-events-none absolute inset-0 h-full w-full"
    , photoBars (length r.photos) photo
    , HE.div [ HA.class' "absolute inset-0 bg-gradient-to-t from-black/80 via-black/20 to-transparent" ] []
    -- Fade in while dragging, see Swipe.js
    , stamp "YUM" "left-5 -rotate-12 border-star text-star" "var(--like, 0)"
    , stamp "NOPE" "right-5 rotate-12 border-white text-white" "var(--nope, 0)"
    , if likedByOthers then
        HE.span [ HA.class' "absolute left-4 top-6 flex items-center gap-1 rounded-lg border-2 border-ink bg-star px-3 py-1 text-sm font-bold text-ink shadow-pop" ]
          [ icon Heart "h-4 w-4 fill-current", HE.text "Someone's keen" ]
      else HE.text ""
    , HE.div [ HA.class' "absolute right-4 top-6 flex flex-col items-end gap-2" ]
        [ mapsLink r "rounded-lg bg-white/90 px-3 py-1 text-sm text-[#1F1F1F] shadow"
        , menuLink r "rounded-lg bg-white/90 px-3 py-1 text-sm font-bold text-[#1F1F1F] shadow"
        ]
    , HE.div [ HA.class' "absolute inset-x-0 bottom-0 flex flex-col gap-2 p-5 text-left" ]
        [ HE.h2 [ HA.class' "font-display text-3xl leading-tight text-white drop-shadow" ] [ HE.text r.name ]
        , HE.div [ HA.class' "flex items-center gap-2" ]
            (maybe [] (pure <<< ratingStars) r.rating <> maybe [] (pure <<< chip) r.price)
        , HE.div [ HA.class' "flex flex-wrap gap-2" ]
            (maybe [] (pure <<< chip) r.category <> map (chip <<< _.label) (filter (\c -> elem c.id r.confirmed) vibe))
        , case r.summary of
            Just s | not summaryOpen ->
              HE.button [ HA.class' "text-left text-sm text-white/90", HA.onClick toggleSummary ]
                [ HE.span [ HA.class' "line-clamp-2" ] [ HE.text s.text ]
                , HE.span [ HA.class' "font-bold underline" ] [ HE.text "More" ]
                ]
            _ -> HE.text ""
        , photoCredit r photo "text-white/70"
        ]
    , case r.summary of
        Just s | summaryOpen -> summaryBox s r.reviews toggleSummary
        _ -> HE.text ""
    ]

-- | Nope and like, sized for thumbs
actionBar :: forall a. { nope :: a, like :: a } -> Html a
actionBar { nope, like } =
  HE.div [ HA.class' "flex items-center justify-center gap-8 py-2" ]
    [ iconButton { label: "Nope", icon: Close, classes: "h-16 w-16 bg-white text-ink/50 hover:text-ink" } nope
    , iconButton { label: "Like", icon: Heart, classes: "h-20 w-20 bg-brand-500 text-white [&_svg]:fill-current" } like
    ]
