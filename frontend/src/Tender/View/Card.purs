module Tender.View.Card (restaurantCard, detailsSheet, ratingStars, actionBar) where

import Prelude

import Data.Array (catMaybes, elem, filter, length, range, take)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.Types (Restaurant, Review)
import Tender.Vibe (Chip)
import Tender.View.Button (iconButton)
import Tender.View.Icon (Icon(..), icon)
import Tender.View.Photo (mapsLink, menuLink, photoCredit, restaurantPhoto)
import Tender.View.Sheet (sheet)

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

-- | "850 m" or "1.4 km"
distanceText :: Number -> String
distanceText m
  | m < 1000.0 = show (round m) <> " m"
  | otherwise = show (toNumber (round (m / 100.0)) / 10.0) <> " km"

-- | How far away it is, whether it's open and where, as one line
whereAndWhen :: forall a. Restaurant -> Html a
whereAndWhen r = case catMaybes [ (_ <> " away") <<< distanceText <$> r.distance, openText <$> r.openNow, r.address ] of
  [] -> HE.text ""
  parts -> HE.p [ HA.class' "text-sm text-ink/80" ] [ HE.text (joinWith " · " parts) ]

openText :: Boolean -> String
openText open = if open then "Open now" else "Closed now"

-- | What the decision needs on one line, e.g. "★ 4.6 · $$ · 850 m · Open now"
facts :: Restaurant -> String
facts r = joinWith " · " $ catMaybes
  [ (\n -> "★ " <> show n) <$> r.rating, r.price, distanceText <$> r.distance, openText <$> r.openNow ]

-- | The vibes it's confirmed for, then the other kinds of place it is
chips :: Array Chip -> Restaurant -> Array String
chips vibe r = map _.label (filter (\c -> elem c.id r.confirmed) vibe) <> maybe [] pure r.category <> r.moreCategories

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

-- | Chevrons at the edges of the card where tapping changes photo. They only
-- | show on a card with more than one photo, and brighten on hover.
photoArrows :: forall a. Int -> Html a
photoArrows count
  | count < 2 = HE.text ""
  | otherwise =
      HE.div [ HA.class' "pointer-events-none absolute inset-x-1 top-1/3 flex justify-between text-white/60 transition hover:text-white", HA.createAttribute "aria-hidden" "true" ]
        [ icon ChevronLeft "h-8 w-8 drop-shadow", icon ChevronRight "h-8 w-8 drop-shadow" ]

-- | The restaurant's photo with what the decision needs over the bottom of
-- | it, and a badge when someone else in the group already liked it. The
-- | rest is in the details sheet. Keyed on the restaurant so each new one
-- | animates in.
restaurantCard :: forall a. { likedByOthers :: Boolean, vibe :: Array Chip, photo :: Int, toggleDetails :: a } -> Restaurant -> Html a
restaurantCard { likedByOthers, vibe, photo, toggleDetails } r =
  HE.article
    [ HA.key (show r.id)
    , HA.createAttribute "data-swipe" ""
    , HA.class' "relative min-h-0 w-full flex-1 animate-card-in cursor-grab touch-none select-none overflow-hidden rounded-[2rem] border-[3px] border-ink bg-brand-200 shadow-card active:cursor-grabbing"
    ]
    [ restaurantPhoto r photo "pointer-events-none absolute inset-0 h-full w-full"
    , photoBars (length r.photos) photo
    , photoArrows (length r.photos)
    , HE.div [ HA.class' "absolute inset-0 bg-gradient-to-t from-black/80 via-black/10 to-transparent" ] []
    -- Fade in while dragging, see Swipe.js
    , stamp "YUM" "left-5 -rotate-12 border-star text-star" "var(--like, 0)"
    , stamp "NOPE" "right-5 rotate-12 border-white text-white" "var(--nope, 0)"
    , if likedByOthers then
        HE.span [ HA.class' "absolute left-4 top-6 flex items-center gap-1 rounded-lg border-2 border-ink bg-star px-3 py-1 text-sm font-bold text-ink shadow-pop" ]
          [ icon Heart "h-4 w-4 fill-current", HE.text "Someone's keen" ]
      else HE.text ""
    , HE.div [ HA.class' "absolute right-4 top-6" ]
        [ mapsLink r "rounded-lg bg-white/90 px-3 py-1 text-sm text-[#1F1F1F] shadow" ]
    , HE.div [ HA.class' "absolute inset-x-0 bottom-0 flex flex-col gap-2 p-4 text-left text-white" ]
        [ HE.h2 [ HA.class' "line-clamp-2 font-display text-3xl leading-tight drop-shadow" ] [ HE.text r.name ]
        , case facts r of
            "" -> HE.text ""
            line -> HE.p [ HA.class' "font-bold drop-shadow" ] [ HE.text line ]
        , HE.div [ HA.class' "flex gap-2 overflow-hidden" ] (map chip (take 2 (chips vibe r)))
        , HE.div [ HA.class' "flex items-end justify-between gap-2" ]
            [ photoCredit r photo "text-white/90"
            , HE.button
                [ HA.class' "flex min-h-[2.75rem] shrink-0 items-center rounded-lg bg-white/90 px-4 font-bold text-ink shadow"
                , HA.onClick toggleDetails
                , HA.createAttribute "aria-label" ("Details about " <> r.name)
                ]
                [ HE.text "Details" ]
            ]
        ]
    ]

-- | Everything else about the restaurant: where and when, every kind of
-- | place it is, the summary and reviews, and links out
detailsSheet :: forall a. Array Chip -> a -> Restaurant -> Html a
detailsSheet vibe close r =
  sheet { title: r.name, close }
    [ HE.div [ HA.class' "flex flex-col gap-3" ]
        [ case joinWith " · " (catMaybes [ (\n -> "★ " <> show n) <$> r.rating, r.price ]) of
            "" -> HE.text ""
            line -> HE.p [ HA.class' "font-bold" ] [ HE.text line ]
        , whereAndWhen r
        , HE.ul [ HA.class' "flex flex-wrap gap-2" ]
            (map (\c -> HE.li [ HA.class' "rounded-lg border-2 border-ink bg-white px-3 py-1 text-sm font-bold" ] [ HE.text c ]) (chips vibe r))
        , case r.summary of
            Just s ->
              HE.div [ HA.class' "flex flex-col gap-1" ]
                [ HE.p [ HA.class' "leading-relaxed" ] [ HE.text s.text ]
                , maybe (HE.text "") (\d -> HE.p [ HA.class' "text-xs italic text-ink/70" ] [ HE.text d ]) s.disclosure
                ]
            Nothing -> HE.text ""
        , reviewList r.reviews
        , HE.div [ HA.class' "flex flex-wrap gap-2 border-t-2 border-ink/10 pt-3" ]
            [ mapsLink r "min-h-[2.75rem] rounded-lg border-2 border-ink bg-white px-3 text-sm text-[#1F1F1F]"
            , menuLink r "min-h-[2.75rem] rounded-lg border-2 border-ink bg-white px-3 text-sm font-bold"
            ]
        ]
    ]

-- | Nope and like, sized for thumbs
actionBar :: forall a. { nope :: a, like :: a } -> Html a
actionBar { nope, like } =
  HE.div [ HA.class' "flex items-center justify-center gap-8 py-2" ]
    [ iconButton { label: "Nope", icon: Close, classes: "h-16 w-16 bg-white text-ink/50 hover:text-ink" } nope
    , iconButton { label: "Like", icon: Heart, classes: "h-20 w-20 bg-brand-500 text-white [&_svg]:fill-current" } like
    ]
