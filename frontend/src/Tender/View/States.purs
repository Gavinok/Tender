-- | Screens for when there is no restaurant to show
module Tender.View.States (cardSkeleton, setting, outOfRestaurants, problem, matchList) where

import Prelude

import Data.Array (null)
import Data.Maybe (Maybe(..), fromMaybe)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.Types (Restaurant)
import Tender.View.Button (Variant(..), button)
import Tender.View.Icon (Icon(..), icon)
import Tender.View.Photo (restaurantPhoto)

shimmer :: String
shimmer = "animate-shimmer bg-gradient-to-r from-brand-100 via-brand-50 to-brand-100 bg-[length:200%_100%]"

-- | Placeholder the shape of a restaurant card and the buttons under it
cardSkeleton :: forall a. Html a
cardSkeleton =
  HE.div [ HA.class' "flex flex-1 flex-col gap-6", HA.createAttribute "aria-busy" "true" ]
    [ HE.div [ HA.class' ("relative min-h-[22rem] flex-1 rounded-[2rem] " <> shimmer) ]
        [ HE.div [ HA.class' "absolute bottom-5 left-5 flex w-2/3 flex-col gap-3" ]
            [ HE.div [ HA.class' "h-8 w-full rounded-full bg-white/70" ] []
            , HE.div [ HA.class' "h-5 w-1/2 rounded-full bg-white/70" ] []
            ]
        ]
    , HE.div [ HA.class' "flex items-center justify-center gap-8 py-2" ]
        [ HE.div [ HA.class' ("h-16 w-16 rounded-2xl " <> shimmer) ] []
        , HE.div [ HA.class' ("h-20 w-20 rounded-2xl " <> shimmer) ] []
        ]
    , HE.span [ HA.class' "sr-only" ] [ HE.text "Loading" ]
    ]

-- | Shown while the session is being set up
setting :: forall a. Html a
setting =
  HE.div [ HA.class' "flex flex-1 flex-col items-center justify-center gap-3 text-brand-500" ]
    [ icon Utensils "h-12 w-12 animate-bounce"
    , HE.p [ HA.class' "text-lg font-bold" ] [ HE.text "Setting the table…" ]
    ]

badge :: forall a. Icon -> String -> Html a
badge i classes = HE.div [ HA.class' ("grid h-20 w-20 place-items-center rounded-2xl border-[3px] border-ink " <> classes) ] [ icon i "h-10 w-10" ]

-- | Restaurants with a small photo and a link to Yelp
matchList :: forall a. Array Restaurant -> Html a
matchList rs =
  HE.ul [ HA.class' "flex w-full flex-col gap-3" ] (map item rs)
  where
  -- Thumbnails can leave out the photo credit since Google Maps shows it
  item r =
    HE.li_
      [ HE.a
          [ HA.href (fromMaybe "#" r.url)
          , HA.target "_blank"
          , HA.rel "noopener"
          , HA.class' "flex items-center gap-3 rounded-xl border-2 border-ink bg-white p-2 pr-4 text-left shadow-pop transition active:translate-y-0.5 active:shadow-none"
          ]
          [ restaurantPhoto r "h-14 w-14 rounded-lg"
          , HE.span [ HA.class' "flex-1 font-bold" ] [ HE.text r.name ]
          , icon ExternalLink "h-5 w-5 text-brand-500"
          ]
      ]

outOfRestaurants :: forall a. Array Restaurant -> a -> Html a
outOfRestaurants matches startOver =
  HE.div [ HA.class' "flex flex-1 flex-col items-center justify-center gap-5 text-center" ]
    [ badge Utensils "bg-brand-100 text-brand-500"
    , HE.h1 [ HA.class' "text-3xl font-display" ] [ HE.text "That's everything nearby" ]
    , if null matches then
        HE.p [ HA.class' "text-lg text-ink/70" ] [ HE.text "No matches this time. Maybe someone's being picky?" ]
      else
        HE.div [ HA.class' "flex w-full flex-col items-center gap-3" ]
          [ HE.p [ HA.class' "text-lg text-ink/70" ] [ HE.text "Here's where you all agreed:" ]
          , matchList matches
          ]
    , button Secondary (Just Refresh) "Start over" startOver
    ]

-- | Something went wrong, with the message and a way to try again
problem :: forall a. String -> a -> Html a
problem message retry =
  HE.div [ HA.class' "flex flex-1 flex-col items-center justify-center gap-5 text-center" ]
    [ badge Close "bg-ink/5 text-ink/60"
    , HE.h1 [ HA.class' "text-3xl font-display" ] [ HE.text "Something went wrong" ]
    , HE.p [ HA.class' "max-w-xs text-lg text-ink/70" ] [ HE.text message ]
    , button Primary (Just Refresh) "Try again" retry
    ]
