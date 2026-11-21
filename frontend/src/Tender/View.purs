module Tender.View (view) where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.App (Message(..), Model(..), Screen(..))
import Tender.QR (qrDataUrl)
import Tender.View.Button (Variant(..), button)
import Tender.View.Card (actionBar, restaurantCard)
import Tender.View.Icon (Icon(..))
import Tender.View.Layout (centred, page)
import Tender.View.Match (celebration, matchesSheet)
import Tender.View.States (cardSkeleton, outOfRestaurants, problem, setting)

heading :: forall a. String -> Html a
heading text = HE.h1 [ HA.class' "text-4xl font-black leading-tight" ] [ HE.text text ]

blurb :: forall a. String -> Html a
blurb text = HE.p [ HA.class' "text-lg text-ink/70" ] [ HE.text text ]

-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view (ServerError e) = page Nothing [ problem e Retry ]

view Loading = page Nothing [ setting ]

view (QR _ link) =
  page Nothing
    [ centred
        [ heading "Where should we eat?"
        , blurb "Have your friends scan this to join, then start swiping."
        , HE.a [ HA.href link, HA.class' "rounded-3xl bg-white p-4 shadow-card" ]
            [ HE.img
                [ HA.src $ qrDataUrl link
                , HA.alt "QR code for the session link"
                , HA.class' "h-56 w-56"
                ]
            ]
        , button Primary (Just Heart) "Start swiping" StartSwiping
        ]
    ]

view (Active swiper screen) =
  page (Just { count: length swiper.matches, open: ToggleMatches }) $
    [ case screen of
        Fetching -> cardSkeleton
        NoneLeft -> outOfRestaurants swiper.matches Retry
        Swiping r ->
          HE.div [ HA.class' "flex max-h-[52rem] flex-1 flex-col gap-6" ]
            [ restaurantCard r
            , actionBar { nope: Dislike, like: Like }
            ]
        Matched r -> celebration r Continue
    ]
      <> if swiper.showMatches then [ matchesSheet swiper.matches ToggleMatches ] else []
