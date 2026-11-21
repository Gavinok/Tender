module Tender.View (view) where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.App (Message(..), Model(..), Screen(..))
import Tender.QR (qrDataUrl)
import Tender.Types (Restaurant)
import Tender.View.Button (Variant(..), button, linkButton)
import Tender.View.Card (actionBar, restaurantCard)
import Tender.View.Icon (Icon(..))
import Tender.View.Layout (centred, page)

heading :: forall a. String -> Html a
heading text = HE.h1 [ HA.class' "text-4xl font-black leading-tight" ] [ HE.text text ]

blurb :: forall a. String -> Html a
blurb text = HE.p [ HA.class' "text-lg text-ink/70" ] [ HE.text text ]

photo :: forall a. Restaurant -> Html a
photo r =
  HE.img
    [ HA.class' "aspect-[4/5] w-full rounded-3xl object-cover shadow-card"
    , HA.src r.imageLink
    , HA.alt r.name
    ]

loading :: Html Message
loading = page Nothing [ centred [ blurb "Loading…" ] ]

-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view (ServerError e) =
  page Nothing [ centred [ heading "Uh oh", blurb e ] ]

view Loading = loading

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

view (Active _ Fetching) = loading

view (Active { matches } NoneLeft) =
  page (Just (length matches))
    [ centred
        [ heading "No more restaurants nearby"
        , blurb case length matches of
            0 -> "You didn't get any matches."
            1 -> "You got 1 match."
            n -> "You got " <> show n <> " matches."
        ]
    ]

view (Active { matches } (Swiping r)) =
  page (Just (length matches))
    [ HE.div [ HA.class' "flex max-h-[52rem] flex-1 flex-col gap-6" ]
        [ restaurantCard r
        , actionBar { nope: Dislike, like: Like }
        ]
    ]

view (Active { matches } (Matched r)) =
  page (Just (length matches))
    [ centred
        [ heading "It's a Match!"
        , photo r
        , HE.h2 [ HA.class' "text-3xl font-black" ] [ HE.text r.name ]
        , linkButton Secondary (Just ExternalLink) "Open in Yelp" r.url
        , button Primary Nothing "Keep swiping" Continue
        ]
    ]
