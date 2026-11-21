module Tender.View (view) where

import Prelude

import Data.Array (head, length)
import Data.Maybe (Maybe(..), maybe)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.App (Message(..), Model(..), Screen(..))
import Tender.Types (photoUrl)
import Tender.View.Card (actionBar, restaurantCard)
import Tender.View.Layout (page)
import Tender.View.Lobby (lobby)
import Tender.View.Match (celebration, matchesSheet)
import Tender.View.States (cardSkeleton, outOfRestaurants, problem, setting)

-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view (ServerError e) = page Nothing [ problem e Retry ]

view Loading = page Nothing [ setting ]

view (QR l) = page Nothing [ lobby l { share: ShareInvite, start: StartSwiping } ]

view (Active swiper screen) =
  page (Just { count: length swiper.matches, open: ToggleMatches }) $
    [ case screen of
        Fetching -> cardSkeleton
        NoneLeft -> outOfRestaurants swiper.matches Retry
        Swiping r ->
          HE.div [ HA.class' "flex max-h-[52rem] flex-1 flex-col gap-6" ]
            -- The card is keyed so each restaurant gets a fresh element. Flame
            -- needs keyed elements kept apart from unkeyed siblings.
            [ HE.div [ HA.class' "flex flex-1 flex-col" ] [ restaurantCard r ]
            , actionBar { nope: Dislike, like: Like }
            -- Start loading the next photo so it's there when this card goes
            , HE.div [ HA.class' "hidden" ]
                (map (\p -> HE.img [ HA.src (photoUrl p), HA.alt "" ]) (maybe [] pure (head swiper.upcoming >>= _.photo)))
            ]
        Matched r -> celebration r Continue
    ]
      <> if swiper.showMatches then [ matchesSheet swiper.matches ToggleMatches ] else []
