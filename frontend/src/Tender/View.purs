module Tender.View (view) where

import Prelude

import Data.Array (elem, head, length)
import Data.Int (round)
import Data.Maybe (Maybe(..), maybe)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.App (Message(..), Model(..), Screen(..))
import Tender.Types (photoUrl)
import Tender.Vibe (chipLabel)
import Tender.View.Card (actionBar, restaurantCard)
import Tender.View.Layout (page)
import Tender.View.Lobby (lobby)
import Tender.View.Match (celebration, matchesSheet)
import Tender.View.States (cardSkeleton, outOfRestaurants, problem, setting)

-- | Metres as whole kilometres, e.g. "10 km"
km :: Number -> String
km m = show (round (m / 1000.0)) <> " km"

-- | The session's vibe above the cards, when it has one
vibeLine :: forall a. Array String -> Html a
vibeLine vibe =
  HE.p [ HA.class' "-mt-2 flex flex-wrap gap-x-3 text-sm font-bold text-ink/70" ]
    (map (HE.span_ <<< pure <<< HE.text <<< chipLabel) vibe)

-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view (ServerError e) = page Nothing [ problem e Retry ]

view Loading = page Nothing [ setting ]

view (QR l) = page Nothing [ lobby l { share: ShareInvite, start: StartSwiping, toggle: ToggleChip } ]

view (Active swiper screen) =
  page (Just { count: length swiper.matches, open: ToggleMatches }) $
    [ case screen of
        Fetching -> cardSkeleton
        NoneLeft ->
          outOfRestaurants swiper.matches
            { startOver: Retry
            , further:
                if swiper.radius < swiper.maxRadius then
                  Just { label: "Search further (up to " <> km (min swiper.maxRadius (swiper.radius * 2.0)) <> ")", search: SearchFurther }
                else Nothing
            }
        Swiping r ->
          HE.div [ HA.class' "flex max-h-[52rem] flex-1 flex-col gap-6" ]
            -- The card is keyed so each restaurant gets a fresh element. Flame
            -- needs keyed elements kept apart from unkeyed siblings.
            [ vibeLine swiper.vibe
            , HE.div [ HA.class' "flex flex-1 flex-col" ] [ restaurantCard { likedByOthers: elem r.id swiper.othersLiked, photo: swiper.photo, summaryOpen: swiper.summaryOpen, toggleSummary: ToggleSummary } r ]
            , actionBar { nope: Dislike, like: Like }
            -- Start loading the next photo so it's there when this card goes
            , HE.div [ HA.class' "hidden" ]
                (map (\url -> HE.img [ HA.src url, HA.alt "" ]) (maybe [] pure (head swiper.upcoming >>= (_.photos >>> head) >>= photoUrl)))
            ]
        Matched r -> celebration r Continue
    ]
      <> if swiper.showMatches then [ matchesSheet swiper.matches ToggleMatches ] else []
