module Tender.View (view) where

import Prelude

import Data.Array (elem, head, length)
import Data.Int (round)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.App (Message(..), Model(..), Screen(..), Swiper)
import Tender.Types (Found, photoUrl)
import Tender.Vibe (Chip)
import Tender.View.Card (actionBar, restaurantCard)
import Tender.View.Icon (Icon(..), icon)
import Tender.View.Layout (page)
import Tender.View.PlacePicker (placePicker)
import Tender.View.Presence (presenceStrip)
import Tender.View.Lobby (lobby)
import Tender.View.Match (announcement, celebration, matchesSheet, toast)
import Tender.View.States (cardSkeleton, needLocation, outOfRestaurants, problem, setting)

-- | Metres as whole kilometres, e.g. "10 km"
km :: Number -> String
km m = show (round (m / 1000.0)) <> " km"

-- | How far the search reached, with a way to widen it while there are still cards
reach :: Swiper -> Html Message
reach swiper =
  HE.p [ HA.class' "-mt-4 flex items-center gap-2 text-sm text-ink/70" ]
    ( [ HE.span_ [ HE.text ("Within " <> km swiper.radius) ] ]
        <>
          if swiper.radius < swiper.maxRadius && not swiper.widening then
            [ HE.button [ HA.type' "button", HA.class' "font-bold underline", HA.onClick SearchFurther ] [ HE.text ("Search further (up to " <> km (min swiper.maxRadius (swiper.radius * 2.0)) <> ")") ] ]
          else []
    )

-- | The session's vibe above the cards, when it has one
vibeLine :: forall a. Array Chip -> Html a
vibeLine vibe =
  HE.p [ HA.class' "-mt-2 flex flex-wrap gap-x-3 text-sm font-bold text-ink/70" ]
    (map (HE.span_ <<< pure <<< HE.text <<< _.label) vibe)

-- | Why the places might be far away, when the browser couldn't find the user
locationWarning :: forall a. a -> Html a
locationWarning dismiss =
  HE.div [ HA.class' "mb-4 flex items-center gap-2 rounded-xl border-2 border-ink bg-star/30 px-3 py-2 text-sm font-bold", HA.createAttribute "role" "status" ]
    [ icon MapPin "h-5 w-5 shrink-0"
    , HE.span [ HA.class' "flex-1" ] [ HE.text "Couldn't get your location. Showing places near where your group started." ]
    , HE.button [ HA.class' "rounded-lg p-1 hover:bg-ink/10", HA.onClick dismiss, HA.createAttribute "aria-label" "Dismiss" ] [ icon Close "h-5 w-5" ]
    ]

pickerMessages :: { open :: Message, clear :: Message, query :: String -> Message, find :: Message, choose :: Found -> Message, close :: Message }
pickerMessages = { open: OpenPicker, clear: ClearPlace, query: PlaceQuery, find: FindPlaces, choose: ChoosePlace, close: ClosePicker }

-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view (ServerError e) = page { matches: Nothing, newSession: Just NewSession } [ problem e Retry ]

view (NeedLocation { message, picker }) =
  page { matches: Nothing, newSession: Just NewSession }
    [ case picker of
        Just p -> HE.div [ HA.class' "flex flex-1 flex-col justify-center" ] [ placePicker p pickerMessages ]
        Nothing -> needLocation message { retry: RetryLocation, choose: OpenPicker }
    ]

view Loading = page { matches: Nothing, newSession: Nothing } [ setting ]

view (QR l) = page { matches: Nothing, newSession: Just NewSession } [ lobby l { share: ShareInvite, start: StartSwiping, toggle: ToggleChip, picker: pickerMessages } ]

view (Active swiper screen) =
  page { matches: Just { count: length swiper.matches, open: ToggleMatches }, newSession: Just NewSession } $
    [ announcement
        ( case screen of
            Matched r -> Just r
            _ -> Nothing
        )
    , presenceStrip swiper.people
    ]
      <> (if isNothing swiper.loc && not swiper.warningDismissed then [ locationWarning DismissWarning ] else [])
      <>
        [ case screen of
            Fetching -> cardSkeleton
            NoneLeft ->
              outOfRestaurants swiper.matches
                { startOver: NewSession
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
                , reach swiper
                , HE.div [ HA.class' "flex flex-1 flex-col" ] [ restaurantCard { likedByOthers: elem r.id swiper.othersLiked, vibe: swiper.vibe, photo: swiper.photo, summaryOpen: swiper.summaryOpen, toggleSummary: ToggleSummary } r ]
                , actionBar { nope: Dislike, like: Like }
                -- Start loading the next photo so it's there when this card goes
                , HE.div [ HA.class' "hidden" ]
                    (map (\url -> HE.img [ HA.src url, HA.alt "" ]) (maybe [] pure (head swiper.upcoming >>= (_.photos >>> head) >>= photoUrl)))
                ]
            Matched r -> celebration r Continue
        ]
      <> maybe [] (\r -> [ toast r ToggleMatches ]) swiper.toast
      <> if swiper.showMatches then [ matchesSheet swiper.matches ToggleMatches ] else []
