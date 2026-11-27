module Tender.View (view) where

import Prelude

import Data.Foldable (foldMap)

import Data.Array (elem, head, length, null)
import Data.Int (round)
import Data.Maybe (Maybe(..), isNothing)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.App (Message(..), Model(..), Screen(..), Swiper)
import Tender.Types (Found, photoUrl)
import Tender.Vibe (Chip)
import Tender.View.Card (actionBar, detailsSheet, restaurantCard)
import Tender.View.Icon (Icon(..), icon)
import Tender.View.Layout (page)
import Tender.View.PlacePicker (placePicker)
import Tender.View.Presence (presenceStrip, sessionButton, waitingOn)
import Tender.View.Sheet (sheet)
import Tender.View.Button (Variant(..), button)
import Tender.View.Lobby (lobby)
import Tender.View.Match (announcement, celebration, matchesSheet, toast)
import Tender.View.States (cardSkeleton, needLocation, outOfRestaurants, problem, setting)

-- | Metres as whole kilometres, e.g. "10 km"
km :: Number -> String
km m = show (round (m / 1000.0)) <> " km"

-- | How far the search reached, with a way to widen it while there are still cards
reach :: Swiper -> Html Message
reach swiper =
  sheetSection "How far"
    ( [ HE.p_ [ HE.text ("Within " <> km swiper.radius) ] ]
        <>
          if swiper.radius < swiper.maxRadius && not swiper.widening then
            [ button Secondary Nothing ("Search further (up to " <> km (min swiper.maxRadius (swiper.radius * 2.0)) <> ")") SearchFurther ]
          else []
    )

-- | The session's vibe, when it has one
vibeLine :: forall a. Array Chip -> Html a
vibeLine [] = HE.text ""
vibeLine vibe =
  sheetSection "Vibe"
    [ HE.ul [ HA.class' "flex flex-wrap gap-2" ]
        (map (\c -> HE.li [ HA.class' "rounded-lg border-2 border-ink bg-white px-3 py-1 text-sm font-bold" ] [ HE.text c.label ]) vibe)
    ]

sheetSection :: forall a. String -> Array (Html a) -> Html a
sheetSection title body =
  HE.section [ HA.class' "flex flex-col gap-2 border-t-2 border-ink/10 pt-4" ]
    ([ HE.h3 [ HA.class' "font-display text-lg" ] [ HE.text title ] ] <> body)

-- | Who's here, the vibe, how far the search goes, and a way to invite more
-- | people or leave for a new session
sessionSheet :: Swiper -> Html Message
sessionSheet swiper =
  sheet { title: "Your session", close: ToggleSession }
    [ HE.div [ HA.class' "flex flex-col gap-4" ]
        [ presenceStrip swiper.people
        , vibeLine swiper.vibe
        , reach swiper
        , sheetSection "Invite"
            [ if swiper.copied then button Secondary (Just Check) "Link copied!" ShareInvite
              else button Secondary (Just Share) "Share invite" ShareInvite
            ]
        , HE.div [ HA.class' "border-t-2 border-ink/10 pt-4" ]
            [ button Secondary (Just Plus) "Start a new session" NewSession ]
        ]
    ]

-- | The most important notice, if any: unsent swipes stay until they're
-- | sent, while the location warning can be dismissed
notice :: Swiper -> Html Message
notice swiper
  | not (null swiper.unsent) = offlineNotice
  | isNothing swiper.loc && not swiper.warningDismissed = locationWarning DismissWarning
  | otherwise = HE.text ""

-- | Why the places might be far away, when the browser couldn't find the user
locationWarning :: forall a. a -> Html a
locationWarning dismiss =
  HE.div [ HA.class' "mb-3 flex items-center gap-2 rounded-xl border-2 border-ink bg-star/30 px-3 py-2 text-sm font-bold", HA.createAttribute "role" "status" ]
    [ icon MapPin "h-5 w-5 shrink-0"
    , HE.span [ HA.class' "flex-1" ] [ HE.text "Couldn't get your location. Showing places near where your group started." ]
    , HE.button [ HA.class' "-my-2 -mr-2 grid h-11 w-11 shrink-0 place-items-center rounded-lg hover:bg-ink/10", HA.onClick dismiss, HA.createAttribute "aria-label" "Dismiss" ] [ icon Close "h-5 w-5" ]
    ]

-- | Shown while swipes are waiting to be sent
offlineNotice :: forall a. Html a
offlineNotice =
  HE.p [ HA.class' "mb-3 rounded-xl border-2 border-ink bg-star/30 px-3 py-2 text-sm font-bold", HA.createAttribute "role" "status" ]
    [ HE.text "Can't reach the server. Your swipes are saved and will be sent once it's back." ]

pickerMessages :: { open :: Message, clear :: Message, query :: String -> Message, find :: Message, choose :: Found -> Message, close :: Message }
pickerMessages = { open: OpenPicker, clear: ClearPlace, query: PlaceQuery, find: FindPlaces, choose: ChoosePlace, close: ClosePicker }

-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view (ServerError e) = page { matches: Nothing, newSession: Just NewSession, session: Nothing, fill: false } [ problem e Retry ]

view (NeedLocation { message, picker }) =
  page { matches: Nothing, newSession: Just NewSession, session: Nothing, fill: false }
    [ case picker of
        Just p -> HE.div [ HA.class' "flex flex-1 flex-col justify-center" ] [ placePicker p pickerMessages ]
        Nothing -> needLocation message { retry: RetryLocation, choose: OpenPicker }
    ]

view Loading = page { matches: Nothing, newSession: Nothing, session: Nothing, fill: false } [ setting ]

view (QR l) = page { matches: Nothing, newSession: Just NewSession, session: Nothing, fill: true } [ lobby l { share: ShareInvite, start: StartSwiping, toggle: ToggleChip, bigQR: ToggleQR, picker: pickerMessages } ]

view (Active swiper screen) =
  page { matches: Just { count: length swiper.matches, open: ToggleMatches }, newSession: Nothing, session: Just (sessionButton swiper.people ToggleSession), fill: true } $
    [ announcement
        ( case screen of
            Matched r -> Just r
            _ -> Nothing
        )
    , notice swiper
    ]
      <>
        [ case screen of
            Fetching -> cardSkeleton
            NoneLeft ->
              outOfRestaurants swiper.matches
                { startOver: NewSession
                , waiting: waitingOn swiper.people
                , further:
                    if swiper.radius < swiper.maxRadius then
                      Just { label: "Search further (up to " <> km (min swiper.maxRadius (swiper.radius * 2.0)) <> ")", search: SearchFurther }
                    else Nothing
                }
            Swiping r ->
              HE.div [ HA.class' "flex max-h-[52rem] min-h-0 flex-1 flex-col gap-4" ]
                -- The card is keyed so each restaurant gets a fresh element. Flame
                -- needs keyed elements kept apart from unkeyed siblings.
                [ HE.div [ HA.class' "flex min-h-0 flex-1 flex-col" ] [ restaurantCard { likedByOthers: elem r.id swiper.othersLiked, vibe: swiper.vibe, photo: swiper.photo, toggleDetails: ToggleDetails } r ]
                , actionBar { nope: Dislike, like: Like }
                -- Start loading the next photo so it's there when this card goes
                , HE.div [ HA.class' "hidden" ]
                    (map (\url -> HE.img [ HA.src url, HA.alt "" ]) (foldMap pure (head swiper.upcoming >>= (_.photos >>> head) >>= photoUrl)))
                ]
            Matched r -> celebration r Continue
        ]
      <> foldMap (\t -> [ HE.div_ [ toast t { open: ToggleMatches, dismiss: HideToast t.match.id } ] ]) swiper.toast
      <> (if swiper.showMatches then [ matchesSheet swiper.matches ToggleMatches ] else [])
      <> (if swiper.showSession then [ sessionSheet swiper ] else [])
      <> case screen of
        Swiping r | swiper.detailsOpen -> [ detailsSheet swiper.vibe ToggleDetails r ]
        _ -> []
