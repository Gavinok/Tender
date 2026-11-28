-- | Saying where the group is eating, for when locations are wrong or
-- | missing: type a neighbourhood, address or city, then pick a match
module Tender.View.PlacePicker (placePicker, placeSearch, searchingNear) where

import Prelude

import Data.Array (null)
import Data.Maybe (Maybe(..), isJust, maybe)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.App (Picker)
import Tender.Types (Found, Place)

placePicker
  :: forall a r
   . Picker
  -> { query :: String -> a, find :: a, choose :: Found -> a, close :: a | r }
  -> Html a
placePicker picker on =
  HE.section [ HA.class' "flex w-full flex-col gap-3 rounded-2xl border-[3px] border-ink bg-white p-4 text-left shadow-pop" ]
    [ HE.h2 [ HA.class' "font-display text-2xl" ] [ HE.text "Where are you eating?" ]
    , placeSearch picker on
    , HE.button [ HA.class' "self-start text-sm font-bold underline", HA.onClick on.close ] [ HE.text "Cancel" ]
    ]

-- | The lookup and its results on their own, for a sheet that already has a
-- | title and a way to close
placeSearch
  :: forall a r
   . Picker
  -> { query :: String -> a, find :: a, choose :: Found -> a | r }
  -> Html a
placeSearch picker { query, find, choose } =
  HE.div [ HA.class' "flex w-full flex-col gap-3 text-left" ]
    [ HE.form [ HA.class' "flex gap-2", HA.onSubmit find ]
        [ HE.input
            [ HA.type' "search"
            , HA.class' "min-w-0 flex-1 rounded-lg border-2 border-ink px-3 py-2"
            , HA.placeholder "Neighbourhood or address"
            , HA.value picker.query
            , HA.onInput query
            , HA.createAttribute "aria-label" "Where are you eating?"
            , HA.autofocus true
            ]
        , HE.button
            [ HA.type' "submit"
            , HA.class' "rounded-lg border-2 border-ink bg-brand-500 px-4 py-2 font-bold text-white disabled:opacity-60"
            , HA.disabled picker.searching
            ]
            [ HE.text if picker.searching then "Looking…" else "Search" ]
        ]
    , case picker.problem, picker.results of
        Just problem, _ -> HE.p [ HA.class' "text-sm font-bold text-ink/70" ] [ HE.text problem ]
        Nothing, Just found | null found -> HE.p [ HA.class' "text-sm text-ink/70" ] [ HE.text "Nothing matched. Try a neighbourhood or city." ]
        Nothing, Just found -> HE.ul [ HA.class' "flex flex-col gap-2" ] (map (result choose) found)
        Nothing, Nothing -> HE.text ""
    ]

result :: forall a. (Found -> a) -> Found -> Html a
result choose found =
  HE.li_
    [ HE.button
        [ HA.class' "flex w-full flex-col rounded-lg border-2 border-ink px-3 py-2 text-left hover:bg-brand-50"
        , HA.onClick (choose found)
        ]
        [ HE.span [ HA.class' "font-bold" ] [ HE.text found.name ]
        , if found.address == "" then HE.text "" else HE.span [ HA.class' "text-sm text-ink/70" ] [ HE.text found.address ]
        ]
    ]

-- | Where the session will search, on one line that never wraps (a long
-- | name is cut short), and a way to change it until it's locked. When
-- | someone else changes it, the name lights up and screen readers hear it.
-- | Without a place it's the first location anyone in the group sends.
searchingNear
  :: forall a s
   . { place :: Maybe Place, locked :: Boolean, changed :: Boolean }
  -> { open :: a | s }
  -> Html a
searchingNear { place, locked, changed } { open } =
  HE.div [ HA.class' "flex max-w-full flex-col items-center" ]
    [ HE.p [ HA.class' "flex max-w-full items-center gap-x-2 text-sm text-ink/70" ]
        ( [ HE.span [ HA.class' "truncate" ]
              [ HE.text "Searching near "
              , HE.strong [ HA.class' ("rounded px-0.5 text-ink transition-colors duration-700 " <> if changed then "bg-star" else "bg-transparent") ]
                  [ HE.text (maybe "you all" _.name place) ]
              ]
          ]
            <>
              if locked then [ HE.span [ HA.class' "shrink-0" ] [ HE.text "· Locked in" ] ]
              else
                -- Its tap area reaches over the text above and below, so it's
                -- 44px tall without making the line any taller
                [ HE.button [ HA.class' "relative shrink-0 font-bold text-ink underline before:absolute before:-inset-x-1 before:-inset-y-3 before:content-['']", HA.onClick open ]
                    [ HE.text (if isJust place then "Change" else "Somewhere else?") ]
                ]
        )
    , HE.p [ HA.class' "sr-only", HA.createAttribute "role" "status" ]
        [ HE.text if changed then maybe "Someone switched back to your own locations" (\p -> "Someone set the place to " <> p.name) place else "" ]
    ]
