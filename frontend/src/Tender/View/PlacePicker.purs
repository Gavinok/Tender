-- | Saying where the group is eating, for when locations are wrong or
-- | missing: type a neighbourhood, address or city, then pick a match
module Tender.View.PlacePicker (placePicker, searchingNear) where

import Prelude

import Data.Array (null)
import Data.Maybe (Maybe(..), maybe)
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
placePicker picker { query, find, choose, close } =
  HE.section [ HA.class' "flex w-full flex-col gap-3 rounded-2xl border-[3px] border-ink bg-white p-4 text-left shadow-pop" ]
    [ HE.h2 [ HA.class' "font-display text-2xl" ] [ HE.text "Where are you eating?" ]
    , HE.form [ HA.class' "flex gap-2", HA.onSubmit find ]
        [ HE.input
            [ HA.type' "search"
            , HA.class' "min-w-0 flex-1 rounded-lg border-2 border-ink px-3 py-2"
            , HA.placeholder "Neighbourhood, address or city"
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
    , HE.button [ HA.class' "self-start text-sm font-bold underline", HA.onClick close ] [ HE.text "Cancel" ]
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

-- | Where the session will search, and ways to change it until it's locked
searchingNear
  :: forall a r s
   . { place :: Maybe Place, locked :: Boolean | r }
  -> { open :: a, clear :: a | s }
  -> Html a
searchingNear { place, locked } { open, clear } =
  HE.p [ HA.class' "flex flex-wrap items-center gap-x-2 text-sm text-ink/70" ]
    ( [ HE.span_ [ HE.text "Searching near ", HE.strong [ HA.class' "text-ink" ] [ HE.text (maybe "your location" _.name place) ] ] ]
        <>
          if locked then []
          else case place of
            Nothing -> [ link "Somewhere else?" open ]
            Just _ -> [ link "Change" open, link "Use our locations" clear ]
    )
  where
  link label message = HE.button [ HA.class' "font-bold text-ink underline", HA.onClick message ] [ HE.text label ]
