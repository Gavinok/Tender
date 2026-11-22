-- | Restaurant photos and the attribution each source requires
module Tender.View.Photo (restaurantPhoto, photoCredit, mapsLink) where

import Prelude

import Data.Array (index)
import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.Types (Restaurant, photoUrl)
import Tender.View.Icon (Icon(..), icon)

-- | One of the restaurant's photos filling an element with the given classes,
-- | or a placeholder for places without one
restaurantPhoto :: forall a. Restaurant -> Int -> String -> Html a
restaurantPhoto r i classes = case index r.photos i >>= photoUrl of
  Just url -> HE.img [ HA.src url, HA.alt "", HA.class' ("object-cover " <> classes) ]
  Nothing ->
    HE.div [ HA.class' ("grid place-items-center bg-gradient-to-br from-brand-400 to-brand-700 text-white/40 " <> classes) ]
      [ icon Utensils "h-1/3 w-1/3" ]

-- | Credit for the author of the photo showing, or for OpenStreetMap's data
photoCredit :: forall a. Restaurant -> Int -> String -> Html a
photoCredit r i classes = case r.source, index r.photos i >>= _.author of
  "openstreetmap", _ ->
    HE.p [ HA.class' ("text-xs " <> classes) ]
      [ HE.a [ HA.href "https://www.openstreetmap.org/copyright", HA.target "_blank", HA.rel "noopener", HA.class' "underline" ]
          [ HE.text "© OpenStreetMap contributors" ]
      ]
  _, Just author ->
    HE.p [ HA.class' ("text-xs " <> classes) ]
      [ HE.text "Photo: "
      , case author.uri of
          Just uri -> HE.a [ HA.href uri, HA.target "_blank", HA.rel "noopener", HA.class' "underline" ] [ HE.text author.name ]
          Nothing -> HE.text author.name
      ]
  _, Nothing -> HE.text ""

-- | Link to the place on its source. Google asks for exactly "Google Maps",
-- | not wrapped or translated, preferably in Roboto.
mapsLink :: forall a. Restaurant -> String -> Html a
mapsLink r classes = case r.url of
  Nothing -> HE.text ""
  Just url ->
    HE.a
      [ HA.href url
      , HA.target "_blank"
      , HA.rel "noopener"
      , HA.createAttribute "translate" "no"
      , HA.class' ("flex items-center gap-1 whitespace-nowrap font-['Roboto',Arial,sans-serif] " <> classes)
      ]
      [ HE.text (sourceName r.source), icon ExternalLink "h-4 w-4" ]

sourceName :: String -> String
sourceName = case _ of
  "yelp" -> "Yelp"
  "openstreetmap" -> "OpenStreetMap"
  _ -> "Google Maps"
