module Tender.View (view) where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Tender.App (Message(..), Model(..), Screen(..))
import Tender.QR (qrDataUrl)
import Tender.Types (Restaurant)

page :: Array (Html Message) -> Html Message
page = HE.main [ HA.id "main" ]

headerText :: forall a. String -> Html a
headerText text = HE.h1 [ HA.class' "text-xl font-bold" ] [ HE.text text ]

header :: forall a. Array (Html a) -> Html a
header = HE.header [ HA.class' "bg-red-500 text-white py-4 px-6" ]

image :: String -> Html Message
image link =
  HE.div
    [ HA.class' "image-frame" ]
    [ HE.figure [ HA.class' "" ]
        [ HE.img
            [ HA.class' "image"
            , HA.src link
            , HA.alt "Restaurant Image"
            ]
        ]
    ]

restaurantStats :: forall a. Restaurant -> Html a
restaurantStats r = HE.div [ HA.class' "text-center" ]
  [ HE.ul_ $ map (HE.li_ <<< pure <<< HE.text) $ category <> ratings <> pricing ]
  where
  ratings = [ " Rating: " <> show r.rating ]
  pricing = case r.price of
    Nothing -> []
    Just p -> [ " Price: " <> p ]
  category = case r.category of
    Nothing -> []
    Just c -> [ " Category: " <> c ]

btn :: forall a. a -> String -> Array (NodeData a) -> Html a
btn m t c = HE.button ([ HA.class' "button", HA.onClick m ] <> c) [ HE.text t ]

grey :: forall a. Array (NodeData a)
grey = [ HA.class' "bg-gray-900 hover:bg-gray-800" ]

centered :: forall a. NodeData a
centered = HA.class' "flex justify-center"

footer :: forall a. Array (Html a) -> Html a
footer = HE.div [ HA.class' "bg-gray-200 py-4 px-6 flex justify-around" ]

footerItem :: forall a. Html a -> Html a
footerItem content = HE.div [ HA.class' "footer-item" ] [ content ]

yelplink :: forall a. Restaurant -> Html a
yelplink r = HE.div [ HA.class' "yelp-link" ]
  [ HE.a [ HA.href r.url, HA.target "_blank" ] [ HE.text "Yelp Page" ] ]

loading :: Html Message
loading = page [ HE.div [ HA.class' "loading-main" ] [ HE.text "LOADING…" ] ]

matchCount :: forall a. Int -> Html a
matchCount n = HE.text $ show n <> if n == 1 then " Match" else " Matches"

-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view (ServerError e) = page [ HE.text $ "Error: " <> e ]

view Loading = loading

view (QR _ link) =
  page
    [ HE.div [ HA.class' "qr-main" ]
        [ HE.div [ HA.class' "join-button" ]
            [ HE.a [ HA.href link ] [ HE.text "Join Session" ] ]
        , HE.img
            [ HA.src $ qrDataUrl link
            , HA.alt "QR code for the session link"
            , HA.class' "w-64 h-64"
            ]
        , HE.div [ HA.class' "my-2" ]
            [ btn StartSwiping "Start Swiping" grey
            ]
        ]
    ]

view (Active _ Fetching) = loading

view (Active { matches } NoneLeft) =
  page
    [ HE.div [ HA.class' "loading-main" ]
        [ HE.text $ "No more restaurants nearby. " <> case length matches of
            0 -> "You didn't get any matches."
            1 -> "You got 1 match."
            n -> "You got " <> show n <> " matches."
        ]
    ]

view (Active { matches } (Swiping r)) =
  page
    [ HE.div [ HA.class' "flex flex-col h-screen" ]
        [ header [ headerText r.name ]
        , HE.div [ HA.class' "flex-grow overflow-y-auto" ]
            [ HE.div [ HA.class' "flex flex-col items-center py-8" ]
                [ image r.imageLink
                , HE.div [ centered ] [ yelplink r ]
                , HE.div [ centered ] [ matchCount (length matches) ]
                ]
            ]
        , HE.div_
            [ footer
                [ footerItem $ btn Dislike "-" [ HA.class' "bg-red-900 hover:bg-red-800" ]
                , footerItem $ restaurantStats r
                , footerItem $ btn Like "+" [ HA.class' "bg-green-900 hover:bg-green-800" ]
                ]
            ]
        ]
    ]

view (Active _ (Matched r)) =
  page
    [ HE.div_
        [ HE.div [ HA.class' "flex justify-center" ] [ headerText "Yay You Got A Match!" ]
        , header [ headerText r.name ]
        , HE.div [ HA.class' "main-background" ]
            [ image r.imageLink
            , HE.div [ centered ] [ yelplink r ]
            ]
        , HE.div_
            [ footer
                [ HE.div [ HA.class' "footer-item col-start-2" ] [ restaurantStats r ]
                , footerItem $ btn Continue "Continue Anyways" grey
                ]
            ]
        ]
    ]
