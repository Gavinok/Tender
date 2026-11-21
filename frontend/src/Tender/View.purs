module Tender.View (view) where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element (class ToNode)
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Tender.App (Message(..), Model(..), Screen(..))
import Tender.Types (Restaurant)

headerText :: forall a b. ToNode a b Html => a -> Html b
headerText text = HE.h1
  [ HA.class'
      "text-xl font-bold"
  ]
  text

header :: forall a b. ToNode a b Html => a -> Html b
header content =
  HE.header
    [ HA.class'
        "bg-red-500 text-white py-4 px-6"
    ]
    content

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
restaurantStats r = HE.div [ HA.class' "text-center" ] $ HE.ul_
  let
    ratings = [ " Rating: " <> (show r.rating) ]
    pricing = case r.price of
      Nothing -> []
      Just p -> [ " Price: " <> p ]
    category = case r.category of
      Nothing -> []
      Just c -> [ " Category: " <> c ]
  in
    map HE.li_ $ category <> ratings <> pricing

buttonShape :: forall a. NodeData a
buttonShape = HA.class' "button"

btn :: forall a b. ToNode a b Html => b -> a -> Array (NodeData b) -> Html b
btn m t c =
  let
    attrs =
      [ buttonShape
      , HA.onClick m
      ]
  in
    HE.button (attrs <> c) t

grey :: forall a. Array (NodeData a)
grey = [ HA.class' "bg-gray-900 hover:bg-gray-800" ]

centered :: forall a. NodeData a
centered = HA.class' "flex justify-center"

footer :: forall a b. ToNode b a Html => b -> Html a
footer content =
  HE.div [ HA.class' "bg-gray-200 py-4 px-6 flex justify-around" ]
    [ content
    ]

yelplink :: forall a. Restaurant -> Html a
yelplink r = HE.div [ HA.class' "yelp-link" ]
  [ HE.a [ HA.href r.url, HA.target "_blank", HA.value "Yelp Page" ] "Yelp Page" ]

loading :: Html Message
loading =
  HE.main "main"
    [ HE.div [ HA.class' "loading-main" ]
        [ HE.text "LOADING…" ]
    ]

-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view (ServerError e) = HE.main "main" [ HE.text $ "Error: " <> e ]

view Loading = loading

view (QR _ link) =
  HE.main "main"
    [ HE.div [ HA.class' "qr-main" ]
        [ HE.div [ HA.class' "join-button" ]
            [ HE.a [ HA.href link, HA.value "Join Session" ] "Join Session" ]
        , HE.img
            [ HA.src $ "https://api.qrserver.com/v1/create-qr-code/?data=" <> link
            , HA.alt ""
            ]
        , HE.div [ HA.class' "my-2" ]
            [ btn StartSwiping "Start Swiping" grey
            ]
        ]
    ]

view (Active _ Fetching) = loading

view (Active { matches } NoneLeft) =
  HE.main "main"
    [ HE.div [ HA.class' "loading-main" ]
        [ HE.text $ "No more restaurants nearby. " <> case length matches of
            0 -> "You didn't get any matches."
            1 -> "You got 1 match."
            n -> "You got " <> show n <> " matches."
        ]
    ]

view (Active { matches } (Swiping r)) =
  HE.main "main"
    [ HE.div [ HA.class' "flex flex-col h-screen" ]
        [ header $ headerText r.name
        , HE.div [ HA.class' "flex-grow overflow-y-auto" ]
            [ HE.div [ HA.class' "flex flex-col items-center py-8" ]
                [ image r.imageLink
                , HE.div [ centered ] $ yelplink r
                , HE.div [ centered ] $ (show $ length matches) <>
                    if (length matches) == 1 then " Match"
                    else " Matches"
                ]
            ]
        , HE.div_
            [ footer
                [ HE.div [ HA.class' "footer-item" ]
                    $ btn Dislike "-" [ HA.class' "bg-red-900 hover:bg-red-800" ]

                , HE.div [ HA.class' "footer-item" ] $ restaurantStats r

                , HE.div [ HA.class' "footer-item" ]
                    $ btn Like "+" [ HA.class' "bg-green-900 hover:bg-green-800" ]
                ]
            ]
        ]
    ]

view (Active _ (Matched r)) =
  HE.main "main"
    [ HE.div_
        [ HE.div [ HA.class' "flex justify-center" ] $ headerText "Yay You Got A Match!"
        , header [ headerText r.name ]
        , HE.div [ HA.class' "main-background" ]
            [ image r.imageLink
            , HE.div [ centered ] $ yelplink r
            ]
        , HE.div_
            [ footer
                [ HE.div [ HA.class' "footer-item col-start-2" ] $ restaurantStats r
                , HE.div [ HA.class' "footer-item" ] $ btn Continue "Continue Anyways" grey
                ]
            ]
        ]
    ]
