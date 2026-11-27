-- | Restaurants the tests share, so each is defined once.
module Fixtures (pho, fishStore, plain) where

import Data.Text (Text)

import Restaurant

-- | A restaurant with only the given name and position
plain :: Source -> Text -> Loc -> Restaurant
plain src n = Restaurant src (RestaurantId n) n Nothing Nothing Nothing Nothing [] Nothing [] []

-- | Every field filled in
pho :: Restaurant
pho =
    Restaurant
        Google
        (RestaurantId "ChIJ1")
        "Pho Real"
        (Just "https://maps.google.com/?cid=1")
        (Just 4.5)
        (Just Moderate)
        (Just "Vietnamese Restaurant")
        [ Photo (GooglePhoto "places/ChIJ1/photos/p1") (Just (Author "Sam" (Just "https://maps.google.com/contrib/1")))
        , Photo (PhotoUrl "https://example.com/pho.jpg") Nothing
        ]
        (Just (Summary "Big bowls of pho." (Just "Summarized with Gemini")))
        [ Review (Author "Kai" (Just "https://maps.google.com/contrib/2")) (Just 5) "Rich broth, fast service." (Just "a month ago")
        , Review (Author "Rae" Nothing) Nothing "Cash only." Nothing
        ]
        [("patio", True), ("veggie", False)]
        (Loc 48.43 (-123.35))

-- | A bare restaurant, from a source with nothing to say about it
fishStore :: Restaurant
fishStore = (plain OpenStreetMap "osm:node/1" (Loc 48.44 (-123.36))){name = "Fish Store"}
