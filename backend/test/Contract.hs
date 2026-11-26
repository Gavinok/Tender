-- | The JSON contract with the frontend, both ways.
--
-- Responses: each endpoint's JSON is pinned in .golden/<name>/golden, encoded
-- from the same types the handlers send (Wire). The frontend's tests decode
-- these files with its own decoders. If a change is meant, delete the file
-- and run the tests again.
--
-- Requests: the frontend's tests pin what it sends in
-- frontend/.golden/<name>/golden, and here they're decoded with the handlers'
-- own FromJSON instances. Optional fields are compared too, since a renamed
-- one would otherwise decode as missing.
module Contract (spec) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.UUID qualified as UUID
import Test.Hspec
import Test.Hspec.Golden (Golden, defaultGolden)

import Presence (Activity (..), Person (..))
import Restaurant
import User (SessionId (..), UserId (..))
import Vibe (Chip (..), Found (..), Place (..), allChips)
import Wire

-- | A response's JSON, pinned. Decoded as UTF-8, since labels have emoji.
goldenJSON :: (ToJSON a) => String -> a -> Golden String
goldenJSON name = defaultGolden name . TL.unpack . TLE.decodeUtf8 . encodePretty

-- | A request the frontend pinned, read as the handler reads it
decodesTo :: (FromJSON a, Eq a, Show a) => String -> a -> Expectation
decodesTo name expected =
    eitherDecodeFileStrict ("../frontend/.golden/" <> name <> "/golden") `shouldReturn` Right expected

spec :: Spec
spec = do
    describe "responses the frontend reads" $ do
        it "the session and user ids" $ goldenJSON "ids-response" (SessionId UUID.nil, someone)
        it "restaurants" $ goldenJSON "restaurants-response" (RestaurantsResponse [pho, fishStore] [restaurantId pho] [Patio, Veggie] 5000 50000)
        it "a decision" $ goldenJSON "decision-response" (DecisionResponse [restaurantId pho] [restaurantId fishStore])
        it "who's in the session" $ goldenJSON "presence-response" (PresenceResponse everyStatus [restaurantId pho])
        it "the lobby" $ goldenJSON "lobby-response" [Lobby [] False Nothing, Lobby allChips True (Just fernwood)]
        it "place lookups" $ goldenJSON "places-response" [Found fernwood "Fernwood, Victoria, BC, Canada"]
        it "errors" $ goldenJSON "error-response" (ErrorResponse "Unknown user")

    describe "requests the frontend sends" $ do
        it "restaurants, with a location and searching further" $
            "restaurants-request" `decodesTo` RestaurantsRequest someone (Just (Loc 48.43 (-123.35))) (Just 5000)
        it "restaurants, without a location" $
            "restaurants-request-without-location" `decodesTo` RestaurantsRequest someone Nothing Nothing
        it "a decision" $ "decision-request" `decodesTo` DecisionRequest someone (restaurantId pho) True
        it "a heartbeat" $ "presence-request" `decodesTo` PresenceRequest someone
        it "the vibe" $ "vibe-request" `decodesTo` VibeRequest ["patio", "veggie"]
        it "where they're eating" $ "place-request" `decodesTo` PlaceRequest (Just fernwood)
        it "back to their own locations" $ "place-request-clear" `decodesTo` PlaceRequest Nothing
  where
    someone = UserId UUID.nil
    fernwood = Place "Fernwood" (Loc 48.43 (-123.34))
    -- Every status, so a new one reaches the frontend's tests
    everyStatus = zipWith (\i a -> Person (i == (0 :: Int)) a 3 (cardsLeft a)) [0 ..] [minBound .. maxBound]
    -- A match on every status, so adding one warns here until it has a sample
    cardsLeft = \case
        Swiping -> Just 12
        Done -> Just 0
        Away -> Nothing
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
            (Loc 48.43 (-123.35))
    fishStore = Restaurant OpenStreetMap (RestaurantId "osm:node/1") "Fish Store" Nothing Nothing Nothing Nothing [] Nothing (Loc 48.44 (-123.36))
