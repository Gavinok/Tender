module Test.Main where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Tender.App (Message(..), Model(..), Screen(..), Swiper, update)
import Tender.Browser (sessionFromPath)
import Tender.Types (Restaurant, RestaurantId(..), SessionId(..), UserId(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

restaurant :: String -> Restaurant
restaurant id =
  { id: RestaurantId id
  , name: id
  , url: "https://example.com"
  , price: Nothing
  , rating: 4.0
  , imageLink: "https://example.com/img.jpg"
  , category: Nothing
  }

swiper :: Swiper
swiper = { user: UserId "u", loc: { latitude: 1.0, longitude: 2.0 }, matches: [] }

-- | The model after a message, and how many requests it started
step :: Model -> Message -> Tuple Model Int
step model msg = case update model msg of
  Tuple m effects -> Tuple m (length effects)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "update" do
    it "fetches the first restaurant once started" do
      step (QR (SessionId "s") "link") (Started (UserId "u") swiper.loc)
        `shouldEqual` Tuple (Active swiper Fetching) 1

    it "shows a restaurant" do
      step (Active swiper Fetching) (Show (restaurant "a"))
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 0

    it "records a match" do
      step (Active swiper (Swiping (restaurant "a"))) (GotMatch (restaurant "a"))
        `shouldEqual` Tuple (Active swiper { matches = [ restaurant "a" ] } (Matched (restaurant "a"))) 0

    it "sends a like while swiping" do
      step (Active swiper (Swiping (restaurant "a"))) Like
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 1

    it "sends a dislike while swiping" do
      step (Active swiper (Swiping (restaurant "a"))) Dislike
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 1

    it "ignores a like when not looking at a restaurant" do
      step (Active swiper Fetching) Like `shouldEqual` Tuple (Active swiper Fetching) 0

    it "moves on from a match" do
      step (Active swiper (Matched (restaurant "a"))) Continue
        `shouldEqual` Tuple (Active swiper (Matched (restaurant "a"))) 1

    it "shows when there are no restaurants left" do
      step (Active swiper (Swiping (restaurant "a"))) OutOfRestaurants
        `shouldEqual` Tuple (Active swiper NoneLeft) 0

    it "shows errors" do
      step Loading (FailedToLoad "nope") `shouldEqual` Tuple (ServerError "nope") 0

  describe "sessionFromPath" do
    it "is empty at the root" do
      sessionFromPath "/" `shouldEqual` Nothing
    it "reads the session" do
      sessionFromPath "/abc" `shouldEqual` Just (SessionId "abc")
    it "ignores a trailing slash" do
      sessionFromPath "/abc/" `shouldEqual` Just (SessionId "abc")
