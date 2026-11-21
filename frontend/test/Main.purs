module Test.Main where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Flame.Renderer.String (render)
import Effect (Effect)
import Tender.App (Lobby, Message(..), Model(..), Screen(..), Swiper, update)
import Tender.Browser (sessionFromPath)
import Tender.Types (Restaurant, RestaurantId(..), SessionId(..), UserId(..))
import Tender.View (view)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

restaurant :: String -> Restaurant
restaurant id =
  { source: "google"
  , id: RestaurantId id
  , name: id
  , url: Just "https://maps.google.com/?cid=1"
  , price: Nothing
  , rating: Just 4.0
  , category: Nothing
  , photo: Just { name: Just ("places/" <> id <> "/photos/p1"), url: Nothing, author: Just { name: "Sam", uri: Just "https://maps.google.com/contrib/1" } }
  }

-- | A place with nothing but a name, as Google sometimes returns
bare :: String -> Restaurant
bare id = (restaurant id) { url = Nothing, rating = Nothing, photo = Nothing }

swiper :: Swiper
swiper = { user: UserId "u", upcoming: [], matches: [], showMatches: false }

here :: { latitude :: Number, longitude :: Number }
here = { latitude: 1.0, longitude: 2.0 }

lobbyAt :: Lobby
lobbyAt = { session: SessionId "s", link: "http://example.com/s", copied: false }

-- | The model after a message, and how many requests it started
step :: Model -> Message -> Tuple Model Int
step model msg = case update model msg of
  Tuple m effects -> Tuple m (length effects)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "update" do
    it "fetches the restaurants once started" do
      step (QR lobbyAt) (Started (UserId "u") here)
        `shouldEqual` Tuple (Active swiper Fetching) 1

    it "shows the first restaurant and keeps the rest" do
      step (Active swiper Fetching) (Loaded [ restaurant "a", restaurant "b" ])
        `shouldEqual` Tuple (Active swiper { upcoming = [ restaurant "b" ] } (Swiping (restaurant "a"))) 0

    it "says there are none when the search finds nothing" do
      step (Active swiper Fetching) (Loaded []) `shouldEqual` Tuple (Active swiper NoneLeft) 0

    it "moves to the next restaurant without asking the server" do
      step (Active swiper { upcoming = [ restaurant "b" ] } (Swiping (restaurant "a"))) (Next (RestaurantId "a"))
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "b"))) 0

    it "ignores a late Next for a restaurant already passed" do
      step (Active swiper { upcoming = [ restaurant "c" ] } (Swiping (restaurant "b"))) (Next (RestaurantId "a"))
        `shouldEqual` Tuple (Active swiper { upcoming = [ restaurant "c" ] } (Swiping (restaurant "b"))) 0

    it "records a match" do
      step (Active swiper (Swiping (restaurant "a"))) (GotMatch (restaurant "a"))
        `shouldEqual` Tuple (Active swiper { matches = [ restaurant "a" ] } (Matched (restaurant "a"))) 1

    it "sends a like while swiping" do
      step (Active swiper (Swiping (restaurant "a"))) Like
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 1

    it "sends a dislike while swiping" do
      step (Active swiper (Swiping (restaurant "a"))) Dislike
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 1

    it "likes with the right arrow key" do
      step (Active swiper (Swiping (restaurant "a"))) (KeyPressed "ArrowRight")
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 1

    it "ignores other keys" do
      step (Active swiper (Swiping (restaurant "a"))) (KeyPressed "x")
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 0

    it "ignores a like when not looking at a restaurant" do
      step (Active swiper Fetching) Like `shouldEqual` Tuple (Active swiper Fetching) 0

    it "moves on from a match" do
      step (Active swiper { upcoming = [ restaurant "b" ] } (Matched (restaurant "a"))) Continue
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "b"))) 0

    it "shows the link was copied, then hides it again" do
      step (QR lobbyAt) LinkCopied `shouldEqual` Tuple (QR lobbyAt { copied = true }) 1
      step (QR lobbyAt { copied = true }) HideCopied `shouldEqual` Tuple (QR lobbyAt) 0

    it "opens and closes the list of matches" do
      step (Active swiper (Swiping (restaurant "a"))) ToggleMatches
        `shouldEqual` Tuple (Active swiper { showMatches = true } (Swiping (restaurant "a"))) 0

    it "shows when there are no restaurants left" do
      step (Active swiper (Swiping (restaurant "a"))) (Next (RestaurantId "a"))
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

  describe "view" do
    let
      renders model text = do
        html <- liftEffect $ render (view model)
        html `shouldSatisfy` contains (Pattern text)
    it "shows a local QR code and the share link in the lobby" do
      renders (QR lobbyAt) "src=\"data:image/svg+xml"
      renders (QR lobbyAt) "http://example.com/s"
    it "shows the restaurant while swiping" do
      renders (Active swiper (Swiping (restaurant "Pho Real"))) "Pho Real"
      renders (Active swiper (Swiping (restaurant "Pho Real"))) "aria-label=\"0 matches\""
    it "shows the photo through the backend with its credit and a Google Maps link" do
      renders (Active swiper (Swiping (restaurant "a"))) "src=\"/api/photo?name=places/a/photos/p1\""
      renders (Active swiper (Swiping (restaurant "a"))) "Photo: "
      renders (Active swiper (Swiping (restaurant "a"))) ">Google Maps<"
    it "credits OpenStreetMap for its places" do
      renders (Active swiper (Swiping ((bare "Pho Real") { source = "openstreetmap", url = Just "https://www.openstreetmap.org/node/1" }))) "© OpenStreetMap contributors"
      renders (Active swiper (Swiping ((bare "Pho Real") { source = "openstreetmap", url = Just "https://www.openstreetmap.org/node/1" }))) ">OpenStreetMap<"
    it "loads Yelp photos straight from their url" do
      renders (Active swiper (Swiping ((restaurant "a") { source = "yelp", photo = Just { name: Nothing, url: Just "https://s3-media1.fl.yelpcdn.com/a.jpg", author: Nothing } }))) "src=\"https://s3-media1.fl.yelpcdn.com/a.jpg\""
    it "copes with a place that has no photo, rating or link" do
      renders (Active swiper (Swiping (bare "Mystery Diner"))) "Mystery Diner"
    it "celebrates a match" do
      renders (Active swiper (Matched (restaurant "a"))) "a match!"
    it "offers to try again after an error" do
      renders (ServerError "Could not reach the server") "Try again"
    it "says when there is nothing left" do
      renders (Active swiper NoneLeft) "everything nearby"
