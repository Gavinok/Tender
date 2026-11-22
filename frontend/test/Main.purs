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
  , photos:
      [ { name: Just ("places/" <> id <> "/photos/p1"), url: Nothing, author: Just { name: "Sam", uri: Just "https://maps.google.com/contrib/1" } }
      , { name: Just ("places/" <> id <> "/photos/p2"), url: Nothing, author: Just { name: "Kai", uri: Nothing } }
      ]
  }

-- | A place with nothing but a name, as Google sometimes returns
bare :: String -> Restaurant
bare id = (restaurant id) { url = Nothing, rating = Nothing, photos = [] }

swiper :: Swiper
swiper = { user: UserId "u", upcoming: [], photo: 0, othersLiked: [], matches: [], showMatches: false, vibe: [] }

here :: { latitude :: Number, longitude :: Number }
here = { latitude: 1.0, longitude: 2.0 }

lobbyAt :: Lobby
lobbyAt = { session: SessionId "s", link: "http://example.com/s", copied: false, vibe: { chips: [], locked: false } }

decided :: String -> Boolean -> Array String -> Message
decided id match others = Decided { id: RestaurantId id, match, othersLiked: map RestaurantId others }

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
      step (Active swiper Fetching) (Loaded { restaurants: [ restaurant "a", restaurant "b" ], othersLiked: [], vibe: [] })
        `shouldEqual` Tuple (Active swiper { upcoming = [ restaurant "b" ] } (Swiping (restaurant "a"))) 0

    it "starts with what others already liked" do
      step (Active swiper Fetching) (Loaded { restaurants: [ restaurant "a", restaurant "b", restaurant "c" ], othersLiked: [ RestaurantId "c" ], vibe: [] })
        `shouldEqual` Tuple (Active swiper { upcoming = [ restaurant "a", restaurant "b" ], othersLiked = [ RestaurantId "c" ] } (Swiping (restaurant "c"))) 0

    it "says there are none when the search finds nothing" do
      step (Active swiper Fetching) (Loaded { restaurants: [], othersLiked: [], vibe: [] }) `shouldEqual` Tuple (Active swiper NoneLeft) 0

    it "moves to the next restaurant without asking the server" do
      step (Active swiper { upcoming = [ restaurant "b" ] } (Swiping (restaurant "a"))) (decided "a" false [])
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "b"))) 0

    it "moves restaurants others just liked to the front" do
      step (Active swiper { upcoming = [ restaurant "b", restaurant "c", restaurant "d" ] } (Swiping (restaurant "a"))) (decided "a" false [ "d" ])
        `shouldEqual` Tuple (Active swiper { upcoming = [ restaurant "b", restaurant "c" ], othersLiked = [ RestaurantId "d" ] } (Swiping (restaurant "d"))) 0

    it "ignores a late decision for a restaurant already passed" do
      step (Active swiper { upcoming = [ restaurant "c" ] } (Swiping (restaurant "b"))) (decided "a" false [])
        `shouldEqual` Tuple (Active swiper { upcoming = [ restaurant "c" ] } (Swiping (restaurant "b"))) 0

    it "records a match" do
      step (Active swiper (Swiping (restaurant "a"))) (decided "a" true [])
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

    it "flips through a card's photos without going past either end" do
      step (Active swiper (Swiping (restaurant "a"))) NextPhoto `shouldEqual` Tuple (Active swiper { photo = 1 } (Swiping (restaurant "a"))) 0
      step (Active swiper { photo = 1 } (Swiping (restaurant "a"))) NextPhoto `shouldEqual` Tuple (Active swiper { photo = 1 } (Swiping (restaurant "a"))) 0
      step (Active swiper (Swiping (restaurant "a"))) PreviousPhoto `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 0

    it "starts each new card on its first photo" do
      step (Active swiper { photo = 1, upcoming = [ restaurant "b" ] } (Swiping (restaurant "a"))) (decided "a" false [])
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "b"))) 0

    it "toggles a vibe chip and saves it" do
      step (QR lobbyAt) (ToggleChip "patio") `shouldEqual` Tuple (QR lobbyAt { vibe = { chips: [ "patio" ], locked: false } }) 1
      step (QR lobbyAt { vibe = { chips: [ "patio" ], locked: false } }) (ToggleChip "patio") `shouldEqual` Tuple (QR lobbyAt) 1

    it "doesn't change chips once swiping has started" do
      let locked = lobbyAt { vibe = { chips: [ "patio" ], locked: true } }
      step (QR locked) (ToggleChip "drinks") `shouldEqual` Tuple (QR locked) 0

    it "shows the vibe others picked and checks again later" do
      step (QR lobbyAt) (VibePolled { chips: [ "drinks" ], locked: false })
        `shouldEqual` Tuple (QR lobbyAt { vibe = { chips: [ "drinks" ], locked: false } }) 1

    it "stops checking the vibe once swiping" do
      step (Active swiper Fetching) PollVibe `shouldEqual` Tuple (Active swiper Fetching) 0

    it "keeps the session's vibe for the swiping screen" do
      step (Active swiper Fetching) (Loaded { restaurants: [ restaurant "a" ], othersLiked: [], vibe: [ "patio" ] })
        `shouldEqual` Tuple (Active swiper { vibe = [ "patio" ] } (Swiping (restaurant "a"))) 0

    it "opens and closes the list of matches" do
      step (Active swiper (Swiping (restaurant "a"))) ToggleMatches
        `shouldEqual` Tuple (Active swiper { showMatches = true } (Swiping (restaurant "a"))) 0

    it "shows when there are no restaurants left" do
      step (Active swiper (Swiping (restaurant "a"))) (decided "a" false [])
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
    it "shows vibe chips in the lobby, marking the chosen ones" do
      renders (QR lobbyAt { vibe = { chips: [ "patio" ], locked: false } }) "aria-pressed=\"true\">☀️ Patio<"
      renders (QR lobbyAt { vibe = { chips: [ "patio" ], locked: true } }) "Locked in"
    it "shows a local QR code and the share link in the lobby" do
      renders (QR lobbyAt) "src=\"data:image/svg+xml"
      renders (QR lobbyAt) "http://example.com/s"
    it "shows the restaurant while swiping" do
      renders (Active swiper (Swiping (restaurant "Pho Real"))) "Pho Real"
      renders (Active swiper (Swiping (restaurant "Pho Real"))) "aria-label=\"0 matches\""
    it "marks restaurants someone else liked" do
      renders (Active swiper { othersLiked = [ RestaurantId "a" ] } (Swiping (restaurant "a"))) "Someone&#39;s keen"
    it "shows the photo through the backend with its credit and a Google Maps link" do
      renders (Active swiper (Swiping (restaurant "a"))) "src=\"/api/photo?name=places/a/photos/p1\""
      renders (Active swiper (Swiping (restaurant "a"))) "Photo: "
      renders (Active swiper (Swiping (restaurant "a"))) ">Google Maps<"
    it "credits OpenStreetMap for its places" do
      renders (Active swiper (Swiping ((bare "Pho Real") { source = "openstreetmap", url = Just "https://www.openstreetmap.org/node/1" }))) "© OpenStreetMap contributors"
      renders (Active swiper (Swiping ((bare "Pho Real") { source = "openstreetmap", url = Just "https://www.openstreetmap.org/node/1" }))) ">OpenStreetMap<"
    it "shows the photo picked, its own credit, and a bar per photo" do
      renders (Active swiper { photo = 1 } (Swiping (restaurant "a"))) "src=\"/api/photo?name=places/a/photos/p2\""
      renders (Active swiper { photo = 1 } (Swiping (restaurant "a"))) "Photo: Kai"
      renders (Active swiper { photo = 1 } (Swiping (restaurant "a"))) "aria-label=\"Photo 2 of 2\""
    it "loads Yelp photos straight from their url" do
      renders (Active swiper (Swiping ((restaurant "a") { source = "yelp", photos = [ { name: Nothing, url: Just "https://s3-media1.fl.yelpcdn.com/a.jpg", author: Nothing } ] }))) "src=\"https://s3-media1.fl.yelpcdn.com/a.jpg\""
    it "copes with a place that has no photo, rating or link" do
      renders (Active swiper (Swiping (bare "Mystery Diner"))) "Mystery Diner"
    it "celebrates a match" do
      renders (Active swiper (Matched (restaurant "a"))) "a match!"
    it "offers to try again after an error" do
      renders (ServerError "Could not reach the server") "Try again"
    it "says when there is nothing left" do
      renders (Active swiper NoneLeft) "everything nearby"
