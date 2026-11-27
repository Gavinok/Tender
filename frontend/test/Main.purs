module Test.Main where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..), fst)
import Effect.Class (liftEffect)
import Flame.Renderer.String (render)
import Effect (Effect)
import Tender.App (Lobby, Message(..), Model(..), Picker, Screen(..), Swiper, locationText, update)
import Tender.Browser (LocationProblem(..), sessionFromPath)
import Tender.Types (Found, Person, Place, Restaurant, RestaurantId(..), SessionId(..), UserId(..))
import Tender.Types (Status(..)) as Status
import Tender.View (view)
import Tender.Vibe (Chip)
import Test.Contract as Contract
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
  , moreCategories: []
  , address: Nothing
  , openNow: Nothing
  , distance: Nothing
  , photos:
      [ { name: Just ("places/" <> id <> "/photos/p1"), url: Nothing, author: Just { name: "Sam", uri: Just "https://maps.google.com/contrib/1" } }
      , { name: Just ("places/" <> id <> "/photos/p2"), url: Nothing, author: Just { name: "Kai", uri: Nothing } }
      ]
  , summary: Just { text: "Bright noodle spot known for its broth.", disclosure: Just "Summarized with Gemini" }
  , reviews:
      [ { author: { name: "Kai", uri: Just "https://maps.google.com/contrib/2" }, rating: Just 5.0, text: "Rich broth, fast service.", when: Just "a month ago" }
      , { author: { name: "Rae", uri: Nothing }, rating: Nothing, text: "Cash only.", when: Nothing }
      ]
  , confirmed: [ "patio" ]
  }

-- | A place with nothing but a name, as Google sometimes returns
bare :: String -> Restaurant
bare id = (restaurant id) { url = Nothing, rating = Nothing, photos = [], summary = Nothing, reviews = [], confirmed = [] }

swiper :: Swiper
swiper = { user: UserId "u", loc: Just here, radius: 5000.0, maxRadius: 50000.0, upcoming: [], photo: 0, summaryOpen: false, othersLiked: [], liked: [], matches: [], people: [], heartbeatIn: 10, showMatches: false, warningDismissed: false, vibe: [] }

here :: { latitude :: Number, longitude :: Number }
here = { latitude: 1.0, longitude: 2.0 }

lobbyAt :: Lobby
lobbyAt = { session: SessionId "s", link: "http://example.com/s", copied: false, vibe: { chips: [], choices: [ patio, veggie ], locked: false, place: Nothing }, picker: Nothing }

patio :: Chip
patio = { id: "patio", label: "☀️ Patio" }

veggie :: Chip
veggie = { id: "veggie", label: "🥦 Veggie" }

blankPicker :: Picker
blankPicker = { query: "", results: Nothing, searching: false, problem: Nothing }

kelowna :: Found
kelowna = { name: "Kelowna", address: "Kelowna, BC, Canada", latitude: 49.88, longitude: -119.49 }

kelownaPlace :: Place
kelownaPlace = { name: "Kelowna", latitude: 49.88, longitude: -119.49 }

-- | The user and a friend who has finished
me :: Person
me = { you: true, status: Status.Swiping, swiped: 3, left: Just 12 }

friend :: Person
friend = { you: false, status: Status.Done, swiped: 20, left: Just 0 }

-- | A decision saved, with what others liked and no matches
decided :: String -> Boolean -> Array String -> Message
decided id liked others = Decided { id: RestaurantId id, liked, matches: [], othersLiked: map RestaurantId others }

-- | A like saved, with the session's matches this person is part of
likedWithMatches :: String -> Array String -> Message
likedWithMatches id matches = Decided { id: RestaurantId id, liked: true, matches: map RestaurantId matches, othersLiked: [] }

withMatches :: Array String -> Message -> Message
withMatches ids = case _ of
  Decided d -> Decided d { matches = map RestaurantId ids }
  m -> m

-- | The model after a message, and how many requests it started
step :: Model -> Message -> Tuple Model Int
step model msg = case update model msg of
  Tuple m effects -> Tuple m (length effects)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Contract.spec

  describe "update" do
    it "fetches the restaurants once started" do
      step (QR lobbyAt) (Started (SessionId "s") (UserId "u") (Right here))
        `shouldEqual` Tuple (Active swiper { radius = 0.0, maxRadius = 0.0 } Fetching) 2

    it "shows the first restaurant and keeps the rest" do
      step (Active swiper Fetching) (Loaded { restaurants: [ restaurant "a", restaurant "b" ], othersLiked: [], vibe: [], radius: 5000.0, maxRadius: 50000.0 })
        `shouldEqual` Tuple (Active swiper { upcoming = [ restaurant "b" ] } (Swiping (restaurant "a"))) 0

    it "starts with what others already liked" do
      step (Active swiper Fetching) (Loaded { restaurants: [ restaurant "a", restaurant "b", restaurant "c" ], othersLiked: [ RestaurantId "c" ], vibe: [], radius: 5000.0, maxRadius: 50000.0 })
        `shouldEqual` Tuple (Active swiper { upcoming = [ restaurant "a", restaurant "b" ], othersLiked = [ RestaurantId "c" ] } (Swiping (restaurant "c"))) 0

    it "says there are none when the search finds nothing" do
      step (Active swiper Fetching) (Loaded { restaurants: [], othersLiked: [], vibe: [], radius: 5000.0, maxRadius: 50000.0 }) `shouldEqual` Tuple (Active swiper NoneLeft) 0

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
      step (Active swiper (Swiping (restaurant "a"))) (likedWithMatches "a" [ "a" ])
        `shouldEqual` Tuple (Active swiper { liked = [ restaurant "a" ], matches = [ restaurant "a" ] } (Matched (restaurant "a"))) 1

    it "remembers a like that isn't a match yet" do
      step (Active swiper { upcoming = [ restaurant "b" ] } (Swiping (restaurant "a"))) (likedWithMatches "a" [])
        `shouldEqual` Tuple (Active swiper { liked = [ restaurant "a" ] } (Swiping (restaurant "b"))) 0

    it "celebrates a match someone else completed after this person liked it" do
      step (Active swiper { liked = [ restaurant "a" ] } (Swiping (restaurant "b"))) (decided "b" false [] # withMatches [ "a" ])
        `shouldEqual` Tuple (Active swiper { liked = [ restaurant "a" ], matches = [ restaurant "a" ] } (Matched (restaurant "a"))) 1

    it "doesn't celebrate a match twice" do
      step (Active swiper { liked = [ restaurant "a" ], matches = [ restaurant "a" ], upcoming = [ restaurant "c" ] } (Swiping (restaurant "b"))) (decided "b" false [] # withMatches [ "a" ])
        `shouldEqual` Tuple (Active swiper { liked = [ restaurant "a" ], matches = [ restaurant "a" ] } (Swiping (restaurant "c"))) 0

    it "sends a like while swiping" do
      step (Active swiper (Swiping (restaurant "a"))) Like
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 1

    it "sends a dislike while swiping" do
      step (Active swiper (Swiping (restaurant "a"))) Dislike
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 1

    it "likes with the right arrow key" do
      step (Active swiper (Swiping (restaurant "a"))) (KeyPressed "ArrowRight")
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 1

    it "closes the open dialog with Escape, the celebration before the matches" do
      fst (step (Active swiper { upcoming = [ restaurant "b" ] } (Matched (restaurant "a"))) (KeyPressed "Escape"))
        `shouldEqual` Active swiper { upcoming = [] } (Swiping (restaurant "b"))
      fst (step (Active swiper { showMatches = true } (Swiping (restaurant "a"))) (KeyPressed "Escape"))
        `shouldEqual` Active swiper (Swiping (restaurant "a"))
      fst (step (Active swiper { summaryOpen = true } (Swiping (restaurant "a"))) (KeyPressed "Escape"))
        `shouldEqual` Active swiper (Swiping (restaurant "a"))

    it "stops showing the location warning once dismissed" do
      step (Active swiper { loc = Nothing } (Swiping (restaurant "a"))) DismissWarning
        `shouldEqual` Tuple (Active swiper { loc = Nothing, warningDismissed = true } (Swiping (restaurant "a"))) 0

    it "ignores other keys" do
      step (Active swiper (Swiping (restaurant "a"))) (KeyPressed "x")
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 0

    it "ignores a like when not looking at a restaurant" do
      step (Active swiper Fetching) Like `shouldEqual` Tuple (Active swiper Fetching) 0

    it "moves on from a match" do
      step (Active swiper { upcoming = [ restaurant "b" ] } (Matched (restaurant "a"))) Continue
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "b"))) 0

    it "searches from the session's location when the user's couldn't be found" do
      step Loading (Started (SessionId "s") (UserId "u") (Left NotAllowed))
        `shouldEqual` Tuple (Active swiper { loc = Nothing, radius = 0.0, maxRadius = 0.0 } Fetching) 2

    it "asks for the location again, as the same user, when there's none to search from" do
      step (Active swiper Fetching) (LocationFailed (SessionId "s") (UserId "u") Blocked "Nobody else has one.")
        `shouldEqual` Tuple (NeedLocation { session: SessionId "s", user: UserId "u", message: locationText Blocked <> " Nobody else has one.", picker: Nothing }) 0
      step (NeedLocation { session: SessionId "s", user: UserId "u", message: "x", picker: Nothing }) RetryLocation `shouldEqual` Tuple Loading 1
      step (QR lobbyAt) RetryLocation `shouldEqual` Tuple (QR lobbyAt) 0

    it "says what to do for each way finding the location can fail" do
      locationText Blocked `shouldSatisfy` contains (Pattern "address bar")
      locationText DeviceOff `shouldSatisfy` contains (Pattern "device's settings")
      locationText NotAllowed `shouldSatisfy` contains (Pattern "allow it when your browser asks")
      locationText TimedOut `shouldSatisfy` contains (Pattern "too long")

    it "opens the picker in the lobby until it's locked" do
      step (QR lobbyAt) OpenPicker `shouldEqual` Tuple (QR lobbyAt { picker = Just blankPicker }) 0
      step (QR lobbyAt { vibe { locked = true } }) OpenPicker `shouldEqual` Tuple (QR lobbyAt { vibe { locked = true } }) 0
      step (QR lobbyAt { picker = Just blankPicker }) ClosePicker `shouldEqual` Tuple (QR lobbyAt) 0

    it "looks up what was typed once it's long enough" do
      step (QR lobbyAt { picker = Just blankPicker }) (PlaceQuery "K")
        `shouldEqual` Tuple (QR lobbyAt { picker = Just blankPicker { query = "K" } }) 0
      -- The backend says if it's too short, so even one letter is sent
      step (QR lobbyAt { picker = Just blankPicker { query = "K" } }) FindPlaces
        `shouldEqual` Tuple (QR lobbyAt { picker = Just blankPicker { query = "K", searching = true } }) 1
      step (QR lobbyAt { picker = Just blankPicker { query = "Kelowna" } }) FindPlaces
        `shouldEqual` Tuple (QR lobbyAt { picker = Just blankPicker { query = "Kelowna", searching = true } }) 1
      step (QR lobbyAt { picker = Just blankPicker { searching = true } }) (PlacesFound (Right [ kelowna ]))
        `shouldEqual` Tuple (QR lobbyAt { picker = Just blankPicker { results = Just [ kelowna ] } }) 0

    it "shares the chosen place with the session, and can go back to locations" do
      step (QR lobbyAt { picker = Just blankPicker }) (ChoosePlace kelowna)
        `shouldEqual` Tuple (QR lobbyAt { vibe { place = Just kelownaPlace } }) 1
      step (QR lobbyAt { vibe { place = Just kelownaPlace } }) ClearPlace `shouldEqual` Tuple (QR lobbyAt) 1

    it "starts from a chosen place when there's no location" do
      step (NeedLocation { session: SessionId "s", user: UserId "u", message: "x", picker: Nothing }) OpenPicker
        `shouldEqual` Tuple (NeedLocation { session: SessionId "s", user: UserId "u", message: "x", picker: Just blankPicker }) 0
      step (NeedLocation { session: SessionId "s", user: UserId "u", message: "x", picker: Just blankPicker }) (ChoosePlace kelowna)
        `shouldEqual` Tuple Loading 1

    it "leaves for a new session from any screen" do
      step (Active swiper (Swiping (restaurant "a"))) NewSession `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 1
      step (QR lobbyAt) NewSession `shouldEqual` Tuple (QR lobbyAt) 1
      step (ServerError "x") NewSession `shouldEqual` Tuple (ServerError "x") 1

    it "shows the link was copied, then hides it again" do
      step (QR lobbyAt) LinkCopied `shouldEqual` Tuple (QR lobbyAt { copied = true }) 1
      step (QR lobbyAt { copied = true }) HideCopied `shouldEqual` Tuple (QR lobbyAt) 0

    it "flips through a card's photos without going past either end" do
      step (Active swiper (Swiping (restaurant "a"))) NextPhoto `shouldEqual` Tuple (Active swiper { photo = 1 } (Swiping (restaurant "a"))) 0
      step (Active swiper { photo = 1 } (Swiping (restaurant "a"))) NextPhoto `shouldEqual` Tuple (Active swiper { photo = 1 } (Swiping (restaurant "a"))) 0
      step (Active swiper (Swiping (restaurant "a"))) PreviousPhoto `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 0

    it "opens and closes the summary, closed again on the next card" do
      step (Active swiper (Swiping (restaurant "a"))) ToggleSummary `shouldEqual` Tuple (Active swiper { summaryOpen = true } (Swiping (restaurant "a"))) 0
      step (Active swiper { summaryOpen = true, upcoming = [ restaurant "b" ] } (Swiping (restaurant "a"))) (decided "a" false [])
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "b"))) 0

    it "starts each new card on its first photo" do
      step (Active swiper { photo = 1, upcoming = [ restaurant "b" ] } (Swiping (restaurant "a"))) (decided "a" false [])
        `shouldEqual` Tuple (Active swiper (Swiping (restaurant "b"))) 0

    it "toggles a vibe chip and saves it" do
      step (QR lobbyAt) (ToggleChip "patio") `shouldEqual` Tuple (QR lobbyAt { vibe = { chips: [ "patio" ], choices: [ patio, veggie ], locked: false, place: Nothing } }) 1
      step (QR lobbyAt { vibe = { chips: [ "patio" ], choices: [ patio, veggie ], locked: false, place: Nothing } }) (ToggleChip "patio") `shouldEqual` Tuple (QR lobbyAt) 1

    it "doesn't change chips once swiping has started" do
      let locked = lobbyAt { vibe = { chips: [ "patio" ], choices: [ patio, veggie ], locked: true, place: Nothing } }
      step (QR locked) (ToggleChip "drinks") `shouldEqual` Tuple (QR locked) 0

    it "shows the vibe others picked and checks again later" do
      step (QR lobbyAt) (VibePolled { chips: [ "drinks" ], choices: [ patio, veggie ], locked: false, place: Nothing })
        `shouldEqual` Tuple (QR lobbyAt { vibe = { chips: [ "drinks" ], choices: [ patio, veggie ], locked: false, place: Nothing } }) 1

    it "stops checking the vibe once swiping" do
      step (Active swiper Fetching) PollVibe `shouldEqual` Tuple (Active swiper Fetching) 0

    it "keeps the session's vibe for the swiping screen" do
      step (Active swiper Fetching) (Loaded { restaurants: [ restaurant "a" ], othersLiked: [], vibe: [ patio ], radius: 5000.0, maxRadius: 50000.0 })
        `shouldEqual` Tuple (Active swiper { vibe = [ patio ] } (Swiping (restaurant "a"))) 0

    it "searches further from the out of restaurants screen" do
      step (Active swiper NoneLeft) SearchFurther `shouldEqual` Tuple (Active swiper Fetching) 1

    it "doesn't search further than the furthest Google goes" do
      step (Active swiper { radius = 50000.0 } NoneLeft) SearchFurther `shouldEqual` Tuple (Active swiper { radius = 50000.0 } NoneLeft) 0

    it "keeps the wider radius it was given" do
      step (Active swiper Fetching) (Loaded { restaurants: [ restaurant "z" ], othersLiked: [], vibe: [], radius: 10000.0, maxRadius: 50000.0 })
        `shouldEqual` Tuple (Active swiper { radius = 10000.0 } (Swiping (restaurant "z"))) 0

    it "opens and closes the list of matches" do
      step (Active swiper (Swiping (restaurant "a"))) ToggleMatches
        `shouldEqual` Tuple (Active swiper { showMatches = true } (Swiping (restaurant "a"))) 0

    it "sends a heartbeat from any screen, and keeps the people it hears back" do
      step (Active swiper (Swiping (restaurant "a"))) Heartbeat `shouldEqual` Tuple (Active swiper (Swiping (restaurant "a"))) 1
      step (Active swiper NoneLeft) Heartbeat `shouldEqual` Tuple (Active swiper NoneLeft) 1
      step (Active swiper (Swiping (restaurant "a"))) (HeardBack (Just { people: [ friend ], matches: [], next: 10 }))
        `shouldEqual` Tuple (Active swiper { people = [ friend ] } (Swiping (restaurant "a"))) 1

    it "waits as long as the backend says before the next heartbeat" do
      step (Active swiper NoneLeft) (HeardBack (Just { people: [], matches: [], next: 5 }))
        `shouldEqual` Tuple (Active swiper { heartbeatIn = 5 } NoneLeft) 1

    it "keeps the last interval when a heartbeat fails" do
      step (Active swiper { heartbeatIn = 5 } NoneLeft) (HeardBack Nothing)
        `shouldEqual` Tuple (Active swiper { heartbeatIn = 5 } NoneLeft) 1

    it "keeps going when a heartbeat fails" do
      step (Active swiper { people = [ friend ] } NoneLeft) (HeardBack Nothing)
        `shouldEqual` Tuple (Active swiper { people = [ friend ] } NoneLeft) 1

    it "celebrates a match heard about while out of cards" do
      step (Active swiper { liked = [ restaurant "a" ] } NoneLeft) (HeardBack (Just { people: [], matches: [ RestaurantId "a" ], next: 10 }))
        `shouldEqual` Tuple (Active swiper { liked = [ restaurant "a" ], matches = [ restaurant "a" ] } (Matched (restaurant "a"))) 2

    it "leaves matches heard about while swiping to the next decision" do
      step (Active swiper { liked = [ restaurant "a" ] } (Swiping (restaurant "b"))) (HeardBack (Just { people: [], matches: [ RestaurantId "a" ], next: 10 }))
        `shouldEqual` Tuple (Active swiper { liked = [ restaurant "a" ] } (Swiping (restaurant "b"))) 1

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
      rendersNot model text = do
        html <- liftEffect $ render (view model)
        html `shouldSatisfy` (not <<< contains (Pattern text))
    it "shows the category and the other kinds of place on a card" do
      renders (Active swiper (Swiping (restaurant "a") { category = Just "Pub", moreCategories = [ "Bar", "Cafe" ] })) "Pub</span><span class=\"rounded-lg bg-white/20 px-3 py-1 text-sm font-bold text-white backdrop-blur\">Bar"
    it "shows how far away a place is, whether it's open, and where" do
      renders (Active swiper (Swiping (restaurant "a") { distance = Just 1440.0, openNow = Just true, address = Just "1 Main St" })) "1.4 km away · Open now · 1 Main St"
      renders (Active swiper (Swiping (restaurant "a") { distance = Just 850.0, openNow = Just false })) "850 m away · Closed now"
    it "shows the location warning with a way to dismiss it, until it's dismissed" do
      renders (Active swiper { loc = Nothing } (Swiping (restaurant "a"))) "aria-label=\"Dismiss\""
      html <- liftEffect $ render (view (Active swiper { loc = Nothing, warningDismissed = true } (Swiping (restaurant "a"))))
      html `shouldSatisfy` (not <<< contains (Pattern "get your location"))
    it "shows the rating and price of each match in the list" do
      renders (Active swiper { showMatches = true, matches = [ (restaurant "a") { price = Just "$$" } ] } (Swiping (restaurant "b"))) "Rated 4.0 out of 5\">★ 4.0</span><span>$$"
    it "shows vibe chips in the lobby, marking the chosen ones" do
      renders (QR lobbyAt { vibe = { chips: [ "patio" ], choices: [ patio, veggie ], locked: false, place: Nothing } }) "aria-pressed=\"true\">☀️ Patio<"
      renders (QR lobbyAt { vibe = { chips: [ "patio" ], choices: [ patio, veggie ], locked: true, place: Nothing } }) "Locked in"
    it "shows a local QR code in the lobby, without the unreadable link under it" do
      renders (QR lobbyAt) "src=\"data:image/svg+xml"
      renders (QR lobbyAt) "Share invite"
    it "shows the restaurant while swiping" do
      renders (Active swiper (Swiping (restaurant "Pho Real"))) "Pho Real"
      renders (Active swiper (Swiping (restaurant "Pho Real"))) "aria-label=\"0 matches\""
      renders (Active swiper (Swiping (restaurant "Pho Real"))) "aria-label=\"Start a new session\""
      renders (QR lobbyAt) "aria-label=\"Start a new session\""
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
    it "badges the chosen chips Google confirms, and only those" do
      let
        chips = [ { id: "patio", label: "Patio" }, { id: "veggie", label: "Veggie" } ]
        shown = Active swiper { vibe = chips } (Swiping (restaurant "a"))
      renders shown "backdrop-blur\">Patio"
      rendersNot shown "backdrop-blur\">Veggie"
      rendersNot (Active swiper (Swiping (restaurant "a"))) "backdrop-blur\">Patio"
    it "links each card to a search for its menu" do
      renders (Active swiper (Swiping (restaurant "a"))) "search?q=a%20menu"
    it "shows the reviews under the summary when it's opened, crediting their authors" do
      let open = Active swiper { summaryOpen = true } (Swiping (restaurant "a"))
      renders open "Rich broth, fast service."
      renders open "a month ago"
      renders open "https://maps.google.com/contrib/2"
      renders open "Cash only."
      rendersNot (Active swiper (Swiping (restaurant "a"))) "Rich broth, fast service."
      rendersNot (Active swiper { summaryOpen = true } (Swiping (bare "a") { summary = Just { text: "x", disclosure: Nothing } })) "What people said"

    it "previews the summary and shows it in full with Google's disclosure when opened" do
      renders (Active swiper (Swiping (restaurant "a"))) "line-clamp-2\">Bright noodle spot"
      renders (Active swiper { summaryOpen = true } (Swiping (restaurant "a"))) "Summarized with Gemini"
    it "loads Yelp photos straight from their url" do
      renders (Active swiper (Swiping ((restaurant "a") { source = "yelp", photos = [ { name: Nothing, url: Just "https://s3-media1.fl.yelpcdn.com/a.jpg", author: Nothing } ] }))) "src=\"https://s3-media1.fl.yelpcdn.com/a.jpg\""
    it "copes with a place that has no photo, rating or link" do
      renders (Active swiper (Swiping (bare "Mystery Diner"))) "Mystery Diner"
    it "celebrates a match" do
      renders (Active swiper (Matched (restaurant "a"))) "a match!"
      renders (Active swiper (Matched (restaurant "a"))) "motion-reduce:hidden"
    it "announces a match to screen readers, with an empty region until then" do
      renders (Active swiper (Matched (restaurant "a"))) "aria-live=\"polite\">It&#39;s a match! Everyone&#39;s hungry for a"
      renders (Active swiper (Swiping (restaurant "a"))) "aria-live=\"polite\"></p>"
    it "offers to try again after an error" do
      renders (ServerError "Could not reach the server") "Try again"
    it "offers to search further, saying how far" do
      renders (Active swiper NoneLeft) "Search further (up to 10 km)"
      renders (Active swiper { radius = 40000.0 } NoneLeft) "Search further (up to 50 km)"
    it "says when there is nothing left" do
      renders (Active swiper NoneLeft) "everything nearby"
      renders (NeedLocation { session: SessionId "s", user: UserId "u", message: "Location is blocked for this site.", picker: Nothing }) "We need your location"
      renders (NeedLocation { session: SessionId "s", user: UserId "u", message: "Location is blocked for this site.", picker: Nothing }) "Location is blocked for this site."
      renders (NeedLocation { session: SessionId "s", user: UserId "u", message: "x", picker: Nothing }) ">Try again<"
      renders (NeedLocation { session: SessionId "s", user: UserId "u", message: "x", picker: Nothing }) "Tell us where you&#39;re eating"
      renders (QR lobbyAt) "your location"
      renders (QR lobbyAt) ">Somewhere else?<"
      renders (QR lobbyAt { vibe { place = Just kelownaPlace } }) ">Kelowna<"
      renders (QR lobbyAt { vibe { place = Just kelownaPlace } }) ">Use our locations<"
      renders (QR lobbyAt { picker = Just blankPicker { results = Just [ kelowna ] } }) "Where are you eating?"
      renders (QR lobbyAt { picker = Just blankPicker { results = Just [ kelowna ] } }) "Kelowna, BC, Canada"
      renders (QR lobbyAt { picker = Just blankPicker { results = Just [] } }) "Nothing matched"
      renders (Active swiper { people = [ me, friend, { you: false, status: Status.Away, swiped: 0, left: Nothing } ] } NoneLeft) "2 here · 1 done"
      renders (Active swiper { people = [ me, { you: false, status: Status.Away, swiped: 0, left: Nothing } ] } NoneLeft) "border-dashed"
      renders (Active swiper { people = [ me, friend ] } NoneLeft) ">You<"
      renders (Active swiper { people = [ me, friend ] } NoneLeft) ">Friend 1<"
      renders (Active swiper { people = [ me, friend ] } NoneLeft) ">12 left<"
      renders (Active swiper { people = [ me, friend ] } NoneLeft) "width: 20%"
      renders (Active swiper { loc = Nothing } (Swiping (restaurant "a"))) "Showing places near where your group started"
      renders (Active swiper NoneLeft) "Start a new session<"
