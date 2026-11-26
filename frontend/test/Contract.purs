-- | The JSON contract with the backend, both ways. Mirrors
-- | backend/test/Contract.hs.
-- |
-- | Responses: the backend's tests pin each endpoint's JSON in
-- | backend/.golden/<name>/golden, encoded from the types its handlers send;
-- | here they're decoded as the types the API calls read.
-- |
-- | Requests: each body is pinned in frontend/.golden/<name>/golden, encoded
-- | as the types the API calls send, and the backend's tests decode them with
-- | its own decoders. A missing golden is written; a changed one fails and
-- | leaves `actual` beside it. If a change is meant, delete the golden and
-- | run the tests again.
module Test.Contract (spec) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (take)
import Data.Either (Either, either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign (Foreign)
import Tender.Api (DecisionRequest, DecisionResponse, ErrorResponse, PlaceRequest, PresenceRequest, PresenceResponse, RestaurantsRequest, RestaurantsResponse, VibeRequest)
import Tender.Types (Found, RestaurantId(..), SessionId, UserId(..))
import Tender.Types (Status(..)) as Status
import Tender.Vibe (Vibe)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON as JSON

spec :: Spec Unit
spec = describe "contract with the backend" do
  describe "responses the backend sends" do
    decodes "ids-response" \(ids :: Array Foreign) -> case ids of
      [ s, u ] -> do
        _ :: SessionId <- orThrow (JSON.read s)
        _ :: UserId <- orThrow (JSON.read u)
        pure unit
      _ -> fail "expected a session id and a user id"
    decodes "restaurants-response" \(r :: RestaurantsResponse) -> do
      map _.id r.restaurants `shouldEqual` [ RestaurantId "ChIJ1", RestaurantId "osm:node/1" ]
      map _.price r.restaurants `shouldEqual` [ Just "$$", Nothing ]
    decodes "decision-response" \(r :: DecisionResponse) ->
      r.matches `shouldEqual` [ RestaurantId "ChIJ1" ]
    -- The backend samples every status, so a new one fails to decode here
    decodes "presence-response" \(r :: PresenceResponse) ->
      map _.status r.people `shouldEqual` [ Status.Swiping, Status.Done, Status.Away ]
    decodes "lobby-response" \(lobbies :: Array Vibe) -> do
      map (map _.name <<< _.place) lobbies `shouldEqual` [ Nothing, Just "Fernwood" ]
      -- The chips come from the backend, labels and all
      map (map _.id <<< take 2 <<< _.choices) lobbies `shouldEqual` [ [ "cheap", "fancy" ], [ "cheap", "fancy" ] ]
      map (map _.label <<< take 1 <<< _.choices) lobbies `shouldEqual` [ [ "💸 Cheap eats" ], [ "💸 Cheap eats" ] ]
    decodes "places-response" \(found :: Array Found) ->
      map _.address found `shouldEqual` [ "Fernwood, Victoria, BC, Canada" ]
    decodes "error-response" \(e :: ErrorResponse) ->
      e.error `shouldEqual` "Unknown user"

  describe "requests the frontend sends" do
    pins "restaurants-request" ({ user: someone, loc: Just { latitude: 48.43, longitude: -123.35 }, widenFrom: Just 5000.0 } :: RestaurantsRequest)
    pins "restaurants-request-without-location" ({ user: someone, loc: Nothing, widenFrom: Nothing } :: RestaurantsRequest)
    pins "decision-request" ({ user: someone, restaurant: RestaurantId "ChIJ1", liked: true } :: DecisionRequest)
    pins "presence-request" ({ user: someone } :: PresenceRequest)
    pins "vibe-request" ({ chips: [ "patio", "veggie" ] } :: VibeRequest)
    pins "place-request" ({ place: Just { name: "Fernwood", latitude: 48.43, longitude: -123.34 } } :: PlaceRequest)
    pins "place-request-clear" ({ place: Nothing } :: PlaceRequest)
  where
  someone = UserId "00000000-0000-0000-0000-000000000000"

-- | A response the backend pinned, read as the API calls read it
decodes :: forall a. ReadForeign a => String -> (a -> Aff Unit) -> Spec Unit
decodes name check = it name do
  text <- liftEffect (readBackendGolden name)
  orThrow (JSON.readJSON text) >>= check

-- | A request body, encoded as the API calls encode it, pinned for the backend
pins :: forall a. WriteForeign a => String -> a -> Spec Unit
pins name body = it name do
  let actual = JSON.writePrettyJSON 4 body
  expected <- liftEffect (pinGolden Nothing Just name actual)
  for_ expected (actual `shouldEqual` _)

orThrow :: forall e a. Show e => Either e a -> Aff a
orThrow = either (throwError <<< error <<< show) pure

foreign import readBackendGolden :: String -> Effect String

-- | Nothing when the golden matches or was just written; the golden when it differs
foreign import pinGolden :: forall r. r -> (String -> r) -> String -> String -> Effect r
