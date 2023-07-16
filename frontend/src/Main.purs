module Main
  ( Message(..)
  , ErrorMessage
  , Model
  , Price
  , Resturant
  , UserId
  , Url
  , Loc
  , init
  , main
  , view
  ) where

import Prelude
import Data.Array (cons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Error, makeAff, Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Fetch (Method(..), fetch)
import Flame (QuerySelector(..), Html, (:>), ListUpdate)
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Yoga.JSON as JSON

foreign import _geolocation
  :: forall a
   . ({ latitude :: Number, longitude :: Number } -> a)
  -> (Error -> a)
  -> (a -> Effect Unit)
  -> Effect Unit

createSession :: Aff (Maybe UserId)
createSession = do
  { status, text } <-
    fetch "http://localhost:5000/newsession"
      { method: GET
      , headers: { "Content-Type": "application/json" }
      }
  case status of
    200 -> JSON.readJSON_ <$> text
    _ -> pure Nothing

geolocation :: forall m. MonadAff m => m { latitude :: Number, longitude :: Number }
geolocation =
  liftAff
    $ makeAff \cb ->
        _geolocation Right Left cb $> mempty

type ErrorMessage = String

-- | This datatype is used to signal events to `update`
data Message
  = StartSwiping
  | Like
  | Dislike
  | FailedToLoad ErrorMessage
  | NextResturant Loc UserId
  | Matched Resturant
  | Finish Resturant Loc UserId

type UserId = String

-- | The model represents the state of the app
data Model
  = QR
  | Swiping Resturant Loc UserId
  | Match Resturant
  | ServerError ErrorMessage

type Url = String

type Price = Maybe String

type Resturant =
  { id :: String
  , name :: String
  , price :: Price
  , rating :: Number
  , imageLink :: Url
  }

-- | Location of the current user
type Loc = { latitude :: Number, longitude :: Number }

-- | Location of the current user
type ResturantDecision =
  { id :: String
  , likedResturant :: (Maybe String)
  }

apiUrl :: Url
apiUrl = "http://localhost:5000/gavin"

updateUrl :: Url
updateUrl = "http://localhost:5000/liked"

readApi :: String -> Maybe Resturant
readApi = JSON.readJSON_

-- | Initial state of the app
init :: Model
init = QR

skipDuplicateResturant :: Model -> Resturant -> Message
skipDuplicateResturant (Swiping ogr loc sess) newr =
  if ogr == newr then
    NextResturant loc sess
  else
    Finish newr loc sess

skipDuplicateResturant _ _ = FailedToLoad "Attempted to determin resturant when no resturant is set"

newtype JsonString = JsonString String

post :: JsonString -> Url -> Aff (Maybe Resturant)
post (JsonString x) url = do
  liftEffect $ log "post"
  { status, text } <-
    fetch url
      { method: POST
      , headers: { "Content-Type": "application/json" }
      , body: x
      }
  case status of
    200 -> readApi <$> text
    _ -> pure Nothing

sendLiked :: UserId -> (Maybe String) -> Aff (Maybe Resturant)
sendLiked id liked_ =
  let
    des :: ResturantDecision
    des = ({ id: id, likedResturant: liked_ })
  in
    post (JsonString $ JSON.writeJSON des)
      updateUrl

getResturant :: Loc -> Aff (Maybe Resturant)
getResturant loc = post (JsonString $ JSON.writeJSON loc) apiUrl

-- TODO Render QR code for users to join
-- | `update` is called to handle events
update :: ListUpdate Model Message
update model = case _ of
  -- Final states that updtate the model
  FailedToLoad e -> ServerError e :> []
  Finish resturant loc userId -> (Swiping resturant loc userId) :> []
  Matched x -> Match x :> []
  -- Events coming from the UI
  StartSwiping -> model :>
    [ Just <$> do
        sess <- createSession
        show sess # liftEffect <<< log
        case sess of
          Just s -> do
            loc <- geolocation
            pure $ NextResturant loc s
          Nothing -> pure $ StartSwiping
    ]
  -- TODO Send if they liked the restaurant
  Like ->
    model
      :>
        [ Just
            <$> case model of
              Swiping r l u -> do
                maybeMatch <- sendLiked u (Just r.id)
                "liked " <> show r.id # liftEffect <<< log
                pure case maybeMatch of
                  (Just m) -> Matched m
                  Nothing -> NextResturant l u
              _ -> pure $ FailedToLoad "Attempted to like while not swiping"
        ]
  Dislike ->
    model
      :>
        [ Just
            <$> case model of
              Swiping r l u -> sendLiked u Nothing *> (pure $ NextResturant l u)
              _ -> pure $ FailedToLoad "Attempted to dislike while not swiping"
        ]
  -- Change this to match the yelp API
  -- For some reason this seems to be using options rather than GET
  NextResturant loc s ->
    model
      :>
        [ Just <$> do
            maybeRes <- getResturant loc
            "nextresturant" <> show maybeRes # liftEffect <<< log
            pure
              $ case maybeRes of
                  Nothing -> FailedToLoad "API did not return a resturant"
                  Just res -> case model of
                    QR -> Finish res loc s
                    Swiping _ _ _ -> skipDuplicateResturant model res
                    _ -> FailedToLoad
                      "Attempted to find next resturant when not swiping or starting a session"
        ]

type Color = String

header :: String -> Html Message
header text =
  HE.header
    [ HA.class'
        "relative flex items-center shadow-md justify-center mx-2 rounded-lg"
    ]
    [ HE.h1
        [ HA.class'
            "text-3xl lg:text-3xl font-bold mb-2"
        ]
        text
    ]

image :: String -> Html Message
image link =
  HE.div
    [ HA.class'
        "py-5 flex flex-col items-center justify-center"
    ]
    [ HE.figure
        [ HA.class' "flex flex-row justify-center" ]
        [ HE.img
            [ HA.class' "is-cropped rounded overflow-hidden shadow-lg"
            , HA.src link
            , HA.alt "Resturant Image"
            ]
        ]
    ]

btn :: Message -> String -> Maybe Color -> Html Message
btn m t c =
  let
    attrs =
      [ HA.class' "block py-2 px-10 fill text-white shadow-md rounded hover:shadow-lg"
      , HA.onClick m
      ]
  in
    HE.button
      case c of
        Just color -> cons (HA.class' color) attrs
        Nothing -> attrs
      t

-- TODO Implement QR Code prompt
-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view (ServerError e) = HE.main "main" [ HE.text $ "Error: " <> e ]

view QR =
  HE.main "main"
    [ HE.div [ HA.class' "flex flex-col items-center justify-center " ]
        [ HE.text "Replace with QR code"
        , HE.div_
            [ btn StartSwiping "Start Swiping"
                (Just "bg-gray-900 hover:bg-gray-800")
            ]
        ]
    ]

view (Match { name }) = HE.main "main" [ HE.text ("Yay you got a match on" <> name) ]

view (Swiping apiResults _ _) =
  HE.main "main"
    [ HE.div [ HA.class' "my-0 fill bg-gray-100" ]
        [ HE.div [ HA.class' "justify-center pt-3" ]
            [ HE.div_
                [ header apiResults.name
                , image apiResults.imageLink
                ]
            ]
        , HE.div_
            [ HE.div [ HA.class' "bg-white py-2 shadow-lg px-2 rounded-lg mx-2" ]
                [ HE.div [ HA.class' "fill flex items-center justify-between mb-3lcontainer mx-auto rounded" ]
                    [ HE.div [ HA.class' "items-left" ]
                        $ btn Dislike "-" (Just "bg-red-900 hover:bg-red-800")
                    , HE.ul_
                        let
                          ratings = " Rating: " <> (show apiResults.rating)
                        in
                          map HE.li_ case apiResults.price of
                            Nothing -> [ ratings ]
                            (Just p) -> [ " Price: " <> p, ratings ]
                    , HE.div [ HA.class' "right-0" ]
                        $ btn Like "+" (Just "bg-green-900 hover:bg-green-800")
                    ]
                ]
            ]
        ]
    ]

-- | Mount the application on the given selector
main :: Effect Unit
main =
  F.mount_ (QuerySelector "body")
    { init: init :> []
    , subscribe: [] -- [ FSW.onLoad NextResturant ] -- Load resturant on startup
    , update
    , view
    }
