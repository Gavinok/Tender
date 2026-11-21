module Main
  ( ErrorMessage
  , Loc
  , Message(..)
  , Model(..)
  , Restaurant
  , RestaurantId(..)
  , Screen(..)
  , SessionId(..)
  , Swiper
  , Url
  , UserId(..)
  , init
  , main
  , update
  , view
  ) where

import Prelude

import Data.Array (last, cons, elem, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, message)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Fetch (Method(..), Response, fetch)
import Flame (QuerySelector(..), Html, (:>), ListUpdate)
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element (class ToNode)
import Flame.Html.Element as HE
import Flame.Subscription.Window as FSW
import Flame.Types (NodeData)
import Web.HTML (window)
import Web.HTML.Location (href)
import Web.HTML.Window (location)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON as JSON

foreign import _geolocation :: EffectFnAff Loc

backendUrl :: String
backendUrl = "http://localhost:5001/"

newtype UserId = UserId String

derive newtype instance Eq UserId
derive newtype instance Show UserId
derive newtype instance ReadForeign UserId
derive newtype instance WriteForeign UserId

newtype SessionId = SessionId String

derive newtype instance Eq SessionId
derive newtype instance Show SessionId
derive newtype instance ReadForeign SessionId

newtype RestaurantId = RestaurantId String

derive newtype instance Eq RestaurantId
derive newtype instance Show RestaurantId
derive newtype instance ReadForeign RestaurantId
derive newtype instance WriteForeign RestaurantId

data ApiError
  = NotFound ErrorMessage
  | Failed ErrorMessage

-- | Decode a 200 response, or use the message from an error response
decode
  :: forall a
   . ReadForeign a
  => Either Error Response
  -> Aff (Either ApiError a)
decode (Left e) = pure $ Left $ Failed $ "Could not reach the server: " <> message e
decode (Right { status, text }) = do
  body <- text
  pure case status of
    200 -> lmap (Failed <<< ("Unexpected response: " <> _) <<< show) (JSON.readJSON body)
    404 -> Left $ NotFound $ errorMessage body
    _ -> Left $ Failed $ errorMessage body
  where
  errorMessage body = case JSON.readJSON body of
    Right ({ error } :: { error :: String }) -> error
    Left _ -> "The server responded with " <> show status

get :: forall a. ReadForeign a => Url -> Aff (Either ApiError a)
get url = decode =<< attempt (fetch url { method: GET })

post :: forall body a. WriteForeign body => ReadForeign a => Url -> body -> Aff (Either ApiError a)
post url body =
  decode =<< attempt
    ( fetch url
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: JSON.writeJSON body
        }
    )

createSession :: Aff (Either ApiError SessionId)
createSession = get (backendUrl <> "newsession")

joinSession :: SessionId -> Aff (Either ApiError UserId)
joinSession (SessionId s) = get (backendUrl <> "addtosession/" <> s)

-- | Ask the browser for the user's location. Fails if they refuse.
geolocation :: Aff Loc
geolocation = fromEffectFnAff _geolocation

-- TODO replace localhost url with our real url
sessionUrl :: SessionId -> String
sessionUrl (SessionId session) = "http://localhost:1234/" <> session

type ErrorMessage = String

type Url = String

type Restaurant =
  { id :: RestaurantId
  , name :: String
  , url :: Url
  , price :: Maybe String
  , rating :: Number
  , imageLink :: Url
  , category :: Maybe String
  }

-- | Location of the current user
type Loc = { latitude :: Number, longitude :: Number }

-- | What we know once the user has joined a session
type Swiper =
  { user :: UserId
  , loc :: Loc
  , matches :: Array Restaurant
  }

-- | What a user who has joined a session is looking at
data Screen
  = Fetching
  | Swiping Restaurant
  | Matched Restaurant
  | NoneLeft

-- | The model represents the state of the app
data Model
  = Loading
  | QR SessionId
  | Active Swiper Screen
  | ServerError ErrorMessage

-- | This datatype is used to signal events to `update`
data Message
  = DetermineSession
  | JoinSession SessionId
  | StartSwiping
  | Started UserId Loc
  | Like
  | Dislike
  -- | Skip past the current match and keep swiping
  | Continue
  | Show Restaurant
  | GotMatch Restaurant
  | OutOfRestaurants
  | FailedToLoad ErrorMessage

-- | Record a like (or a dislike with `Nothing`), returning the restaurant if it is now a match
sendLiked :: UserId -> Maybe RestaurantId -> Aff (Either ApiError (Maybe Restaurant))
sendLiked user liked = post (backendUrl <> "liked") { id: user, likedRestaurant: liked }

getRestaurant :: Loc -> Maybe RestaurantId -> Aff (Either ApiError Restaurant)
getRestaurant loc after = post (backendUrl <> "restaurant") { loc, restaurantId: after }

-- | Ask for the restaurant after the given one
nextRestaurant :: Loc -> Maybe RestaurantId -> Aff (Maybe Message)
nextRestaurant loc after = Just <<< toMessage <$> getRestaurant loc after
  where
  toMessage = case _ of
    Left (NotFound _) -> OutOfRestaurants
    Left (Failed e) -> FailedToLoad e
    Right r -> Show r

-- | Show a failed request to the user
failed :: ApiError -> Message
failed = case _ of
  NotFound e -> FailedToLoad e
  Failed e -> FailedToLoad e

orFail :: forall a. (a -> Message) -> Either ApiError a -> Message
orFail = either failed

-- | The session in the page url, if any
existingSession :: Effect (Maybe SessionId)
existingSession = do
  h <- href =<< location =<< window
  pure case last (split (Pattern "/") h) of
    Nothing -> Nothing
    Just "" -> Nothing
    Just s -> Just (SessionId s)

-- | Initial state of the app
init :: Model
init = Loading

-- | `update` is called to handle events
update :: ListUpdate Model Message
update model = case model, _ of
  _, FailedToLoad e -> ServerError e :> []

  _, DetermineSession -> model :>
    [ Just <$> do
        found <- liftEffect existingSession
        case found of
          Just session -> pure $ JoinSession session
          Nothing -> orFail JoinSession <$> createSession
    ]
  _, JoinSession session -> QR session :> []

  QR session, StartSwiping -> model :>
    [ attempt geolocation >>= case _ of
        Left e -> pure $ Just $ FailedToLoad $ "Could not get your location: " <> message e
        Right loc -> Just <<< orFail (\u -> Started u loc) <$> joinSession session
    ]
  _, Started user loc ->
    Active { user, loc, matches: [] } Fetching :> [ nextRestaurant loc Nothing ]

  Active swiper (Swiping r), Like -> model :>
    [ do
        match <- sendLiked swiper.user (Just r.id)
        case match of
          Left e -> pure $ Just $ failed e
          Right (Just m) | not (elem m.id (_.id <$> swiper.matches)) -> pure $ Just $ GotMatch m
          Right _ -> nextRestaurant swiper.loc (Just r.id)
    ]
  Active swiper (Swiping r), Dislike -> model :>
    [ sendLiked swiper.user Nothing *> nextRestaurant swiper.loc (Just r.id) ]
  Active swiper (Matched r), Continue -> model :>
    [ nextRestaurant swiper.loc (Just r.id) ]

  Active swiper _, Show r -> Active swiper (Swiping r) :> []
  Active swiper _, GotMatch r ->
    Active swiper { matches = cons r swiper.matches } (Matched r) :> []
  Active swiper _, OutOfRestaurants -> Active swiper NoneLeft :> []

  -- Anything else is a stale message, e.g. from a double click
  _, _ -> model :> []

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

view (QR session@(SessionId s)) =
  HE.main "main"
    [ HE.div [ HA.class' "qr-main" ]
        [ HE.div [ HA.class' "join-button" ]
            [ HE.a [ HA.href s, HA.value "Join Session" ] "Join Session" ]
        , HE.img
            [ HA.src $ "https://api.qrserver.com/v1/create-qr-code/?data=" <> sessionUrl session
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

-- | Mount the application on the given selector
main :: Effect Unit
main =
  F.mount_ (QuerySelector "body")
    { init: init :> []
    , subscribe: [ FSW.onLoad DetermineSession ]
    , update
    , view
    }
