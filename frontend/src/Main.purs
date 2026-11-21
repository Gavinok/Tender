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
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(..))
import Effect (Effect)
import Effect.Aff (Error, makeAff, Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Fetch (Method(..), fetch)
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

foreign import _geolocation
  :: forall a
   . ({ latitude :: Number, longitude :: Number } -> a)
  -> (Error -> a)
  -> (a -> Effect Unit)
  -> Effect Unit

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

createSession :: Aff (Maybe SessionId)
createSession = do
  { status, text } <-
    fetch (backendUrl <> "newsession")
      { method: GET
      , headers: { "Content-Type": "application/json" }
      }
  case status of
    200 -> JSON.readJSON_ <$> text
    _ -> pure Nothing

joinSession :: SessionId -> Aff (Maybe UserId)
joinSession (SessionId s) = do
  { status, text } <-
    fetch (backendUrl <> "addtosession/" <> s)
      { method: GET
      , headers: { "Content-Type": "application/json" }
      }
  case status of
    200 -> JSON.readJSON_ <$> text
    _ -> pure Nothing

geolocation :: forall m. MonadAff m => m Loc
geolocation =
  liftAff
    $ makeAff \cb ->
        _geolocation Right Left cb $> mempty

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
  | FailedToLoad ErrorMessage

post :: forall body. WriteForeign body => Url -> body -> Aff (Maybe Restaurant)
post url body = do
  { status, text } <-
    fetch url
      { method: POST
      , headers: { "Content-Type": "application/json" }
      , body: JSON.writeJSON body
      }
  case status of
    200 -> JSON.readJSON_ <$> text
    _ -> pure Nothing

sendLiked :: UserId -> Maybe RestaurantId -> Aff (Maybe Restaurant)
sendLiked user liked = post (backendUrl <> "liked") { id: user, likedRestaurant: liked }

getRestaurant :: Loc -> Maybe RestaurantId -> Aff (Maybe Restaurant)
getRestaurant loc after = post (backendUrl <> "restaurant") { loc, restaurantId: after }

-- | Ask for the restaurant after the given one
nextRestaurant :: Loc -> Maybe RestaurantId -> Aff (Maybe Message)
nextRestaurant loc after = Just <<< toMessage <$> getRestaurant loc after
  where
  toMessage = case _ of
    Nothing -> FailedToLoad "API did not return a restaurant"
    Just r -> Show r

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
          Nothing -> createSession <#> case _ of
            Nothing -> FailedToLoad "Could not get a session from server"
            Just s -> JoinSession s
    ]
  _, JoinSession session -> QR session :> []

  QR session, StartSwiping -> model :>
    [ do
        loc <- geolocation
        user <- joinSession session
        pure $ Just case user of
          Just u -> Started u loc
          Nothing -> FailedToLoad "Could not join the session"
    ]
  _, Started user loc ->
    Active { user, loc, matches: [] } Fetching :> [ nextRestaurant loc Nothing ]

  Active swiper (Swiping r), Like -> model :>
    [ do
        match <- sendLiked swiper.user (Just r.id)
        case match of
          Just m | not (elem m.id (_.id <$> swiper.matches)) -> pure $ Just $ GotMatch m
          _ -> nextRestaurant swiper.loc (Just r.id)
    ]
  Active swiper (Swiping r), Dislike -> model :>
    [ sendLiked swiper.user Nothing *> nextRestaurant swiper.loc (Just r.id) ]
  Active swiper (Matched r), Continue -> model :>
    [ nextRestaurant swiper.loc (Just r.id) ]

  Active swiper _, Show r -> Active swiper (Swiping r) :> []
  Active swiper _, GotMatch r ->
    Active swiper { matches = cons r swiper.matches } (Matched r) :> []

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
