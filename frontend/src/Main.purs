module Main
  ( ErrorMessage
  , Loc
  , Message(..)
  , Model
  , Price
  , Resturant
  , ResturantId
  , SessionId
  , Url
  , UserId
  , btn
  , init
  , main
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
import Effect.Console (log)
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
import Yoga.JSON as JSON

foreign import _geolocation
  :: forall a
   . ({ latitude :: Number, longitude :: Number } -> a)
  -> (Error -> a)
  -> (a -> Effect Unit)
  -> Effect Unit

backendUrl :: String
backendUrl = "http://localhost:5001/"

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

joinSession :: SessionId → Aff (Maybe UserId)
joinSession s = do
  { status, text } <-
    fetch (backendUrl <> "addtosession/" <> s)
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

-- TODO replace localhost url with our real url
sessionUrl ∷ String → String
sessionUrl session = "http://localhost:1234/" <> session

type ErrorMessage = String

type ResturantId = String
-- | This datatype is used to signal events to `update`
data Message
  = DeterminSession
  | JoinSession SessionId
  | StartSwiping
  | Like
  | Dislike
  | FailedToLoad ErrorMessage
  | NextResturant Loc UserId (Maybe ResturantId)
  | Matched Resturant Loc UserId
  | Finish Resturant Loc UserId

type UserId = String
type SessionId = String
type Matches = Array Resturant

-- | The model represents the state of the app
data Model
  = Loading
  | QR SessionId
  | Swiping Matches Resturant Loc UserId
  | Match Matches Resturant Loc UserId
  | ServerError ErrorMessage

type Url = String

type Price = Maybe String

type Resturant =
  { id :: String
  , name :: String
  , url ∷ String
  , price :: Price
  , rating :: Number
  , imageLink :: Url
  , category :: Maybe String
  }

-- | Location of the current user
type Loc = { latitude :: Number, longitude :: Number }

-- | Location of the current user
type ResturantDecision =
  { id :: String
  , likedResturant :: (Maybe String)
  }

apiUrl :: Url
apiUrl = backendUrl <> "resturant"

updateUrl :: Url
updateUrl = backendUrl <> "liked"

readApi :: String -> Maybe Resturant
readApi = JSON.readJSON_

-- | Initial state of the app
init :: Model
init = Loading

skipDuplicateResturant :: Model -> Resturant -> Message
skipDuplicateResturant (Swiping match ogr loc sess) newr =
  if ogr == newr then
    NextResturant loc sess (Just ogr.id)
  else
    Finish newr loc sess

skipDuplicateResturant _ _ = FailedToLoad "Attempted to determine restaurant when no restaurant is set"

newtype JsonString = JsonString String

post :: JsonString -> Url -> Aff (Maybe Resturant)
post (JsonString x) url = do
  { status, text } <-
    fetch url
      { method: POST
      , headers: { "Content-Type": "application/json" }
      , body: x
      }
  liftEffect $ log $ "post got a " <> (show status)
  case status of
    200 -> readApi <$> text
    _ -> do
      t <- text
      liftEffect $ log $ t
      pure Nothing

sendLiked :: UserId -> (Maybe String) -> Aff (Maybe Resturant)
sendLiked id liked_ =
  let
    des :: ResturantDecision
    des = ({ id: id, likedResturant: liked_ })
  in
    post (JsonString $ JSON.writeJSON des)
      updateUrl

getResturant :: Loc → Maybe ResturantId -> Aff (Maybe Resturant)
getResturant loc rid = post (JsonString $ JSON.writeJSON { loc: loc, resturantId: rid }) apiUrl

-- | `update` is called to handle events
update :: ListUpdate Model Message
update model = case _ of
  -- Final states that updtate the model
  FailedToLoad e -> ServerError e :> []
  Finish resturant loc userId -> case model of
    Loading → Swiping [] resturant loc userId :> []
    (QR _) → Swiping [] resturant loc userId :> []
    (Swiping m _ _ _) → Swiping m resturant loc userId :> []
    (Match m _ _ _) → Swiping m resturant loc userId :> []
    _ → ServerError "Attempted to finish from invalid state" :> []

  Matched r l u -> case model of
    (Swiping m _ _ _) → Match (cons r m) r l u :> []
    _ → ServerError "Attempted to match from non swiping state" :> []
  JoinSession session → QR session :> []
  -- Events coming from the UI
  DeterminSession → model :>
    [ Just <$> do
        existingSession ← liftEffect $ do
          w ← window
          l ← location w
          h <- href l
          let extension = last (split (Pattern "/") h)
          pure case extension of
            Nothing → Nothing
            Just "" → Nothing
            Just _ → extension
        case existingSession of
          Just session -> pure $ JoinSession session
          Nothing -> do
            session <- createSession
            pure case session of
              Nothing → FailedToLoad "Could not get a session from server"
              Just s → JoinSession s
    ]
  StartSwiping -> model :>
    [ Just <$> case model of
        (QR session) → do
          loc <- geolocation
          user ← joinSession session
          show user # liftEffect <<< log
          pure $ case user of
            Just u → NextResturant loc u Nothing
            Nothing → StartSwiping
        _ → pure $ FailedToLoad "StartSwiping Can only be entered from QR"
    ]

  Like ->
    model
      :>
        [ Just
            <$> case model of
              Swiping m r l u -> do
                maybeMatch <- sendLiked u (Just r.id)
                "liked " <> show r # liftEffect <<< log
                "got " <> show maybeMatch # liftEffect <<< log
                pure case maybeMatch of
                  Just match ->
                    if elem match m then NextResturant l u (Just r.id)
                    else Matched match l u
                  Nothing -> NextResturant l u (Just r.id)
              _ -> pure $ FailedToLoad "Attempted to like while not swiping"
        ]
  Dislike ->
    model
      :>
        [ Just
            <$> case model of
              Swiping m r l u -> sendLiked u Nothing *> (pure $ NextResturant l u (Just r.id))
              _ -> pure $ FailedToLoad "Attempted to dislike while not swiping"
        ]
  -- Change this to match the yelp API
  -- For some reason this seems to be using options rather than GET
  NextResturant loc s rid ->
    model
      :>
        [ Just <$> do
            maybeRes <- getResturant loc rid
            "nextresturant" <> show maybeRes # liftEffect <<< log
            pure
              $ case maybeRes of
                  Nothing -> FailedToLoad "API did not return a restaurant"
                  Just res -> case model of
                    Loading -> Finish res loc s
                    QR _ → Finish res loc s
                    Match _ _ _ _ → Finish res loc s
                    Swiping _ _ _ _ -> skipDuplicateResturant model res
                    _ -> FailedToLoad
                      "Attempted to find next restaurant when not swiping or starting a session"
        ]

type Color = String

headerText :: forall b252 h253. ToNode b252 h253 Html => b252 -> Html h253
headerText text = HE.h1
  [ HA.class'
      "header-text"
  ]
  text

header :: forall a b. ToNode a b Html => a -> Html b
header content =
  HE.header
    [ HA.class'
        "header-body"
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

resturantStats :: forall a. Resturant -> Html a
resturantStats r = HE.div [ HA.class' "text-center" ] $ HE.ul_
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

grey :: forall a272. Array (NodeData a272)
grey = [ HA.class' "bg-gray-900 hover:bg-gray-800" ]

centered :: forall a. NodeData a
centered = HA.class' "flex justify-center"

footer :: forall a b. ToNode b a Html => b -> Html a
footer content =
  HE.div [ HA.class' "footer-body" ]
    [ HE.div [ HA.class' "footer-content" ] content
    ]

yelplink :: forall a. Resturant -> Html a
yelplink r = HE.div [ HA.class' "yelp-link" ]
  [ HE.a [ HA.href r.url, HA.target "_blank", HA.value "Yelp Page" ] "Yelp Page" ]

-- | `view` updates the app markup whenever the model is updated
view :: Model -> Html Message
view (ServerError e) = HE.main "main" [ HE.text $ "Error: " <> e ]

view Loading =
  HE.main "main"
    [ HE.div [ HA.class' "loading-main" ]
        [ HE.text "LOADING…" ]
    ]

view (QR session) =
  HE.main "main"
    [ HE.div [ HA.class' "qr-main" ]
        [ HE.div [ HA.class' "join-button" ]
            [ HE.a [ HA.href session, HA.value "Join Session" ] "Join Session" ]
        , HE.img
            [ HA.src $ "https://api.qrserver.com/v1/create-qr-code/?data=" <> sessionUrl session
            , HA.alt ""
            ]
        , HE.div [ HA.class' "my-2" ]
            [ btn (StartSwiping) "Start Swiping" grey
            ]
        ]
    ]

view (Swiping matches apiResults _ _) =
  HE.main "main"
    [ HE.div_
        [ header $ headerText apiResults.name
        , HE.div [ HA.class' "main-background" ]
            [ image apiResults.imageLink
            , HE.div [ centered ] $ yelplink apiResults
            , HE.div [ centered ] $ (show $ length matches) <>
                if (length matches) == 1 then " Match"
                else " Matches"
            ]
        , HE.div_
            [ footer
                [ HE.div [ HA.class' "footer-item" ]
                    $ btn Dislike "-" [ HA.class' "bg-red-900 hover:bg-red-800" ]

                , HE.div [ HA.class' "footer-item" ] $ resturantStats apiResults

                , HE.div [ HA.class' "footer-item" ]
                    $ btn Like "+" [ HA.class' "bg-green-900 hover:bg-green-800" ]
                ]
            ]
        ]
    ]
view (Match _ r loc id) =
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
                [ HE.div [ HA.class' "footer-item col-start-2" ] $ resturantStats r
                , HE.div [ HA.class' "footer-item" ] $ btn (NextResturant loc id (Just r.id)) "Continue Anyways" grey
                ]
            ]
        ]
    ]

-- | Mount the application on the given selector
main :: Effect Unit
main =
  F.mount_ (QuerySelector "body")
    { init: init :> []
    , subscribe: [ FSW.onLoad DeterminSession ] -- Load resturant on startup
    , update
    , view
    }
