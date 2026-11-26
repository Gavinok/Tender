-- | Properties checked over random input rather than a few examples
module Properties (spec) where

import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Limits (Limit (..), Usage, spend)
import Places (inRing, isPhotoName)
import Presence (Activity (..), Person (..), Seen (..), activity, awayAfter, cardsLeft, dealt, nextHeartbeat, nextHeartbeatFor, seen)
import Restaurant
import User (UserId (..))

spec :: Spec
spec = do
    describe "spend" $
        prop "never lets a key use more than its limit in a window" $
            forAll (listOf spending) $ \events ->
                let limit = Limit 1800 10
                    step (t, usage, ok) (gap, key, cost) =
                        let t' = addUTCTime (fromIntegral gap) t
                            (_, usage') = spend limit t' key cost usage
                         in (t', usage', ok && all ((<= limitMax limit) . snd) usage')
                    (_, _, fine) = foldl' step (start, Map.empty :: Usage, True) events
                 in fine

    describe "presence" $ do
        prop "is away when not heard from within awayAfter, and only then" $
            forAll ((,,) <$> chooseInt (0, 120) <*> ids <*> maybeDeck) $ \(ago, decided, d) ->
                let s = Seen (addUTCTime (negate (fromIntegral ago)) start) d
                 in (activity start decided (Just s) == Away) === (fromIntegral ago > awayAfter)
        prop "is done exactly when every card shown has been decided on" $
            forAll ((,) <$> ids <*> ids) $ \(decided, shown) ->
                let s = Seen start (Just shown)
                 in (activity start decided (Just s) == Done) === all (`elem` decided) shown
        prop "counts the cards shown and not decided on" $
            forAll ((,) <$> ids <*> ids) $ \(decided, shown) ->
                cardsLeft decided (Seen start (Just shown)) === Just (Set.size (Set.difference shown decided))
        prop "keeps every card dealt, and a heartbeat keeps the deck" $
            forAll (listOf (listOf (elements (map RestaurantId ["a", "b", "c", "d"])))) $ \deals ->
                let me = UserId UUID.nil
                    afterDeals = foldl (flip (dealt start me)) Map.empty deals
                    dealtAll = Set.fromList (concat deals)
                    deckOf m = Map.lookup me m >>= deck
                 in conjoin
                        [ null deals || deckOf afterDeals == Just dealtAll
                        , deckOf (seen start me afterDeals) == deckOf afterDeals
                        ]
        it "allows two missed heartbeats before someone is away" $
            awayAfter `shouldSatisfy` (>= 3 * fromIntegral (maximum (map nextHeartbeat [minBound .. maxBound])))
        it "beats sooner for someone done, who is waiting to hear about matches" $
            (nextHeartbeatFor [Person True Done 3 (Just 0)], nextHeartbeatFor [Person True Swiping 3 (Just 5)]) `shouldBe` (5, 10)
        it "is away until heard from" $
            activity start Set.empty Nothing `shouldBe` Away

    describe "inRing" $
        prop "keeps each place between the two radii exactly once" $
            forAll ring $ \(inner, outer, places) ->
                let kept = inRing centre inner outer places
                    between r = let m = metres centre (location r) in m >= inner && m <= outer
                 in conjoin
                        [ counterexample "outside the ring" (all between kept)
                        , counterexample "kept twice" (nub (map restaurantId kept) == map restaurantId kept)
                        , counterexample "missing" (nub (map restaurantId (filter between places)) == map restaurantId kept)
                        ]

    describe "isPhotoName" $ do
        prop "accepts places/<id>/photos/<id> made of letters, digits, - and _" $
            forAll ((,) <$> safeSegment <*> safeSegment) $ \(place, ref) ->
                isPhotoName ("places/" <> place <> "/photos/" <> ref)
        prop "rejects an id with any other character in it" $
            forAll ((,,) <$> safeSegment <*> unsafeSegment <*> arbitrary) $ \(good, bad, placeFirst) ->
                let (place, ref) = if placeFirst then (bad, good) else (good, bad)
                 in not (isPhotoName ("places/" <> place <> "/photos/" <> ref))
        prop "rejects anything not shaped like a photo name" $
            forAll (T.pack <$> arbitrary) $ \t ->
                not ("places/" `T.isPrefixOf` t) ==> not (isPhotoName t)
  where
    start = UTCTime (fromGregorian 2026 1 1) 0
    centre = Loc 48.43 (-123.35)
    -- Seconds since the last spend, which key and how much
    ids = Set.fromList <$> listOf (elements (map RestaurantId ["a", "b", "c", "d", "e"]))
    maybeDeck = oneof [pure Nothing, Just <$> ids]
    spending :: Gen (Int, Text, Int)
    spending = (,,) <$> chooseInt (0, 600) <*> elements ["a", "b", "c"] <*> chooseInt (0, 5)
    ring = do
        inner <- chooseDouble 0 10000
        outer <- chooseDouble inner 20000
        places <- listOf $ do
            i <- elements (map (T.pack . show) [1 .. 10 :: Int])
            d <- chooseDouble 0 25000
            a <- chooseDouble 0 (2 * pi)
            pure (placeAt i d a)
        pure (inner, outer, places)
    -- A place the given distance in metres and angle away from the centre
    placeAt i d a =
        let Loc lat lon = centre
            l = Loc (lat + d * cos a / 111320) (lon + d * sin a / (111320 * cos (lat * pi / 180)))
         in Restaurant Google (RestaurantId i) i Nothing Nothing Nothing Nothing [] Nothing l
    safeChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_"
    safeSegment = T.pack <$> listOf1 (elements safeChars)
    unsafeSegment = do
        prefix <- listOf (elements safeChars)
        c <- elements "/.?#% \\:&=+@"
        suffix <- listOf (elements safeChars)
        pure (T.pack (prefix <> [c] <> suffix))
    chooseDouble lo hi = choose (lo, hi)
