-- | Properties checked over random input rather than a few examples
module Properties (spec) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Limits (Limit (..), Usage, spend)
import Places (inRing, isPhotoName)
import Restaurant

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
