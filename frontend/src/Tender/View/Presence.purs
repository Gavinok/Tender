-- | Who else is in the session and how far through they are, so the group
-- | can see each other deciding
module Tender.View.Presence (presenceStrip, waitingOn) where

import Prelude

import Data.Array (filter, foldl, length, snoc)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.Types (Person, Status(..))

-- | Nothing to show when the user is alone
presenceStrip :: forall a. Array Person -> Html a
presenceStrip everyone
  | length everyone < 2 = HE.text ""
  | otherwise =
      HE.section [ HA.class' "mb-4 flex flex-col gap-2", HA.createAttribute "aria-label" "Who's here" ]
        [ HE.p [ HA.class' "text-sm font-bold text-ink/70" ] [ HE.text (summary everyone) ]
        , HE.ul [ HA.class' "flex flex-wrap gap-2" ] (map chip (labelled everyone))
        ]

-- | Who is still swiping, e.g. "Waiting on Friend 2 (12 left)", for when the
-- | user is out of cards. Nothing when nobody is.
waitingOn :: Array Person -> Maybe String
waitingOn everyone = case still of
  [] -> Nothing
  _ -> Just ("Waiting on " <> joinWith ", " (map describe still))
  where
  still = filter (\{ person } -> not person.you && person.status == Swiping) (labelled everyone)
  describe { label, person } = label <> maybe "" (\n -> " (" <> show n <> " left)") person.left

-- | e.g. "3 here · 1 done"
summary :: Array Person -> String
summary everyone =
  show (count (_ /= Away)) <> " here" <> if done > 0 then " · " <> show done <> " done" else ""
  where
  count p = length (filter (p <<< _.status) everyone)
  done = count (_ == Done)

-- | "You" for the user, and the others numbered in the order they joined
labelled :: Array Person -> Array { label :: String, person :: Person }
labelled people = (foldl step { next: 1, acc: [] } people).acc
  where
  step { next, acc } p
    | p.you = { next, acc: snoc acc { label: "You", person: p } }
    | otherwise = { next: next + 1, acc: snoc acc { label: "Friend " <> show next, person: p } }

chip :: forall a. { label :: String, person :: Person } -> Html a
chip { label, person } =
  HE.li [ HA.class' ("flex min-w-[6rem] flex-col gap-1 rounded-xl border-2 border-ink px-3 py-1.5 text-sm " <> colours person.status) ]
    [ HE.span [ HA.class' "flex items-center justify-between gap-2 font-bold" ]
        [ HE.text label, HE.span [ HA.class' "text-xs font-bold" ] [ HE.text (state person) ] ]
    , progress person
    ]

colours :: Status -> String
colours = case _ of
  Swiping -> "bg-white"
  Done -> "bg-brand-100"
  -- Dashed rather than faded, which would drop the text below readable contrast
  Away -> "border-dashed bg-transparent"

state :: Person -> String
state p = case p.status, p.left of
  Done, _ -> "✓ done"
  Away, _ -> "away"
  Swiping, Just n -> show n <> " left"
  Swiping, Nothing -> "starting"

-- | How much of what they were shown they've decided on
progress :: forall a. Person -> Html a
progress p = case p.left of
  Nothing -> HE.text ""
  Just n ->
    let
      total = p.swiped + n
      percent = if total == 0 then 0 else round (100.0 * toNumber p.swiped / toNumber total)
    in
      HE.span [ HA.class' "h-1.5 w-full overflow-hidden rounded-full bg-ink/10", HA.createAttribute "aria-hidden" "true" ]
        [ HE.span [ HA.class' "block h-full rounded-full bg-brand-500", HA.style1 "width" (show percent <> "%") ] [] ]
