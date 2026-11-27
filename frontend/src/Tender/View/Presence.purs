-- | Who else is in the session and how far through they are, so the group
-- | can see each other deciding
module Tender.View.Presence (presenceStrip, sessionButton, waitingOn) where

import Prelude

import Data.Foldable (foldMap)

import Data.Array (drop, filter, foldl, length, snoc, take)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.Types (Person, Status(..))
import Tender.View.Icon (Icon(..), icon)

-- | Everyone and how far along they are, for the session sheet. Shown even
-- | when the user is alone, so they know others can join.
presenceStrip :: forall a. Array Person -> Html a
presenceStrip people =
  HE.section [ HA.class' "flex flex-col gap-2", HA.createAttribute "aria-label" "Who's here" ]
    [ HE.p [ HA.class' "text-sm font-bold text-ink/70" ] [ HE.text (summary everyone) ]
    , HE.ul [ HA.class' "flex flex-wrap gap-2" ] (map chip (labelled everyone))
    ]
  where
  everyone = orYou people

-- | Who's here as small avatars in the header, each ringed with how far
-- | along they are. Opens the session sheet.
sessionButton :: forall a. Array Person -> a -> Html a
sessionButton people open =
  HE.button
    [ HA.class' "flex min-h-[2.75rem] items-center rounded-lg border-2 border-ink bg-white px-2 shadow-pop transition active:translate-y-0.5 active:shadow-none focus:outline-none focus-visible:ring-4 focus-visible:ring-star"
    , HA.onClick open
    , HA.createAttribute "aria-label" ("Session: " <> summary everyone)
    ]
    ( [ HE.span [ HA.class' "flex gap-1", HA.createAttribute "aria-hidden" "true" ] (map avatar (take shown (labelled everyone))) ]
        <> if length everyone > shown then [ HE.span [ HA.class' "pl-1 text-xs font-bold", HA.createAttribute "aria-hidden" "true" ] [ HE.text ("+" <> show (length (drop shown everyone))) ] ] else []
    )
  where
  everyone = orYou people
  shown = 3

-- | The user, before the first heartbeat says who else is here
orYou :: Array Person -> Array Person
orYou [] = [ { you: true, status: Swiping, swiped: 0, left: Nothing } ]
orYou people = people

-- | A ring filled as far as they've got, round their initials, or a tick
-- | once they're done
avatar :: forall a. { label :: String, short :: String, person :: Person } -> Html a
avatar { short, person } = case person.status of
  Done ->
    HE.span [ HA.class' (circle <> " bg-brand-500 text-white") ] [ icon Check "h-4 w-4" ]
  Away ->
    HE.span [ HA.class' (circle <> " border-2 border-dashed border-ink bg-surface text-[0.65rem] font-bold") ] [ HE.text short ]
  Swiping ->
    HE.span [ HA.class' (circle <> " p-[3px]"), HA.style1 "background" ("conic-gradient(#2a4bff " <> show (percent person) <> "%, #14122b26 0)") ]
      [ HE.span [ HA.class' "grid h-full w-full place-items-center rounded-full bg-white text-[0.65rem] font-bold" ] [ HE.text short ] ]
  where
  circle = "grid h-8 w-8 place-items-center rounded-full"

-- | Who is still swiping, e.g. "Waiting on Friend 2 (12 left)", for when the
-- | user is out of cards. Nothing when nobody is.
waitingOn :: Array Person -> Maybe String
waitingOn everyone = case still of
  [] -> Nothing
  _ -> Just ("Waiting on " <> joinWith ", " (map describe still))
  where
  still = filter (\{ person } -> not person.you && person.status == Swiping) (labelled everyone)
  describe { label, person } = label <> foldMap (\n -> " (" <> show n <> " left)") person.left

-- | e.g. "3 here · 1 done"
summary :: Array Person -> String
summary everyone =
  show (count (_ /= Away)) <> " here" <> if done > 0 then " · " <> show done <> " done" else ""
  where
  count p = length (filter (p <<< _.status) everyone)
  done = count (_ == Done)

-- | "You" for the user, and the others numbered in the order they joined,
-- | with a short form for avatars
labelled :: Array Person -> Array { label :: String, short :: String, person :: Person }
labelled people = (foldl step { next: 1, acc: [] } people).acc
  where
  step { next, acc } p
    | p.you = { next, acc: snoc acc { label: "You", short: "You", person: p } }
    | otherwise = { next: next + 1, acc: snoc acc { label: "Friend " <> show next, short: "F" <> show next, person: p } }

chip :: forall a. { label :: String, short :: String, person :: Person } -> Html a
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

-- | How much of what they were shown they've decided on, as a percentage
percent :: Person -> Int
percent p = case p.left of
  Nothing -> 0
  Just n -> if p.swiped + n == 0 then 0 else round (100.0 * toNumber p.swiped / toNumber (p.swiped + n))

progress :: forall a. Person -> Html a
progress p = case p.left of
  Nothing -> HE.text ""
  Just _ ->
    HE.span [ HA.class' "h-1.5 w-full overflow-hidden rounded-full bg-ink/10", HA.createAttribute "aria-hidden" "true" ]
      [ HE.span [ HA.class' "block h-full rounded-full bg-brand-500", HA.style1 "width" (show (percent p) <> "%") ] [] ]
