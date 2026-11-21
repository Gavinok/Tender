module Tender.View.Lobby (lobby) where


import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Tender.App (Lobby)
import Tender.QR (qrDataUrl)
import Tender.View.Button (Variant(..), button)
import Tender.View.Icon (Icon(..))

-- | Invite people to the session, then start
lobby :: forall a. Lobby -> { share :: a, start :: a } -> Html a
lobby { link, copied } { share, start } =
  HE.div [ HA.class' "flex flex-1 flex-col items-center justify-center gap-6 text-center" ]
    [ HE.h1 [ HA.class' "font-display text-5xl leading-none" ] [ HE.text "Where should we eat?" ]
    , HE.p [ HA.class' "max-w-xs text-lg text-ink/70" ]
        [ HE.text "Get everyone to scan this, then start swiping. Anything you all like is a match." ]
    , HE.figure [ HA.class' "-rotate-2 rounded-3xl border-[3px] border-ink bg-white p-4 shadow-card" ]
        [ HE.img [ HA.src (qrDataUrl link), HA.alt "QR code to join this session", HA.class' "block h-52 w-52" ]
        , HE.p [ HA.class' "w-52 truncate pt-2 font-mono text-xs text-ink/50", HA.title link ] [ HE.text link ]
        ]
    , HE.div [ HA.class' "flex flex-col items-center gap-3" ]
        [ if copied then button Secondary (Just Check) "Link copied!" share
          else button Secondary (Just Share) "Share invite" share
        , button Primary (Just Heart) "Start swiping" start
        ]
    ]
