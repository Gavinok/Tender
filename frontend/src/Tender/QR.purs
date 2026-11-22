module Tender.QR (qrDataUrl) where

import Tender.Types (Url)

-- | A QR code for the text as an SVG data url, usable as an image src
foreign import qrDataUrl :: String -> Url
