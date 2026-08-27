module Digest (digestOf, fnv1a) where

import Data.Bits (shiftR, xor, (.&.))
import Data.Char (intToDigit, ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)

{- | The stable article identifier: FNV-1a over the file name (without
the .md extension), 8 hex digits, git-short-hash style. Collisions are
detected at format time.
-}
digestOf :: Text -> Text
digestOf = fnv1a

{- | FNV-1a 64-bit hash, rendered as 8 hex digits. Good enough for
cache busting and identifiers.
-}
fnv1a :: Text -> Text
fnv1a = T.pack . take 8 . hex . T.foldl' step offset
  where
    offset = 0xcbf29ce484222325 :: Word64
    step h c = (h `xor` fromIntegral (ord c)) * 0x100000001b3
    hex 0 = ""
    hex n = hex (n `shiftR` 4) <> [intToDigit (fromIntegral (n .&. 0xf))]
