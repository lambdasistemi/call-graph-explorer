-- | AES-256-GCM encryption for token storage.
module FFI.Crypto
  ( encrypt
  , decrypt
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import encryptToken
  :: String -> Effect (Promise String)

foreign import decryptToken
  :: String -> Effect (Promise String)

-- | Encrypt a token string. Returns the encrypted
-- | blob as a base64 string.
encrypt :: String -> Aff String
encrypt t = toAffE (encryptToken t)

-- | Decrypt a token blob. Handles migration from
-- | plaintext tokens automatically.
decrypt :: String -> Aff String
decrypt b = toAffE (decryptToken b)
